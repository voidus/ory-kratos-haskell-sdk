{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE ViewPatterns #-}
{-# OPTIONS_GHC -fno-warn-unused-binds -fno-warn-unused-imports -freduction-depth=328 #-}

module OryKratos.API
  ( -- * Client and Server
    Config (..),
    OryKratosBackend (..),
    createOryKratosClient,
    runOryKratosServer,
    runOryKratosMiddlewareServer,
    runOryKratosClient,
    runOryKratosClientWithManager,
    callOryKratos,
    OryKratosClient,
    OryKratosClientError (..),

    -- ** Servant
    OryKratosAPI,

    -- ** Plain WAI Application
    serverWaiApplicationOryKratos,
  )
where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Coerce (coerce)
import Data.Data (Data)
import Data.Function ((&))
import qualified Data.Map as Map
import Data.Monoid ((<>))
import Data.Proxy (Proxy (..))
import Data.Set (Set)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time
import Data.UUID (UUID)
import GHC.Exts (IsString (..))
import GHC.Generics (Generic)
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.HTTP.Types.Method (methodOptions)
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import OryKratos.Types
import Servant (ServerError, serve)
import Servant.API
import Servant.API.Verbs (StdMethod (..), Verb)
import Servant.Client
  ( ClientEnv,
    ClientError,
    Scheme (Http),
    client,
    mkClientEnv,
    parseBaseUrl,
  )
import Servant.Client.Core (baseUrlHost, baseUrlPort)
import Servant.Client.Internal.HttpClient (ClientM (..))
import Servant.Server (Application, Handler (..))
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Web.FormUrlEncoded
import Web.HttpApiData

-- | List of elements parsed from a query.
newtype QueryList (p :: CollectionFormat) a = QueryList
  { fromQueryList :: [a]
  }
  deriving stock (Traversable)
  deriving newtype (Functor, Applicative, Monad, Foldable)

-- | Formats in which a list can be encoded into a HTTP path.
data CollectionFormat
  = -- | CSV format for multiple parameters.
    CommaSeparated
  | -- | Also called "SSV"
    SpaceSeparated
  | -- | Also called "TSV"
    TabSeparated
  | -- | `value1|value2|value2`
    PipeSeparated
  | -- | Using multiple GET parameters, e.g. `foo=bar&foo=baz`. Only for GET params.
    MultiParamArray

instance FromHttpApiData a => FromHttpApiData (QueryList 'CommaSeparated a) where
  parseQueryParam = parseSeparatedQueryList ','

instance FromHttpApiData a => FromHttpApiData (QueryList 'TabSeparated a) where
  parseQueryParam = parseSeparatedQueryList '\t'

instance FromHttpApiData a => FromHttpApiData (QueryList 'SpaceSeparated a) where
  parseQueryParam = parseSeparatedQueryList ' '

instance FromHttpApiData a => FromHttpApiData (QueryList 'PipeSeparated a) where
  parseQueryParam = parseSeparatedQueryList '|'

instance FromHttpApiData a => FromHttpApiData (QueryList 'MultiParamArray a) where
  parseQueryParam = Prelude.error "unimplemented FromHttpApiData for MultiParamArray collection format"

parseSeparatedQueryList :: FromHttpApiData a => Char -> Text -> Either Text (QueryList p a)
parseSeparatedQueryList char = fmap QueryList . mapM parseQueryParam . T.split (== char)

instance ToHttpApiData a => ToHttpApiData (QueryList 'CommaSeparated a) where
  toQueryParam = formatSeparatedQueryList ','

instance ToHttpApiData a => ToHttpApiData (QueryList 'TabSeparated a) where
  toQueryParam = formatSeparatedQueryList '\t'

instance ToHttpApiData a => ToHttpApiData (QueryList 'SpaceSeparated a) where
  toQueryParam = formatSeparatedQueryList ' '

instance ToHttpApiData a => ToHttpApiData (QueryList 'PipeSeparated a) where
  toQueryParam = formatSeparatedQueryList '|'

instance ToHttpApiData a => ToHttpApiData (QueryList 'MultiParamArray a) where
  toQueryParam = Prelude.error "unimplemented ToHttpApiData for MultiParamArray collection format"

formatSeparatedQueryList :: ToHttpApiData a => Char -> QueryList p a -> Text
formatSeparatedQueryList char = T.intercalate (T.singleton char) . map toQueryParam . fromQueryList

-- | Servant type-level API, generated from the OpenAPI spec for OryKratos.
type OryKratosAPI traits metadataAdmin metadataPublic =
  "version" :> Verb 'GET 200 '[JSON] GetVersion200Response -- 'getVersion' route
    :<|> "health" :> "alive" :> Verb 'GET 200 '[JSON] IsAlive200Response -- 'isAlive' route
    :<|> "health" :> "ready" :> Verb 'GET 200 '[JSON] IsAlive200Response -- 'isReady' route
    :<|> "admin" :> "identities" :> ReqBody '[JSON] AdminCreateIdentityBody :> Verb 'POST 200 '[JSON] (Identity traits metadataAdmin metadataPublic) -- 'adminCreateIdentity' route
    :<|> "admin" :> "recovery" :> "link" :> ReqBody '[JSON] AdminCreateSelfServiceRecoveryLinkBody :> Verb 'POST 200 '[JSON] SelfServiceRecoveryLink -- 'adminCreateSelfServiceRecoveryLink' route
    :<|> "admin" :> "identities" :> Capture "id" Text :> Verb 'DELETE 200 '[JSON] NoContent -- 'adminDeleteIdentity' route
    :<|> "admin" :> "identities" :> Capture "id" Text :> "sessions" :> Verb 'DELETE 200 '[JSON] NoContent -- 'adminDeleteIdentitySessions' route
    :<|> "admin" :> "sessions" :> Capture "id" Text :> "extend" :> Verb 'PATCH 200 '[JSON] (Session traits metadataAdmin metadataPublic) -- 'adminExtendSession' route
    :<|> "admin" :> "identities" :> Capture "id" Text :> QueryParam "include_credential" (QueryList 'MultiParamArray (Text)) :> Verb 'GET 200 '[JSON] (Identity traits metadataAdmin metadataPublic) -- 'adminGetIdentity' route
    :<|> "admin" :> "identities" :> QueryParam "per_page" Integer :> QueryParam "page" Integer :> Verb 'GET 200 '[JSON] [(Identity traits metadataAdmin metadataPublic)] -- 'adminListIdentities' route
    :<|> "admin" :> "identities" :> Capture "id" Text :> "sessions" :> QueryParam "per_page" Integer :> QueryParam "page" Integer :> QueryParam "active" Bool :> Verb 'GET 200 '[JSON] [(Session traits metadataAdmin metadataPublic)] -- 'adminListIdentitySessions' route
    :<|> "admin" :> "identities" :> Capture "id" Text :> ReqBody '[JSON] AdminUpdateIdentityBody :> Verb 'PUT 200 '[JSON] (Identity traits metadataAdmin metadataPublic) -- 'adminUpdateIdentity' route
    :<|> "self-service" :> "logout" :> "browser" :> Header "cookie" Text :> Verb 'GET 200 '[JSON] SelfServiceLogoutUrl -- 'createSelfServiceLogoutFlowUrlForBrowsers' route
    :<|> "schemas" :> Capture "id" Text :> Verb 'GET 200 '[JSON] Value -- 'getJsonSchema' route
    :<|> "self-service" :> "errors" :> QueryParam "id" Text :> Verb 'GET 200 '[JSON] SelfServiceError -- 'getSelfServiceError' route
    :<|> "self-service" :> "login" :> "flows" :> QueryParam "id" Text :> Header "Cookie" Text :> Verb 'GET 200 '[JSON] SelfServiceLoginFlow -- 'getSelfServiceLoginFlow' route
    :<|> "self-service" :> "recovery" :> "flows" :> QueryParam "id" Text :> Header "Cookie" Text :> Verb 'GET 200 '[JSON] SelfServiceRecoveryFlow -- 'getSelfServiceRecoveryFlow' route
    :<|> "self-service" :> "registration" :> "flows" :> QueryParam "id" Text :> Header "Cookie" Text :> Verb 'GET 200 '[JSON] SelfServiceRegistrationFlow -- 'getSelfServiceRegistrationFlow' route
    :<|> "self-service" :> "settings" :> "flows" :> QueryParam "id" Text :> Header "X-Session-Token" Text :> Header "Cookie" Text :> Verb 'GET 200 '[JSON] (SelfServiceSettingsFlow traits metadataAdmin metadataPublic) -- 'getSelfServiceSettingsFlow' route
    :<|> "self-service" :> "verification" :> "flows" :> QueryParam "id" Text :> Header "cookie" Text :> Verb 'GET 200 '[JSON] SelfServiceVerificationFlow -- 'getSelfServiceVerificationFlow' route
    :<|> ".well-known" :> "ory" :> "webauthn.js" :> Verb 'GET 200 '[JSON] Text -- 'getWebAuthnJavaScript' route
    :<|> "self-service" :> "login" :> "browser" :> QueryParam "refresh" Bool :> QueryParam "aal" Text :> QueryParam "return_to" Text :> Verb 'GET 200 '[JSON] SelfServiceLoginFlow -- 'initializeSelfServiceLoginFlowForBrowsers' route
    :<|> "self-service" :> "login" :> "api" :> QueryParam "refresh" Bool :> QueryParam "aal" Text :> Header "X-Session-Token" Text :> Verb 'GET 200 '[JSON] SelfServiceLoginFlow -- 'initializeSelfServiceLoginFlowWithoutBrowser' route
    :<|> "self-service" :> "recovery" :> "browser" :> QueryParam "return_to" Text :> Verb 'GET 200 '[JSON] SelfServiceRecoveryFlow -- 'initializeSelfServiceRecoveryFlowForBrowsers' route
    :<|> "self-service" :> "recovery" :> "api" :> Verb 'GET 200 '[JSON] SelfServiceRecoveryFlow -- 'initializeSelfServiceRecoveryFlowWithoutBrowser' route
    :<|> "self-service" :> "registration" :> "browser" :> QueryParam "return_to" Text :> Verb 'GET 200 '[JSON] SelfServiceRegistrationFlow -- 'initializeSelfServiceRegistrationFlowForBrowsers' route
    :<|> "self-service" :> "registration" :> "api" :> Verb 'GET 200 '[JSON] SelfServiceRegistrationFlow -- 'initializeSelfServiceRegistrationFlowWithoutBrowser' route
    :<|> "self-service" :> "settings" :> "browser" :> QueryParam "return_to" Text :> Verb 'GET 200 '[JSON] (SelfServiceSettingsFlow traits metadataAdmin metadataPublic) -- 'initializeSelfServiceSettingsFlowForBrowsers' route
    :<|> "self-service" :> "settings" :> "api" :> Header "X-Session-Token" Text :> Verb 'GET 200 '[JSON] (SelfServiceSettingsFlow traits metadataAdmin metadataPublic) -- 'initializeSelfServiceSettingsFlowWithoutBrowser' route
    :<|> "self-service" :> "verification" :> "browser" :> QueryParam "return_to" Text :> Verb 'GET 200 '[JSON] SelfServiceVerificationFlow -- 'initializeSelfServiceVerificationFlowForBrowsers' route
    :<|> "self-service" :> "verification" :> "api" :> Verb 'GET 200 '[JSON] SelfServiceVerificationFlow -- 'initializeSelfServiceVerificationFlowWithoutBrowser' route
    :<|> "schemas" :> QueryParam "per_page" Integer :> QueryParam "page" Integer :> Verb 'GET 200 '[JSON] [IdentitySchema] -- 'listIdentitySchemas' route
    :<|> "sessions" :> QueryParam "per_page" Integer :> QueryParam "page" Integer :> Header "X-Session-Token" Text :> Header "Cookie" Text :> Verb 'GET 200 '[JSON] [(Session traits metadataAdmin metadataPublic)] -- 'listSessions' route
    :<|> "sessions" :> Capture "id" Text :> Verb 'DELETE 200 '[JSON] NoContent -- 'revokeSession' route
    :<|> "sessions" :> Header "X-Session-Token" Text :> Header "Cookie" Text :> Verb 'DELETE 200 '[JSON] RevokedSessions -- 'revokeSessions' route
    :<|> "self-service" :> "login" :> QueryParam "flow" Text :> ReqBody '[JSON] SubmitSelfServiceLoginFlowBody :> Header "X-Session-Token" Text :> Header "Cookie" Text :> Verb 'POST 200 '[JSON] (SuccessfulSelfServiceLoginWithoutBrowser traits metadataAdmin metadataPublic) -- 'submitSelfServiceLoginFlow' route
    :<|> "self-service" :> "logout" :> QueryParam "token" Text :> QueryParam "return_to" Text :> Verb 'GET 200 '[JSON] NoContent -- 'submitSelfServiceLogoutFlow' route
    :<|> "self-service" :> "logout" :> "api" :> ReqBody '[JSON] SubmitSelfServiceLogoutFlowWithoutBrowserBody :> Verb 'DELETE 200 '[JSON] NoContent -- 'submitSelfServiceLogoutFlowWithoutBrowser' route
    :<|> "self-service" :> "recovery" :> QueryParam "flow" Text :> QueryParam "token" Text :> ReqBody '[JSON] SubmitSelfServiceRecoveryFlowBody :> Header "Cookie" Text :> Verb 'POST 200 '[JSON] SelfServiceRecoveryFlow -- 'submitSelfServiceRecoveryFlow' route
    :<|> "self-service" :> "registration" :> QueryParam "flow" Text :> ReqBody '[JSON] SubmitSelfServiceRegistrationFlowBody :> Header "Cookie" Text :> Verb 'POST 200 '[JSON] (SuccessfulSelfServiceRegistrationWithoutBrowser traits metadataAdmin metadataPublic) -- 'submitSelfServiceRegistrationFlow' route
    :<|> "self-service" :> "settings" :> QueryParam "flow" Text :> ReqBody '[JSON] SubmitSelfServiceSettingsFlowBody :> Header "X-Session-Token" Text :> Header "Cookie" Text :> Verb 'POST 200 '[JSON] (SelfServiceSettingsFlow traits metadataAdmin metadataPublic) -- 'submitSelfServiceSettingsFlow' route
    :<|> "self-service" :> "verification" :> QueryParam "flow" Text :> QueryParam "token" Text :> ReqBody '[JSON] SubmitSelfServiceVerificationFlowBody :> Header "Cookie" Text :> Verb 'POST 200 '[JSON] SelfServiceVerificationFlow -- 'submitSelfServiceVerificationFlow' route
    :<|> "sessions" :> "whoami" :> Header "X-Session-Token" Text :> Header "Cookie" Text :> Verb 'GET 200 '[JSON] (Session traits metadataAdmin metadataPublic) -- 'toSession' route
    :<|> Raw

-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { -- | scheme://hostname:port/path, e.g. "http://localhost:8080/"
    configUrl :: String
  }
  deriving stock (Eq, Ord, Show, Read)

-- | Custom exception type for our Prelude.errors.
newtype OryKratosClientError = OryKratosClientError ClientError
  deriving newtype (Show, Exception)

-- | Configuration, specifying the full url of the service.

-- | Backend for OryKratos.
-- The backend can be used both for the client and the server. The client generated from the OryKratos OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createOryKratosClient@). Alternatively, provided
-- a backend, the API can be served using @runOryKratosMiddlewareServer@.
data OryKratosBackend m traits metadataAdmin metadataPublic = OryKratosBackend
  { -- | This endpoint returns the version of Ory Kratos.  If the service supports TLS Edge Termination, this endpoint does not require the `X-Forwarded-Proto` header to be set.  Be aware that if you are running multiple nodes of this service, the version will never refer to the cluster state, only to a single instance.
    getVersion :: m GetVersion200Response,
    -- | This endpoint returns a HTTP 200 status code when Ory Kratos is accepting incoming HTTP requests. This status does currently not include checks whether the database connection is working.  If the service supports TLS Edge Termination, this endpoint does not require the `X-Forwarded-Proto` header to be set.  Be aware that if you are running multiple nodes of this service, the health status will never refer to the cluster state, only to a single instance.
    isAlive :: m IsAlive200Response,
    -- | This endpoint returns a HTTP 200 status code when Ory Kratos is up running and the environment dependencies (e.g. the database) are responsive as well.  If the service supports TLS Edge Termination, this endpoint does not require the `X-Forwarded-Proto` header to be set.  Be aware that if you are running multiple nodes of Ory Kratos, the health status will never refer to the cluster state, only to a single instance.
    isReady :: m IsAlive200Response,
    -- | This endpoint creates an identity. Learn how identities work in [Ory Kratos' User And Identity Model Documentation](https://www.ory.sh/docs/next/kratos/concepts/identity-user-model).
    adminCreateIdentity :: AdminCreateIdentityBody -> m (Identity traits metadataAdmin metadataPublic),
    -- | This endpoint creates a recovery link which should be given to the user in order for them to recover (or activate) their account.
    adminCreateSelfServiceRecoveryLink :: AdminCreateSelfServiceRecoveryLinkBody -> m SelfServiceRecoveryLink,
    -- | Calling this endpoint irrecoverably and permanently deletes the identity given its ID. This action can not be undone. This endpoint returns 204 when the identity was deleted or when the identity was not found, in which case it is assumed that is has been deleted already.  Learn how identities work in [Ory Kratos' User And Identity Model Documentation](https://www.ory.sh/docs/next/kratos/concepts/identity-user-model).
    adminDeleteIdentity :: Text -> m NoContent,
    -- | This endpoint is useful for:  To forcefully logout Identity from all devices and sessions
    adminDeleteIdentitySessions :: Text -> m NoContent,
    -- | Retrieve the session ID from the `/sessions/whoami` endpoint / `toSession` SDK method.
    adminExtendSession :: Text -> m (Session traits metadataAdmin metadataPublic),
    -- | Learn how identities work in [Ory Kratos' User And Identity Model Documentation](https://www.ory.sh/docs/next/kratos/concepts/identity-user-model).
    adminGetIdentity :: Text -> Maybe [Text] -> m (Identity traits metadataAdmin metadataPublic),
    -- | Lists all identities. Does not support search at the moment.  Learn how identities work in [Ory Kratos' User And Identity Model Documentation](https://www.ory.sh/docs/next/kratos/concepts/identity-user-model).
    adminListIdentities :: Maybe Integer -> Maybe Integer -> m [(Identity traits metadataAdmin metadataPublic)],
    -- | This endpoint is useful for:  Listing all sessions that belong to an Identity in an administrative context.
    adminListIdentitySessions :: Text -> Maybe Integer -> Maybe Integer -> Maybe Bool -> m [(Session traits metadataAdmin metadataPublic)],
    -- | This endpoint updates an identity. The full identity payload (except credentials) is expected. This endpoint does not support patching.  Learn how identities work in [Ory Kratos' User And Identity Model Documentation](https://www.ory.sh/docs/next/kratos/concepts/identity-user-model).
    adminUpdateIdentity :: Text -> AdminUpdateIdentityBody -> m (Identity traits metadataAdmin metadataPublic),
    -- | This endpoint initializes a browser-based user logout flow and a URL which can be used to log out the user.  This endpoint is NOT INTENDED for API clients and only works with browsers (Chrome, Firefox, ...). For API clients you can call the `/self-service/logout/api` URL directly with the Ory Session Token.  The URL is only valid for the currently signed in user. If no user is signed in, this endpoint returns a 401 Prelude.error.  When calling this endpoint from a backend, please ensure to properly forward the HTTP cookies.
    createSelfServiceLogoutFlowUrlForBrowsers :: Maybe Text -> m SelfServiceLogoutUrl,
    -- | Get a JSON Schema
    getJsonSchema :: Text -> m Value,
    -- | This endpoint returns the Prelude.error associated with a user-facing self service Prelude.errors.  This endpoint supports stub values to help you implement the Prelude.error UI:  `?id=stub:500` - returns a stub 500 (Internal Server Error) Prelude.error.  More information can be found at [Ory Kratos User User Facing Error Documentation](https://www.ory.sh/docs/kratos/self-service/flows/user-facing-errors).
    getSelfServiceError :: Maybe Text -> m SelfServiceError,
    -- | This endpoint returns a login flow's context with, for example, Prelude.error details and other information.  Browser flows expect the anti-CSRF cookie to be included in the request's HTTP Cookie Header. For AJAX requests you must ensure that cookies are included in the request or requests will fail.  If you use the browser-flow for server-side apps, the services need to run on a common top-level-domain and you need to forward the incoming HTTP Cookie header to this endpoint:  ```js pseudo-code example router.get('/login', async function (req, res) { const flow = await client.getSelfServiceLoginFlow(req.header('cookie'), req.query['flow'])  res.render('login', flow) }) ```  This request may fail due to several reasons. The `error.id` can be one of:  `session_already_available`: The user is already signed in. `self_service_flow_expired`: The flow is expired and you should request a new one.  More information can be found at [Ory Kratos User Login](https://www.ory.sh/docs/kratos/self-service/flows/user-login) and [User Registration Documentation](https://www.ory.sh/docs/kratos/self-service/flows/user-registration).
    getSelfServiceLoginFlow :: Maybe Text -> Maybe Text -> m SelfServiceLoginFlow,
    -- | This endpoint returns a recovery flow's context with, for example, Prelude.error details and other information.  Browser flows expect the anti-CSRF cookie to be included in the request's HTTP Cookie Header. For AJAX requests you must ensure that cookies are included in the request or requests will fail.  If you use the browser-flow for server-side apps, the services need to run on a common top-level-domain and you need to forward the incoming HTTP Cookie header to this endpoint:  ```js pseudo-code example router.get('/recovery', async function (req, res) { const flow = await client.getSelfServiceRecoveryFlow(req.header('Cookie'), req.query['flow'])  res.render('recovery', flow) }) ```  More information can be found at [Ory Kratos Account Recovery Documentation](../self-service/flows/account-recovery).
    getSelfServiceRecoveryFlow :: Maybe Text -> Maybe Text -> m SelfServiceRecoveryFlow,
    -- | This endpoint returns a registration flow's context with, for example, Prelude.error details and other information.  Browser flows expect the anti-CSRF cookie to be included in the request's HTTP Cookie Header. For AJAX requests you must ensure that cookies are included in the request or requests will fail.  If you use the browser-flow for server-side apps, the services need to run on a common top-level-domain and you need to forward the incoming HTTP Cookie header to this endpoint:  ```js pseudo-code example router.get('/registration', async function (req, res) { const flow = await client.getSelfServiceRegistrationFlow(req.header('cookie'), req.query['flow'])  res.render('registration', flow) }) ```  This request may fail due to several reasons. The `error.id` can be one of:  `session_already_available`: The user is already signed in. `self_service_flow_expired`: The flow is expired and you should request a new one.  More information can be found at [Ory Kratos User Login](https://www.ory.sh/docs/kratos/self-service/flows/user-login) and [User Registration Documentation](https://www.ory.sh/docs/kratos/self-service/flows/user-registration).
    getSelfServiceRegistrationFlow :: Maybe Text -> Maybe Text -> m SelfServiceRegistrationFlow,
    -- | When accessing this endpoint through Ory Kratos' Public API you must ensure that either the Ory Kratos Session Cookie or the Ory Kratos Session Token are set.  Depending on your configuration this endpoint might return a 403 Prelude.error if the session has a lower Authenticator Assurance Level (AAL) than is possible for the identity. This can happen if the identity has password + webauthn credentials (which would result in AAL2) but the session has only AAL1. If this Prelude.error occurs, ask the user to sign in with the second factor or change the configuration.  You can access this endpoint without credentials when using Ory Kratos' Admin API.  If this endpoint is called via an AJAX request, the response contains the flow without a redirect. In the case of an Prelude.error, the `error.id` of the JSON response body can be one of:  `security_csrf_violation`: Unable to fetch the flow because a CSRF violation occurred. `session_inactive`: No Ory Session was found - sign in a user first. `security_identity_mismatch`: The flow was interrupted with `session_refresh_required` but apparently some other identity logged in instead.  More information can be found at [Ory Kratos User Settings & Profile Management Documentation](../self-service/flows/user-settings).
    getSelfServiceSettingsFlow :: Maybe Text -> Maybe Text -> Maybe Text -> m (SelfServiceSettingsFlow traits metadataAdmin metadataPublic),
    -- | This endpoint returns a verification flow's context with, for example, Prelude.error details and other information.  Browser flows expect the anti-CSRF cookie to be included in the request's HTTP Cookie Header. For AJAX requests you must ensure that cookies are included in the request or requests will fail.  If you use the browser-flow for server-side apps, the services need to run on a common top-level-domain and you need to forward the incoming HTTP Cookie header to this endpoint:  ```js pseudo-code example router.get('/recovery', async function (req, res) { const flow = await client.getSelfServiceVerificationFlow(req.header('cookie'), req.query['flow'])  res.render('verification', flow) })  More information can be found at [Ory Kratos Email and Phone Verification Documentation](https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation).
    getSelfServiceVerificationFlow :: Maybe Text -> Maybe Text -> m SelfServiceVerificationFlow,
    -- | This endpoint provides JavaScript which is needed in order to perform WebAuthn login and registration.  If you are building a JavaScript Browser App (e.g. in ReactJS or AngularJS) you will need to load this file:  ```html <script src=\"https://public-kratos.example.org/.well-known/ory/webauthn.js\" type=\"script\" async /> ```  More information can be found at [Ory Kratos User Login](https://www.ory.sh/docs/kratos/self-service/flows/user-login) and [User Registration Documentation](https://www.ory.sh/docs/kratos/self-service/flows/user-registration).
    getWebAuthnJavaScript :: m Text,
    -- | This endpoint initializes a browser-based user login flow. This endpoint will set the appropriate cookies and anti-CSRF measures required for browser-based flows.  If this endpoint is opened as a link in the browser, it will be redirected to `selfservice.flows.login.ui_url` with the flow ID set as the query parameter `?flow=`. If a valid user session exists already, the browser will be redirected to `urls.default_redirect_url` unless the query parameter `?refresh=true` was set.  If this endpoint is called via an AJAX request, the response contains the flow without a redirect. In the case of an Prelude.error, the `error.id` of the JSON response body can be one of:  `session_already_available`: The user is already signed in. `session_aal1_required`: Multi-factor auth (e.g. 2fa) was requested but the user has no session yet. `security_csrf_violation`: Unable to fetch the flow because a CSRF violation occurred. `security_identity_mismatch`: The requested `?return_to` address is not allowed to be used. Adjust this in the configuration!  This endpoint is NOT INTENDED for clients that do not have a browser (Chrome, Firefox, ...) as cookies are needed.  More information can be found at [Ory Kratos User Login](https://www.ory.sh/docs/kratos/self-service/flows/user-login) and [User Registration Documentation](https://www.ory.sh/docs/kratos/self-service/flows/user-registration).
    initializeSelfServiceLoginFlowForBrowsers :: Maybe Bool -> Maybe Text -> Maybe Text -> m SelfServiceLoginFlow,
    -- | This endpoint initiates a login flow for API clients that do not use a browser, such as mobile devices, smart TVs, and so on.  If a valid provided session cookie or session token is provided, a 400 Bad Request Prelude.error will be returned unless the URL query parameter `?refresh=true` is set.  To fetch an existing login flow call `/self-service/login/flows?flow=<flow_id>`.  You MUST NOT use this endpoint in client-side (Single Page Apps, ReactJS, AngularJS) nor server-side (Java Server Pages, NodeJS, PHP, Golang, ...) browser applications. Using this endpoint in these applications will make you vulnerable to a variety of CSRF attacks, including CSRF login attacks.  In the case of an Prelude.error, the `error.id` of the JSON response body can be one of:  `session_already_available`: The user is already signed in. `session_aal1_required`: Multi-factor auth (e.g. 2fa) was requested but the user has no session yet. `security_csrf_violation`: Unable to fetch the flow because a CSRF violation occurred.  This endpoint MUST ONLY be used in scenarios such as native mobile apps (React Native, Objective C, Swift, Java, ...).  More information can be found at [Ory Kratos User Login](https://www.ory.sh/docs/kratos/self-service/flows/user-login) and [User Registration Documentation](https://www.ory.sh/docs/kratos/self-service/flows/user-registration).
    initializeSelfServiceLoginFlowWithoutBrowser :: Maybe Bool -> Maybe Text -> Maybe Text -> m SelfServiceLoginFlow,
    -- | This endpoint initializes a browser-based account recovery flow. Once initialized, the browser will be redirected to `selfservice.flows.recovery.ui_url` with the flow ID set as the query parameter `?flow=`. If a valid user session exists, the browser is returned to the configured return URL.  If this endpoint is called via an AJAX request, the response contains the recovery flow without any redirects or a 400 bad request Prelude.error if the user is already authenticated.  This endpoint is NOT INTENDED for clients that do not have a browser (Chrome, Firefox, ...) as cookies are needed.  More information can be found at [Ory Kratos Account Recovery Documentation](../self-service/flows/account-recovery).
    initializeSelfServiceRecoveryFlowForBrowsers :: Maybe Text -> m SelfServiceRecoveryFlow,
    -- | This endpoint initiates a recovery flow for API clients such as mobile devices, smart TVs, and so on.  If a valid provided session cookie or session token is provided, a 400 Bad Request Prelude.error.  To fetch an existing recovery flow call `/self-service/recovery/flows?flow=<flow_id>`.  You MUST NOT use this endpoint in client-side (Single Page Apps, ReactJS, AngularJS) nor server-side (Java Server Pages, NodeJS, PHP, Golang, ...) browser applications. Using this endpoint in these applications will make you vulnerable to a variety of CSRF attacks.  This endpoint MUST ONLY be used in scenarios such as native mobile apps (React Native, Objective C, Swift, Java, ...).   More information can be found at [Ory Kratos Account Recovery Documentation](../self-service/flows/account-recovery).
    initializeSelfServiceRecoveryFlowWithoutBrowser :: m SelfServiceRecoveryFlow,
    -- | This endpoint initializes a browser-based user registration flow. This endpoint will set the appropriate cookies and anti-CSRF measures required for browser-based flows.  :::info  This endpoint is EXPERIMENTAL and subject to potential breaking changes in the future.  :::  If this endpoint is opened as a link in the browser, it will be redirected to `selfservice.flows.registration.ui_url` with the flow ID set as the query parameter `?flow=`. If a valid user session exists already, the browser will be redirected to `urls.default_redirect_url`.  If this endpoint is called via an AJAX request, the response contains the flow without a redirect. In the case of an Prelude.error, the `error.id` of the JSON response body can be one of:  `session_already_available`: The user is already signed in. `security_csrf_violation`: Unable to fetch the flow because a CSRF violation occurred. `security_identity_mismatch`: The requested `?return_to` address is not allowed to be used. Adjust this in the configuration!  If this endpoint is called via an AJAX request, the response contains the registration flow without a redirect.  This endpoint is NOT INTENDED for clients that do not have a browser (Chrome, Firefox, ...) as cookies are needed.  More information can be found at [Ory Kratos User Login](https://www.ory.sh/docs/kratos/self-service/flows/user-login) and [User Registration Documentation](https://www.ory.sh/docs/kratos/self-service/flows/user-registration).
    initializeSelfServiceRegistrationFlowForBrowsers :: Maybe Text -> m SelfServiceRegistrationFlow,
    -- | This endpoint initiates a registration flow for API clients such as mobile devices, smart TVs, and so on.  If a valid provided session cookie or session token is provided, a 400 Bad Request Prelude.error will be returned unless the URL query parameter `?refresh=true` is set.  To fetch an existing registration flow call `/self-service/registration/flows?flow=<flow_id>`.  You MUST NOT use this endpoint in client-side (Single Page Apps, ReactJS, AngularJS) nor server-side (Java Server Pages, NodeJS, PHP, Golang, ...) browser applications. Using this endpoint in these applications will make you vulnerable to a variety of CSRF attacks.  In the case of an Prelude.error, the `error.id` of the JSON response body can be one of:  `session_already_available`: The user is already signed in. `security_csrf_violation`: Unable to fetch the flow because a CSRF violation occurred.  This endpoint MUST ONLY be used in scenarios such as native mobile apps (React Native, Objective C, Swift, Java, ...).  More information can be found at [Ory Kratos User Login](https://www.ory.sh/docs/kratos/self-service/flows/user-login) and [User Registration Documentation](https://www.ory.sh/docs/kratos/self-service/flows/user-registration).
    initializeSelfServiceRegistrationFlowWithoutBrowser :: m SelfServiceRegistrationFlow,
    -- | This endpoint initializes a browser-based user settings flow. Once initialized, the browser will be redirected to `selfservice.flows.settings.ui_url` with the flow ID set as the query parameter `?flow=`. If no valid Ory Kratos Session Cookie is included in the request, a login flow will be initialized.  If this endpoint is opened as a link in the browser, it will be redirected to `selfservice.flows.settings.ui_url` with the flow ID set as the query parameter `?flow=`. If no valid user session was set, the browser will be redirected to the login endpoint.  If this endpoint is called via an AJAX request, the response contains the settings flow without any redirects or a 401 forbidden Prelude.error if no valid session was set.  Depending on your configuration this endpoint might return a 403 Prelude.error if the session has a lower Authenticator Assurance Level (AAL) than is possible for the identity. This can happen if the identity has password + webauthn credentials (which would result in AAL2) but the session has only AAL1. If this Prelude.error occurs, ask the user to sign in with the second factor (happens automatically for server-side browser flows) or change the configuration.  If this endpoint is called via an AJAX request, the response contains the flow without a redirect. In the case of an Prelude.error, the `error.id` of the JSON response body can be one of:  `security_csrf_violation`: Unable to fetch the flow because a CSRF violation occurred. `session_inactive`: No Ory Session was found - sign in a user first. `security_identity_mismatch`: The requested `?return_to` address is not allowed to be used. Adjust this in the configuration!  This endpoint is NOT INTENDED for clients that do not have a browser (Chrome, Firefox, ...) as cookies are needed.  More information can be found at [Ory Kratos User Settings & Profile Management Documentation](../self-service/flows/user-settings).
    initializeSelfServiceSettingsFlowForBrowsers :: Maybe Text -> m (SelfServiceSettingsFlow traits metadataAdmin metadataPublic),
    -- | This endpoint initiates a settings flow for API clients such as mobile devices, smart TVs, and so on. You must provide a valid Ory Kratos Session Token for this endpoint to respond with HTTP 200 OK.  To fetch an existing settings flow call `/self-service/settings/flows?flow=<flow_id>`.  You MUST NOT use this endpoint in client-side (Single Page Apps, ReactJS, AngularJS) nor server-side (Java Server Pages, NodeJS, PHP, Golang, ...) browser applications. Using this endpoint in these applications will make you vulnerable to a variety of CSRF attacks.  Depending on your configuration this endpoint might return a 403 Prelude.error if the session has a lower Authenticator Assurance Level (AAL) than is possible for the identity. This can happen if the identity has password + webauthn credentials (which would result in AAL2) but the session has only AAL1. If this Prelude.error occurs, ask the user to sign in with the second factor or change the configuration.  In the case of an Prelude.error, the `error.id` of the JSON response body can be one of:  `security_csrf_violation`: Unable to fetch the flow because a CSRF violation occurred. `session_inactive`: No Ory Session was found - sign in a user first.  This endpoint MUST ONLY be used in scenarios such as native mobile apps (React Native, Objective C, Swift, Java, ...).  More information can be found at [Ory Kratos User Settings & Profile Management Documentation](../self-service/flows/user-settings).
    initializeSelfServiceSettingsFlowWithoutBrowser :: Maybe Text -> m (SelfServiceSettingsFlow traits metadataAdmin metadataPublic),
    -- | This endpoint initializes a browser-based account verification flow. Once initialized, the browser will be redirected to `selfservice.flows.verification.ui_url` with the flow ID set as the query parameter `?flow=`.  If this endpoint is called via an AJAX request, the response contains the recovery flow without any redirects.  This endpoint is NOT INTENDED for API clients and only works with browsers (Chrome, Firefox, ...).  More information can be found at [Ory Kratos Email and Phone Verification Documentation](https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation).
    initializeSelfServiceVerificationFlowForBrowsers :: Maybe Text -> m SelfServiceVerificationFlow,
    -- | This endpoint initiates a verification flow for API clients such as mobile devices, smart TVs, and so on.  To fetch an existing verification flow call `/self-service/verification/flows?flow=<flow_id>`.  You MUST NOT use this endpoint in client-side (Single Page Apps, ReactJS, AngularJS) nor server-side (Java Server Pages, NodeJS, PHP, Golang, ...) browser applications. Using this endpoint in these applications will make you vulnerable to a variety of CSRF attacks.  This endpoint MUST ONLY be used in scenarios such as native mobile apps (React Native, Objective C, Swift, Java, ...).  More information can be found at [Ory Kratos Email and Phone Verification Documentation](https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation).
    initializeSelfServiceVerificationFlowWithoutBrowser :: m SelfServiceVerificationFlow,
    -- | Get all Identity Schemas
    listIdentitySchemas :: Maybe Integer -> Maybe Integer -> m [IdentitySchema],
    -- | This endpoint is useful for:  Displaying all other sessions that belong to the logged-in user
    listSessions :: Maybe Integer -> Maybe Integer -> Maybe Text -> Maybe Text -> m [(Session traits metadataAdmin metadataPublic)],
    -- | This endpoint is useful for:  To forcefully logout the current user from another device or session
    revokeSession :: Text -> m NoContent,
    -- | This endpoint is useful for:  To forcefully logout the current user from all other devices and sessions
    revokeSessions :: Maybe Text -> Maybe Text -> m RevokedSessions,
    -- | :::info  This endpoint is EXPERIMENTAL and subject to potential breaking changes in the future.  :::  Use this endpoint to complete a login flow. This endpoint behaves differently for API and browser flows.  API flows expect `application/json` to be sent in the body and responds with HTTP 200 and a application/json body with the session token on success; HTTP 410 if the original flow expired with the appropriate Prelude.error messages set and optionally a `use_flow_id` parameter in the body; HTTP 400 on form validation Prelude.errors.  Browser flows expect a Content-Type of `application/x-www-form-urlencoded` or `application/json` to be sent in the body and respond with a HTTP 303 redirect to the post/after login URL or the `return_to` value if it was set and if the login succeeded; a HTTP 303 redirect to the login UI URL with the flow ID containing the validation Prelude.errors otherwise.  Browser flows with an accept header of `application/json` will not redirect but instead respond with HTTP 200 and a application/json body with the signed in identity and a `Set-Cookie` header on success; HTTP 303 redirect to a fresh login flow if the original flow expired with the appropriate Prelude.error messages set; HTTP 400 on form validation Prelude.errors.  If this endpoint is called with `Accept: application/json` in the header, the response contains the flow without a redirect. In the case of an Prelude.error, the `error.id` of the JSON response body can be one of:  `session_already_available`: The user is already signed in. `security_csrf_violation`: Unable to fetch the flow because a CSRF violation occurred. `security_identity_mismatch`: The requested `?return_to` address is not allowed to be used. Adjust this in the configuration! `browser_location_change_required`: Usually sent when an AJAX request indicates that the browser needs to open a specific URL. Most likely used in Social Sign In flows.  More information can be found at [Ory Kratos User Login](https://www.ory.sh/docs/kratos/self-service/flows/user-login) and [User Registration Documentation](https://www.ory.sh/docs/kratos/self-service/flows/user-registration).
    submitSelfServiceLoginFlow :: Maybe Text -> SubmitSelfServiceLoginFlowBody -> Maybe Text -> Maybe Text -> m (SuccessfulSelfServiceLoginWithoutBrowser traits metadataAdmin metadataPublic),
    -- | This endpoint logs out an identity in a self-service manner.  If the `Accept` HTTP header is not set to `application/json`, the browser will be redirected (HTTP 303 See Other) to the `return_to` parameter of the initial request or fall back to `urls.default_return_to`.  If the `Accept` HTTP header is set to `application/json`, a 204 No Content response will be sent on successful logout instead.  This endpoint is NOT INTENDED for API clients and only works with browsers (Chrome, Firefox, ...). For API clients you can call the `/self-service/logout/api` URL directly with the Ory Session Token.  More information can be found at [Ory Kratos User Logout Documentation](https://www.ory.sh/docs/next/kratos/self-service/flows/user-logout).
    submitSelfServiceLogoutFlow :: Maybe Text -> Maybe Text -> m NoContent,
    -- | Use this endpoint to log out an identity using an Ory Session Token. If the Ory Session Token was successfully revoked, the server returns a 204 No Content response. A 204 No Content response is also sent when the Ory Session Token has been revoked already before.  If the Ory Session Token is malformed or does not exist a 403 Forbidden response will be returned.  This endpoint does not remove any HTTP Cookies - use the Browser-Based Self-Service Logout Flow instead.
    submitSelfServiceLogoutFlowWithoutBrowser :: SubmitSelfServiceLogoutFlowWithoutBrowserBody -> m NoContent,
    -- | Use this endpoint to complete a recovery flow. This endpoint behaves differently for API and browser flows and has several states:  `choose_method` expects `flow` (in the URL query) and `email` (in the body) to be sent and works with API- and Browser-initiated flows. For API clients and Browser clients with HTTP Header `Accept: application/json` it either returns a HTTP 200 OK when the form is valid and HTTP 400 OK when the form is invalid. and a HTTP 303 See Other redirect with a fresh recovery flow if the flow was otherwise invalid (e.g. expired). For Browser clients without HTTP Header `Accept` or with `Accept: text/*` it returns a HTTP 303 See Other redirect to the Recovery UI URL with the Recovery Flow ID appended. `sent_email` is the success state after `choose_method` for the `link` method and allows the user to request another recovery email. It works for both API and Browser-initiated flows and returns the same responses as the flow in `choose_method` state. `passed_challenge` expects a `token` to be sent in the URL query and given the nature of the flow (\"sending a recovery link\") does not have any API capabilities. The server responds with a HTTP 303 See Other redirect either to the Settings UI URL (if the link was valid) and instructs the user to update their password, or a redirect to the Recover UI URL with a new Recovery Flow ID which contains an Prelude.error message that the recovery link was invalid.  More information can be found at [Ory Kratos Account Recovery Documentation](../self-service/flows/account-recovery).
    submitSelfServiceRecoveryFlow :: Maybe Text -> Maybe Text -> SubmitSelfServiceRecoveryFlowBody -> Maybe Text -> m SelfServiceRecoveryFlow,
    -- | Use this endpoint to complete a registration flow by sending an identity's traits and password. This endpoint behaves differently for API and browser flows.  API flows expect `application/json` to be sent in the body and respond with HTTP 200 and a application/json body with the created identity success - if the session hook is configured the `session` and `session_token` will also be included; HTTP 410 if the original flow expired with the appropriate Prelude.error messages set and optionally a `use_flow_id` parameter in the body; HTTP 400 on form validation Prelude.errors.  Browser flows expect a Content-Type of `application/x-www-form-urlencoded` or `application/json` to be sent in the body and respond with a HTTP 303 redirect to the post/after registration URL or the `return_to` value if it was set and if the registration succeeded; a HTTP 303 redirect to the registration UI URL with the flow ID containing the validation Prelude.errors otherwise.  Browser flows with an accept header of `application/json` will not redirect but instead respond with HTTP 200 and a application/json body with the signed in identity and a `Set-Cookie` header on success; HTTP 303 redirect to a fresh login flow if the original flow expired with the appropriate Prelude.error messages set; HTTP 400 on form validation Prelude.errors.  If this endpoint is called with `Accept: application/json` in the header, the response contains the flow without a redirect. In the case of an Prelude.error, the `error.id` of the JSON response body can be one of:  `session_already_available`: The user is already signed in. `security_csrf_violation`: Unable to fetch the flow because a CSRF violation occurred. `security_identity_mismatch`: The requested `?return_to` address is not allowed to be used. Adjust this in the configuration! `browser_location_change_required`: Usually sent when an AJAX request indicates that the browser needs to open a specific URL. Most likely used in Social Sign In flows.  More information can be found at [Ory Kratos User Login](https://www.ory.sh/docs/kratos/self-service/flows/user-login) and [User Registration Documentation](https://www.ory.sh/docs/kratos/self-service/flows/user-registration).
    submitSelfServiceRegistrationFlow :: Maybe Text -> SubmitSelfServiceRegistrationFlowBody -> Maybe Text -> m (SuccessfulSelfServiceRegistrationWithoutBrowser traits metadataAdmin metadataPublic),
    -- | Use this endpoint to complete a settings flow by sending an identity's updated password. This endpoint behaves differently for API and browser flows.  API-initiated flows expect `application/json` to be sent in the body and respond with HTTP 200 and an application/json body with the session token on success; HTTP 303 redirect to a fresh settings flow if the original flow expired with the appropriate Prelude.error messages set; HTTP 400 on form validation Prelude.errors. HTTP 401 when the endpoint is called without a valid session token. HTTP 403 when `selfservice.flows.settings.privileged_session_max_age` was reached or the session's AAL is too low. Implies that the user needs to re-authenticate.  Browser flows without HTTP Header `Accept` or with `Accept: text/*` respond with a HTTP 303 redirect to the post/after settings URL or the `return_to` value if it was set and if the flow succeeded; a HTTP 303 redirect to the Settings UI URL with the flow ID containing the validation Prelude.errors otherwise. a HTTP 303 redirect to the login endpoint when `selfservice.flows.settings.privileged_session_max_age` was reached or the session's AAL is too low.  Browser flows with HTTP Header `Accept: application/json` respond with HTTP 200 and a application/json body with the signed in identity and a `Set-Cookie` header on success; HTTP 303 redirect to a fresh login flow if the original flow expired with the appropriate Prelude.error messages set; HTTP 401 when the endpoint is called without a valid session cookie. HTTP 403 when the page is accessed without a session cookie or the session's AAL is too low. HTTP 400 on form validation Prelude.errors.  Depending on your configuration this endpoint might return a 403 Prelude.error if the session has a lower Authenticator Assurance Level (AAL) than is possible for the identity. This can happen if the identity has password + webauthn credentials (which would result in AAL2) but the session has only AAL1. If this Prelude.error occurs, ask the user to sign in with the second factor (happens automatically for server-side browser flows) or change the configuration.  If this endpoint is called with a `Accept: application/json` HTTP header, the response contains the flow without a redirect. In the case of an Prelude.error, the `error.id` of the JSON response body can be one of:  `session_refresh_required`: The identity requested to change something that needs a privileged session. Redirect the identity to the login init endpoint with query parameters `?refresh=true&return_to=<the-current-browser-url>`, or initiate a refresh login flow otherwise. `security_csrf_violation`: Unable to fetch the flow because a CSRF violation occurred. `session_inactive`: No Ory Session was found - sign in a user first. `security_identity_mismatch`: The flow was interrupted with `session_refresh_required` but apparently some other identity logged in instead. `security_identity_mismatch`: The requested `?return_to` address is not allowed to be used. Adjust this in the configuration! `browser_location_change_required`: Usually sent when an AJAX request indicates that the browser needs to open a specific URL. Most likely used in Social Sign In flows.  More information can be found at [Ory Kratos User Settings & Profile Management Documentation](../self-service/flows/user-settings).
    submitSelfServiceSettingsFlow :: Maybe Text -> SubmitSelfServiceSettingsFlowBody -> Maybe Text -> Maybe Text -> m (SelfServiceSettingsFlow traits metadataAdmin metadataPublic),
    -- | Use this endpoint to complete a verification flow. This endpoint behaves differently for API and browser flows and has several states:  `choose_method` expects `flow` (in the URL query) and `email` (in the body) to be sent and works with API- and Browser-initiated flows. For API clients and Browser clients with HTTP Header `Accept: application/json` it either returns a HTTP 200 OK when the form is valid and HTTP 400 OK when the form is invalid and a HTTP 303 See Other redirect with a fresh verification flow if the flow was otherwise invalid (e.g. expired). For Browser clients without HTTP Header `Accept` or with `Accept: text/*` it returns a HTTP 303 See Other redirect to the Verification UI URL with the Verification Flow ID appended. `sent_email` is the success state after `choose_method` when using the `link` method and allows the user to request another verification email. It works for both API and Browser-initiated flows and returns the same responses as the flow in `choose_method` state. `passed_challenge` expects a `token` to be sent in the URL query and given the nature of the flow (\"sending a verification link\") does not have any API capabilities. The server responds with a HTTP 303 See Other redirect either to the Settings UI URL (if the link was valid) and instructs the user to update their password, or a redirect to the Verification UI URL with a new Verification Flow ID which contains an Prelude.error message that the verification link was invalid.  More information can be found at [Ory Kratos Email and Phone Verification Documentation](https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation).
    submitSelfServiceVerificationFlow :: Maybe Text -> Maybe Text -> SubmitSelfServiceVerificationFlowBody -> Maybe Text -> m SelfServiceVerificationFlow,
    -- | Uses the HTTP Headers in the GET request to determine (e.g. by using checking the cookies) who is authenticated. Returns a session object in the body or 401 if the credentials are invalid or no credentials were sent. Additionally when the request it successful it adds the user ID to the 'X-Kratos-Authenticated-Identity-Id' header in the response.  If you call this endpoint from a server-side application, you must forward the HTTP Cookie Header to this endpoint:  ```js pseudo-code example router.get('/protected-endpoint', async function (req, res) { const session = await client.toSession(undefined, req.header('cookie'))  console.log(session) }) ```  When calling this endpoint from a non-browser application (e.g. mobile app) you must include the session token:  ```js pseudo-code example ... const session = await client.toSession(\"the-session-token\")  console.log(session) ```  Depending on your configuration this endpoint might return a 403 status code if the session has a lower Authenticator Assurance Level (AAL) than is possible for the identity. This can happen if the identity has password + webauthn credentials (which would result in AAL2) but the session has only AAL1. If this Prelude.error occurs, ask the user to sign in with the second factor or change the configuration.  This endpoint is useful for:  AJAX calls. Remember to send credentials and set up CORS correctly! Reverse proxies and API Gateways Server-side calls - use the `X-Session-Token` header!  This endpoint authenticates users by checking  if the `Cookie` HTTP header was set containing an Ory Kratos Session Cookie; if the `Authorization: bearer <ory-session-token>` HTTP header was set with a valid Ory Kratos Session Token; if the `X-Session-Token` HTTP header was set with a valid Ory Kratos Session Token.  If none of these headers are set or the cooke or token are invalid, the endpoint returns a HTTP 401 status code.  As explained above, this request may fail due to several reasons. The `error.id` can be one of:  `session_inactive`: No active session was found in the request (e.g. no Ory Session Cookie / Ory Session Token). `session_aal2_required`: An active session was found but it does not fulfil the Authenticator Assurance Level, implying that the session must (e.g.) authenticate the second factor.
    toSession :: Maybe Text -> Maybe Text -> m (Session traits metadataAdmin metadataPublic)
  }

newtype OryKratosClient a = OryKratosClient
  { runClient :: ClientEnv -> ExceptT ClientError IO a
  }
  deriving stock (Functor)

instance Applicative OryKratosClient where
  pure x = OryKratosClient (\_ -> pure x)
  (OryKratosClient f) <*> (OryKratosClient x) =
    OryKratosClient (\env -> f env <*> x env)

instance Monad OryKratosClient where
  (OryKratosClient a) >>= f =
    OryKratosClient
      ( \env -> do
          value <- a env
          runClient (f value) env
      )

instance MonadIO OryKratosClient where
  liftIO io = OryKratosClient (\_ -> liftIO io)

createOryKratosClient ::
  forall traits metadataAdmin metadataPublic.
  (FromJSON traits, FromJSON metadataAdmin, FromJSON metadataPublic) =>
  OryKratosBackend OryKratosClient traits metadataAdmin metadataPublic
createOryKratosClient = OryKratosBackend {..}
  where
    ( (coerce -> getVersion)
        :<|> (coerce -> isAlive)
        :<|> (coerce -> isReady)
        :<|> (coerce -> adminCreateIdentity)
        :<|> (coerce -> adminCreateSelfServiceRecoveryLink)
        :<|> (coerce -> adminDeleteIdentity)
        :<|> (coerce -> adminDeleteIdentitySessions)
        :<|> (coerce -> adminExtendSession)
        :<|> (coerce -> adminGetIdentity)
        :<|> (coerce -> adminListIdentities)
        :<|> (coerce -> adminListIdentitySessions)
        :<|> (coerce -> adminUpdateIdentity)
        :<|> (coerce -> createSelfServiceLogoutFlowUrlForBrowsers)
        :<|> (coerce -> getJsonSchema)
        :<|> (coerce -> getSelfServiceError)
        :<|> (coerce -> getSelfServiceLoginFlow)
        :<|> (coerce -> getSelfServiceRecoveryFlow)
        :<|> (coerce -> getSelfServiceRegistrationFlow)
        :<|> (coerce -> getSelfServiceSettingsFlow)
        :<|> (coerce -> getSelfServiceVerificationFlow)
        :<|> (coerce -> getWebAuthnJavaScript)
        :<|> (coerce -> initializeSelfServiceLoginFlowForBrowsers)
        :<|> (coerce -> initializeSelfServiceLoginFlowWithoutBrowser)
        :<|> (coerce -> initializeSelfServiceRecoveryFlowForBrowsers)
        :<|> (coerce -> initializeSelfServiceRecoveryFlowWithoutBrowser)
        :<|> (coerce -> initializeSelfServiceRegistrationFlowForBrowsers)
        :<|> (coerce -> initializeSelfServiceRegistrationFlowWithoutBrowser)
        :<|> (coerce -> initializeSelfServiceSettingsFlowForBrowsers)
        :<|> (coerce -> initializeSelfServiceSettingsFlowWithoutBrowser)
        :<|> (coerce -> initializeSelfServiceVerificationFlowForBrowsers)
        :<|> (coerce -> initializeSelfServiceVerificationFlowWithoutBrowser)
        :<|> (coerce -> listIdentitySchemas)
        :<|> (coerce -> listSessions)
        :<|> (coerce -> revokeSession)
        :<|> (coerce -> revokeSessions)
        :<|> (coerce -> submitSelfServiceLoginFlow)
        :<|> (coerce -> submitSelfServiceLogoutFlow)
        :<|> (coerce -> submitSelfServiceLogoutFlowWithoutBrowser)
        :<|> (coerce -> submitSelfServiceRecoveryFlow)
        :<|> (coerce -> submitSelfServiceRegistrationFlow)
        :<|> (coerce -> submitSelfServiceSettingsFlow)
        :<|> (coerce -> submitSelfServiceVerificationFlow)
        :<|> (coerce -> toSession)
        :<|> _
      ) = client (Proxy :: Proxy (OryKratosAPI traits adminMetadata publicMetadata))

-- | Run requests in the OryKratosClient monad.
runOryKratosClient :: Config -> OryKratosClient a -> ExceptT ClientError IO a
runOryKratosClient clientConfig cl = do
  manager <- liftIO $ newManager tlsManagerSettings
  runOryKratosClientWithManager manager clientConfig cl

-- | Run requests in the OryKratosClient monad using a custom manager.
runOryKratosClientWithManager :: Manager -> Config -> OryKratosClient a -> ExceptT ClientError IO a
runOryKratosClientWithManager manager Config {..} cl = do
  url <- parseBaseUrl configUrl
  runClient cl $ mkClientEnv manager url

-- | Like @runClient@, but returns the response or throws
--   a OryKratosClientError
callOryKratos ::
  (MonadIO m, MonadThrow m) =>
  ClientEnv ->
  OryKratosClient a ->
  m a
callOryKratos env f = do
  res <- liftIO $ runExceptT $ runClient f env
  case res of
    Left err -> throwM (OryKratosClientError err)
    Right response -> pure response

requestMiddlewareId :: Application -> Application
requestMiddlewareId a = a

-- | Run the OryKratos server at the provided host and port.
runOryKratosServer ::
  (MonadIO m, MonadThrow m, ToJSON traits, ToJSON adminMetadata, ToJSON publicMetadata) =>
  Config ->
  OryKratosBackend (ExceptT ServerError IO) traits adminMetadata publicMetadata ->
  m ()
runOryKratosServer config backend = runOryKratosMiddlewareServer config requestMiddlewareId backend

-- | Run the OryKratos server at the provided host and port.
runOryKratosMiddlewareServer ::
  (MonadIO m, MonadThrow m, ToJSON traits, ToJSON adminMetadata, ToJSON publicMetadata) =>
  Config ->
  Middleware ->
  OryKratosBackend (ExceptT ServerError IO) traits adminMetadata publicMetadata ->
  m ()
runOryKratosMiddlewareServer Config {..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings =
        Warp.defaultSettings
          & Warp.setPort (baseUrlPort url)
          & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serverWaiApplicationOryKratos backend

-- | Plain "Network.Wai" Application for the OryKratos server.
--
-- Can be used to implement e.g. tests that call the API without a full webserver.
serverWaiApplicationOryKratos ::
  forall traits adminMetadata publicMetadata.
  (ToJSON traits, ToJSON adminMetadata, ToJSON publicMetadata) =>
  OryKratosBackend (ExceptT ServerError IO) traits adminMetadata publicMetadata ->
  Application
serverWaiApplicationOryKratos backend = serve (Proxy :: Proxy (OryKratosAPI traits adminMetadata publicMetadata)) (serverFromBackend backend)
  where
    serverFromBackend OryKratosBackend {..} =
      ( coerce getVersion
          :<|> coerce isAlive
          :<|> coerce isReady
          :<|> coerce adminCreateIdentity
          :<|> coerce adminCreateSelfServiceRecoveryLink
          :<|> coerce adminDeleteIdentity
          :<|> coerce adminDeleteIdentitySessions
          :<|> coerce adminExtendSession
          :<|> coerce adminGetIdentity
          :<|> coerce adminListIdentities
          :<|> coerce adminListIdentitySessions
          :<|> coerce adminUpdateIdentity
          :<|> coerce createSelfServiceLogoutFlowUrlForBrowsers
          :<|> coerce getJsonSchema
          :<|> coerce getSelfServiceError
          :<|> coerce getSelfServiceLoginFlow
          :<|> coerce getSelfServiceRecoveryFlow
          :<|> coerce getSelfServiceRegistrationFlow
          :<|> coerce getSelfServiceSettingsFlow
          :<|> coerce getSelfServiceVerificationFlow
          :<|> coerce getWebAuthnJavaScript
          :<|> coerce initializeSelfServiceLoginFlowForBrowsers
          :<|> coerce initializeSelfServiceLoginFlowWithoutBrowser
          :<|> coerce initializeSelfServiceRecoveryFlowForBrowsers
          :<|> coerce initializeSelfServiceRecoveryFlowWithoutBrowser
          :<|> coerce initializeSelfServiceRegistrationFlowForBrowsers
          :<|> coerce initializeSelfServiceRegistrationFlowWithoutBrowser
          :<|> coerce initializeSelfServiceSettingsFlowForBrowsers
          :<|> coerce initializeSelfServiceSettingsFlowWithoutBrowser
          :<|> coerce initializeSelfServiceVerificationFlowForBrowsers
          :<|> coerce initializeSelfServiceVerificationFlowWithoutBrowser
          :<|> coerce listIdentitySchemas
          :<|> coerce listSessions
          :<|> coerce revokeSession
          :<|> coerce revokeSessions
          :<|> coerce submitSelfServiceLoginFlow
          :<|> coerce submitSelfServiceLogoutFlow
          :<|> coerce submitSelfServiceLogoutFlowWithoutBrowser
          :<|> coerce submitSelfServiceRecoveryFlow
          :<|> coerce submitSelfServiceRegistrationFlow
          :<|> coerce submitSelfServiceSettingsFlow
          :<|> coerce submitSelfServiceVerificationFlow
          :<|> coerce toSession
          :<|> serveDirectoryFileServer "static"
      )
