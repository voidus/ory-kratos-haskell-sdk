-- | Client and Server
module OryKratos.API
  ( Config (..),
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
  )
where

import Control.Monad.Catch (Exception, MonadThrow, throwM)
import Control.Monad.Except (ExceptT, runExceptT)
import Control.Monad.IO.Class
import Control.Monad.Trans.Reader (ReaderT (..))
import Data.Aeson (Value)
import Data.Coerce (coerce)
import Data.Function ((&))
import Data.Proxy (Proxy (..))
import Data.Text (Text)
import qualified Data.Text as T
import GHC.Exts (IsString (..))
import Network.HTTP.Client (Manager, newManager)
import Network.HTTP.Client.TLS (tlsManagerSettings)
import Network.Wai (Middleware)
import qualified Network.Wai.Handler.Warp as Warp
import OryKratos.Types hiding (error)
import Servant (ServerError, serve)
import Servant.API
import Servant.Client
  ( ClientEnv,
    ClientError,
    client,
    mkClientEnv,
    parseBaseUrl,
  )
import Servant.Client.Core (baseUrlHost, baseUrlPort)
import Servant.Client.Internal.HttpClient (ClientM (..))
import Servant.Server (Application, Handler (..))
import Servant.Server.StaticFiles (serveDirectoryFileServer)
import Prelude

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
type OryKratosAPI =
  "identities" :> ReqBody '[JSON] CreateIdentity :> Verb 'POST 200 '[JSON] Identity -- 'createIdentity' route
    :<|> "recovery" :> "link" :> ReqBody '[JSON] CreateRecoveryLink :> Verb 'POST 200 '[JSON] RecoveryLink -- 'createRecoveryLink' route
    :<|> "identities" :> Capture "id" Text :> Verb 'DELETE 200 '[JSON] NoContent -- 'deleteIdentity' route
    :<|> "identities" :> Capture "id" Text :> Verb 'GET 200 '[JSON] Identity -- 'getIdentity' route
    :<|> "identities" :> QueryParam "per_page" Integer :> QueryParam "page" Integer :> Verb 'GET 200 '[JSON] [Identity] -- 'listIdentities' route
    :<|> "metrics" :> "prometheus" :> Verb 'GET 200 '[JSON] NoContent -- 'prometheus' route
    :<|> "identities" :> Capture "id" Text :> ReqBody '[JSON] UpdateIdentity :> Verb 'PUT 200 '[JSON] Identity -- 'updateIdentity' route
    :<|> "health" :> "alive" :> Verb 'GET 200 '[JSON] HealthStatus -- 'isInstanceAlive' route
    :<|> "health" :> "ready" :> Verb 'GET 200 '[JSON] HealthStatus -- 'isInstanceReady' route
    :<|> "schemas" :> Capture "id" Text :> Verb 'GET 200 '[JSON] Value -- 'getSchema' route
    :<|> "self-service" :> "errors" :> QueryParam "error" Text :> Verb 'GET 200 '[JSON] SelfServiceErrorContainer -- 'getSelfServiceError' route
    :<|> "self-service" :> "login" :> "flows" :> QueryParam "id" Text :> Verb 'GET 200 '[JSON] LoginFlow -- 'getSelfServiceLoginFlow' route
    :<|> "self-service" :> "recovery" :> "flows" :> QueryParam "id" Text :> Verb 'GET 200 '[JSON] RecoveryFlow -- 'getSelfServiceRecoveryFlow' route
    :<|> "self-service" :> "registration" :> "flows" :> QueryParam "id" Text :> Verb 'GET 200 '[JSON] RegistrationFlow -- 'getSelfServiceRegistrationFlow' route
    :<|> "self-service" :> "settings" :> "flows" :> QueryParam "id" Text :> Header "X-Session-Token" Text :> Verb 'GET 200 '[JSON] SettingsFlow -- 'getSelfServiceSettingsFlow' route
    :<|> "self-service" :> "verification" :> "flows" :> QueryParam "id" Text :> Verb 'GET 200 '[JSON] VerificationFlow -- 'getSelfServiceVerificationFlow' route
    :<|> "self-service" :> "browser" :> "flows" :> "logout" :> Verb 'GET 200 '[JSON] NoContent -- 'initializeSelfServiceBrowserLogoutFlow' route
    :<|> "self-service" :> "login" :> "browser" :> QueryParam "refresh" Bool :> Verb 'GET 200 '[JSON] NoContent -- 'initializeSelfServiceLoginForBrowsers' route
    :<|> "self-service" :> "login" :> "api" :> QueryParam "refresh" Bool :> Verb 'GET 200 '[JSON] LoginFlow -- 'initializeSelfServiceLoginForNativeApps' route
    :<|> "self-service" :> "recovery" :> "browser" :> Verb 'GET 200 '[JSON] NoContent -- 'initializeSelfServiceRecoveryForBrowsers' route
    :<|> "self-service" :> "recovery" :> "api" :> Verb 'GET 200 '[JSON] RecoveryFlow -- 'initializeSelfServiceRecoveryForNativeApps' route
    :<|> "self-service" :> "registration" :> "browser" :> Verb 'GET 200 '[JSON] NoContent -- 'initializeSelfServiceRegistrationForBrowsers' route
    :<|> "self-service" :> "registration" :> "api" :> Verb 'GET 200 '[JSON] RegistrationFlow -- 'initializeSelfServiceRegistrationForNativeApps' route
    :<|> "self-service" :> "settings" :> "browser" :> Verb 'GET 200 '[JSON] NoContent -- 'initializeSelfServiceSettingsForBrowsers' route
    :<|> "self-service" :> "settings" :> "api" :> Header "X-Session-Token" Text :> Verb 'GET 200 '[JSON] SettingsFlow -- 'initializeSelfServiceSettingsForNativeApps' route
    :<|> "self-service" :> "verification" :> "browser" :> Verb 'GET 200 '[JSON] NoContent -- 'initializeSelfServiceVerificationForBrowsers' route
    :<|> "self-service" :> "verification" :> "api" :> Verb 'GET 200 '[JSON] VerificationFlow -- 'initializeSelfServiceVerificationForNativeApps' route
    :<|> "sessions" :> ReqBody '[JSON] RevokeSession :> Verb 'DELETE 200 '[JSON] NoContent -- 'revokeSession' route
    :<|> "self-service" :> "login" :> QueryParam "flow" Text :> ReqBody '[JSON] Value :> Verb 'POST 200 '[JSON] LoginViaApiResponse -- 'submitSelfServiceLoginFlow' route
    :<|> "self-service" :> "recovery" :> QueryParam "flow" Text :> ReqBody '[JSON] Value :> Verb 'POST 200 '[JSON] NoContent -- 'submitSelfServiceRecoveryFlow' route
    :<|> "self-service" :> "recovery" :> "methods" :> "link" :> QueryParam "token" Text :> QueryParam "flow" Text :> ReqBody '[JSON] SubmitSelfServiceRecoveryFlowWithLinkMethod :> Verb 'POST 200 '[JSON] NoContent -- 'submitSelfServiceRecoveryFlowWithLinkMethod' route
    :<|> "self-service" :> "registration" :> QueryParam "flow" Text :> ReqBody '[JSON] Value :> Verb 'POST 200 '[JSON] RegistrationViaApiResponse -- 'submitSelfServiceRegistrationFlow' route
    :<|> "self-service" :> "settings" :> QueryParam "flow" Text :> ReqBody '[JSON] Value :> Header "X-Session-Token" Text :> Verb 'POST 200 '[JSON] SettingsViaApiResponse -- 'submitSelfServiceSettingsFlow' route
    :<|> "self-service" :> "verification" :> "methods" :> "link" :> QueryParam "flow" Text :> ReqBody '[JSON] Value :> Verb 'POST 200 '[JSON] NoContent -- 'submitSelfServiceVerificationFlow' route
    :<|> "sessions" :> "whoami" :> Header "X-Session-Token" Text :> Verb 'GET 200 '[JSON] Session -- 'toSession' route
    :<|> "version" :> Verb 'GET 200 '[JSON] Version -- 'getVersion' route
    :<|> Raw

-- | Server or client configuration, specifying the host and port to query or serve on.
data Config = Config
  { -- | scheme://hostname:port/path, e.g. "http://localhost:8080/"
    configUrl :: String
  }
  deriving stock (Eq, Ord, Show, Read)

-- | Custom exception type for our errors.
newtype OryKratosClientError = OryKratosClientError ClientError
  deriving newtype (Show, Exception)

-- | Configuration, specifying the full url of the service.

-- | Backend for OryKratos.
-- The backend can be used both for the client and the server. The client generated from the OryKratos OpenAPI spec
-- is a backend that executes actions by sending HTTP requests (see @createOryKratosClient@). Alternatively, provided
-- a backend, the API can be served using @runOryKratosMiddlewareServer@.
data OryKratosBackend m = OryKratosBackend
  { -- | This endpoint creates an identity. It is NOT possible to set an identity's credentials (password, ...) using this method! A way to achieve that will be introduced in the future.  Learn how identities work in [Ory Kratos' User And Identity Model Documentation](https://www.ory.sh/docs/next/kratos/concepts/identity-user-model).
    createIdentity :: CreateIdentity -> m Identity,
    -- | This endpoint creates a recovery link which should be given to the user in order for them to recover (or activate) their account.
    createRecoveryLink :: CreateRecoveryLink -> m RecoveryLink,
    -- | Calling this endpoint irrecoverably and permanently deletes the identity given its ID. This action can not be undone. This endpoint returns 204 when the identity was deleted or when the identity was not found, in which case it is assumed that is has been deleted already.  Learn how identities work in [Ory Kratos' User And Identity Model Documentation](https://www.ory.sh/docs/next/kratos/concepts/identity-user-model).
    deleteIdentity :: Text -> m NoContent,
    -- | Learn how identities work in [Ory Kratos' User And Identity Model Documentation](https://www.ory.sh/docs/next/kratos/concepts/identity-user-model).
    getIdentity :: Text -> m Identity,
    -- | Get a Traits Schema Definition
    getSchema :: Text -> m Value,
    -- | This endpoint returns the error associated with a user-facing self service errors.  This endpoint supports stub values to help you implement the error UI:  `?error=stub:500` - returns a stub 500 (Internal Server Error) error.  More information can be found at [Ory Kratos User User Facing Error Documentation](https://www.ory.sh/docs/kratos/self-service/flows/user-facing-errors).
    getSelfServiceError :: Maybe Text -> m SelfServiceErrorContainer,
    -- | This endpoint returns a login flow's context with, for example, error details and other information.  More information can be found at [Ory Kratos User Login and User Registration Documentation](https://www.ory.sh/docs/next/kratos/self-service/flows/user-login-user-registration).
    getSelfServiceLoginFlow :: Maybe Text -> m LoginFlow,
    -- | This endpoint returns a recovery flow's context with, for example, error details and other information.  More information can be found at [Ory Kratos Account Recovery Documentation](../self-service/flows/account-recovery.mdx).
    getSelfServiceRecoveryFlow :: Maybe Text -> m RecoveryFlow,
    -- | This endpoint returns a registration flow's context with, for example, error details and other information.  More information can be found at [Ory Kratos User Login and User Registration Documentation](https://www.ory.sh/docs/next/kratos/self-service/flows/user-login-user-registration).
    getSelfServiceRegistrationFlow :: Maybe Text -> m RegistrationFlow,
    -- | When accessing this endpoint through Ory Kratos' Public API you must ensure that either the Ory Kratos Session Cookie or the Ory Kratos Session Token are set. The public endpoint does not return 404 status codes but instead 403 or 500 to improve data privacy.  You can access this endpoint without credentials when using Ory Kratos' Admin API.  More information can be found at [Ory Kratos User Settings & Profile Management Documentation](../self-service/flows/user-settings).
    getSelfServiceSettingsFlow :: Maybe Text -> Maybe Text -> m SettingsFlow,
    -- | This endpoint returns a verification flow's context with, for example, error details and other information.  More information can be found at [Ory Kratos Email and Phone Verification Documentation](https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation).
    getSelfServiceVerificationFlow :: Maybe Text -> m VerificationFlow,
    -- | Lists all identities. Does not support search at the moment.  Learn how identities work in [Ory Kratos' User And Identity Model Documentation](https://www.ory.sh/docs/next/kratos/concepts/identity-user-model).
    listIdentities :: Maybe Integer -> Maybe Integer -> m [Identity],
    -- | ``` metadata: annotations: prometheus.io/port: \"4434\" prometheus.io/path: \"/metrics/prometheus\" ```
    prometheus :: m NoContent,
    -- | This endpoint updates an identity. It is NOT possible to set an identity's credentials (password, ...) using this method! A way to achieve that will be introduced in the future.  The full identity payload (except credentials) is expected. This endpoint does not support patching.  Learn how identities work in [Ory Kratos' User And Identity Model Documentation](https://www.ory.sh/docs/next/kratos/concepts/identity-user-model).
    updateIdentity :: Text -> UpdateIdentity -> m Identity,
    -- | This endpoint returns a 200 status code when the HTTP server is up running. This status does currently not include checks whether the database connection is working.  If the service supports TLS Edge Termination, this endpoint does not require the `X-Forwarded-Proto` header to be set.  Be aware that if you are running multiple nodes of this service, the health status will never refer to the cluster state, only to a single instance.
    isInstanceAlive :: m HealthStatus,
    -- | This endpoint returns a 200 status code when the HTTP server is up running and the environment dependencies (e.g. the database) are responsive as well.  If the service supports TLS Edge Termination, this endpoint does not require the `X-Forwarded-Proto` header to be set.  Be aware that if you are running multiple nodes of this service, the health status will never refer to the cluster state, only to a single instance.
    isInstanceReady :: m HealthStatus,
    -- | This endpoint initializes a logout flow.  > This endpoint is NOT INTENDED for API clients and only works with browsers (Chrome, Firefox, ...).  On successful logout, the browser will be redirected (HTTP 302 Found) to the `return_to` parameter of the initial request or fall back to `urls.default_return_to`.  More information can be found at [Ory Kratos User Logout Documentation](https://www.ory.sh/docs/next/kratos/self-service/flows/user-logout).
    initializeSelfServiceBrowserLogoutFlow :: m NoContent,
    -- | This endpoint initializes a browser-based user login flow. Once initialized, the browser will be redirected to `selfservice.flows.login.ui_url` with the flow ID set as the query parameter `?flow=`. If a valid user session exists already, the browser will be redirected to `urls.default_redirect_url` unless the query parameter `?refresh=true` was set.  This endpoint is NOT INTENDED for API clients and only works with browsers (Chrome, Firefox, ...).  More information can be found at [Ory Kratos User Login and User Registration Documentation](https://www.ory.sh/docs/next/kratos/self-service/flows/user-login-user-registration).
    initializeSelfServiceLoginForBrowsers :: Maybe Bool -> m NoContent,
    -- | This endpoint initiates a login flow for API clients such as mobile devices, smart TVs, and so on.  If a valid provided session cookie or session token is provided, a 400 Bad Request error will be returned unless the URL query parameter `?refresh=true` is set.  To fetch an existing login flow call `/self-service/login/flows?flow=<flow_id>`.  :::warning  You MUST NOT use this endpoint in client-side (Single Page Apps, ReactJS, AngularJS) nor server-side (Java Server Pages, NodeJS, PHP, Golang, ...) browser applications. Using this endpoint in these applications will make you vulnerable to a variety of CSRF attacks, including CSRF login attacks.  This endpoint MUST ONLY be used in scenarios such as native mobile apps (React Native, Objective C, Swift, Java, ...).  :::  More information can be found at [Ory Kratos User Login and User Registration Documentation](https://www.ory.sh/docs/next/kratos/self-service/flows/user-login-user-registration).
    initializeSelfServiceLoginForNativeApps :: Maybe Bool -> m LoginFlow,
    -- | This endpoint initializes a browser-based account recovery flow. Once initialized, the browser will be redirected to `selfservice.flows.recovery.ui_url` with the flow ID set as the query parameter `?flow=`. If a valid user session exists, the browser is returned to the configured return URL.  This endpoint is NOT INTENDED for API clients and only works with browsers (Chrome, Firefox, ...).  More information can be found at [Ory Kratos Account Recovery Documentation](../self-service/flows/account-recovery.mdx).
    initializeSelfServiceRecoveryForBrowsers :: m NoContent,
    -- | This endpoint initiates a recovery flow for API clients such as mobile devices, smart TVs, and so on.  If a valid provided session cookie or session token is provided, a 400 Bad Request error.  To fetch an existing recovery flow call `/self-service/recovery/flows?flow=<flow_id>`.  :::warning  You MUST NOT use this endpoint in client-side (Single Page Apps, ReactJS, AngularJS) nor server-side (Java Server Pages, NodeJS, PHP, Golang, ...) browser applications. Using this endpoint in these applications will make you vulnerable to a variety of CSRF attacks.  This endpoint MUST ONLY be used in scenarios such as native mobile apps (React Native, Objective C, Swift, Java, ...).  :::  More information can be found at [Ory Kratos Account Recovery Documentation](../self-service/flows/account-recovery.mdx).
    initializeSelfServiceRecoveryForNativeApps :: m RecoveryFlow,
    -- | This endpoint initializes a browser-based user registration flow. Once initialized, the browser will be redirected to `selfservice.flows.registration.ui_url` with the flow ID set as the query parameter `?flow=`. If a valid user session exists already, the browser will be redirected to `urls.default_redirect_url` unless the query parameter `?refresh=true` was set.  :::note  This endpoint is NOT INTENDED for API clients and only works with browsers (Chrome, Firefox, ...).  :::  More information can be found at [Ory Kratos User Login and User Registration Documentation](https://www.ory.sh/docs/next/kratos/self-service/flows/user-login-user-registration).
    initializeSelfServiceRegistrationForBrowsers :: m NoContent,
    -- | This endpoint initiates a registration flow for API clients such as mobile devices, smart TVs, and so on.  If a valid provided session cookie or session token is provided, a 400 Bad Request error will be returned unless the URL query parameter `?refresh=true` is set.  To fetch an existing registration flow call `/self-service/registration/flows?flow=<flow_id>`.  :::warning  You MUST NOT use this endpoint in client-side (Single Page Apps, ReactJS, AngularJS) nor server-side (Java Server Pages, NodeJS, PHP, Golang, ...) browser applications. Using this endpoint in these applications will make you vulnerable to a variety of CSRF attacks.  This endpoint MUST ONLY be used in scenarios such as native mobile apps (React Native, Objective C, Swift, Java, ...).  :::  More information can be found at [Ory Kratos User Login and User Registration Documentation](https://www.ory.sh/docs/next/kratos/self-service/flows/user-login-user-registration).
    initializeSelfServiceRegistrationForNativeApps :: m RegistrationFlow,
    -- | This endpoint initializes a browser-based user settings flow. Once initialized, the browser will be redirected to `selfservice.flows.settings.ui_url` with the flow ID set as the query parameter `?flow=`. If no valid Ory Kratos Session Cookie is included in the request, a login flow will be initialized.  :::note  This endpoint is NOT INTENDED for API clients and only works with browsers (Chrome, Firefox, ...).  :::  More information can be found at [Ory Kratos User Settings & Profile Management Documentation](../self-service/flows/user-settings).
    initializeSelfServiceSettingsForBrowsers :: m NoContent,
    -- | This endpoint initiates a settings flow for API clients such as mobile devices, smart TVs, and so on. You must provide a valid Ory Kratos Session Token for this endpoint to respond with HTTP 200 OK.  To fetch an existing settings flow call `/self-service/settings/flows?flow=<flow_id>`.  :::warning  You MUST NOT use this endpoint in client-side (Single Page Apps, ReactJS, AngularJS) nor server-side (Java Server Pages, NodeJS, PHP, Golang, ...) browser applications. Using this endpoint in these applications will make you vulnerable to a variety of CSRF attacks.  This endpoint MUST ONLY be used in scenarios such as native mobile apps (React Native, Objective C, Swift, Java, ...).  :::  More information can be found at [Ory Kratos User Settings & Profile Management Documentation](../self-service/flows/user-settings).
    initializeSelfServiceSettingsForNativeApps :: Maybe Text -> m SettingsFlow,
    -- | This endpoint initializes a browser-based account verification flow. Once initialized, the browser will be redirected to `selfservice.flows.verification.ui_url` with the flow ID set as the query parameter `?flow=`.  This endpoint is NOT INTENDED for API clients and only works with browsers (Chrome, Firefox, ...).  More information can be found at [Ory Kratos Email and Phone Verification Documentation](https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation).
    initializeSelfServiceVerificationForBrowsers :: m NoContent,
    -- | This endpoint initiates a verification flow for API clients such as mobile devices, smart TVs, and so on.  To fetch an existing verification flow call `/self-service/verification/flows?flow=<flow_id>`.  :::warning  You MUST NOT use this endpoint in client-side (Single Page Apps, ReactJS, AngularJS) nor server-side (Java Server Pages, NodeJS, PHP, Golang, ...) browser applications. Using this endpoint in these applications will make you vulnerable to a variety of CSRF attacks.  This endpoint MUST ONLY be used in scenarios such as native mobile apps (React Native, Objective C, Swift, Java, ...).  :::  More information can be found at [Ory Kratos Email and Phone Verification Documentation](https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation).
    initializeSelfServiceVerificationForNativeApps :: m VerificationFlow,
    -- | Use this endpoint to revoke a session using its token. This endpoint is particularly useful for API clients such as mobile apps to log the user out of the system and invalidate the session.  This endpoint does not remove any HTTP Cookies - use the Browser-Based Self-Service Logout Flow instead.
    revokeSession :: RevokeSession -> m NoContent,
    -- | Use this endpoint to complete a login flow. This endpoint behaves differently for API and browser flows.  API flows expect `application/json` to be sent in the body and responds with HTTP 200 and a application/json body with the session token on success; HTTP 302 redirect to a fresh login flow if the original flow expired with the appropriate error messages set; HTTP 400 on form validation errors.  Browser flows expect `application/x-www-form-urlencoded` to be sent in the body and responds with a HTTP 302 redirect to the post/after login URL or the `return_to` value if it was set and if the login succeeded; a HTTP 302 redirect to the login UI URL with the flow ID containing the validation errors otherwise.  More information can be found at [Ory Kratos User Login and User Registration Documentation](https://www.ory.sh/docs/next/kratos/self-service/flows/user-login-user-registration).
    submitSelfServiceLoginFlow :: Maybe Text -> Value -> m LoginViaApiResponse,
    -- | Use this endpoint to complete a recovery flow. This endpoint behaves differently for API and browser flows and has several states:  `choose_method` expects `flow` (in the URL query) and `email` (in the body) to be sent and works with API- and Browser-initiated flows. For API clients it either returns a HTTP 200 OK when the form is valid and HTTP 400 OK when the form is invalid and a HTTP 302 Found redirect with a fresh recovery flow if the flow was otherwise invalid (e.g. expired). For Browser clients it returns a HTTP 302 Found redirect to the Recovery UI URL with the Recovery Flow ID appended. `sent_email` is the success state after `choose_method` for the `link` method and allows the user to request another recovery email. It works for both API and Browser-initiated flows and returns the same responses as the flow in `choose_method` state. `passed_challenge` expects a `token` to be sent in the URL query and given the nature of the flow (\"sending a recovery link\") does not have any API capabilities. The server responds with a HTTP 302 Found redirect either to the Settings UI URL (if the link was valid) and instructs the user to update their password, or a redirect to the Recover UI URL with a new Recovery Flow ID which contains an error message that the recovery link was invalid.  More information can be found at [Ory Kratos Account Recovery Documentation](../self-service/flows/account-recovery.mdx).
    submitSelfServiceRecoveryFlow :: Maybe Text -> Value -> m NoContent,
    -- | Use this endpoint to complete a recovery flow using the link method. This endpoint behaves differently for API and browser flows and has several states:  `choose_method` expects `flow` (in the URL query) and `email` (in the body) to be sent and works with API- and Browser-initiated flows. For API clients it either returns a HTTP 200 OK when the form is valid and HTTP 400 OK when the form is invalid and a HTTP 302 Found redirect with a fresh recovery flow if the flow was otherwise invalid (e.g. expired). For Browser clients it returns a HTTP 302 Found redirect to the Recovery UI URL with the Recovery Flow ID appended. `sent_email` is the success state after `choose_method` and allows the user to request another recovery email. It works for both API and Browser-initiated flows and returns the same responses as the flow in `choose_method` state. `passed_challenge` expects a `token` to be sent in the URL query and given the nature of the flow (\"sending a recovery link\") does not have any API capabilities. The server responds with a HTTP 302 Found redirect either to the Settings UI URL (if the link was valid) and instructs the user to update their password, or a redirect to the Recover UI URL with a new Recovery Flow ID which contains an error message that the recovery link was invalid.  More information can be found at [Ory Kratos Account Recovery Documentation](../self-service/flows/account-recovery.mdx).
    submitSelfServiceRecoveryFlowWithLinkMethod :: Maybe Text -> Maybe Text -> SubmitSelfServiceRecoveryFlowWithLinkMethod -> m NoContent,
    -- | Use this endpoint to complete a registration flow by sending an identity's traits and password. This endpoint behaves differently for API and browser flows.  API flows expect `application/json` to be sent in the body and respond with HTTP 200 and a application/json body with the created identity success - if the session hook is configured the `session` and `session_token` will also be included; HTTP 302 redirect to a fresh registration flow if the original flow expired with the appropriate error messages set; HTTP 400 on form validation errors.  Browser flows expect `application/x-www-form-urlencoded` to be sent in the body and responds with a HTTP 302 redirect to the post/after registration URL or the `return_to` value if it was set and if the registration succeeded; a HTTP 302 redirect to the registration UI URL with the flow ID containing the validation errors otherwise.  More information can be found at [Ory Kratos User Login and User Registration Documentation](https://www.ory.sh/docs/next/kratos/self-service/flows/user-login-user-registration).
    submitSelfServiceRegistrationFlow :: Maybe Text -> Value -> m RegistrationViaApiResponse,
    -- | Use this endpoint to complete a settings flow by sending an identity's updated password. This endpoint behaves differently for API and browser flows.  API-initiated flows expect `application/json` to be sent in the body and respond with HTTP 200 and an application/json body with the session token on success; HTTP 302 redirect to a fresh settings flow if the original flow expired with the appropriate error messages set; HTTP 400 on form validation errors. HTTP 401 when the endpoint is called without a valid session token. HTTP 403 when `selfservice.flows.settings.privileged_session_max_age` was reached. Implies that the user needs to re-authenticate.  Browser flows expect `application/x-www-form-urlencoded` to be sent in the body and responds with a HTTP 302 redirect to the post/after settings URL or the `return_to` value if it was set and if the flow succeeded; a HTTP 302 redirect to the Settings UI URL with the flow ID containing the validation errors otherwise. a HTTP 302 redirect to the login endpoint when `selfservice.flows.settings.privileged_session_max_age` was reached.  More information can be found at [Ory Kratos User Settings & Profile Management Documentation](../self-service/flows/user-settings).
    submitSelfServiceSettingsFlow :: Maybe Text -> Value -> Maybe Text -> m SettingsViaApiResponse,
    -- | Use this endpoint to complete a verification flow. This endpoint behaves differently for API and browser flows and has several states:  `choose_method` expects `flow` (in the URL query) and `email` (in the body) to be sent and works with API- and Browser-initiated flows. For API clients it either returns a HTTP 200 OK when the form is valid and HTTP 400 OK when the form is invalid and a HTTP 302 Found redirect with a fresh verification flow if the flow was otherwise invalid (e.g. expired). For Browser clients it returns a HTTP 302 Found redirect to the Verification UI URL with the Verification Flow ID appended. `sent_email` is the success state after `choose_method` when using the `link` method and allows the user to request another verification email. It works for both API and Browser-initiated flows and returns the same responses as the flow in `choose_method` state. `passed_challenge` expects a `token` to be sent in the URL query and given the nature of the flow (\"sending a verification link\") does not have any API capabilities. The server responds with a HTTP 302 Found redirect either to the Settings UI URL (if the link was valid) and instructs the user to update their password, or a redirect to the Verification UI URL with a new Verification Flow ID which contains an error message that the verification link was invalid.  More information can be found at [Ory Kratos Email and Phone Verification Documentation](https://www.ory.sh/docs/kratos/selfservice/flows/verify-email-account-activation).
    submitSelfServiceVerificationFlow :: Maybe Text -> Value -> m NoContent,
    -- | Uses the HTTP Headers in the GET request to determine (e.g. by using checking the cookies) who is authenticated. Returns a session object in the body or 401 if the credentials are invalid or no credentials were sent. Additionally when the request it successful it adds the user ID to the 'X-Kratos-Authenticated-Identity-Id' header in the response.  This endpoint is useful for:  AJAX calls. Remember to send credentials and set up CORS correctly! Reverse proxies and API Gateways Server-side calls - use the `X-Session-Token` header!
    toSession :: Maybe Text -> m Session,
    -- | This endpoint returns the service version typically notated using semantic versioning.  If the service supports TLS Edge Termination, this endpoint does not require the `X-Forwarded-Proto` header to be set.  Be aware that if you are running multiple nodes of this service, the health status will never refer to the cluster state, only to a single instance.
    getVersion :: m Version
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

createOryKratosClient :: OryKratosBackend OryKratosClient
createOryKratosClient = OryKratosBackend {..}
  where
    ((coerce -> createIdentity) :<|>
     (coerce -> createRecoveryLink) :<|>
     (coerce -> deleteIdentity) :<|>
     (coerce -> getIdentity) :<|>
     (coerce -> listIdentities) :<|>
     (coerce -> prometheus) :<|>
     (coerce -> updateIdentity) :<|>
     (coerce -> isInstanceAlive) :<|>
     (coerce -> isInstanceReady) :<|>
     (coerce -> getSchema) :<|>
     (coerce -> getSelfServiceError) :<|>
     (coerce -> getSelfServiceLoginFlow) :<|>
     (coerce -> getSelfServiceRecoveryFlow) :<|>
     (coerce -> getSelfServiceRegistrationFlow) :<|>
     (coerce -> getSelfServiceSettingsFlow) :<|>
     (coerce -> getSelfServiceVerificationFlow) :<|>
     (coerce -> initializeSelfServiceBrowserLogoutFlow) :<|>
     (coerce -> initializeSelfServiceLoginForBrowsers) :<|>
     (coerce -> initializeSelfServiceLoginForNativeApps) :<|>
     (coerce -> initializeSelfServiceRecoveryForBrowsers) :<|>
     (coerce -> initializeSelfServiceRecoveryForNativeApps) :<|>
     (coerce -> initializeSelfServiceRegistrationForBrowsers) :<|>
     (coerce -> initializeSelfServiceRegistrationForNativeApps) :<|>
     (coerce -> initializeSelfServiceSettingsForBrowsers) :<|>
     (coerce -> initializeSelfServiceSettingsForNativeApps) :<|>
     (coerce -> initializeSelfServiceVerificationForBrowsers) :<|>
     (coerce -> initializeSelfServiceVerificationForNativeApps) :<|>
     (coerce -> revokeSession) :<|>
     (coerce -> submitSelfServiceLoginFlow) :<|>
     (coerce -> submitSelfServiceRecoveryFlow) :<|>
     (coerce -> submitSelfServiceRecoveryFlowWithLinkMethod) :<|>
     (coerce -> submitSelfServiceRegistrationFlow) :<|>
     (coerce -> submitSelfServiceSettingsFlow) :<|>
     (coerce -> submitSelfServiceVerificationFlow) :<|>
     (coerce -> toSession) :<|>
     (coerce -> getVersion) :<|>
     _) = client (Proxy :: Proxy OryKratosAPI)

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
  (MonadIO m, MonadThrow m) =>
  Config ->
  OryKratosBackend (ExceptT ServerError IO) ->
  m ()
runOryKratosServer config =
  runOryKratosMiddlewareServer config requestMiddlewareId

-- | Run the OryKratos server at the provided host and port.
runOryKratosMiddlewareServer ::
  (MonadIO m, MonadThrow m) =>
  Config ->
  Middleware ->
  OryKratosBackend (ExceptT ServerError IO) ->
  m ()
runOryKratosMiddlewareServer Config {..} middleware backend = do
  url <- parseBaseUrl configUrl
  let warpSettings =
        Warp.defaultSettings
          & Warp.setPort (baseUrlPort url)
          & Warp.setHost (fromString $ baseUrlHost url)
  liftIO $ Warp.runSettings warpSettings $ middleware $ serve (Proxy :: Proxy OryKratosAPI) (serverFromBackend backend)
  where
    serverFromBackend OryKratosBackend {..} =
      coerce createIdentity :<|>
       coerce createRecoveryLink :<|>
       coerce deleteIdentity :<|>
       coerce getIdentity :<|>
       coerce listIdentities :<|>
       coerce prometheus :<|>
       coerce updateIdentity :<|>
       coerce isInstanceAlive :<|>
       coerce isInstanceReady :<|>
       coerce getSchema :<|>
       coerce getSelfServiceError :<|>
       coerce getSelfServiceLoginFlow :<|>
       coerce getSelfServiceRecoveryFlow :<|>
       coerce getSelfServiceRegistrationFlow :<|>
       coerce getSelfServiceSettingsFlow :<|>
       coerce getSelfServiceVerificationFlow :<|>
       coerce initializeSelfServiceBrowserLogoutFlow :<|>
       coerce initializeSelfServiceLoginForBrowsers :<|>
       coerce initializeSelfServiceLoginForNativeApps :<|>
       coerce initializeSelfServiceRecoveryForBrowsers :<|>
       coerce initializeSelfServiceRecoveryForNativeApps :<|>
       coerce initializeSelfServiceRegistrationForBrowsers :<|>
       coerce initializeSelfServiceRegistrationForNativeApps :<|>
       coerce initializeSelfServiceSettingsForBrowsers :<|>
       coerce initializeSelfServiceSettingsForNativeApps :<|>
       coerce initializeSelfServiceVerificationForBrowsers :<|>
       coerce initializeSelfServiceVerificationForNativeApps :<|>
       coerce revokeSession :<|>
       coerce submitSelfServiceLoginFlow :<|>
       coerce submitSelfServiceRecoveryFlow :<|>
       coerce submitSelfServiceRecoveryFlowWithLinkMethod :<|>
       coerce submitSelfServiceRegistrationFlow :<|>
       coerce submitSelfServiceSettingsFlow :<|>
       coerce submitSelfServiceVerificationFlow :<|>
       coerce toSession :<|>
       coerce getVersion :<|>
       serveDirectoryFileServer "static"