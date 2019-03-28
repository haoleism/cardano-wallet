{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE UndecidableInstances #-}

module Test.Integration.Framework.Request
    ( request
    , request'
    , request_
    , successfulRequest
    , Headers(..)
    , Payload(..)
    , RequestException(..)
    , ($-)
    ) where

import Prelude

import Control.Exception
    ( try )
import Control.Lens
    ( Lens', view )
import Control.Monad
    ( void )
import Control.Monad.Catch
    ( Exception (..), MonadCatch (..), MonadThrow, throwM )
import Control.Monad.IO.Class
    ( MonadIO, liftIO )
import Control.Monad.Reader
    ( MonadReader (..) )
import Data.Aeson
    ( FromJSON )
import Data.Bifunctor
    ( first )
import Data.ByteString.Lazy
    ( ByteString )
import Data.Functor
    ( ($>) )
import Data.Generics.Product.Typed
    ( HasType, typed )
import Data.Text
    ( Text )
import Network.HTTP.Client
    ( HttpException (..)
    , HttpExceptionContent (..)
    , Manager
    , RequestBody (..)
    , httpLbs
    , method
    , parseRequest
    , requestBody
    , requestHeaders
    , responseBody
    , responseStatus
    )
import Network.HTTP.Types.Header
    ( RequestHeaders )
import Network.HTTP.Types.Method
    ( Method )
import Network.HTTP.Types.Status
    ( status200, status300, status404 )

import qualified Data.Aeson as Aeson
import qualified Data.ByteString.Lazy.Char8 as L8
import qualified Data.Text as T
import qualified Network.HTTP.Client as HTTP


class (HasType (Text, Manager) ctx) => HasManager ctx where
    manager :: Lens' ctx (Text, Manager)
    manager = typed @(Text, Manager)
instance (HasType (Text, Manager) ctx) => HasManager ctx

-- | The result when 'request' fails.
data RequestException
    = HttpException HttpException
      -- ^ Wraps an exception from "Network.HTTP.Client"
    | DecodeFailure ByteString
      -- ^ JSON decoding the given response data failed.
    | ClientError Aeson.Value
      -- ^ The HTTP response status code indicated failure.
    deriving (Show)

instance Exception RequestException

-- | The payload of the request
data Payload
    = Json Aeson.Value
    | NonJson ByteString
    | Empty

-- | The headers of the request
data Headers
    = Headers RequestHeaders
    | Default
    | None

-- | Makes a request to the API and decodes the response.
request
    :: forall a m ctx.
        ( FromJSON a
        , MonadIO m
        , MonadThrow m
        , MonadCatch m
        , MonadReader ctx m
        , HasManager ctx
        )
    => (Method, Text)
        -- ^ HTTP method and request path
    -> Payload
        -- ^ Request body
    -> m (Either RequestException a)
request (verb, path) body = (>>= handleResponse) <$> request' (verb, path) Default body
  where
    -- Either decode response body, or provide a RequestException.
    handleResponse (req, res) = case responseStatus res of
        s
            | s >= status200 && s <= status300 ->
                maybe
                    (Left $ decodeFailure res)
                    Right
                    (Aeson.decode $ responseBody res)
            | s == status404 ->
                Left
                    $ HttpException
                    $ HttpExceptionRequest req
                    $ StatusCodeException (res $> ())
                    $ L8.toStrict $ responseBody res

        _ -> Left $ decodeFailure res
             -- TODO: decode API error responses into ClientError

    decodeFailure :: HTTP.Response ByteString -> RequestException
    decodeFailure res = DecodeFailure $ responseBody res

request'
    :: forall m ctx.
        ( MonadIO m
        , MonadThrow m
        , MonadCatch m
        , MonadReader ctx m
        , HasManager ctx
        )
    => (Method, Text)
        -- ^ HTTP method and request path
    -> Headers
        -- ^ Request headers
    -> Payload
        -- ^ Request body
    -> m (Either RequestException (HTTP.Request, HTTP.Response ByteString))
request' (verb, path) reqHeaders body = do
    (base, man) <- view manager
    tryHttp $ do
        req <- parseRequest $ T.unpack $ base <> path
        res <- httpLbs (prepareReq req reqHeaders) man
        pure (req, res)
    where
        prepareReq :: HTTP.Request -> Headers -> HTTP.Request
        prepareReq req h = req
            { method = verb
            , requestBody = payload
            , requestHeaders = headers
            }
            where
                headers = case h of
                    Headers x -> x
                    Default -> [ ("Content-Type", "application/json")
                               , ("Accept", "application/json")
                               ]
                    None -> mempty

                payload = case body of
                    Json x -> (RequestBodyLBS . Aeson.encode) x
                    NonJson x -> RequestBodyLBS x
                    Empty -> mempty

        -- Catch HttpExceptions and turn them into
        -- Either RequestExceptions.
        tryHttp :: IO r -> m (Either RequestException r)
        tryHttp = liftIO . fmap (first HttpException) . try

-- | Makes a request to the API, ignoring the response, or any errors.
request_
    :: forall m ctx.
        ( MonadIO m
        , MonadThrow m
        , MonadCatch m
        , MonadReader ctx m
        , HasManager ctx
        )
    => (Method, Text)
    -> Payload
    -> m ()
request_ req body = void $ request' req Default body

-- | Makes a request to the API, but throws if it fails.
successfulRequest
    :: forall a m ctx.
        ( FromJSON a
        , MonadIO m
        , MonadThrow m
        , MonadCatch m
        , MonadReader ctx m
        , HasManager ctx
        )
    => (Method, Text)
    -> Payload
    -> m a
successfulRequest req body = request req body >>= either throwM pure

-- | Provide "next" arguments to a function, leaving the first one untouched.
--
-- e.g.
--    myFunction  :: Ctx -> Int -> String -> Result
--    myFunction' :: Ctx -> Result
--    myFunction' = myFunction $- 14 $- "patate"
infixl 1 $-
($-) :: (a -> b -> c) -> b -> a -> c
($-) = flip
