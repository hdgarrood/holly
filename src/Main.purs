module Main where

import Prelude
import Control.Bind
import Data.Foldable
import Data.Maybe
import Data.Either

import qualified Data.Array as A
import qualified Data.Array.Unsafe as AU
import qualified Data.String as S

import Data.Foreign.Class (IsForeign, read, readProp)
import Control.Monad.Trans (lift)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Eff.Exception (error)
import Control.Monad.Error.Class (throwError)
import Control.Monad.Aff
import qualified Network.HTTP.Affjax as Affjax
import qualified Network.HTTP.Affjax.Response as Affjax
import qualified Network.HTTP.MimeType.Common as Mime
import qualified Network.HTTP.RequestHeader as Header
import Node.IRC

type Effects e = (ajax :: Affjax.AJAX, console :: CONSOLE | e)

main = launchAff $ do
  let chan = Channel "#purescript"
  connect (Host "irc.freenode.net") (Nick "holly_") chan $ do
    sayChannel chan (MessageText "Hello, world")
    onChannelMessage chan \event -> do
      let mreq = parseRequest $ runMessageText event.text
      whenJust mreq \req -> do
        result <- lift $ performRequest req
        sayChannel chan (MessageText result)

-- | All the things holly can be asked to do.
data Request
  = PursuitQuery String

parseRequest :: String -> Maybe Request
parseRequest =
  S.stripPrefix "@" >>> map (S.split " ") >=> go
  where
  go words = case A.uncons words of
    Just { head = "pursuit", tail = tail } ->
      Just $ PursuitQuery $ S.joinWith " " tail
    _ ->
      Nothing

performRequest :: forall e. Request -> Aff (Effects e) String
performRequest req = case req of
  PursuitQuery query -> performPursuitQuery query

pursuitBaseURL :: String
pursuitBaseURL = "http://localhost:3000"

performPursuitQuery :: forall e. String -> Aff (Effects e) String
performPursuitQuery =
  makeUrl
    >>> getJSON
    -- >>> assertOk200
    >>> map _.response
    >=> readForeign
    >>> map (AU.head >>> renderResult)
  where
  makeUrl q = pursuitBaseURL <> "/search?q=" <> q

  readForeign = either (throwError <<< error <<< show) return <<< read

getJSON :: forall e a. (Affjax.Respondable a) => String -> Affjax.Affjax e a
getJSON url =
  Affjax.affjax $ Affjax.defaultRequest
    { url = url
    , headers = [Header.Accept Mime.applicationJSON]
    }

whenJust :: forall m a. (Applicative m) => Maybe a -> (a -> m Unit) -> m Unit
whenJust = for_

newtype PursuitResult = PursuitResult
  { text :: String
  , package :: String
  }

instance isForeignPursuitResult :: IsForeign PursuitResult where
  read o =
    map PursuitResult $
      { text: _, package: _ }
        <$> readProp "text" o
        <*> readProp "package" o

renderResult :: PursuitResult -> String
renderResult (PursuitResult r) = r.text <> " (" <> r.package <> ")"
