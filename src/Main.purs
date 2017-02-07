module Main where

import Prelude
import Data.Array as A
import Data.Array.Partial as AU
import Data.MediaType.Common as Mime
import Data.String as S
import Network.HTTP.RequestHeader as Header
import Control.Alt ((<|>))
import Control.Monad.Aff (Aff, launchAff)
import Control.Monad.Eff.Console (CONSOLE)
import Control.Monad.Eff.Exception (error)
import Control.Monad.Except (runExcept, throwError)
import Control.Monad.Trans.Class (lift)
import Data.Either (either)
import Data.Foldable (for_)
import Data.Foreign.Class (class IsForeign, read, readProp)
import Data.Foreign.Index (prop)
import Data.Maybe (Maybe(..))
import Data.String (Pattern(..))
import Network.HTTP.Affjax (AJAX, Affjax, affjax, defaultRequest) as Affjax
import Network.HTTP.Affjax.Response (class Respondable) as Affjax
import Node.IRC (Channel(..), Host(..), MessageText(..), Nick(..), connect, onChannelMessage, runMessageText, sayChannel)
import Partial.Unsafe (unsafePartial)

type Effects e = (ajax :: Affjax.AJAX, console :: CONSOLE | e)

main = launchAff $ do
  let chanName = "#purescript"
  let chan = Channel chanName
  connect (Host "irc.freenode.net") (Nick "holly_") chan $ do
    sayChannel chan (MessageText $ "Hello " <> chanName)
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
  S.stripPrefix (Pattern "@") >>> map (S.split $ Pattern " ") >=> go
  where
  go words = case A.uncons words of
    Just { head : "pursuit", tail : tail } ->
      Just $ PursuitQuery $ S.joinWith " " tail
    _ ->
      Nothing

performRequest :: forall e. Request -> Aff (Effects e) String
performRequest req = case req of
  PursuitQuery query -> performPursuitQuery query

pursuitBaseURL :: String
pursuitBaseURL = "https://pursuit.purescript.org"

performPursuitQuery :: forall e. String -> Aff (Effects e) String
performPursuitQuery =
  makeUrl
    >>> getJSON
    -- >>> assertOk200
    >>> map _.response
    >=> readForeign
    >>> map (\x -> renderResult (unsafePartial $ AU.head x))
  where
  makeUrl q = pursuitBaseURL <> "/search?q=" <> q

  readForeign = either (throwError <<< error <<< show) pure <<< runExcept <<< read

getJSON :: forall e a. (Affjax.Respondable a) => String -> Affjax.Affjax e a
getJSON url =
  Affjax.affjax $ Affjax.defaultRequest
    { url = url
    , headers = [Header.Accept Mime.applicationJSON]
    }

whenJust :: forall m a. (Applicative m) => Maybe a -> (a -> m Unit) -> m Unit
whenJust = for_

newtype PursuitResult = PursuitResult
  { title :: String
  , info :: String
  , package :: String
  , url :: String
  }

instance isForeignPursuitResult :: IsForeign PursuitResult where
  read o =
    map PursuitResult $ do
      title <- (prop "info" >=> readProp "title") o <|> pure ""
      info <- (prop "info" >=> readProp "typeText" >=> (" :: " <> _) >>> pure) o <|> pure ""
      package <- readProp "package" o
      url <- readProp "url" o
      pure { title: title, info: info, package: package, url: url }

renderResult :: PursuitResult -> String
renderResult (PursuitResult r) = r.title <> r.info <> "\n" <> r.url <> " (" <> r.package <> ")"
