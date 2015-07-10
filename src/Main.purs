module Main where

import Prelude
import Control.Bind ((>=>))
import Data.Foldable (for_)
import Data.Maybe
import qualified Data.Array as A
import qualified Data.String as S
import Control.Monad.Trans (lift)
import Control.Monad.Eff.Console (CONSOLE())
import Control.Monad.Aff
import Network.HTTP.Affjax
import Node.IRC

type Effects e = (ajax :: AJAX, console :: CONSOLE | e)

main = launchAff $ do
  let chan = Channel "#purescript"
  connect (Host "irc.freenode.net") (Nick "holly") chan $ do
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
  stripPrefix "@" >>> map (S.split " ") >=> go
  where
  go words = case A.uncons words of
    Just { head = "pursuit", tail = tail } ->
      Just $ PursuitQuery $ S.joinWith " " tail
    _ ->
      Nothing

performRequest :: forall e. Request -> Aff (Effects e) String
performRequest req = case req of
  PursuitQuery query -> performPursuitQuery query

performPursuitQuery :: forall e. String -> Aff (Effects e) String
performPursuitQuery = map _.response <<< get <<< ("/search?q=" <>)

whenJust :: forall m a. (Applicative m) => Maybe a -> (a -> m Unit) -> m Unit
whenJust = for_

stripPrefix :: String -> String -> Maybe String
stripPrefix prefix str =
  case S.indexOf prefix str of
    Just 0 -> Just $ S.drop (S.length prefix) str
    _      -> Nothing
