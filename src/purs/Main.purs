module Main where

import Prelude

import Control.Lazy as Lazy
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Maybe.Trans as Maybe.Trans
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Fetch as Fetch
import Node.Process as Process
import Promise.Aff as Promise.Aff
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel as UtfLabel
import Web.Streams.ReadableStream as ReadableStream
import Web.Streams.Reader as Streams.Reader

url :: String
url = "https://api-v3.mbta.com/predictions/?filter[route]=9&filter[stop]=place-43&stop_sequence=8"

stream :: String -> Effect Unit
stream apiKey = do
  textDecoder <- TextDecoder.new UtfLabel.utf8
  let
    headers =
      { "Accept": "text/event-stream"
      , "X-API-Key": apiKey
      }
  Aff.launchAff_ do
    response <- Fetch.fetch url { headers }
    reader <- Class.liftEffect do
      ReadableStream.getReader =<< _.body response
    _ <- Lazy.fix \loop -> Maybe.Trans.runMaybeT do
      arrayView <- MaybeT do
        Promise.Aff.toAffE (Streams.Reader.read reader)
      result <- Class.liftEffect do
        TextDecoder.decodeWithOptions arrayView { stream: true } textDecoder
      Console.log result
      MaybeT loop
    pure unit

main :: Effect Unit
main = do
  apiKey <- Process.lookupEnv "API_KEY"
    # onNothingM (Exception.throw "API_KEY not found")
  stream apiKey

onNothingM :: forall a m. Monad m => m a -> m (Maybe a) -> m a
onNothingM ma mMaybeA = do
  maybeA <- mMaybeA
  case maybeA of
    Just a -> pure a
    Nothing -> ma

