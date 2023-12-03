module Main where

import Prelude

import Control.Lazy as Lazy
import Control.Monad.Maybe.Trans (MaybeT(..))
import Control.Monad.Maybe.Trans as Maybe.Trans
import Data.Either (Either(..))
import Data.Foldable as Foldable
import Data.Maybe (Maybe(..))
import Effect (Effect)
import Effect.Aff as Aff
import Effect.Class as Class
import Effect.Class.Console as Console
import Effect.Exception as Exception
import FRP.Event (Event)
import FRP.Event as Event
import Fetch as Fetch
import JS.Intl.DateTimeFormat as DateTimeFormat
import JS.Intl.Locale as Locale
import Node.Process as Process
import Prediction (Prediction)
import Prediction as Prediction
import Promise.Aff as Promise.Aff
import ServerSentEvent as ServerSentEvent
import Web.Encoding.TextDecoder as TextDecoder
import Web.Encoding.UtfLabel as UtfLabel
import Web.Streams.ReadableStream as ReadableStream
import Web.Streams.Reader as Streams.Reader

url :: String
url = "https://api-v3.mbta.com/predictions/?filter[route]=9&filter[stop]=place-43&stop_sequence=8"

main :: Effect Unit
main = do
  apiKey <- Process.lookupEnv "API_KEY"
    # onNothingM (Exception.throw "API_KEY not found")
  stream apiKey

stream :: String -> Effect Unit
stream apiKey = do
  textDecoder <- TextDecoder.new UtfLabel.utf8

  en_US <- Locale.new_ "en-US"

  dateTimeFormat <-
    DateTimeFormat.new [ en_US ]
      { weekday: "long"
      , hour: "numeric"
      , minute: "numeric"
      , second: "numeric"
      , timeZone: "America/New_York"
      }

  let
    headers =
      { "Accept": "text/event-stream"
      , "X-API-Key": apiKey
      }

  Aff.launchAff_ do
    response <- Fetch.fetch url { headers }

    reader <- Class.liftEffect do
      ReadableStream.getReader =<< _.body response

    let
      (x :: Event ({ event :: String, prediction :: Prediction })) = Event.makeEvent \callback -> do
        Aff.launchAff_ do
          void do
            Lazy.fix \loop -> Maybe.Trans.runMaybeT do
              arrayView <- MaybeT do
                Promise.Aff.toAffE (Streams.Reader.read reader)

              result <- Class.liftEffect do
                TextDecoder.decodeWithOptions arrayView { stream: true } textDecoder

              Class.liftEffect case ServerSentEvent.parse result of
                Right events -> Foldable.for_ events case _ of
                  { event: "reset", data_ } -> do
                    Prediction.parseMany data_ >>=
                      case _ of
                        Left error -> Console.error error
                        Right predictions -> do
                          Console.log "Reset"
                          Foldable.for_ predictions \prediction -> do
                            callback { event: "Reset", prediction }

                  { event: "add", data_ } -> do
                    Prediction.parse data_ >>=
                      case _ of
                        Left error -> Console.error error
                        Right prediction -> do
                          callback { event: "Add", prediction }

                  { event: "update", data_ } -> do
                    Prediction.parse data_ >>=
                      case _ of
                        Left error -> Console.error error
                        Right prediction -> do
                          callback { event: "Update", prediction }

                  { event } -> do
                    Console.warn ("Unhandled event: " <> event)

                Left error -> Console.error error

              Console.log ""

              MaybeT loop

        pure (pure unit)

    pure unit

onNothingM :: forall a m. Monad m => m a -> m (Maybe a) -> m a
onNothingM ma mMaybeA = do
  maybeA <- mMaybeA
  case maybeA of
    Just a -> pure a
    Nothing -> ma

