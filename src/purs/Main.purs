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
import Effect.Class as Effect.Class
import Effect.Class.Console as Console
import Effect.Exception as Exception
import Effect.Ref as Ref
import FRP.Event (Event)
import FRP.Event as Event
import Fetch as Fetch
import JS.Intl.DateTimeFormat as DateTimeFormat
import JS.Intl.Locale as Locale
import Node.Process as Process
import Prediction.Event (Direction(..))
import Prediction.Event as Prediction.Event
import Promise.Aff as Promise.Aff
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

    reader <- Effect.Class.liftEffect do
      ReadableStream.getReader =<< _.body response

    let
      (event :: Event Prediction.Event.Event) = Event.makeEvent \callback -> do
        done <- Ref.new false

        Aff.launchAff_ do
          void do
            Lazy.fix \loop -> Maybe.Trans.runMaybeT do
              arrayView <- MaybeT do
                Promise.Aff.toAffE (Streams.Reader.read reader)

              string <- Effect.Class.liftEffect do
                TextDecoder.decodeWithOptions arrayView { stream: true } textDecoder

              Effect.Class.liftEffect case Prediction.Event.parse string of
                Right events -> Foldable.for_ events callback
                Left parseFailures -> Foldable.for_ parseFailures \failure ->
                  Console.error (Prediction.Event.printParseFailure failure)

              weAreDone <- Effect.Class.liftEffect (Ref.read done)
              unless weAreDone (MaybeT loop)

        pure (Ref.write true done)

    _ <- Effect.Class.liftEffect do
      Event.subscribe event case _ of
        Prediction.Event.Reset predictions -> do
          Foldable.for_ predictions \prediction -> do
            when (prediction.direction == Inbound) do
              Console.log (DateTimeFormat.format dateTimeFormat prediction.arrivalTime)
              Console.log prediction.tripID
              Foldable.for_ prediction.vehicleID Console.log

        Prediction.Event.Add prediction -> do
          when (prediction.direction == Inbound) do
            Console.log (DateTimeFormat.format dateTimeFormat prediction.arrivalTime)
            Console.log prediction.tripID
            Console.log prediction.vehicleID

        Prediction.Event.Update prediction -> do
          when (prediction.direction == Inbound) do
            Console.log (DateTimeFormat.format dateTimeFormat prediction.arrivalTime)
            Console.log prediction.tripID
            Console.log prediction.vehicleID

    pure unit

onNothingM :: forall a m. Monad m => m a -> m (Maybe a) -> m a
onNothingM ma mMaybeA = do
  maybeA <- mMaybeA
  case maybeA of
    Just a -> pure a
    Nothing -> ma

