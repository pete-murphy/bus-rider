module Prediction where

import Prelude

import Codec.JSON.DecodeError as Codec.JSON.DecodeError
import Control.Monad.Except (ExceptT(..))
import Control.Monad.Except as Except
import Data.Bifunctor as Bifunctor
import Data.Codec.JSON (Codec)
import Data.Codec.JSON as Codec.JSON
import Data.Codec.JSON.Record as Codec.JSON.Record
import Data.DateTime (DateTime)
import Data.Either (Either)
import Data.Either as Either
import Data.JSDate as JSDate
import Data.Maybe (Maybe(..))
import Data.Traversable as Foldable
import Effect (Effect)
import Effect.Exception as Exception
import JSON (JSON)

-- {
--   "attributes": {
--     "arrival_time": "2023-12-01T06:43:15-05:00",
--     "departure_time": "2023-12-01T06:43:15-05:00",
--     "direction_id": 1,
--     "schedule_relationship": null,
--     "status": null,
--     "stop_sequence": 8
--   },
--   "id": "prediction-58367712-43-8",
--   "relationships": {
--     "route": { "data": { "id": "9", "type": "route" } },
--     "stop": { "data": { "id": "43", "type": "stop" } },
--     "trip": { "data": { "id": "58367712", "type": "trip" } },
--     "vehicle": { "data": { "id": "y3281", "type": "vehicle" } }
--   },
--   "type": "prediction"
-- },

type Prediction =
  { arrivalTime :: DateTime
  , vehicleID :: String
  , direction :: Boolean
  }

type RawPrediction =
  { attributes ::
      { arrival_time :: String
      , direction_id :: Int
      }
  , relationships ::
      { route :: { data :: { id :: String } }
      , stop :: { data :: { id :: String } }
      , vehicle :: { data :: { id :: String } }
      }
  }

codec :: Codec RawPrediction
codec = Codec.JSON.Record.object
  { attributes: Codec.JSON.Record.object
      { arrival_time: Codec.JSON.string
      , direction_id: Codec.JSON.int
      }
  , relationships: Codec.JSON.Record.object
      { route: Codec.JSON.Record.object
          { data: Codec.JSON.Record.object
              { id: Codec.JSON.string
              }
          }
      , stop: Codec.JSON.Record.object
          { data: Codec.JSON.Record.object
              { id: Codec.JSON.string
              }
          }
      , vehicle: Codec.JSON.Record.object
          { data: Codec.JSON.Record.object
              { id: Codec.JSON.string
              }
          }
      }
  }

parse :: JSON -> Effect (Either String Prediction)
parse json = Except.runExceptT do
  let printError error = Exception.name error <> ": " <> Exception.message error
  decoded <- Except.except do
    Bifunctor.lmap Codec.JSON.DecodeError.print do
      Codec.JSON.decode codec json
  arrivalTime' <- ExceptT do
    Bifunctor.lmap printError <$>
      Exception.try (JSDate.parse decoded.attributes.arrival_time)
  arrivalTime <- Except.except do
    Either.note ("Failed to convert JSDate to DateTime: " <> show arrivalTime')
      (JSDate.toDateTime arrivalTime')
  direction <- Except.except do
    Either.note ("Failed to convert direction_id to Boolean: " <> show decoded.attributes.direction_id)
      ( case decoded.attributes.direction_id of
          0 -> Just false
          1 -> Just true
          _ -> Nothing
      )
  pure
    { arrivalTime
    , direction
    , vehicleID: decoded.relationships.vehicle.data.id
    }

parseMany :: JSON -> Effect (Either String (Array Prediction))
parseMany json = Except.runExceptT do
  let printError error = Exception.name error <> ": " <> Exception.message error
  arrayDecoded <- Except.except do
    Bifunctor.lmap Codec.JSON.DecodeError.print do
      Codec.JSON.decode (Codec.JSON.array codec) json
  Foldable.for arrayDecoded \decoded -> do
    arrivalTime' <- ExceptT do
      Bifunctor.lmap printError <$>
        Exception.try (JSDate.parse decoded.attributes.arrival_time)
    arrivalTime <- Except.except do
      Either.note ("Failed to convert JSDate to DateTime: " <> show arrivalTime')
        (JSDate.toDateTime arrivalTime')
    direction <- Except.except do
      Either.note ("Failed to convert direction_id to Boolean: " <> show decoded.attributes.direction_id)
        ( case decoded.attributes.direction_id of
            0 -> Just false
            1 -> Just true
            _ -> Nothing
        )
    pure
      { arrivalTime
      , direction
      , vehicleID: decoded.relationships.vehicle.data.id
      }
