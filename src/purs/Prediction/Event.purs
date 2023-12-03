module Prediction.Event where

import Prelude

import Codec.JSON.DecodeError (DecodeError)
import Codec.JSON.DecodeError as DecodeError
import Control.Monad.Error.Class as Error.Class
import Data.Codec.JSON (Codec)
import Data.Codec.JSON as Codec.JSON
import Data.DateTime (DateTime)
import Data.Either (Either(..))
import Data.Either as Either
import Data.JSDate (JSDate)
import Data.JSDate as JSDate
import Data.List.NonEmpty (NonEmptyList)
import Data.List.NonEmpty as NonEmptyList
import Data.Maybe (Maybe)
import Data.Newtype as Newtype
import Data.Semigroup.Foldable as Semigroup.Foldable
import Data.String as String
import Data.Traversable as Traversable
import Data.Validation.Semigroup (V(..))
import Effect.Exception (Error)
import Effect.Exception as Exception
import Effect.Unsafe as Unsafe
import JSON (JSON)
import JSON as JSON
import Prediction.Raw as Raw
import ServerSentEvent (ServerSentEvent)
import ServerSentEvent as ServerSentEvent
import StringParser as Parser

type Identity :: forall k. k -> k
type Identity x = x

data Direction = Inbound | Outbound

derive instance Eq Direction

instance Show Direction where
  show Inbound = "Inbound"
  show Outbound = "Outbound"

type Prediction f =
  { arrivalTime :: DateTime
  , vehicleID :: f String
  , tripID :: String
  , direction :: Direction
  }

data Event
  = Reset (Array (Prediction Maybe))
  | Add (Prediction Identity)
  | Update (Prediction Identity)

data FromServerSentEventFailure
  = FailedToDecodeJSON DecodeError
  | FailedToParseJSDate Error
  | FailedToParseDateTime JSDate
  | FailedToParseDirection Int
  | UnhandledEvent ServerSentEvent
  | InvariantMissingVehicleID

fromServerSentEvent
  :: ServerSentEvent
  -> Either (NonEmptyList FromServerSentEventFailure) Event
fromServerSentEvent { event, data_ } = do
  let
    parseDateTime :: String -> Either FromServerSentEventFailure DateTime
    parseDateTime =
      JSDate.parse
        >>> Exception.try
        >>> Unsafe.unsafePerformEffect
        >>> mapLeft FailedToParseJSDate
        >=> \jsDate -> JSDate.toDateTime jsDate
          # Either.note (FailedToParseDateTime jsDate)

    parseDirection :: Int -> Either FromServerSentEventFailure Direction
    parseDirection = case _ of
      0 -> Right Outbound
      1 -> Right Inbound
      direction -> Left (FailedToParseDirection direction)

    parseSingleMaybe
      :: Raw.Prediction
      -> Either FromServerSentEventFailure (Prediction Maybe)
    parseSingleMaybe raw = do
      arrivalTime <- parseDateTime raw.attributes.arrival_time
      direction <- parseDirection raw.attributes.direction_id
      pure
        { arrivalTime
        , vehicleID: raw.relationships.vehicle.data <#> _.id
        , tripID: raw.relationships.trip.data.id
        , direction
        }

    parseSingle
      :: Raw.Prediction
      -> Either FromServerSentEventFailure (Prediction Identity)
    parseSingle raw = do
      arrivalTime <- parseDateTime raw.attributes.arrival_time
      vehicleID <- raw.relationships.vehicle.data <#> _.id
        # Either.note InvariantMissingVehicleID
      direction <- parseDirection raw.attributes.direction_id
      pure
        { arrivalTime
        , vehicleID
        , tripID: raw.relationships.trip.data.id
        , direction
        }

    decodeJSON :: forall a. Codec a -> JSON -> Either FromServerSentEventFailure a
    decodeJSON codec =
      Codec.JSON.decode codec >>>
        mapLeft FailedToDecodeJSON

  case event of
    "reset" ->
      map Reset do
        decodeJSON (Codec.JSON.array Raw.codec) data_ # liftFailure
          >>= Newtype.alaF V Traversable.traverse (parseSingleMaybe >>> liftFailure)

    "update" ->
      map Update do
        decodeJSON Raw.codec data_
          >>= parseSingle
        # liftFailure

    "add" ->
      map Add do
        decodeJSON Raw.codec data_
          >>= parseSingle
        # liftFailure

    _ ->
      Error.Class.throwError (NonEmptyList.singleton (UnhandledEvent { event, data_ }))

data ParseFailure
  = ServerSentEventParseFailure ServerSentEvent.ParseFailure
  | FromServerSentEventFailures (NonEmptyList FromServerSentEventFailure)

printParseFailure :: ParseFailure -> String
printParseFailure parseFailure =
  case parseFailure of
    ServerSentEventParseFailure failure ->
      String.joinWith "\n" case failure of
        ServerSentEvent.FailedToParseJSON errorMessage ->
          [ "Failed to parse JSON:", errorMessage ]
        ServerSentEvent.FailedToParseEvent parseError ->
          [ "Failed to parse event:", Parser.printParserError parseError ]
    FromServerSentEventFailures failures ->
      failures # Semigroup.Foldable.intercalateMap "\n\n" \failure ->
        String.joinWith "\n" case failure of
          FailedToDecodeJSON decodeError ->
            [ "Failed to decode JSON:", DecodeError.print decodeError ]
          FailedToParseJSDate error ->
            [ "Failed to parse JSDate:", Exception.name error, Exception.message error ]
          FailedToParseDateTime jsDate ->
            [ "Failed to parse DateTime", show jsDate ]
          FailedToParseDirection direction ->
            [ "Failed to parse direction:", show direction ]
          UnhandledEvent { event, data_ } ->
            [ "Unhandled event:", show event, JSON.print data_ ]
          InvariantMissingVehicleID ->
            [ "Invariant violation: missing vehicle ID" ]

parse
  :: String
  -> Either
       (NonEmptyList ParseFailure)
       (Array Event)
parse string = do
  ServerSentEvent.parse string
    # mapLeft ServerSentEventParseFailure
    # liftFailure
    >>= Newtype.alaF V Traversable.traverse
      (fromServerSentEvent >>> mapLeft FromServerSentEventFailures >>> liftFailure)

liftFailure :: forall e a. Either e a -> Either (NonEmptyList e) a
liftFailure = mapLeft NonEmptyList.singleton

mapLeft :: forall e e' a. (e -> e') -> Either e a -> Either e' a
mapLeft f = case _ of
  Left e -> Left (f e)
  Right a -> Right a