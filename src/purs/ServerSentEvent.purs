module ServerSentEvent
  ( codec
  , encode
  , parse
  ) where

import Prelude

import Data.Array as Array
import Data.Codec.JSON (Codec)
import Data.Codec.JSON as Codec.JSON
import Data.Codec.JSON.Record as Codec.JSON.Record
import Data.Either (Either(..))
import Data.String as CodePoint
import Data.Traversable as Traversable
import JSON (JSON)
import JSON as JSON
import StringParser (Parser)
import StringParser as StringParser

type ServerSentEvent =
  { event :: String
  , data_ :: JSON
  }

parser
  :: Parser (Array { event :: String, data_ :: String })
parser = do
  let
    single = do
      _ <- StringParser.optional (StringParser.many (StringParser.string ("\n")))
      _ <- StringParser.string "event: "
      event <- (CodePoint.fromCodePointArray <<< Array.fromFoldable) <$>
        StringParser.manyTill StringParser.anyCodePoint (StringParser.string "\n")
      _ <- StringParser.string "data: "
      data_ <- (CodePoint.fromCodePointArray <<< Array.fromFoldable) <$>
        StringParser.manyTill StringParser.anyCodePoint (StringParser.string "\n")
      _ <- StringParser.optional (StringParser.many (StringParser.string ("\n")))
      pure { event, data_ }
  Array.fromFoldable <$> StringParser.many single

parse
  :: String
  -> Either
       String
       (Array { event :: String, data_ :: JSON })
parse str = case StringParser.runParser parser str of
  Left parseError ->
    Left (StringParser.printParserError parseError)
  Right events ->
    Traversable.for events \{ event, data_ } ->
      JSON.parse data_ <#>
        \data_' -> { event, data_: data_' }

codec :: Codec ServerSentEvent
codec = Codec.JSON.Record.object
  { event: Codec.JSON.string
  , data_: Codec.JSON.json
  }

encode :: ServerSentEvent -> JSON
encode = Codec.JSON.encode codec