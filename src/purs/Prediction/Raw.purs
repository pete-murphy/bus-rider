module Prediction.Raw where

import Data.Codec.JSON (Codec)
import Data.Codec.JSON as Codec.JSON
import Data.Codec.JSON.Record as Codec.JSON.Record
import Data.Maybe (Maybe)

type Prediction =
  { attributes ::
      { arrival_time :: String
      , direction_id :: Int
      }
  , relationships ::
      { route :: { data :: { id :: String } }
      , stop :: { data :: { id :: String } }
      , trip :: { data :: { id :: String } }
      , vehicle :: { data :: Maybe { id :: String } }
      }
  }

codec :: Codec Prediction
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
      , trip: Codec.JSON.Record.object
          { data: Codec.JSON.Record.object
              { id: Codec.JSON.string
              }
          }
      , vehicle: Codec.JSON.Record.object
          { data: Codec.JSON.nullable
              ( Codec.JSON.Record.object
                  { id: Codec.JSON.string
                  }
              )
          }
      }
  }