module Changes.World.JSON where

import Changes.Core
import Data.Aeson qualified as JSON
import Data.Aeson.Key qualified as JSON
import Data.Aeson.KeyMap qualified as JSON
import Shapes

jsonCodec :: ReasonCodec LazyByteString JSON.Value
jsonCodec = MkCodec (resultFromMaybe (fromString "fail to decode to JSON") . JSON.decode) JSON.encode

propertyLens :: Text -> PureLens JSON.Object (Maybe JSON.Value)
propertyLens (JSON.fromText -> key) = let
    lensGet = JSON.lookup key
    lensPutback Nothing hm = Identity $ JSON.delete key hm
    lensPutback (Just value) hm = Identity $ JSON.insert key value hm
    in MkLens {..}

jsonValueCodec :: (JSON.FromJSON a, JSON.ToJSON a) => ReasonCodec JSON.Value a
jsonValueCodec = let
    resultToResult :: JSON.Result a -> Result Text a
    resultToResult (JSON.Success a) = SuccessResult a
    resultToResult (JSON.Error s) = FailureResult $ fromString s
    in MkCodec (\v -> resultToResult $ JSON.fromJSON v) JSON.toJSON
