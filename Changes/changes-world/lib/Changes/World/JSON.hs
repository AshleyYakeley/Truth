module Changes.World.JSON where

import Changes.Core
import qualified Data.Aeson as JSON
import qualified Data.Aeson.Key as JSON
import qualified Data.Aeson.KeyMap as JSON
import Shapes

jsonCodec :: ReasonCodec LazyByteString JSON.Value
jsonCodec = MkCodec (resultFromMaybe (fromString "fail to decode to JSON") . JSON.decode) JSON.encode

propertyLens :: Text -> Lens' Identity JSON.Object (Maybe JSON.Value)
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
