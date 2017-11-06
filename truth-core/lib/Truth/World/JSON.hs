module Truth.World.JSON where

import qualified Data.Aeson as JSON
import Truth.Core
import Truth.Core.Import

jsonCodec :: ReasonCodec ByteString JSON.Value
jsonCodec = MkCodec (resultFromMaybe "fail to decode to JSON" . JSON.decode) JSON.encode

propertyLens :: Text -> Lens' Identity JSON.Object (Maybe JSON.Value)
propertyLens = hashMapLens

jsonValueCodec :: (JSON.FromJSON a, JSON.ToJSON a) => ReasonCodec JSON.Value a
jsonValueCodec =
    let resultToResult :: JSON.Result a -> Result String a
        resultToResult (JSON.Success a) = SuccessResult a
        resultToResult (JSON.Error s) = FailureResult s
    in MkCodec (\v -> resultToResult $ JSON.fromJSON v) JSON.toJSON
