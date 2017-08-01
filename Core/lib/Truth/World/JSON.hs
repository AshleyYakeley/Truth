module Truth.World.JSON where
{
    import Truth.Core.Import;
    import qualified Data.Aeson as JSON;
    import Data.Text(Text);
    --import Truth.Core;

    jsonCodec :: Codec ByteString JSON.Value;
    jsonCodec = MkCodec JSON.decode JSON.encode;

    propertyLens :: Text -> Lens' Identity JSON.Object (Maybe JSON.Value);
    propertyLens = hashMapLens;

    jsonValueCodec :: (JSON.FromJSON a,JSON.ToJSON a) => Codec' (Result String) JSON.Value a;
    jsonValueCodec = let
    {
        resultToResult :: JSON.Result a -> Result String a;
        resultToResult (JSON.Success a) = SuccessResult a;
        resultToResult (JSON.Error s) = FailureResult s;
    } in MkCodec (\v -> resultToResult $ JSON.fromJSON v) JSON.toJSON;
}
