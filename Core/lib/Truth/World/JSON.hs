module Truth.World.JSON where
{
    import Truth.Core.Import;
    import qualified Data.Aeson as JSON;
    import Data.Text(Text);
    import Truth.Core;

    jsonCodec :: ReasonCodec ByteString JSON.Value;
    jsonCodec = MkCodec (rmFromMaybe "fail to decode to JSON" . JSON.decode) JSON.encode;

    propertyLens :: Text -> Lens' Identity JSON.Object (Maybe JSON.Value);
    propertyLens = hashMapLens;

    jsonValueCodec :: (JSON.FromJSON a,JSON.ToJSON a) => ReasonCodec JSON.Value a;
    jsonValueCodec = let
    {
        resultToResult :: JSON.Result a -> ReasonM a;
        resultToResult (JSON.Success a) = SuccessResult a;
        resultToResult (JSON.Error s) = fail s;
    } in MkCodec (\v -> resultToResult $ JSON.fromJSON v) JSON.toJSON;
}
