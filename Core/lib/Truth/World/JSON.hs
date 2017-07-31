module Truth.World.JSON where
{
    import Truth.Core.Import;
    import qualified Data.Aeson as JSON;
    --import Truth.Core;

    jsonCodec :: Codec ByteString JSON.Value;
    jsonCodec = MkCodec JSON.decode JSON.encode;
}
