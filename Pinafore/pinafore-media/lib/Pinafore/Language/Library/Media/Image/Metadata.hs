module Pinafore.Language.Library.Media.Image.Metadata
    ( metadataStuff
    , LangHasMetadata(..)
    , hasMetadataGroundType
    , keyMapToMetadata
    , metadataToKeyMap
    ) where

import Data.Media.Image
import Pinafore.Base
import Pinafore.Language.API
import Shapes

newtype LangHasMetadata =
    MkLangHasMetadata (Map Text Literal)

hasMetadataGroundType :: QGroundType '[] LangHasMetadata
hasMetadataGroundType =
    (stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily LangHasMetadata)|]) "HasMetadata.Metadata.Image.")

instance HasQGroundType '[] LangHasMetadata where
    qGroundType = hasMetadataGroundType

mkHasMetadata :: [(Text, Literal)] -> LangHasMetadata
mkHasMetadata pairs = MkLangHasMetadata $ mapFromList pairs

getAllMetadata :: LangHasMetadata -> [(Text, Literal)]
getAllMetadata (MkLangHasMetadata mp) = mapToList mp

lookupMetadata :: Text -> LangHasMetadata -> Maybe Literal
lookupMetadata key (MkLangHasMetadata pairs) = lookup key pairs

updateMetadata :: Text -> Maybe Literal -> LangHasMetadata -> LangHasMetadata
updateMetadata key (Just val) (MkLangHasMetadata mp) = MkLangHasMetadata $ insertMap key val mp
updateMetadata key Nothing (MkLangHasMetadata mp) = MkLangHasMetadata $ deleteMap key mp

textKey :: Text -> BindDocStuff ()
textKey name =
    valPatBDS (UnqualifiedFullNameRef $ MkName name) (plainText $ "Standard metadata key \"" <> name <> "\"") name $
    ImpureFunction $ \n ->
        if n == name
            then Just ()
            else Nothing

keyName :: ImageDataKey a -> String
keyName (Data.Media.Image.Unknown s) = s
keyName key = show key

valueToLiteral :: Value -> Literal
valueToLiteral (Int x) = toLiteral $ toInteger x
valueToLiteral (Double x) = toLiteral x
valueToLiteral (String x) = toLiteral x

keyToLiteral :: ImageDataKey a -> a -> Maybe Literal
keyToLiteral Gamma x = Just $ toLiteral x
keyToLiteral DpiX x = Just $ toLiteral $ toInteger x
keyToLiteral DpiY x = Just $ toLiteral $ toInteger x
keyToLiteral Width x = Just $ toLiteral $ toInteger x
keyToLiteral Height x = Just $ toLiteral $ toInteger x
keyToLiteral Title x = Just $ toLiteral x
keyToLiteral Description x = Just $ toLiteral x
keyToLiteral Author x = Just $ toLiteral x
keyToLiteral Copyright x = Just $ toLiteral x
keyToLiteral Software x = Just $ toLiteral x
keyToLiteral Comment x = Just $ toLiteral x
keyToLiteral Disclaimer x = Just $ toLiteral x
keyToLiteral Source x = Just $ toLiteral x
keyToLiteral Warning x = Just $ toLiteral x
keyToLiteral (Data.Media.Image.Unknown _) x = Just $ valueToLiteral x
keyToLiteral _ _ = Nothing

keyToPair :: SomeOf ImageDataKey -> Maybe (Text, Literal)
keyToPair (MkSomeOf w v) = do
    lit <- keyToLiteral w v
    return (pack $ keyName w, lit)

keyMapToMetadata :: WitnessMapOf ImageDataKey -> LangHasMetadata
keyMapToMetadata wmap = MkLangHasMetadata $ mapFromList $ mapMaybe keyToPair $ witnessMapOfToList wmap

literalToValue :: Literal -> Maybe Value
literalToValue l =
    (fmap (Int . fromInteger) $ fromLiteral l) <|> (fmap Double $ fromLiteral l) <|> (fmap String $ fromLiteral l)

pairToKey :: (Text, Literal) -> Maybe (SomeOf ImageDataKey)
pairToKey (name, lit) = do
    v <- literalToValue lit
    return $ MkSomeOf (Data.Media.Image.Unknown $ unpack name) v

metadataToKeyMap :: [(Text, Literal)] -> WitnessMapOf ImageDataKey
metadataToKeyMap pairs = witnessMapOfFromList $ mapMaybe pairToKey pairs

metadataResolution :: LangHasMetadata -> Maybe (Int, Int)
metadataResolution md = do
    litx <- lookupMetadata "DpiX" md
    dx <- fromLiteral litx
    lity <- lookupMetadata "DpiY" md
    dy <- fromLiteral lity
    return (fromInteger dx, fromInteger dy)

metadataStuff :: BindDocStuff ()
metadataStuff =
    headingBDS "Metadata" "" $
    pure $
    namespaceBDS
        "Metadata"
        [ typeBDS
              "HasMetadata"
              "Something that has metadata."
              (MkSomeGroundType hasMetadataGroundType)
              [ valPatBDS "Mk" "Construct metadata out of key-value pairs. Duplicates will be removed." mkHasMetadata $
                PureFunction $ \hm -> (getAllMetadata hm, ())
              ]
        , valBDS "lookup" "Look up metadata by name." lookupMetadata
        , valBDS "update" "Update metadata item." updateMetadata
        , valBDS "resolution" "The resolution of an image (in dots/inch), if available." metadataResolution
        , headingBDS
              "Keys"
              "Constructors for standard metadata keys"
              [ textKey "Title"
              , textKey "Description"
              , textKey "Author"
              , textKey "Copyright"
              , textKey "Software"
              , textKey "Comment"
              , textKey "Disclaimer"
              , textKey "Source"
              , textKey "Warning"
              , textKey "Gamma"
              , textKey "DpiX"
              , textKey "DpiY"
              ]
        ]
