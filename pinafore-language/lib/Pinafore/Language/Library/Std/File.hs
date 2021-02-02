{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Std.File
    ( fileLibEntries
    ) where

import Pinafore.Storage

import Changes.Core
import Changes.World.ReferenceStore
import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.DocTree
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std.Convert ()
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Shapes

--import Changes.World.File
{-
file_import ::
        LangFiniteSetRef '( A, A)
    -> (A -> PinaforeAction ())
    -> PinaforeAction ()
file_import set continue = do
    chooseFile <- pinaforeActionRequest witChooseFile
    mpath <- liftIO chooseFile
    case mpath of
        Nothing -> return ()
        Just path -> do
            let sourcereference = fileReference path
            newEntity set $ \entity -> do
                mdestreference <-
                    pinaforeLiftView $
                    viewMapEdit (pinaforeFileItemLens entity) $ do
                        let
                            MkResource rr asub = sub
                        liftIO $
                            runWMFunction rr $ do
                                pushEdit $ refEdit [SingleReferenceDeleteCreate]
                                aModelRead asub ReadSingleReferenceStore
                destreference <-
                    case mdestreference of
                        Nothing -> fail "failed to create reference " ++ show entity
                        Just reference -> return reference
                liftIO $ copyReference sourcereference destreference
                continue entity
-}
fileGroundType :: PinaforeGroundType '[] FileEntity
fileGroundType =
    SimpleGroundType NilListType NilDolanVarianceMap ("File", 0) $
    MkProvidedType $(iowitness [t|'MkWitKind (HetEqual FileEntity)|]) HetRefl

-- FileEntity
instance ToShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Positive) FileEntity where
    toShimWit = mkShimWit $ GroundDolanSingularType fileGroundType NilDolanArguments

instance ToShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) FileEntity where
    toShimWit = singleDolanShimWit toJMShimWit

instance FromShimWit (PinaforePolyShim Type) (PinaforeSingularType 'Negative) FileEntity where
    fromShimWit = mkShimWit $ GroundDolanSingularType fileGroundType NilDolanArguments

instance FromShimWit (PinaforePolyShim Type) (PinaforeType 'Negative) FileEntity where
    fromShimWit = singleDolanShimWit fromJMShimWit

fileModel :: (?pinafore :: PinaforeContext) => FileEntity -> PinaforeImmutableWholeRef (Reference ByteStringEdit)
fileModel fe =
    MkPinaforeImmutableWholeRef $
    eaMapReadOnlyWhole maybeToKnow . eaMap (singleReferenceUpdateFunction . pinaforeFileItemLens fe) $
    MkWModel pinaforeFileModel

byteStringSize :: Reference ByteStringEdit -> PinaforeAction Int64
byteStringSize ref = do
    rc <- pinaforeResourceContext
    liftIO $ runResource rc ref $ \aref -> refRead aref ReadByteStringLength

fileSize :: (?pinafore :: PinaforeContext) => FileEntity -> PinaforeImmutableWholeRef (PinaforeAction Int64)
fileSize fe = fmap byteStringSize $ fileModel fe

fileLibEntries :: [DocTreeEntry BindDoc]
fileLibEntries =
    [ docTreeEntry
          "Files"
          ""
                  {-
                  mkValEntry "file_import" "Import a file into a set." $ file_import
              -}
          [ mkTypeEntry "File" "A file." $ MkBoundType fileGroundType
          , mkSubtypeRelationEntry "File" "Entity" "" $
            pure $
            simpleSubtypeConversionEntry fileGroundType (EntityPinaforeGroundType NilListType TopEntityGroundType) $
            nilSubtypeConversion $ coerceEnhanced "file entity"
          , mkValEntry "fileSize" "The size of a file." fileSize
          ]
    ]
