{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Storage
    ( storageLibSection
    ) where

import Import
import Pinafore.Context
import Pinafore.Language.Convert
import Pinafore.Language.Expression
import Pinafore.Language.Interpreter
import Pinafore.Language.Library.Convert ()
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Optics
import Pinafore.Language.SpecialForm
import Pinafore.Language.Type
import Pinafore.Language.Value

-- QStore
storeGroundType :: QGroundType '[] QStore
storeGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily QStore)|]) "Store"

instance HasQGroundType '[] QStore where
    qGroundType = storeGroundType

openDefaultStore :: (?qcontext :: InvocationInfo) => View QStore
openDefaultStore = do
    model <- iiDefaultStorageModel ?qcontext
    liftIO $ mkQStore model

storageLibSection :: LibraryStuff InvocationInfo
storageLibSection =
    headingBDS "Storage" "" $
    [ typeBDS "Store" "Storage of information." (MkSomeGroundType storeGroundType) []
    , namespaceBDS
          "Store"
          [ addNameInRootBDS $
            specialFormBDS
                "property"
                "A property for this anchor. `A` and `B` are types that are subtypes of `Entity`."
                ["@A", "@B", "<anchor>"]
                "Store -> Property A B" $
            MkQSpecialForm
                (ConsListType AnnotNonpolarType $ ConsListType AnnotNonpolarType $ ConsListType AnnotAnchor NilListType) $ \(MkSome ta, (MkSome tb, (anchor, ()))) -> do
                eta <- getMonoStorableType ta
                etb <- getMonoStorableType tb
                saa <- monoStoreAdapter eta
                sab <- monoStoreAdapter etb
                MkShimWit rtap (MkPolarShim praContra) <- return $ nonpolarToNegative @QTypeSystem ta
                MkShimWit rtaq (MkPolarShim praCo) <- return $ nonpolarToPositive @QTypeSystem ta
                MkShimWit rtbp (MkPolarShim prbContra) <- return $ nonpolarToNegative @QTypeSystem tb
                MkShimWit rtbq (MkPolarShim prbCo) <- return $ nonpolarToPositive @QTypeSystem tb
                let
                    typem =
                        typeToDolan $
                        MkDolanGroundedType propertyGroundType $
                        ConsCCRArguments (RangeCCRPolarArgument rtap rtaq) $
                        ConsCCRArguments (RangeCCRPolarArgument rtbp rtbq) NilCCRArguments
                    typeStorage = typeToDolan $ MkDolanGroundedType storeGroundType NilCCRArguments
                    typef = qFunctionPosWitness typeStorage typem
                    property = predicateProperty saa sab (MkPredicate anchor)
                    pinaproperty =
                        \qstore ->
                            MkLangProperty $
                            storageModelBased qstore $
                            cfmap4 (MkCatDual $ shimToFunction praContra) $
                            cfmap3 (shimToFunction praCo) $
                            cfmap2 (MkCatDual $ shimToFunction prbContra) $ cfmap1 (shimToFunction prbCo) property
                    anyval = MkSomeOf typef pinaproperty
                return anyval
          , valBDS
                "openDefault"
                "Open the default `Store`. Will be closed at the end of the lifecycle."
                openDefaultStore
          ]
    ]
