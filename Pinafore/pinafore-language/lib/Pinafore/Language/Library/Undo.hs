{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Undo
    ( undoLibSection
    )
where

import Import
import Pinafore.Language.Convert
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.LibraryModule
import Pinafore.Language.Library.Storage ()
import Pinafore.Language.Type
import Pinafore.Language.Value

-- UndoHandler
undoHandlerGroundType :: QGroundType '[] UndoHandler
undoHandlerGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily UndoHandler)|]) "UndoHandler"

instance HasQGroundType '[] UndoHandler where
    qGroundType = undoHandlerGroundType

handleModel ::
    forall t.
    IsInvertibleModel t =>
    UndoHandler ->
    t ->
    t
handleModel uh t =
    runIdentity
        $ invertibleModelLens
            ( \mdict model ->
                Identity
                    $ case mdict of
                        Just Dict -> undoHandlerModel uh model
                        Nothing -> model
            )
            t

handleSetModel :: UndoHandler -> LangSetModel A -> LangSetModel EnA
handleSetModel uh model = handleModel uh $ contramap meet2 model

handleStore :: UndoHandler -> QStore -> IO QStore
handleStore uh store = mkQStore $ undoHandlerModel uh $ qStoreModel store

undoLibSection :: LibraryStuff
undoLibSection =
    headingBDS "Undo" "Undo and redo changes to models."
        $ [ typeBDS "UndoHandler" "A queue of undo (and redo) actions." (MkSomeGroundType undoHandlerGroundType) []
          , namespaceBDS
                "UndoHandler"
                [ valBDS "new" "Create a new `UndoHandler`." newUndoHandler
                , addNameInRootBDS
                    $ valBDS "queueUndo" "Undo an action."
                    $ \uh -> do
                        rc <- actionResourceContext
                        liftIO $ undoHandlerUndo uh rc noEditSource
                , addNameInRootBDS
                    $ valBDS "queueRedo" "Redo an action."
                    $ \uh -> do
                        rc <- actionResourceContext
                        liftIO $ undoHandlerRedo uh rc noEditSource
                , valBDS "handleStore" "Handle undo/redo for this model." handleStore
                , valBDS "handleWholeModel" "Handle undo/redo for this model." $ handleModel @(LangWholeModel '(A, A))
                , valBDS "handleTextModel" "Handle undo/redo for this model." $ handleModel @LangTextModel
                , valBDS "handleListModel" "Handle undo/redo for this model." $ handleModel @(LangListModel '(A, A))
                , valBDS "handleFiniteSetModel" "Handle undo/redo for this model."
                    $ handleModel @(LangFiniteSetModel '(P, Q))
                , valBDS "handleSetModel" "Handle undo/redo for this model." handleSetModel
                ]
          ]
