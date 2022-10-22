{-# OPTIONS -fno-warn-orphans #-}

module Pinafore.Language.Library.Undo
    ( undoLibraryModule
    ) where

import Changes.Core
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Library.Defs
import Pinafore.Language.Library.Std ()
import Pinafore.Language.Library.Storage ()
import Pinafore.Language.Type
import Pinafore.Language.Value
import Pinafore.Language.Var
import Shapes

-- UndoHandler
undoHandlerGroundType :: QGroundType '[] UndoHandler
undoHandlerGroundType = stdSingleGroundType $(iowitness [t|'MkWitKind (SingletonFamily UndoHandler)|]) "UndoHandler"

instance HasQGroundType '[] UndoHandler where
    qGroundType = undoHandlerGroundType

handleModel ::
       forall t. IsInvertibleModel t
    => UndoHandler
    -> t
    -> t
handleModel uh t =
    runIdentity $
    invertibleModelLens
        (\mdict model ->
             Identity $
             case mdict of
                 Just Dict -> undoHandlerModel uh model
                 Nothing -> model)
        t

handleSetModel :: UndoHandler -> LangSetModel A -> LangSetModel EnA
handleSetModel uh model = handleModel uh $ contramap meet2 model

handleStore :: UndoHandler -> QStore -> IO QStore
handleStore uh store = mkQStore $ undoHandlerModel uh $ qStoreGetModel store

undoLibraryModule :: LibraryModule context
undoLibraryModule =
    MkLibraryModule $
    MkDocTree
        "Undo"
        "Undo and redo changes to models."
        [ mkTypeEntry "UndoHandler" "A queue of undo (and redo) actions." $ MkSomeGroundType undoHandlerGroundType
        , mkValEntry "newUndoHandler" "Create a new `UndoHandler`." newUndoHandler
        , mkValEntry "queueUndo" "Undo an action." $ \uh -> do
              rc <- actionResourceContext
              liftIO $ undoHandlerUndo uh rc noEditSource
        , mkValEntry "queueRedo" "Redo an action." $ \uh -> do
              rc <- actionResourceContext
              liftIO $ undoHandlerRedo uh rc noEditSource
        , mkValEntry "handleStore" "Handle undo/redo for this model." handleStore
        , mkValEntry "handleWholeModel" "Handle undo/redo for this model." $ handleModel @(LangWholeModel '( A, A))
        , mkValEntry "handleTextModel" "Handle undo/redo for this model." $ handleModel @LangTextModel
        , mkValEntry "handleListModel" "Handle undo/redo for this model." $ handleModel @(LangListModel '( A, A))
        , mkValEntry "handleFiniteSetModel" "Handle undo/redo for this model." $
          handleModel @(LangFiniteSetModel '( P, Q))
        , mkValEntry "handleSetModel" "Handle undo/redo for this model." handleSetModel
        ]
