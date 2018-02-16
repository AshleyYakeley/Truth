module Truth.Core.UI.Table where

import Truth.Core.Edit
import Truth.Core.Import
import Truth.Core.Read
import Truth.Core.Types
import Truth.Core.UI.Lens
import Truth.Core.UI.Specifier

data TableCellProps = MkTableCellProps
    { tcItalic :: Bool
    }

tableCellPlain :: TableCellProps
tableCellPlain = let
    tcItalic = False
    in MkTableCellProps {..}

data KeyColumn tedit key = MkKeyColumn
    { kcName :: Text
    , kcContents :: key -> IO (EditLens tedit (WholeEdit Text), EditFunction tedit (WholeEdit TableCellProps))
    }

readOnlyKeyColumn :: Text -> (key -> IO (EditFunction tedit (WholeEdit (Text, TableCellProps)))) -> KeyColumn tedit key
readOnlyKeyColumn kcName getter = let
    kcContents key = do
        func <- getter key
        return (readOnlyEditLens $ funcEditFunction fst . func, funcEditFunction snd . func)
    in MkKeyColumn {..}

data UITable tedit where
    MkUITable
        :: forall cont tedit iedit.
           ( IONewItemKeyContainer cont
           , FullSubjectReader (EditReader iedit)
           --, ApplicableEdit tedit
           --, ApplicableEdit iedit
           , HasKeyReader cont (EditReader iedit)
           )
        => [KeyColumn tedit (ContainerKey cont)]
        -> (ContainerKey cont -> Aspect tedit)
        -> EditLens tedit (KeyEdit cont iedit)
        -> UITable tedit

uiTable ::
       forall cont tedit iedit.
       ( IONewItemKeyContainer cont
       , FullSubjectReader (EditReader iedit)
       --, ApplicableEdit tedit
       --, ApplicableEdit iedit
       , HasKeyReader cont (EditReader iedit)
       )
    => [KeyColumn tedit (ContainerKey cont)]
    -> (ContainerKey cont -> Aspect tedit)
    -> EditLens tedit (KeyEdit cont iedit)
    -> UISpec tedit
uiTable cols getaspect lens = MkUISpec $ MkUITable cols getaspect lens

uiSimpleTable ::
       forall cont iedit.
       ( IONewItemKeyContainer cont
       , FullSubjectReader (EditReader iedit)
       , ApplicableEdit iedit
       , HasKeyReader cont (EditReader iedit)
       )
    => [KeyColumn (KeyEdit cont iedit) (ContainerKey cont)]
    -> Aspect (MaybeEdit iedit)
    -> UISpec (KeyEdit cont iedit)
uiSimpleTable cols aspect = uiTable cols (\key -> ioMapAspect (getKeyElementEditLens key) aspect) id

instance Show (UITable edit) where
    show (MkUITable cols _ _) = "table (" ++ intercalate ", " (fmap (unpack . kcName) cols) ++ ")"

instance UIType UITable where
    uiWitness = $(iowitness [t|UITable|])
