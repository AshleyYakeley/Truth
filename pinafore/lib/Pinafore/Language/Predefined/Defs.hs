module Pinafore.Language.Predefined.Defs where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

type EnA = MeetType Entity A

data DefBind
    = ValueDefBind (Maybe (PinaforeContext -> QValue))
                   (Maybe QPatternConstructor)
    | SpecialFormDefBind PinaforeSpecialForm

data BindDoc = MkBindDoc
    { bdBind :: Maybe (Name, DefBind)
    , bdDoc :: DefDoc
    }

mkDefDocEntry :: DefDoc -> BindDoc
mkDefDocEntry bdDoc = let
    bdBind = Nothing
    in MkBindDoc {..}

mkValEntry ::
       forall t. (ToPinaforeType t)
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext) => t)
    -> DocTreeEntry BindDoc
mkValEntry name docDescription val = let
    dbValue =
        Just $ \bc -> let
            ?pinafore = bc
            in jmToValue val
    bdBind = Just (name, ValueDefBind dbValue Nothing)
    docName = unName name
    docValueType = qPositiveTypeDescription @t
    docIsSupertype = False
    docIsPattern = False
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSupertypeEntry ::
       forall t. (ToPinaforeType t)
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext) => t)
    -> DocTreeEntry BindDoc
mkSupertypeEntry name docDescription _val = let
    bdBind = Just (name, ValueDefBind Nothing Nothing)
    docName = unName name
    docValueType = qPositiveTypeDescription @t
    docIsSupertype = True
    docIsPattern = False
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkValPatEntry ::
       forall t v lt.
       (ToPinaforeType t, FromPinaforeType v, ToListShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) lt)
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext) => t)
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry BindDoc
mkValPatEntry name docDescription val pat = let
    dbValue =
        Just $ \bc -> let
            ?pinafore = bc
            in jmToValue val
    dbPattern = Just $ qToPatternConstructor pat
    bdBind = Just (name, ValueDefBind dbValue dbPattern)
    docName = unName name
    docValueType = qPositiveTypeDescription @t
    docIsSupertype = False
    docIsPattern = True
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkPatEntry ::
       forall v lt. (FromPinaforeType v, ToListShimWit (PinaforePolyShim Type) (PinaforeType 'Positive) lt)
    => Name
    -> Text
    -> Text
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry BindDoc
mkPatEntry name docDescription docValueType pat = let
    dbPattern = Just $ qToPatternConstructor pat
    bdBind = Just (name, ValueDefBind Nothing dbPattern)
    docName = unName name
    docIsSupertype = False
    docIsPattern = True
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSpecialFormEntry :: Name -> Text -> Text -> Text -> PinaforeSpecialForm -> DocTreeEntry BindDoc
mkSpecialFormEntry name docDescription params docValueType sf = let
    bdBind = Just (name, SpecialFormDefBind sf)
    docName = unName name <> " " <> params
    docIsSupertype = False
    docIsPattern = False
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}
