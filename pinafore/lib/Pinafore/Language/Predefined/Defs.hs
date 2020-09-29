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
    = NullDefBind
    | ValueDefBind (PinaforeContext -> QValue)
    | PatternDefBind QValue
                     QPatternConstructor
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
    dbValue bc = let
        ?pinafore = bc
        in jmToValue val
    bdBind = Just (name, ValueDefBind dbValue)
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
    bdBind = Just (name, NullDefBind)
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
    -> t
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry BindDoc
mkValPatEntry name docDescription val pat = let
    bdBind = Just (name, PatternDefBind (jmToValue val) (qToPatternConstructor pat))
    docName = unName name
    docValueType = qPositiveTypeDescription @t
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
