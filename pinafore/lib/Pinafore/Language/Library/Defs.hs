module Pinafore.Language.Library.Defs where

import Pinafore.Base
import Pinafore.Context
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Expression
import Pinafore.Language.Interpret
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Pinafore.Language.Var
import Shapes

type EnA = MeetType Entity A

data BindDoc = MkBindDoc
    { bdBind :: Maybe (Name, Maybe (PinaforeContext -> PinaforeBinding))
    , bdDoc :: DefDoc
    }

mkDefDocEntry :: DefDoc -> BindDoc
mkDefDocEntry bdDoc = let
    bdBind = Nothing
    in MkBindDoc {..}

mkValEntry ::
       forall t. ToPinaforeType t
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext) => t)
    -> DocTreeEntry BindDoc
mkValEntry name docDescription val = let
    bdBind =
        Just
            ( name
            , Just $ \pc -> let
                  ?pinafore = pc
                  in ValueBinding (qConstExprAny $ jmToValue val) Nothing)
    docName = unName name
    docValueType = qPositiveTypeDescription @t
    docIsSupertype = False
    docIsPattern = False
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSupertypeEntry ::
       forall t. ToPinaforeType t
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext) => t)
    -> DocTreeEntry BindDoc
mkSupertypeEntry name docDescription _val = let
    bdBind = Just (name, Nothing)
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
    bdBind = Just (name, Just $ \_ -> ValueBinding (qConstExprAny $ jmToValue val) $ Just $ qToPatternConstructor pat)
    docName = unName name
    docValueType = qPositiveTypeDescription @t
    docIsSupertype = False
    docIsPattern = True
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkSpecialFormEntry :: Name -> Text -> Text -> Text -> PinaforeSpecialForm -> DocTreeEntry BindDoc
mkSpecialFormEntry name docDescription params docValueType sf = let
    bdBind = Just (name, Just $ \_ -> SpecialFormBinding sf)
    docName = unName name <> " " <> params
    docIsSupertype = False
    docIsPattern = False
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}
