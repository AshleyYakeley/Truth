module Pinafore.Language.Predefined.Defs where

import Language.Expression.Sealed
import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.Doc
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Type
import Shapes

type A = UVar "a"

type B = UVar "b"

type C = UVar "c"

type AP = UVar "ap"

type BP = UVar "bp"

type CP = UVar "cp"

type AQ = UVar "aq"

type BQ = UVar "bq"

type CQ = UVar "cq"

data BindDoc baseedit = MkBindDoc
    { bdName :: Name
    , bdValue :: Maybe (PinaforeContext baseedit -> QValue baseedit)
    , bdPattern :: Maybe (QPatternConstructor baseedit)
    , bdDoc :: DefDoc
    }

mkValEntry ::
       forall baseedit t. (HasPinaforeEntityEdit baseedit, ToPinaforeType baseedit t)
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext baseedit) => t)
    -> DocTreeEntry (BindDoc baseedit)
mkValEntry name docDescription val = let
    bdName = name
    bdValue =
        Just $ \bc -> let
            ?pinafore = bc
            in toValue val
    bdPattern = Nothing
    docName = name
    docValueType = qTypeDescription @baseedit @t
    docIsPattern = False
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkValPatEntry ::
       forall baseedit t v lt.
       ( HasPinaforeEntityEdit baseedit
       , ToPinaforeType baseedit t
       , FromPinaforeType baseedit v
       , ToTypeF (HListWit (PinaforeType baseedit 'Positive)) (HList lt)
       )
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext baseedit) => t)
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry (BindDoc baseedit)
mkValPatEntry name docDescription val pat = let
    bdName = name
    bdValue =
        Just $ \bc -> let
            ?pinafore = bc
            in toValue val
    bdPattern = Just $ toPatternConstructor pat
    docName = name
    docValueType = qTypeDescription @baseedit @t
    docIsPattern = True
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkPatEntry ::
       forall baseedit v lt.
       ( HasPinaforeEntityEdit baseedit
       , FromPinaforeType baseedit v
       , ToTypeF (HListWit (PinaforeType baseedit 'Positive)) (HList lt)
       )
    => Name
    -> Text
    -> Text
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry (BindDoc baseedit)
mkPatEntry name docDescription docValueType pat = let
    bdName = name
    bdValue = Nothing
    bdPattern = Just $ toPatternConstructor pat
    docName = name
    docIsPattern = True
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}
