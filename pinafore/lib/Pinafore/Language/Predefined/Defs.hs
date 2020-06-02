module Pinafore.Language.Predefined.Defs where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.Shim
import Pinafore.Language.Type
import Shapes

type A = UVar "a"

type B = UVar "b"

type C = UVar "c"

type X = UVar "x"

type Y = UVar "y"

type AP = UVar "ap"

type BP = UVar "bp"

type CP = UVar "cp"

type AQ = UVar "aq"

type BQ = UVar "bq"

type CQ = UVar "cq"

type AX = UVar "ax"

type BX = UVar "bx"

type CX = UVar "cx"

type AY = UVar "ay"

type BY = UVar "by"

type CY = UVar "cy"

type EnA = MeetType Entity A

data BindDoc = MkBindDoc
    { bdName :: Name
    , bdValue :: Maybe (PinaforeContext -> QValue)
    , bdPattern :: Maybe (QPatternConstructor)
    , bdDoc :: DefDoc
    }

mkValEntry ::
       forall t. (ToPinaforeType t)
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext) => t)
    -> DocTreeEntry BindDoc
mkValEntry name docDescription val = let
    bdName = name
    bdValue =
        Just $ \bc -> let
            ?pinafore = bc
            in jmToValue val
    bdPattern = Nothing
    docName = name
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
    bdName = name
    bdValue = Nothing
    bdPattern = Nothing
    docName = name
    docValueType = qPositiveTypeDescription @t
    docIsSupertype = True
    docIsPattern = False
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkValPatEntry ::
       forall t v lt.
       (ToPinaforeType t, FromPinaforeType v, ToListShimWit (PinaforeShim Type) (PinaforeType 'Positive) lt)
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext) => t)
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry BindDoc
mkValPatEntry name docDescription val pat = let
    bdName = name
    bdValue =
        Just $ \bc -> let
            ?pinafore = bc
            in jmToValue val
    bdPattern = Just $ qToPatternConstructor pat
    docName = name
    docValueType = qPositiveTypeDescription @t
    docIsSupertype = False
    docIsPattern = True
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkPatEntry ::
       forall v lt. (FromPinaforeType v, ToListShimWit (PinaforeShim Type) (PinaforeType 'Positive) lt)
    => Name
    -> Text
    -> Text
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry BindDoc
mkPatEntry name docDescription docValueType pat = let
    bdName = name
    bdValue = Nothing
    bdPattern = Just $ qToPatternConstructor pat
    docName = name
    docIsSupertype = False
    docIsPattern = True
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}
