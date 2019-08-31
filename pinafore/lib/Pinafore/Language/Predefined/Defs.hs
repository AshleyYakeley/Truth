module Pinafore.Language.Predefined.Defs where

import Pinafore.Base
import Pinafore.Language.Convert
import Pinafore.Language.DocTree
import Pinafore.Language.Expression
import Pinafore.Language.Name
import Pinafore.Language.TypeSystem
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

data BindDoc baseupdate = MkBindDoc
    { bdName :: Name
    , bdValue :: Maybe (PinaforeContext baseupdate -> QValue baseupdate)
    , bdPattern :: Maybe (QPatternConstructor baseupdate)
    , bdDoc :: DefDoc
    }

mkValEntry ::
       forall baseupdate t. (HasPinaforeEntityUpdate baseupdate, ToPinaforeType baseupdate t)
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext baseupdate) => t)
    -> DocTreeEntry (BindDoc baseupdate)
mkValEntry name docDescription val = let
    bdName = name
    bdValue =
        Just $ \bc -> let
            ?pinafore = bc
            in jmToValue val
    bdPattern = Nothing
    docName = name
    docValueType = qTypeDescription @baseupdate @t
    docIsPattern = False
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkValPatEntry ::
       forall baseupdate t v lt.
       ( HasPinaforeEntityUpdate baseupdate
       , ToPinaforeType baseupdate t
       , FromPinaforeType baseupdate v
       , ToListShimWit PinaforeShim (PinaforeType baseupdate 'Positive) lt
       )
    => Name
    -> Text
    -> ((?pinafore :: PinaforeContext baseupdate) => t)
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry (BindDoc baseupdate)
mkValPatEntry name docDescription val pat = let
    bdName = name
    bdValue =
        Just $ \bc -> let
            ?pinafore = bc
            in jmToValue val
    bdPattern = Just $ qToPatternConstructor pat
    docName = name
    docValueType = qTypeDescription @baseupdate @t
    docIsPattern = True
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}

mkPatEntry ::
       forall baseupdate v lt.
       ( HasPinaforeEntityUpdate baseupdate
       , FromPinaforeType baseupdate v
       , ToListShimWit PinaforeShim (PinaforeType baseupdate 'Positive) lt
       )
    => Name
    -> Text
    -> Text
    -> (v -> Maybe (HList lt))
    -> DocTreeEntry (BindDoc baseupdate)
mkPatEntry name docDescription docValueType pat = let
    bdName = name
    bdValue = Nothing
    bdPattern = Just $ qToPatternConstructor pat
    docName = name
    docIsPattern = True
    bdDoc = MkDefDoc {..}
    in EntryDocTreeEntry MkBindDoc {..}
