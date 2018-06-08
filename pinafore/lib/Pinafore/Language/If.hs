module Pinafore.Language.If
    ( qifthenelse
    ) where

import Data.List (zipWith)
import Pinafore.Entity
import Pinafore.Language.Convert
import Pinafore.Language.Lifted
import Pinafore.Language.Value
import Pinafore.Morphism
import Pinafore.PredicateMorphism
import Pinafore.Types
import Shapes
import Truth.Core

fvalIfThenElse ::
       forall baseedit t.
       IO t
    -> QLiteral baseedit Bool
    -> PinaforeFunctionValue baseedit t
    -> PinaforeFunctionValue baseedit t
    -> PinaforeFunctionValue baseedit t
fvalIfThenElse iovdef vi vt ve = let
    mIfThenElse :: (Maybe Bool, (t, t)) -> IO t
    mIfThenElse (Just True, (v, _)) = return v
    mIfThenElse (Just False, (_, v)) = return v
    mIfThenElse (Nothing, _) = iovdef
    in ioFuncEditFunction mIfThenElse . (pairJoinEditFunctions vi $ pairJoinEditFunctions vt ve)

qfifthenelse ::
       forall baseedit. HasPinaforeEntityEdit baseedit
    => QLiteral baseedit Bool
    -> QValue baseedit
    -> QValue baseedit
    -> QValue baseedit
qfifthenelse _ v@(MkAny QTException _) _ = v
qfifthenelse _ _ v@(MkAny QTException _) = v
qfifthenelse f (MkAny QTList lt) (MkAny QTList le) =
    if length lt == length le
        then MkAny QTList $ zipWith (qfifthenelse f) lt le
        else qexception $ pack $ "cannot match lists of lengths " ++ show (length lt) ++ " and " ++ show (length le)
qfifthenelse f t e
    | SuccessResult vt <- fromQValue @baseedit @(QLiteral baseedit Text) t
    , SuccessResult ve <- fromQValue @baseedit @(QLiteral baseedit Text) e =
        toQValue $ fvalIfThenElse (return Nothing) f vt ve
qfifthenelse f t e
    | SuccessResult vt <- fromQValue @baseedit @(QPoint baseedit) t
    , SuccessResult ve <- fromQValue @baseedit @(QPoint baseedit) e = toQValue $ fvalIfThenElse newPoint f vt ve
qfifthenelse f t e
    | SuccessResult vt <- fromQValue @baseedit @(QSetPoint baseedit) t
    , SuccessResult ve <- fromQValue @baseedit @(QSetPoint baseedit) e =
        toQValue $ fvalIfThenElse (return mempty) f vt ve
-- possibly add cases for QTMorphism and QTInverseMorphism?
qfifthenelse f t e
    | SuccessResult vt <- qpartialapply t
    , SuccessResult ve <- qpartialapply e = MkAny QTFunction $ \a -> qfifthenelse f (vt a) (ve a)
qfifthenelse f (MkAny QTUserInterface vt) (MkAny QTUserInterface ve) = let
    pickUISpec :: Maybe Bool -> UISpec (ConstEdit Point) baseedit
    pickUISpec Nothing = uiNull
    pickUISpec (Just True) = vt
    pickUISpec (Just False) = ve
    in toQValue $ wholeEditFunction pickUISpec . f
qfifthenelse f t e
    | SuccessResult vt <- fromQValue @baseedit @(QAction baseedit) t
    , SuccessResult ve <- fromQValue @baseedit @(QAction baseedit) e =
        toQValue $ do
            mf <- qGetFunctionValue f
            case mf of
                Just True -> vt
                Just False -> ve
                Nothing -> liftIO $ fail "\"if\"-clause not boolean"
qfifthenelse _ (MkAny tt _) (MkAny te _) =
    qexception $ pack $ "\"then\"/\"else\" cannot match " ++ show tt ++ " and " ++ show te

qifthenelse ::
       HasPinaforeEntityEdit baseedit => Lifted baseedit Bool -> QValue baseedit -> QValue baseedit -> QValue baseedit
qifthenelse (LiftedConstant True) valt _ = valt
qifthenelse (LiftedConstant False) _ vale = vale
qifthenelse (LiftedFunction func) valt vale = qfifthenelse func valt vale