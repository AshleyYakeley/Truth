module Control.Stream.Filter where

import Data.Lens
import Shapes.Import

data EndOrItem a
    = Item a
    | End

instance Functor EndOrItem where
    fmap ab (Item a) = Item $ ab a
    fmap _ End = End

eoiToMaybe :: EndOrItem a -> Maybe a
eoiToMaybe (Item a) = Just a
eoiToMaybe End = Nothing

data Filter m a b =
    forall s. MkFilter s
                       (EndOrItem a -> StateT s m [b])

instance Monad m => Functor (Filter m a) where
    fmap ab (MkFilter s0 f) = MkFilter s0 $ (fmap $ fmap $ fmap ab) f

instance Monad m => Category (Filter m) where
    id =
        MkFilter () $ \ma ->
            return $
            case ma of
                End -> []
                Item a -> [a]
    MkFilter s0bc fbc . MkFilter s0ab fab =
        MkFilter (s0ab, s0bc) $ \ma -> do
            bb <- lensStateT fstLens $ fab ma
            let
                bmb =
                    case ma of
                        Item _ -> fmap Item bb
                        End -> fmap Item bb <> [End]
            ccc <- for bmb $ \mb -> lensStateT sndLens $ fbc mb
            return $ mconcat ccc

funcFilter :: Monad m => (a -> b) -> Filter m a b
funcFilter ab = fmap ab id

lineBufferFilter :: Monad m => Filter m Text Text
lineBufferFilter =
    MkFilter "" $ \case
        End -> do
            old <- get
            return [old]
        Item new -> do
            old <- get
            let ll = splitElem '\n' $ old <> new
            put $ lastEx ll
            return $ fmap (\t -> t <> "\n") $ initEx ll
