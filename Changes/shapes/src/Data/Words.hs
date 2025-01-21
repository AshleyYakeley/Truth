module Data.Words where

import Shapes.Import

class PairWords single double | single -> double, double -> single where
    hi :: double -> single
    lo :: double -> single
    hilo :: single -> single -> double

lohi ::
    forall single double.
    PairWords single double =>
    single ->
    single ->
    double
lohi l h = hilo h l

instance PairWords Word8 Word16 where
    hi w = fromIntegral $ shiftR w 8
    lo w = fromIntegral w
    hilo h l = shiftL (fromIntegral h) 8 + fromIntegral l

instance PairWords Word16 Word32 where
    hi w = fromIntegral $ shiftR w 16
    lo w = fromIntegral w
    hilo h l = shiftL (fromIntegral h) 16 + fromIntegral l
