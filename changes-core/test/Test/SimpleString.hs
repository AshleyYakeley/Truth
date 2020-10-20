module Test.SimpleString
    ( SimpleString(..)
    ) where

import Prelude
import Shapes.Test

newtype SimpleString = MkSimpleString
    { getSimpleString :: String
    } deriving (Eq)

instance Show SimpleString where
    show (MkSimpleString s) = show s

simplifyChar :: Char -> [Char]
simplifyChar 'A' = []
simplifyChar t
    | t < 'A' = ['A', succ t]
simplifyChar t = ['A', pred t]

simplifyChars :: String -> [String]
simplifyChars [] = []
simplifyChars (c:cc) = let
    rest :: [String]
    rest = fmap ((:) c) $ simplifyChars cc
    changes :: [String]
    changes = fmap (\c' -> c' : cc) $ simplifyChar c
    in changes <> rest

instance Arbitrary SimpleString where
    arbitrary = MkSimpleString . getPrintableString <$> arbitrary
    shrink (MkSimpleString []) = []
    shrink (MkSimpleString s@(_:cc)) = MkSimpleString <$> (cc : simplifyChars s)
