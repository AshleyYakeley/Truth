module Test.Numeric
    ( testNumeric
    ) where

import Pinafore.Base
import Shapes
import Shapes.Numeric
import Shapes.Test

class HasNaN t where
    nan :: t
    isnan :: t -> Bool

instance HasNaN Double where
    nan = 0 / 0
    isnan = isNaN

instance HasNaN SafeRational where
    nan = SRNaN
    isnan SRNaN = True
    isnan (SRNumber _) = False

instance HasNaN Number where
    nan = 0 / 0
    isnan = numberIsNaN

testBothProperty :: String -> (forall r. (RealFrac r, HasNaN r) => Proxy r -> IO ()) -> TestTree
testBothProperty name prop =
    testTree
        name
        [ testTree "Double" $ prop @Double Proxy
        , testTree "Number" $ prop @Number Proxy
        , testTree "SafeRational" $ prop @SafeRational Proxy
        ]

testNumeric :: TestTree
testNumeric =
    testTree
        "numeric"
        [ testBothProperty "isnan nan" $ \(Proxy :: Proxy r) -> assertEqual "" True $ isnan @r nan
        , testBothProperty "isnan 3" $ \(Proxy :: Proxy r) -> assertEqual "" False $ isnan @r 3
        , testBothProperty "nan == nan" $ \(Proxy :: Proxy r) -> assertEqual "" False $ nan @r == nan
        , testBothProperty "3 == nan" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (3 :: r) == nan
        , testBothProperty "nan == 3" $ \(Proxy :: Proxy r) -> assertEqual "" False $ nan @r == 3
        , testBothProperty "3 == 3" $ \(Proxy :: Proxy r) -> assertEqual "" True $ (3 :: r) == 3
        , testBothProperty "nan /= nan" $ \(Proxy :: Proxy r) -> assertEqual "" True $ nan @r /= nan
        , testBothProperty "3 /= nan" $ \(Proxy :: Proxy r) -> assertEqual "" True $ (3 :: r) /= nan
        , testBothProperty "nan /= 3" $ \(Proxy :: Proxy r) -> assertEqual "" True $ nan @r /= 3
        , testBothProperty "3 /= 3" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (3 :: r) /= 3
        , testBothProperty "compare nan nan" $ \(Proxy :: Proxy r) -> assertEqual "" GT $ compare (nan @r) nan
        , testBothProperty "nan > nan" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (nan @r) > nan
        , testBothProperty "nan < nan" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (nan @r) < nan
        , testBothProperty "nan >= nan" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (nan @r) >= nan
        , testBothProperty "nan <= nan" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (nan @r) <= nan
        , testBothProperty "nan > 3" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (nan @r) > 3
        , testBothProperty "nan < 3" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (nan @r) < 3
        , testBothProperty "nan >= 3" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (nan @r) >= 3
        , testBothProperty "nan <= 3" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (nan @r) <= 3
        , testBothProperty "3 > nan" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (3 :: r) > nan
        , testBothProperty "3 < nan" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (3 :: r) < nan
        , testBothProperty "3 >= nan" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (3 :: r) >= nan
        , testBothProperty "3 <= nan" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (3 :: r) <= nan
        , testBothProperty "3 > 3" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (3 :: r) > 3
        , testBothProperty "3 < 3" $ \(Proxy :: Proxy r) -> assertEqual "" False $ (3 :: r) < 3
        , testBothProperty "3 >= 3" $ \(Proxy :: Proxy r) -> assertEqual "" True $ (3 :: r) >= 3
        , testBothProperty "3 <= 3" $ \(Proxy :: Proxy r) -> assertEqual "" True $ (3 :: r) <= 3
        , testBothProperty "isnan $ 0 / 0" $ \(Proxy :: Proxy r) -> assertEqual "" True $ isnan @r $ 0 / 0
        , testTree "isnan $ 1 / 0" [testTree "SafeRational" $ assertEqual "" True $ isnan @SafeRational $ 1 / 0]
        ]
