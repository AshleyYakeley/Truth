module Test.Module
    ( testModule
    ) where

import Shapes
import Test.RunScript

testModule :: TestTree
testModule =
    runScriptTestTree $
    tWith ["Function", "Action", "Entity"] $
    tGroup
        "module"
        [ tGroup
              "none"
              [ testExpectSuccess "pure ()"
              , testExpectThrow "fail \"wrong\""
              , testExpectSuccess "let {a=4} if a == 4 then pure () else fail \"wrong\""
              ]
        , tModule "m" "a=4" $
          tGroup
              "exprs"
              [ testExpectSuccess "import \"m\" if a == 4 then pure () else fail \"wrong\""
              , testExpectSuccess "import \"m\" let {b = a} if b == 4 then pure () else fail \"wrong\""
              ]
        , tModule "m" "datatype T {T1; T2}" $
          tGroup
              "type"
              [ testExpectSuccess "import \"m\" T1.T >- fn {T1.T => pure (); T2.T => fail \"wrong\"}"
              , testExpectSuccess "import \"m\" let {t: T = T1.T} t >- fn {T1.T => pure (); T2.T => fail \"wrong\"}"
              , testExpectSuccess "import \"m\" let {f: T -> T = fn x => x} pure ()"
              , testExpectSuccess "let {import \"m\" expose T         } let {f: T -> T = fn x => x} pure ()"
              , testExpectReject " let {import \"m\" expose           } let {f: T -> T = fn x => x} pure ()"
              , testExpectReject " let {import \"m\" expose T1.T, T2.T} let {f: T -> T = fn x => x} pure ()"
              , testExpectSuccess "let {import \"m\" expose T1.T      } let {f = T1.T} pure ()"
              , testExpectReject " let {import \"m\" expose T2.T      } let {f = T1.T} pure ()"
              ]
        , tModule "m" "entitytype T" $
          tGroup "entitytype" [testExpectSuccess "import \"m\" let {datatype D {MkD T};} pure ()"]
        , tModule "m" "entitytype P; entitytype Q" $
          tModule "n" "let {import \"m\"; subtype P <: Q} expose" $
          tGroup
              "subtype"
              [ testExpectReject "import \"m\" let {f: P -> Q = fn x => x} pure ()"
              , testExpectSuccess "import \"m\" import \"n\" let {f: P -> Q = fn x => x} pure ()"
              , testExpectSuccess "import \"n\" import \"m\" let {f: P -> Q = fn x => x} pure ()"
              , testExpectSuccess "import \"m\", \"n\" let {f: P -> Q = fn x => x} pure ()"
              , testExpectSuccess "import \"n\", \"m\" let {f: P -> Q = fn x => x} pure ()"
              , testExpectSuccess "import \"m\" let {import \"n\" expose; f: P -> Q = fn x => x} pure ()"
              ]
        , tGroup
              "purity"
              [ tModule "m" "a = 1" $ testExpectSuccess "import \"m\" pure ()"
              , tModule "m" "a = b" $ testExpectReject "import \"m\" pure ()"
              , tModule "m" "a = ?b" $ testExpectSuccess "import \"m\" pure ()"
              , testExpectSuccess "let {f = fn x => let {y = x} y} pure ()"
              , testExpectSuccess "let {f = fn x => let {let {y = x} expose y} y} pure ()"
              , testExpectReject "let {f = fn x => let {let {y = z} expose y} y} pure ()"
              , testExpectSuccess "let {y = ?x} pure ()"
              , testExpectSuccess "let {let {y = ?x} expose y} pure ()"
              ]
        ]
