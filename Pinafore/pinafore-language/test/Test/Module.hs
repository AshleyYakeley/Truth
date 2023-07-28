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
              , testExpectSuccess "let a=4 in if a == 4 then pure () else fail \"wrong\""
              ]
        , tModule "m" "expose a of a=4 end" $
          tGroup
              "exprs"
              [ testExpectSuccess "import \"m\" in if a == 4 then pure () else fail \"wrong\""
              , testExpectSuccess "import \"m\" in let b = a in if b == 4 then pure () else fail \"wrong\""
              ]
        , tModule "m" "expose T, T1.T, T2.T of datatype T of T1; T2 end end" $
          tGroup
              "type"
              [ testExpectSuccess "import \"m\" in T1.T >- match T1.T => pure (); T2.T => fail \"wrong\" end"
              , testExpectSuccess
                    "import \"m\" in let t: T = T1.T in t >- match T1.T => pure (); T2.T => fail \"wrong\" end"
              , testExpectSuccess "import \"m\" in let f: T -> T = fn x => x in pure ()"
              , testExpectSuccess "let expose T of import \"m\" end in let f: T -> T = fn x => x in pure ()"
              , testExpectReject "let expose of import \"m\" end in let f: T -> T = fn x => x in pure ()"
              , testExpectReject "let expose T1.T, T2.T of import \"m\" end in let f: T -> T = fn x => x in pure ()"
              , testExpectSuccess "let expose T1.T of import \"m\" end in let f = T1.T in pure ()"
              , testExpectReject "let expose T2.T of import \"m\" end in let f = T1.T in pure ()"
              ]
        , tModule "m" "expose a of a = b end" $ testExpectReject "import \"m\" in pure ()"
        , tModule "m" "expose T of opentype T end" $
          tGroup "opentype" [testExpectSuccess "import \"m\" in let datatype D of MkD T end; in pure ()"]
        , tModule "m" "expose P, Q of opentype P; opentype Q end" $
          tModule "n" "expose of import \"m\" end; subtype P <: Q end" $
          tGroup
              "subtype"
              [ testExpectReject "import \"m\" in let f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "import \"m\" in import \"n\" in let f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "import \"n\" in import \"m\" in let f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "import \"m\", \"n\" in let f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "import \"n\", \"m\" in let f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "import \"m\" in let expose of import \"n\" end; f: P -> Q = fn x => x in pure ()"
              ]
        ]
