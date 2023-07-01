module Test.Module
    ( testModule
    ) where

import Shapes
import Test.RunScript

testModule :: TestTree
testModule =
    runScriptTestTree $
    tDecls ["using Function", "using Action", "using Entity"] $
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
              [ testExpectSuccess "let import \"m\" in if a == 4 then pure () else fail \"wrong\""
              , testExpectSuccess "let import \"m\"; b = a in if b == 4 then pure () else fail \"wrong\""
              ]
        , tModule "m" "expose T, T1.T, T2.T of datatype T of T1; T2 end end" $
          tGroup
              "type"
              [ testExpectSuccess "let import \"m\" in T1.T >- match T1.T => pure (); T2.T => fail \"wrong\" end"
              , testExpectSuccess
                    "let import \"m\" in let t: T = T1.T in t >- match T1.T => pure (); T2.T => fail \"wrong\" end"
              , testExpectSuccess
                    "let import \"m\"; t: T = T1.T in t >- match T1.T => pure (); T2.T => fail \"wrong\" end"
              , testExpectSuccess "let import \"m\"; f: T -> T = fn x => x in pure ()"
              , testExpectSuccess "let import \"m\" in let f: T -> T = fn x => x in pure ()"
              , testExpectSuccess "let expose T of import \"m\" end; f: T -> T = fn x => x in pure ()"
              , testExpectReject "let expose of import \"m\" end; f: T -> T = fn x => x in pure ()"
              , testExpectReject "let expose T1.T, T2.T of import \"m\" end; f: T -> T = fn x => x in pure ()"
              , testExpectSuccess "let expose T1.T of import \"m\" end; f = T1.T in pure ()"
              , testExpectReject "let expose T2.T of import \"m\" end; f = T1.T in pure ()"
              ]
        , tModule "m" "expose a of a = b end" $ testExpectReject "let import \"m\" in pure ()"
        , tModule "m" "expose T of opentype T end" $
          tGroup "opentype" [testExpectSuccess "let import \"m\"; datatype D of MkD T end; in pure ()"]
        , tModule "m" "expose P, Q of opentype P; opentype Q end" $
          tModule "n" "expose of import \"m\"; subtype P <: Q end" $
          tGroup
              "subtype"
              [ testExpectReject "let import \"m\"; f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "let import \"m\"; import \"n\"; f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "let import \"n\"; import \"m\"; f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "let import \"m\"; expose of import \"n\" end; f: P -> Q = fn x => x in pure ()"
              ]
        ]
