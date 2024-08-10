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
        , tModule "m" "a=4" $
          tGroup
              "exprs"
              [ testExpectSuccess "import \"m\" in if a == 4 then pure () else fail \"wrong\""
              , testExpectSuccess "import \"m\" in let b = a in if b == 4 then pure () else fail \"wrong\""
              ]
        , tModule "m" "datatype T of T1; T2 end" $
          tGroup
              "type"
              [ testExpectSuccess "import \"m\" in T1.T >- match T1.T => pure (); T2.T => fail \"wrong\" end"
              , testExpectSuccess
                    "import \"m\" in let t: T = T1.T in t >- match T1.T => pure (); T2.T => fail \"wrong\" end"
              , testExpectSuccess "import \"m\" in let f: T -> T = fn x => x in pure ()"
              , testExpectSuccess "let import \"m\" in expose T          in let f: T -> T = fn x => x in pure ()"
              , testExpectReject " let import \"m\" in expose            in let f: T -> T = fn x => x in pure ()"
              , testExpectReject " let import \"m\" in expose T1.T, T2.T in let f: T -> T = fn x => x in pure ()"
              , testExpectSuccess "let import \"m\" in expose T1.T       in let f = T1.T in pure ()"
              , testExpectReject " let import \"m\" in expose T2.T       in let f = T1.T in pure ()"
              ]
        , tModule "m" "entitytype T" $
          tGroup "entitytype" [testExpectSuccess "import \"m\" in let datatype D of MkD T end; in pure ()"]
        , tModule "m" "entitytype P; entitytype Q" $
          tModule "n" "let import \"m\" end; subtype P <: Q in expose" $
          tGroup
              "subtype"
              [ testExpectReject "import \"m\" in let f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "import \"m\" in import \"n\" in let f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "import \"n\" in import \"m\" in let f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "import \"m\", \"n\" in let f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "import \"n\", \"m\" in let f: P -> Q = fn x => x in pure ()"
              , testExpectSuccess "import \"m\" in let import \"n\" in expose; f: P -> Q = fn x => x in pure ()"
              ]
        , tGroup
              "purity"
              [ tModule "m" "a = 1" $ testExpectSuccess "import \"m\" in pure ()"
              , tModule "m" "a = b" $ testExpectReject "import \"m\" in pure ()"
              , tModule "m" "a = ?b" $ testExpectSuccess "import \"m\" in pure ()"
              , testExpectSuccess "let f = fn x => let y = x in y in pure ()"
              , testExpectSuccess "let f = fn x => let let y = x in expose y in y in pure ()"
              , testExpectReject "let f = fn x => let let y = z in expose y in y in pure ()"
              , testExpectSuccess "let y = ?x in pure ()"
              , testExpectSuccess "let let y = ?x in expose y in pure ()"
              ]
        ]
