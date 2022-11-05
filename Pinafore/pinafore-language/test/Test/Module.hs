module Test.Module
    ( testModule
    , testNamespace
    ) where

import Shapes
import Test.RunScript

testModule :: TestTree
testModule =
    runScriptTestTree $
    tGroup
        "module"
        [ tGroup
              "none"
              [ testExpectSuccess "return ()"
              , testExpectThrow "fail \"wrong\""
              , testExpectSuccess "let a=4 in if a == 4 then return () else fail \"wrong\""
              ]
        , tModule "m" "expose a of a=4 end" $
          tGroup
              "exprs"
              [ testExpectSuccess "let import \"m\" in if a == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let import \"m\"; b = a in if b == 4 then return () else fail \"wrong\""
              ]
        , tModule "m" "expose T, T1, T2 of datatype T of T1; T2 end end" $
          tGroup
              "type"
              [ testExpectSuccess "let import \"m\" in T1 >- match T1 => return (); T2 => fail \"wrong\" end"
              , testExpectSuccess
                    "let import \"m\" in let t: T = T1 in t >- match T1 => return (); T2 => fail \"wrong\" end"
              , testExpectSuccess "let import \"m\"; t: T = T1 in t >- match T1 => return (); T2 => fail \"wrong\" end"
              , testExpectSuccess "let import \"m\"; f: T -> T = fn x => x in return ()"
              , testExpectSuccess "let import \"m\" in let f: T -> T = fn x => x in return ()"
              , testExpectSuccess "let expose T of import \"m\" end; f: T -> T = fn x => x in return ()"
              , testExpectReject "let expose of import \"m\" end; f: T -> T = fn x => x in return ()"
              , testExpectReject "let expose T1, T2 of import \"m\" end; f: T -> T = fn x => x in return ()"
              , testExpectSuccess "let expose T1 of import \"m\" end; f = T1 in return ()"
              , testExpectReject "let expose T2 of import \"m\" end; f = T1 in return ()"
              ]
        , tModule "m" "expose a of a = b end" $ testExpectReject "let import \"m\" in return ()"
        , tModule "m" "expose T of opentype T end" $
          tGroup "opentype" [testExpectSuccess "let import \"m\"; datatype D of MkD T end; in return ()"]
        , tModule "m" "expose P, Q of opentype P; opentype Q end" $
          tModule "n" "expose of import \"m\"; subtype P <: Q end" $
          tGroup
              "subtype"
              [ testExpectReject "let import \"m\"; f: P -> Q = fn x => x in return ()"
              , testExpectSuccess "let import \"m\"; import \"n\"; f: P -> Q = fn x => x in return ()"
              , testExpectSuccess "let import \"n\"; import \"m\"; f: P -> Q = fn x => x in return ()"
              , testExpectSuccess "let import \"m\"; expose of import \"n\" end; f: P -> Q = fn x => x in return ()"
              ]
        ]

testNamespace :: TestTree
testNamespace =
    runScriptTestTree $
    tDecls ["pass = return ()", "test = fn b => if b then pass else fail \"wrong\""] $
    tGroup
        "namespace"
        [ tGroup
              "none"
              [ testExpectSuccess "pass"
              , testExpectThrow "fail \"wrong\""
              , testExpectSuccess "test True"
              , testExpectThrow "test False"
              , testExpectSuccess "let a=4 in test $ a == 4"
              ]
        , tGroup
              "exprs"
              [ tDecls ["namespace M of a=4 end"] $
                tGroup
                    "namespace"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "let using M in test $ a == 4"
                    , testExpectSuccess "test $  M.a == 4"
                    , testExpectSuccess "let a = 3 in test $ M.a == 4"
                    , testExpectSuccess "let using M; b = a in test $ b == 4"
                    , testExpectSuccess "let b = M.a in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = M.a in test $ b == 4"
                    ]
              , tDecls ["expose a of a=4 end"] $
                tGroup
                    "expose"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "test $ a == 4"
                    , testExpectSuccess "let b = a in test $ b == 4"
                    ]
              , tDecls ["expose a of b=4 end"] $ tGroup "non-expose" [testExpectReject "pass"]
              , tDecls ["namespace M of expose a of a=4 end end"] $
                tGroup
                    "namespace-expose"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "let using M in test $ a == 4"
                    , testExpectSuccess "test $ M.a == 4"
                    , testExpectSuccess "let a = 3 in test $ M.a == 4"
                    , testExpectSuccess "let using M; b = a in test $ b == 4"
                    , testExpectSuccess "let b = M.a in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = M.a in test $ b == 4"
                    ]
              , tDecls ["expose M.a of namespace M of a=4 end end"] $
                tGroup
                    "expose-namespace"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "let using M in test $ a == 4"
                    , testExpectSuccess "test $ M.a == 4"
                    , testExpectSuccess "let a = 3 in test $ M.a == 4"
                    , testExpectSuccess "let using M; b = a in test $ b == 4"
                    , testExpectSuccess "let b = M.a in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = M.a in test $ b == 4"
                    ]
              , tDecls ["expose namespace M of namespace M of a=4 end end"] $
                tGroup
                    "expose-whole-namespace"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "let using M in test $ a == 4"
                    , testExpectSuccess "test $ M.a == 4"
                    , testExpectSuccess "let a = 3 in test $ M.a == 4"
                    , testExpectSuccess "let using M; b = a in test $ b == 4"
                    , testExpectSuccess "let b = M.a in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = M.a in test $ b == 4"
                    ]
              ]
        , tDecls ["namespace M of expose T, T1, T2 of datatype T of T1; T2 end end end"] $
          tGroup
              "type"
              [ testExpectSuccess "pass"
              , testExpectSuccess "let using M in T1 >- match T1 => pass; T2 => fail \"wrong\" end"
              , testExpectSuccess "M.T1 >- match M.T1 => pass; M.T2 => fail \"wrong\" end"
              , testExpectSuccess "let using M in let t: T = T1 in t >- match T1 => pass; T2 => fail \"wrong\" end"
              , testExpectSuccess "let using M; t: T = T1 in t >- match T1 => pass; T2 => fail \"wrong\" end"
              , testExpectSuccess "let t: M.T = M.T1 in t >- match M.T1 => pass; M.T2 => fail \"wrong\" end"
              , testExpectSuccess "let using M; f: T -> T = fn x => x in pass"
              , testExpectSuccess "let using M in let f: T -> T = fn x => x in pass"
              , testExpectSuccess "let f: M.T -> M.T = fn x => x in pass"
              ]
        , tDecls ["namespace M of expose a of a = b end end"] $ testExpectReject "let using M in pass"
        , tDecls ["namespace M of expose T of opentype T end end"] $
          tGroup
              "opentype"
              [ testExpectSuccess "let using M; datatype D of MkD T end; in pass"
              , testExpectSuccess "let datatype D of MkD M.T end; in pass"
              ]
        , tGroup
              "subtype"
              [ tDecls ["namespace M of opentype P; opentype Q end", "namespace N of using .M; subtype P <: Q end"] $
                tGroup
                    "namespace"
                    [testExpectSuccess "pass", testExpectSuccess "let using M; f: P -> Q = fn x => x in pass"]
              , tDecls
                    [ "namespace M of expose P, Q of opentype P; opentype Q end end"
                    , "namespace N of expose of using .M; subtype P <: Q end end"
                    ] $
                tGroup
                    "expose"
                    [testExpectSuccess "pass", testExpectSuccess "let using M; f: P -> Q = fn x => x in pass"]
              ]
        , tGroup
              "names"
              [ testExpectSuccess "let namespace M of a = 6; b = a end; in test $ M.b == 6"
              , testExpectSuccess "let namespace M of a = 6; b = a end; using M; in test $ b == 6"
              , testExpectSuccess "let namespace P of a = 6; namespace Q of b = a end end; in test $ P.Q.b == 6"
              , testExpectSuccess "let namespace P of a = 6 end; namespace P.Q of b = a end; in test $ P.Q.b == 6"
              ]
        , tGroup
              "decl"
              [ testExpectSuccess "let nna=1; namespace M of nna=2 end in test $ M.nna == 2"
              , testExpectSuccess "let nna=1; namespace M of nnb=2 end in test $ nna == 1"
              , testExpectSuccess "let nna=1; namespace M of nna=2 end in test $ nna == 1"
              , testExpectSuccess "let nna=1; namespace M of nna=2 end in test $ .nna == 1"
              ]
        , tDecls ["testeq = fns e f => if e == f then pass else fail $ \"found: \" <> show f"] $
          tDecls
              [ "a = 1"
              , "a0 = a"
              , "namespace M of a=2 end"
              , "a1 = a"
              , "using M"
              , "a2 = a"
              , "a = 3"
              , "i = fn a => a"
              , "t = fn a => testeq 4 a"
              ] $
          tGroup
              "scope"
              [ testExpectSuccess "testeq 0 0"
              , testExpectSuccess "testeq 1 a0"
              , testExpectSuccess "testeq 1 a1"
              , testExpectSuccess "testeq 1 a2"
              , testExpectSuccess "testeq 3 a"
              , testExpectSuccess "testeq 4 $ i 4"
              , testExpectSuccess "t 4"
              ]
        , tGroup
              "clash"
              [ testExpectReject "let namespace M of opentype M end; i: M -> M = fn x => x in pass"
              , testExpectSuccess "let namespace M of opentype M end; using M; i: M -> M = fn x => x in pass"
              ]
        ]
