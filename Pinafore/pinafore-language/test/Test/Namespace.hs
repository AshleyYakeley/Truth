module Test.Namespace
    ( testNamespace
    ) where

import Shapes
import Test.RunScript

testNamespace :: TestTree
testNamespace =
    runScriptTestTree $
    tDecls
        [ "using Function"
        , "using Action"
        , "using Entity"
        , "using Showable"
        , "pass = return ()"
        , "test = fn b => if b then pass else fail \"wrong\""
        ] $
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
                    , testExpectSuccess "test $ a.M. == 4"
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let a = 3 in test $ a.M == 4"
                    , testExpectSuccess "let using M; b = a in test $ b == 4"
                    , testExpectSuccess "let b = a.M in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = a.M in test $ b == 4"
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
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let a = 3 in test $ a.M == 4"
                    , testExpectSuccess "let using M; b = a in test $ b == 4"
                    , testExpectSuccess "let b = a.M in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = a.M in test $ b == 4"
                    ]
              , tDecls ["expose a.M of namespace M of a=4 end end"] $
                tGroup
                    "expose-namespace"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "let using M in test $ a == 4"
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let a = 3 in test $ a.M == 4"
                    , testExpectSuccess "let using M; b = a in test $ b == 4"
                    , testExpectSuccess "let b = a.M in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = a.M in test $ b == 4"
                    ]
              , tDecls ["expose namespace M of namespace M of a=4 end end"] $
                tGroup
                    "expose-whole-namespace"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "let using M in test $ a == 4"
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let a = 3 in test $ a.M == 4"
                    , testExpectSuccess "let using M; b = a in test $ b == 4"
                    , testExpectSuccess "let b = a.M in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = a.M in test $ b == 4"
                    ]
              ]
        , tDecls ["namespace M of expose T, T1, T2 of datatype T of T1; T2 end end end"] $
          tGroup
              "type"
              [ testExpectSuccess "pass"
              , testExpectSuccess "let using M in T1 >- match T1 => pass; T2 => fail \"wrong\" end"
              , testExpectSuccess "T1.M >- match T1.M => pass; T2.M => fail \"wrong\" end"
              , testExpectSuccess "let using M in let t: T = T1 in t >- match T1 => pass; T2 => fail \"wrong\" end"
              , testExpectSuccess "let using M; t: T = T1 in t >- match T1 => pass; T2 => fail \"wrong\" end"
              , testExpectSuccess "let t: T.M = T1.M in t >- match T1.M => pass; T2.M => fail \"wrong\" end"
              , testExpectSuccess "let using M; f: T -> T = fn x => x in pass"
              , testExpectSuccess "let using M in let f: T -> T = fn x => x in pass"
              , testExpectSuccess "let f: T.M -> T.M = fn x => x in pass"
              ]
        , tDecls ["namespace M of expose a of a = b end end"] $ testExpectReject "let using M in pass"
        , tDecls ["namespace M of expose T of opentype T end end"] $
          tGroup
              "opentype"
              [ testExpectSuccess "let using M; datatype D of MkD T end; in pass"
              , testExpectSuccess "let datatype D of MkD T.M end; in pass"
              ]
        , tGroup
              "subtype"
              [ tDecls ["namespace M of opentype P; opentype Q end", "namespace N of using M.; subtype P <: Q end"] $
                tGroup
                    "namespace"
                    [testExpectSuccess "pass", testExpectSuccess "let using M; f: P -> Q = fn x => x in pass"]
              , tDecls
                    [ "namespace M of expose P, Q of opentype P; opentype Q end end"
                    , "namespace N of expose of using M.; subtype P <: Q end end"
                    ] $
                tGroup
                    "expose"
                    [testExpectSuccess "pass", testExpectSuccess "let using M; f: P -> Q = fn x => x in pass"]
              ]
        , tGroup
              "names"
              [ testExpectSuccess "let namespace M of a = 6; b = a end; in test $ b.M == 6"
              , testExpectSuccess "let namespace M of a = 6; b = a end; using M; in test $ b == 6"
              , testExpectSuccess "let namespace P of a = 6; namespace Q of b = a end end; in test $ b.Q.P == 6"
              , testExpectSuccess "let namespace P of a = 6 end; namespace Q.P of b = a end; in test $ b.Q.P == 6"
              ]
        , tGroup
              "decl"
              [ testExpectSuccess "let nna=1; namespace M of nna=2 end in test $ nna.M == 2"
              , testExpectSuccess "let nna=1; namespace M of nnb=2 end in test $ nna == 1"
              , testExpectSuccess "let nna=1; namespace M of nna=2 end in test $ nna == 1"
              , testExpectSuccess "let nna=1; namespace M of nna=2 end in test $ nna. == 1"
              ]
        , tDecls ["testeq = fns e f => if e == f then pass else fail $ \"found: \" <>.Text show f"] $
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
        , tGroup
              "pattern"
              [ testExpectSuccess "test $ 3 >- fn x => x == 3"
              , testExpectSuccess "test $ 3 >- fn x as M => x.M == 3"
              , testExpectSuccess "test $ 3 >- fn x as M as N => x.M.N == 3"
              , testExpectReject "test $ 3 >- fn x as M => x == 3"
              , testExpectSuccess "test $ Just 3 >- match (Just x) as M => x.M == 3; Nothing => False; end"
              , testExpectSuccess "test $ Just 3 >- match Just (x as M) => x.M == 3; Nothing => False; end"
              ]
        , tGroup
              "qualified"
              [ testExpectSuccess "let x = fns a b => a ++.Property b in pass"
              , testExpectSuccess "let x = (++.Property) in pass"
              , testExpectSuccess "let x = fns a b => a ++.Property. b in pass"
              , testExpectSuccess "let x = (++.Property.) in pass"
              , testExpectSuccess "let x = fns a b => a ..Property b in pass"
              , testExpectSuccess "let x = (..Property) in pass"
              , testExpectSuccess "let x = fns a b => a ..Property. b in pass"
              , testExpectSuccess "let x = (..Property.) in pass"
              ]
        ]
