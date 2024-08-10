module Test.Namespace
    ( testNamespace
    ) where

import Shapes
import Test.RunScript

testNamespace :: TestTree
testNamespace =
    runScriptTestTree $
    tWith ["Function", "Action", "Entity", "Showable"] $
    tDecls ["pass = pure ()", "test = fn b => if b then pass else fail \"wrong\""] $
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
              "let"
              [ testExpectSuccess "let f: Integer = undefined in pass"
              , testExpectSuccess "let entitytype P; f: P = undefined in pass"
              , testExpectSuccess "let let entitytype P in expose P; f: P = undefined in pass"
              , testExpectSuccess "let namespace M of entitytype P end; with M. in f: P = undefined in pass"
              ]
        , tGroup
              "exprs"
              [ tDecls ["namespace M of a=4 end"] $
                tGroup
                    "namespace"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "with M in test $ a == 4"
                    , testExpectSuccess "test $ a.M. == 4"
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let a = 3 in test $ a.M == 4"
                    , testExpectSuccess "with M in let b = a in test $ b == 4"
                    , testExpectSuccess "let b = a.M in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = a.M in test $ b == 4"
                    , testExpectSuccess "let a = 3 in with M in let b = a in test $ b == 4"
                    ]
              , tDecls ["namespace M of not = fn x => x end"] $
                tGroup
                    "library"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "test $ not False"
                    , testExpectSuccess "let not = fn x => x in test $ not True"
                    , testExpectSuccess "with M in test $ not True"
                    ]
              , tDecls ["let a=4 in expose a"] $
                tGroup
                    "expose"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "test $ a == 4"
                    , testExpectSuccess "let b = a in test $ b == 4"
                    ]
              , tDecls ["let b=4 in expose a"] $ tGroup "non-expose" [testExpectReject "pass"]
              , tDecls ["namespace M of let a=4 in expose a end"] $
                tGroup
                    "namespace-expose"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "with M in test $ a == 4"
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let a = 3 in test $ a.M == 4"
                    , testExpectSuccess "with M in let b = a in test $ b == 4"
                    , testExpectSuccess "let b = a.M in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = a.M in test $ b == 4"
                    ]
              , tDecls ["let namespace M of a=4 end in expose a.M"] $
                tGroup
                    "expose-namespace"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "with M in test $ a == 4"
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let a = 3 in test $ a.M == 4"
                    , testExpectSuccess "with M in let b = a in test $ b == 4"
                    , testExpectSuccess "let b = a.M in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = a.M in test $ b == 4"
                    ]
              , tDecls ["let namespace M of a=4 end in expose namespace M"] $
                tGroup
                    "expose-whole-namespace"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "with M in test $ a == 4"
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let a = 3 in test $ a.M == 4"
                    , testExpectSuccess "with M in let b = a in test $ b == 4"
                    , testExpectSuccess "let b = a.M in test $ b == 4"
                    , testExpectSuccess "let a = 3; b = a.M in test $ b == 4"
                    ]
              ]
        , tDecls ["namespace M of let datatype T of T1; T2 end in expose T, namespace T end"] $
          tGroup
              "type"
              [ testExpectSuccess "pass"
              , testExpectSuccess "with M in T1.T >- match T1.T => pass; T2.T => fail \"wrong\" end"
              , testExpectSuccess "T1.T.M >- match T1.T.M => pass; T2.T.M => fail \"wrong\" end"
              , testExpectSuccess "with M in let t: T = T1.T in t >- match T1.T => pass; T2.T => fail \"wrong\" end"
              , testExpectSuccess "with M in let t: T = T1.T in t >- match T1.T => pass; T2.T => fail \"wrong\" end"
              , testExpectSuccess "let t: T.M = T1.T.M in t >- match T1.T.M => pass; T2.T.M => fail \"wrong\" end"
              , testExpectSuccess "with M in let f: T -> T = fn x => x in pass"
              , testExpectSuccess "let f: T.M -> T.M = fn x => x in pass"
              ]
        , tDecls ["namespace M of let a = b in expose a end"] $ testExpectReject "with M in pass"
        , tDecls ["namespace M of let entitytype T in expose T end"] $
          tGroup
              "entitytype"
              [ testExpectSuccess "with M in let datatype D of MkD T end; in pass"
              , testExpectSuccess "let datatype D of MkD T.M end; in pass"
              ]
        , tGroup
              "subtype"
              [ tDecls
                    ["namespace M of entitytype P; entitytype Q end", "namespace N of with M. end; subtype P <: Q end"] $
                tGroup
                    "namespace"
                    [testExpectSuccess "pass", testExpectSuccess "with M in let f: P -> Q = fn x => x in pass"]
              , tDecls
                    [ "namespace M of let entitytype P; entitytype Q in expose P, Q end"
                    , "namespace N of with M. in subtype P <: Q end"
                    ] $
                tGroup
                    "expose"
                    [testExpectSuccess "pass", testExpectSuccess "with M in let f: P -> Q = fn x => x in pass"]
              ]
        , tGroup
              "names"
              [ testExpectSuccess "let namespace M of a = 6; b = a end; in test $ b.M == 6"
              , testExpectSuccess "let namespace M of a = 6; b = a end in with M in test $ b == 6"
              , testExpectSuccess "let namespace P of a = 6; namespace Q of b = a end end; in test $ b.Q.P == 6"
              , testExpectSuccess
                    "let namespace P of a = 6 end; namespace P of namespace Q of b = a end end in test $ b.Q.P == 6"
              ]
        , tGroup
              "decl"
              [ testExpectSuccess "let nna=1; namespace M of nna=2 end in test $ nna.M == 2"
              , testExpectSuccess "let nna=1; namespace M of nnb=2 end in test $ nna == 1"
              , testExpectSuccess "let nna=1; namespace M of nna=2 end in test $ nna == 1"
              , testExpectSuccess "let nna=1; namespace M of nna=2 end in test $ nna. == 1"
              ]
        , tDecls ["testeq = fn e, f => if e == f then pass else fail $ \"found: \" <>.Text show f"] $
          tGroup
              "with"
              [ tDecls
                    [ "a = 1"
                    , "a0 = a"
                    , "namespace M of a=2 end"
                    , "a1 = a"
                    , "with M end"
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
                    , testExpectSuccess "testeq 2 a2"
                    , testExpectSuccess "testeq 3 a"
                    , testExpectSuccess "testeq 4 $ i 4"
                    , testExpectSuccess "t 4"
                    ]
              , tDecls ["a = 1", "namespace M of a=2; b=3 end"] $
                tGroup
                    "scope"
                    [ testExpectSuccess "with M in testeq 2 a"
                    , testExpectSuccess "with M() in testeq 1 a"
                    , testExpectSuccess "with M(a) in testeq 2 a"
                    , testExpectSuccess "with M(b) in testeq 1 a"
                    , testExpectSuccess "with M(a,b) in testeq 2 a"
                    , testExpectSuccess "with M except () in testeq 2 a"
                    , testExpectSuccess "with M except (a) in testeq 1 a"
                    , testExpectSuccess "with M except (b) in testeq 2 a"
                    , testExpectSuccess "with M except (a,b) in testeq 1 a"
                    ]
              , tDecls ["namespace D of a=1 end", "namespace M of a=2; b=3 end"] $
                tGroup
                    "as"
                    [ testExpectSuccess "with M as D in testeq 2 a.D"
                    , testExpectSuccess "with M() as D in testeq 1 a.D"
                    , testExpectSuccess "with M(a) as D in testeq 2 a.D"
                    , testExpectSuccess "with M(b) as D in testeq 1 a.D"
                    , testExpectSuccess "with M(a,b) as D in testeq 2 a.D"
                    , testExpectSuccess "with M except () as D in testeq 2 a.D"
                    , testExpectSuccess "with M except (a) as D in testeq 1 a.D"
                    , testExpectSuccess "with M except (b) as D in testeq 2 a.D"
                    , testExpectSuccess "with M except (a,b) as D in testeq 1 a.D"
                    ]
              , tDecls
                    [ "namespace D of a=4 end"
                    , "namespace N of a=1 end"
                    , "namespace M of a=3; namespace N of a=2 end end"
                    ] $
                tGroup
                    "namespace"
                    [ testExpectSuccess "testeq 1 a.N"
                    , testExpectSuccess "with M() in testeq 1 a.N"
                    , testExpectSuccess "with M(a.N) in testeq 2 a.N"
                    , testExpectSuccess "with M in testeq 2 a.N"
                    , testExpectSuccess "with M (namespace N) in testeq 2 a.N"
                    , testExpectSuccess "with M except (namespace N) in testeq 1 a.N"
                    , testExpectSuccess "testeq 4 a.D"
                    , testExpectSuccess "with M as D in testeq 3 a.D"
                    , testExpectSuccess "with M(namespace N) as D in testeq 2 a.N.D"
                    ]
              ]
        , tGroup
              "clash"
              [ testExpectReject "let namespace M of entitytype M end; i: M -> M = fn x => x in pass"
              , testExpectSuccess "let namespace M of entitytype M end in with M in let i: M -> M = fn x => x in pass"
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
              [ testExpectSuccess "let x = fn a, b => a $ b in pass"
              , testExpectSuccess "let x = ($) in pass"
              , testExpectSuccess "let x = fn a, b => a $.Function b in pass"
              , testExpectSuccess "let x = ($.Function) in pass"
              , testExpectSuccess "let x = fn a, b => a $.Function. b in pass"
              , testExpectSuccess "let x = ($.Function.) in pass"
              , testExpectSuccess "let x = fn a, b => a .. b in pass"
              , testExpectSuccess "let x = (..) in pass"
              , testExpectSuccess "let x = fn a, b => a ..Function b in pass"
              , testExpectSuccess "let x = (..Function) in pass"
              , testExpectSuccess "let x = fn a, b => a ..Function. b in pass"
              , testExpectSuccess "let x = (..Function.) in pass"
              ]
        ]
