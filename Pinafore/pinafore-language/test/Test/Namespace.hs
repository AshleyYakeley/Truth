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
              , testExpectSuccess "let {a=4} test $ a == 4"
              ]
        , tGroup
              "let"
              [ testExpectSuccess "let {f: Integer = undefined} pass"
              , testExpectSuccess "let {entitytype P; f: P = undefined} pass"
              , testExpectSuccess "let {let {entitytype P} expose P; f: P = undefined} pass"
              , testExpectSuccess "let {namespace M {entitytype P}; with M. f: P = undefined} pass"
              ]
        , tGroup
              "exprs"
              [ tDecls ["namespace M {a=4}"] $
                tGroup
                    "namespace"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "with M test $ a == 4"
                    , testExpectSuccess "test $ a.M. == 4"
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let {a = 3} test $ a.M == 4"
                    , testExpectSuccess "with M let {b = a} test $ b == 4"
                    , testExpectSuccess "let {b = a.M} test $ b == 4"
                    , testExpectSuccess "let {a = 3; b = a.M} test $ b == 4"
                    , testExpectSuccess "let {a = 3} with M let {b = a} test $ b == 4"
                    ]
              , tDecls ["namespace M {not = fn x => x}"] $
                tGroup
                    "library"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "test $ not False"
                    , testExpectSuccess "let {not = fn x => x} test $ not True"
                    , testExpectSuccess "with M test $ not True"
                    ]
              , tDecls ["let {a=4} expose a"] $
                tGroup
                    "expose"
                    [ testExpectSuccess "pass"
                    , testExpectSuccess "test $ a == 4"
                    , testExpectSuccess "let {b = a} test $ b == 4"
                    ]
              , tDecls ["let {b=4} expose a"] $ tGroup "non-expose" [testExpectReject "pass"]
              , tDecls ["namespace M {let {a=4} expose a}"] $
                tGroup
                    "namespace-expose"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "with M test $ a == 4"
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let {a = 3} test $ a.M == 4"
                    , testExpectSuccess "with M let {b = a} test $ b == 4"
                    , testExpectSuccess "let {b = a.M} test $ b == 4"
                    , testExpectSuccess "let {a = 3; b = a.M} test $ b == 4"
                    ]
              , tDecls ["let {namespace M {a=4}} expose a.M"] $
                tGroup
                    "expose-namespace"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "with M test $ a == 4"
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let {a = 3} test $ a.M == 4"
                    , testExpectSuccess "with M let {b = a} test $ b == 4"
                    , testExpectSuccess "let {b = a.M} test $ b == 4"
                    , testExpectSuccess "let {a = 3; b = a.M} test $ b == 4"
                    ]
              , tDecls ["let {namespace M {a=4}} expose namespace M"] $
                tGroup
                    "expose-whole-namespace"
                    [ testExpectSuccess "pass"
                    , testExpectReject "test $ a == 4"
                    , testExpectSuccess "with M test $ a == 4"
                    , testExpectSuccess "test $ a.M == 4"
                    , testExpectSuccess "let {a = 3} test $ a.M == 4"
                    , testExpectSuccess "with M let {b = a} test $ b == 4"
                    , testExpectSuccess "let {b = a.M} test $ b == 4"
                    , testExpectSuccess "let {a = 3; b = a.M} test $ b == 4"
                    ]
              ]
        , tDecls ["namespace M {let {datatype T {T1; T2}} expose T, namespace T}"] $
          tGroup
              "type"
              [ testExpectSuccess "pass"
              , testExpectSuccess "with M T1.T >- fn {T1.T => pass; T2.T => fail \"wrong\"}"
              , testExpectSuccess "T1.T.M >- fn {T1.T.M => pass; T2.T.M => fail \"wrong\"}"
              , testExpectSuccess "with M let {t: T = T1.T} t >- fn {T1.T => pass; T2.T => fail \"wrong\"}"
              , testExpectSuccess "with M let {t: T = T1.T} t >- fn {T1.T => pass; T2.T => fail \"wrong\"}"
              , testExpectSuccess "let {t: T.M = T1.T.M} t >- fn {T1.T.M => pass; T2.T.M => fail \"wrong\"}"
              , testExpectSuccess "with M let {f: T -> T = fn x => x} pass"
              , testExpectSuccess "let {f: T.M -> T.M = fn x => x} pass"
              ]
        , tDecls ["namespace M {let {a = b} expose a}"] $ testExpectReject "with M pass"
        , tDecls ["namespace M {let {entitytype T} expose T}"] $
          tGroup
              "entitytype"
              [ testExpectSuccess "with M let {datatype D {MkD T};} pass"
              , testExpectSuccess "let {datatype D {MkD T.M};} pass"
              ]
        , tGroup
              "subtype"
              [ tDecls ["namespace M {entitytype P; entitytype Q}", "namespace N {with M.; subtype P <: Q}"] $
                tGroup
                    "namespace"
                    [testExpectSuccess "pass", testExpectSuccess "with M let {f: P -> Q = fn x => x} pass"]
              , tDecls
                    [ "namespace M {let {entitytype P; entitytype Q} expose P, Q}"
                    , "namespace N {with M. subtype P <: Q}"
                    ] $
                tGroup "expose" [testExpectSuccess "pass", testExpectSuccess "with M let {f: P -> Q = fn x => x} pass"]
              ]
        , tGroup
              "names"
              [ testExpectSuccess "let {namespace M {a = 6; b = a};} test $ b.M == 6"
              , testExpectSuccess "let {namespace M {a = 6; b = a}} with M test $ b == 6"
              , testExpectSuccess "let {namespace P {a = 6; namespace Q {b = a}};} test $ b.Q.P == 6"
              , testExpectSuccess "let {namespace P {a = 6}; namespace P {namespace Q {b = a}}} test $ b.Q.P == 6"
              ]
        , tGroup
              "decl"
              [ testExpectSuccess "let {nna=1; namespace M {nna=2}} test $ nna.M == 2"
              , testExpectSuccess "let {nna=1; namespace M {nnb=2}} test $ nna == 1"
              , testExpectSuccess "let {nna=1; namespace M {nna=2}} test $ nna == 1"
              , testExpectSuccess "let {nna=1; namespace M {nna=2}} test $ nna. == 1"
              ]
        , tDecls ["testeq = fn e, f => if e == f then pass else fail $ \"found: \" <>.Text show f"] $
          tGroup
              "with"
              [ tDecls
                    [ "a = 1"
                    , "a0 = a"
                    , "namespace M {a=2}"
                    , "a1 = a"
                    , "with M"
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
              , tDecls ["a = 1", "namespace M {a=2; b=3}"] $
                tGroup
                    "scope"
                    [ testExpectSuccess "with M testeq 2 a"
                    , testExpectSuccess "with M() testeq 1 a"
                    , testExpectSuccess "with M(a) testeq 2 a"
                    , testExpectSuccess "with M(b) testeq 1 a"
                    , testExpectSuccess "with M(a,b) testeq 2 a"
                    , testExpectSuccess "with M except () testeq 2 a"
                    , testExpectSuccess "with M except (a) testeq 1 a"
                    , testExpectSuccess "with M except (b) testeq 2 a"
                    , testExpectSuccess "with M except (a,b) testeq 1 a"
                    ]
              , tDecls ["namespace D {a=1}", "namespace M {a=2; b=3}"] $
                tGroup
                    "as"
                    [ testExpectSuccess "with M as D testeq 2 a.D"
                    , testExpectSuccess "with M() as D testeq 1 a.D"
                    , testExpectSuccess "with M(a) as D testeq 2 a.D"
                    , testExpectSuccess "with M(b) as D testeq 1 a.D"
                    , testExpectSuccess "with M(a,b) as D testeq 2 a.D"
                    , testExpectSuccess "with M except () as D testeq 2 a.D"
                    , testExpectSuccess "with M except (a) as D testeq 1 a.D"
                    , testExpectSuccess "with M except (b) as D testeq 2 a.D"
                    , testExpectSuccess "with M except (a,b) as D testeq 1 a.D"
                    ]
              , tDecls ["namespace D {a=4}", "namespace N {a=1}", "namespace M {a=3; namespace N {a=2}}"] $
                tGroup
                    "namespace"
                    [ testExpectSuccess "testeq 1 a.N"
                    , testExpectSuccess "with M() testeq 1 a.N"
                    , testExpectSuccess "with M(a.N) testeq 2 a.N"
                    , testExpectSuccess "with M testeq 2 a.N"
                    , testExpectSuccess "with M (namespace N) testeq 2 a.N"
                    , testExpectSuccess "with M except (namespace N) testeq 1 a.N"
                    , testExpectSuccess "testeq 4 a.D"
                    , testExpectSuccess "with M as D testeq 3 a.D"
                    , testExpectSuccess "with M(namespace N) as D testeq 2 a.N.D"
                    ]
              ]
        , tGroup
              "clash"
              [ testExpectReject "let {namespace M {entitytype M}; i: M -> M = fn x => x} pass"
              , testExpectSuccess "let {namespace M {entitytype M}} with M let {i: M -> M = fn x => x} pass"
              ]
        , tGroup
              "pattern"
              [ testExpectSuccess "test $ 3 >- fn x => x == 3"
              , testExpectSuccess "test $ 3 >- fn x as M => x.M == 3"
              , testExpectSuccess "test $ 3 >- fn x as M as N => x.M.N == 3"
              , testExpectReject "test $ 3 >- fn x as M => x == 3"
              , testExpectSuccess "test $ Just 3 >- fn {(Just x) as M => x.M == 3; Nothing => False;}"
              , testExpectSuccess "test $ Just 3 >- fn {Just (x as M) => x.M == 3; Nothing => False;}"
              ]
        , tGroup
              "qualified"
              [ testExpectSuccess "let {x = fn a, b => a $ b} pass"
              , testExpectSuccess "let {x = ($)} pass"
              , testExpectSuccess "let {x = fn a, b => a $.Function b} pass"
              , testExpectSuccess "let {x = ($.Function)} pass"
              , testExpectSuccess "let {x = fn a, b => a $.Function. b} pass"
              , testExpectSuccess "let {x = ($.Function.)} pass"
              , testExpectSuccess "let {x = fn a, b => a .. b} pass"
              , testExpectSuccess "let {x = (..)} pass"
              , testExpectSuccess "let {x = fn a, b => a ..Function b} pass"
              , testExpectSuccess "let {x = (..Function)} pass"
              , testExpectSuccess "let {x = fn a, b => a ..Function. b} pass"
              , testExpectSuccess "let {x = (..Function.)} pass"
              ]
        ]
