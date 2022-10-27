module Test.Module
    ( testModule
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
        , tModule "M" "expose a of a=4 end" $
          tGroup
              "exprs"
              [ testExpectSuccess "let import M in if a == 4 then return () else fail \"wrong\""
              , testExpectSuccess "if M.a == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let a = 3 in if M.a == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let import M; b = a in if b == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let b = M.a in if b == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let a = 3; b = M.a in if b == 4 then return () else fail \"wrong\""
              ]
        , tModule "M" "expose T, T1, T2 of datatype T of T1; T2 end end" $
          tGroup
              "type"
              [ testExpectSuccess "let import M in T1 >- match T1 => return (); T2 => fail \"wrong\" end"
              , testExpectSuccess "M.T1 >- match M.T1 => return (); M.T2 => fail \"wrong\" end"
              , testExpectSuccess
                    "let import M in let t: T = T1 in t >- match T1 => return (); T2 => fail \"wrong\" end"
              , testExpectSuccess "let import M; t: T = T1 in t >- match T1 => return (); T2 => fail \"wrong\" end"
              , testExpectSuccess "let t: M.T = M.T1 in t >- match M.T1 => return (); M.T2 => fail \"wrong\" end"
              , testExpectSuccess "let import M; f: T -> T = fn x => x in return ()"
              , testExpectSuccess "let import M in let f: T -> T = fn x => x in return ()"
              , testExpectSuccess "let f: M.T -> M.T = fn x => x in return ()"
              , testExpectSuccess "let expose T of import M end; f: T -> T = fn x => x in return ()"
              , testExpectReject "let expose of import M end; f: T -> T = fn x => x in return ()"
              , testExpectReject "let expose T1, T2 of import M end; f: T -> T = fn x => x in return ()"
              , testExpectSuccess "let expose T1 of import M end; f = T1 in return ()"
              , testExpectReject "let expose T2 of import M end; f = T1 in return ()"
              ]
        , tModule "M" "expose a of a = b end" $ testExpectReject "let import M in return ()"
        , tModule "M" "expose T of opentype T end" $
          tGroup
              "opentype"
              [ testExpectSuccess "let import M; datatype D of MkD T end; in return ()"
              , testExpectSuccess "let datatype D of MkD M.T end; in return ()"
              ]
        , tModule "M" "expose P, Q of opentype P; opentype Q end" $
          tModule "N" "expose of import M; subtype P <: Q end" $
          tGroup
              "subtype"
              [ testExpectReject "let import M; f: P -> Q = fn x => x in return ()"
              , testExpectSuccess "let import M; import N; f: P -> Q = fn x => x in return ()"
              , testExpectSuccess "let import N; import M; f: P -> Q = fn x => x in return ()"
              , testExpectSuccess "let import M; expose of import N end; f: P -> Q = fn x => x in return ()"
              ]
        ]
