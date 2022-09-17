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
        , tModule "M" "let a=4 in expose a" $
          tGroup
              "exprs"
              [ testExpectSuccess "let import M in if a == 4 then return () else fail \"wrong\""
              , testExpectSuccess "if M.a == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let a = 3 in if M.a == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let import M; b = a in if b == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let b = M.a in if b == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let a = 3; b = M.a in if b == 4 then return () else fail \"wrong\""
              ]
        , tModule "M" "let datatype T of T1; T2 end in expose T, T1, T2" $
          tGroup
              "type"
              [ testExpectSuccess "let import M in case T1 of T1 => return (); T2 => fail \"wrong\" end"
              , testExpectSuccess "case M.T1 of M.T1 => return (); M.T2 => fail \"wrong\" end"
              , testExpectSuccess "let import M in let t: T = T1 in case t of T1 => return (); T2 => fail \"wrong\" end"
              , testExpectSuccess "let import M; t: T = T1 in case t of T1 => return (); T2 => fail \"wrong\" end"
              , testExpectSuccess "let t: M.T = M.T1 in case t of M.T1 => return (); M.T2 => fail \"wrong\" end"
              , testExpectSuccess "let import M; f: T -> T = fn x => x in return ()"
              , testExpectSuccess "let import M in let f: T -> T = fn x => x in return ()"
              , testExpectSuccess "let f: M.T -> M.T = fn x => x in return ()"
              , testExpectSuccess "let import M (T); f: T -> T = fn x => x in return ()"
              , testExpectReject "let import M (); f: T -> T = fn x => x in return ()"
              , testExpectReject "let import M (T1, T2); f: T -> T = fn x => x in return ()"
              , testExpectSuccess "let import M (T1); f = T1 in return ()"
              , testExpectReject "let import M (T2); f = T1 in return ()"
              ]
        , tModule "M" "let a = b in expose a" $ testExpectReject "let import M in return ()"
        , tModule "M" "let opentype T in expose T" $
          tGroup
              "opentype"
              [ testExpectSuccess "let import M; datatype D of MkD T end; in return ()"
              , testExpectSuccess "let datatype D of MkD M.T end; in return ()"
              ]
        , tModule "M" "let opentype P; opentype Q in expose P, Q" $
          tModule "N" "let import M; subtype P <: Q in expose" $
          tGroup
              "subtype"
              [ testExpectReject "let import M; f: P -> Q = fn x => x in return ()"
              , testExpectSuccess "let import M; import N; f: P -> Q = fn x => x in return ()"
              , testExpectSuccess "let import N; import M; f: P -> Q = fn x => x in return ()"
              , testExpectSuccess "let import M; import N(); f: P -> Q = fn x => x in return ()"
              ]
        ]
