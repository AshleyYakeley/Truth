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
        , tModule "M" "let a=4 in export a" $
          tGroup
              "exprs"
              [ testExpectSuccess "let import M in if a == 4 then return () else fail \"wrong\""
              , testExpectSuccess "if M.a == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let a = 3 in if M.a == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let import M; b = a in if b == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let b = M.a in if b == 4 then return () else fail \"wrong\""
              , testExpectSuccess "let a = 3; b = M.a in if b == 4 then return () else fail \"wrong\""
              ]
        , tModule "M" "let datatype T = T1 | T2 in export T T1 T2" $
          tGroup
              "type"
              [ testExpectSuccess "let import M in case T1 of T1 -> return (); T2 -> fail \"wrong\" end"
              , testExpectSuccess "case M.T1 of M.T1 -> return (); M.T2 -> fail \"wrong\" end"
              , testExpectSuccess
                    "let import M in let t: T; t = T1 in case t of T1 -> return (); T2 -> fail \"wrong\" end"
              , testExpectSuccess "let import M; t: T; t = T1 in case t of T1 -> return (); T2 -> fail \"wrong\" end"
              , testExpectSuccess "let t: M.T; t = M.T1 in case t of M.T1 -> return (); M.T2 -> fail \"wrong\" end"
              , testExpectSuccess "let import M; f: T -> T; f x = x in return ()"
              , testExpectSuccess "let import M in let f: T -> T; f x = x in return ()"
              , testExpectSuccess "let f: M.T -> M.T; f x = x in return ()"
              ]
        , tModule "M" "let a = b in export a" $ testExpectReject "let import M in return ()"
        , tModule "M" "let opentype T in export T" $
          tGroup
              "opentype"
              [ testExpectSuccess "let import M; datatype D = MkD T; in return ()"
              , testExpectSuccess "let datatype D = MkD M.T; in return ()"
              ]
        ]
