module Pinafore.Language.Var where

import Import

newtype Var (name :: Symbol) = MkVar
    { unVar :: UVarT name
    }

type A = Var "a"

type B = Var "b"

type C = Var "c"

type X = Var "x"

type Y = Var "y"

type P = Var "p"

type Q = Var "q"

type AP = Var "ap"

type BP = Var "bp"

type CP = Var "cp"

type AQ = Var "aq"

type BQ = Var "bq"

type CQ = Var "cq"

type AX = Var "ax"

type BX = Var "bx"

type CX = Var "cx"

type AY = Var "ay"

type BY = Var "by"

type CY = Var "cy"
