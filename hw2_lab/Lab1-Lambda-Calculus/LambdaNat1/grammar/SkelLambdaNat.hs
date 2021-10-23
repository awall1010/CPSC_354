-- Haskell module generated by the BNF converter

{-# OPTIONS_GHC -fno-warn-unused-matches #-}

module SkelLambdaNat where

import Prelude (($), Either(..), String, (++), Show, show)
import qualified AbsLambdaNat

type Err = Either String
type Result = Err String

failure :: Show a => a -> Result
failure x = Left $ "Undefined case: " ++ show x

transId :: AbsLambdaNat.Id -> Result
transId x = case x of
  AbsLambdaNat.Id string -> failure x

transProgram :: AbsLambdaNat.Program -> Result
transProgram x = case x of
  AbsLambdaNat.Prog exp -> failure x

transExp :: AbsLambdaNat.Exp -> Result
transExp x = case x of
  AbsLambdaNat.EAbs id exp -> failure x
  AbsLambdaNat.EApp exp1 exp2 -> failure x
  AbsLambdaNat.ENat0 -> failure x
  AbsLambdaNat.ENatS exp -> failure x
  AbsLambdaNat.EVar id -> failure x
