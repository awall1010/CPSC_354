-- Haskell data types for the abstract syntax.
-- Generated by the BNF converter.

-- | The abstract syntax of language numbers.

module AbsNumbers where

import Prelude (Integer)
import qualified Prelude as C (Eq, Ord, Show, Read)

data Exp
    = Plus Exp Exp
    | Times Exp Exp
    | Minus Exp Exp
    | Num Integer
    | Power Exp Exp
    | Divide Exp Exp
    | Absolute Exp
    | Modulo Exp Exp
    | Negate Exp
    | Square Exp
  deriving (C.Eq, C.Ord, C.Show, C.Read)

