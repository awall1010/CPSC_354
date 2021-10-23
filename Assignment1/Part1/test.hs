-- A Virtual Machine (VM) for Arithmetic (template)

-----------------------
-- Data types of the VM
-----------------------

-- Natural numbers
data NN = O | S NN
  deriving (Eq,Show) -- for equality and printing

-- Integers
data II = II NN NN
  deriving (Eq,Show) -- for equality and printing

-- Positive integers (to avoid dividing by 0)
data PP = I | T PP
  deriving (Eq,Show)

-- Rational numbers
data QQ =  QQ II PP

------------------------
-- Arithmetic on the  VM
------------------------

----------------
-- NN Arithmetic
----------------

-- add natural numbers
addN :: NN -> NN -> NN
addN O m = m
addN (S n) m = S (addN n m)

-- subtract natural numbers
subN :: NN -> NN -> NN
subN O m = O
subN m O = m
subN (S n) (S m) = subN n m

-- multiply natural numbers
multN :: NN -> NN -> NN
multN O m = O
multN (S n) m = addN (multN n m) m

----------------
-- II Arithmetic
----------------
-- Addition: (a-b)+(c-d)=(a+c)-(b+d)
addI :: II -> II -> II
addI (a b) (c d) = (addN a c) (addN b d)

-- Multiplication: (a-b)*(c-d)=(ac+bd)-(ad+bc)
multI :: II -> II -> II
multI (a b) (c d) = (addN (multN a c) (multN b d)) (addN (multN a d) (b c))

-- Subtraction: (a-b)-(c-d)=(a+d)-(b+c)
subtrI :: II -> II -> II
subtrI (a b) (c d) = (subN (addN(a d)) addN(b c))

-- Negation: -(a-b)=(b-a)
negI :: II -> II


----------------
-- QQ Arithmetic
----------------
addP :: PP -> PP -> PP
-- Adding 1 plus integer equals the successor of integer:


-- add positive numbers
addP :: PP -> PP -> PP
addP I m = (T m)
addP (T n) m = T(addP n m)

-- multiply positive numbers
multP :: PP -> PP -> PP
multP I n  = n
multP (T n) m = addP m (multP n m)

-- convert numbers of type PP to numbers of type II
ii_pp :: PP -> II
ii_pp I = II (S O) O


-- Addition: (a/b)+(c/d)=(ad+bc)/(bd)
addQ :: QQ -> QQ -> QQ


-- Multiplication: (a/b)*(c/d)=(ac)/(bd)
multQ :: QQ -> QQ -> QQ
-- multQ (a b)

----------------
-- Normalisation
----------------
normalizeI :: II -> II
-- Base case:
normalizeI m O = (m O)
normalizeI (S m) (S n) = (m n)

----------------------------------------------------
-- Converting between VM-numbers and Haskell-numbers
----------------------------------------------------
-- Precondition: Inputs are non-negative
nn_int :: Integer -> NN
nn_int 0 = O
nn_int n = add (S(O)) (nn_int(n-1))

int_nn :: NN -> Integer
int_nn O = 0
int_nn (S n) = 1 + int_nn n

ii_int :: Integer -> II
ii_int n = II (nn_int) O

int_ii :: II -> Integer
int_ii a b = int_nn (subN a b)

-- Precondition: Inputs are positive
pp_int :: Integer -> PP
pp_int 1 = I
pp_int n = (T(pp_int (n-1)))

int_pp :: PP -> Integer
int_pp I = 1
int_pp (T n) = 1 + int_pp n

float_qq :: QQ -> Float

------------------------------
-- Normalisation by Evaluation
------------------------------

nbv :: II -> II

----------
-- Testing
----------
main = do
    print $ addN (S (S O)) (S O)
    print $ multN (S (S O)) (S (S (S O)))
  -- Test cases for addP:
    print $ addP (I) (T I)
    print $ addP (T (T I)) (T I)
  -- Test cases for Normalisation:
    print $ normalizeI (S m) (S n) = (m n)
