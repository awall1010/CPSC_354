-- A Virtual Machine (VM) for Arithmetic (template)

-----------------------
-- Data types of the VM
-----------------------

--nn_int :: Integer -> NN
--int_nn :: NN->Integer
--ii_int :: Integer -> II
--int_ii :: II -> Integer
--pp_int :: Integer -> PP
--int_pp :: PP->Integer





-- Natural numbers
data NN = O | S NN
  deriving (Eq,Show) -- for equality and printing

-- Integers
data II = II NN NN
  deriving (Eq,Show) -- for equality and printing

-- Positive integers (to avoid dividing by 0)
data PP = I | T PP
  deriving(Eq, Show)

-- Rational numbers
data QQ =  QQ II PP --type QQ requires II numerator and PP denominator

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


-- multiply natural numbers
multN :: NN -> NN -> NN
multN O m = O
multN (S n) m = addN (multN n m) m


-- subtract natural numbers
subN :: NN -> NN -> NN
subN O m = O
subN m O = m
subN (S n) (S m) = subN n m


----------------
-- II Arithmetic (II is natural number natural number) with declarer
-- -- Addition: (ac)-(bd)=
----------------
addI :: II -> II -> II
addI (II a b) (II c d) = II (addN a c) (addN b d)

multI :: II -> II -> II
multI (II a b) (II c d) = II (addN (multN a c) (multN b d)) (addN (multN a d)(multN b c))

negI :: II -> II
negI (II a b)  = II b a


-- II Subtraction (a-b)-(c-d)=(a+d)-(b+c)
subtrI :: II -> II -> II
subtrI (II a b) (II c d) =   II (addN a d) (addN b c)

----------------
-- QQ Arithmetic
----------------

-- add positive numbers
addP :: PP -> PP -> PP
addP I m = (T m)
addP (T n) m = T (addP n m)

-- multiply positive numbers
multP :: PP -> PP -> PP
multP I m = m
multP (T n)m = addP(multP n m )m



-- convert numbers of type PP to numbers of type II
ii_pp :: PP -> II
ii_pp I = II(S O) O

nn_int :: Integer -> NN
nn_int 0 = O
nn_int n = addN (S(O)) (nn_int(n-1))


int_nn :: NN->Integer
int_nn O = 0
int_nn (S n) = 1+ int_nn n

ii_int:: Integer->II
ii_int n = II(nn_int n ) O

pp_int :: Integer -> PP
pp_int 1 = I
pp_int n = (T(pp_int(n-1)))


int_pp ::PP->Integer
int_pp I =1
int_pp(T n) = 1+ int_pp n





int_ii ::  II -> Integer
int_ii (II (O) (O)) = 0
int_ii (II (n) (m)) = int_nn(n) - int_nn(m)


float_qq :: QQ -> Float --truncating float to int data QQ =  QQ II PP
float_qq (QQ n m) = fromInteger(int_ii(n))/fromInteger(int_pp(m))

-- Addition: (a/b)+(c/d)=(ad+bc)/(bd)
addQ :: QQ -> QQ -> QQ
addQ (QQ a b) (QQ c d) = QQ (addI (multI a (ii_pp d)) (multI (ii_pp b) c)) (multP b d)

-- Multiplication: (a/b)*(c/d)=(ac)/(bd)
multQ :: QQ -> QQ -> QQ
multQ (QQ a b) (QQ c d) = QQ (multI a c) (multP b d)





--addition (a/b)+(c/d)=(ad+bc)/(bd)




----------------
-- Normalization
----------------
--normalizeI :: II -> II


----------------------------------------------------
-- Converting between VM-numbers and Haskell-numbers
----------------------------------------------------

--convert number of type PP to type II (PP is a recursive data type)


--nbv :: II -> II
--nbv n =
----------
-- Testing
----------
main = do
    print $ addN (S (S O)) (S O)
    --print $
    let i = 4
    let j = 2
    let k = 1
    let l = 3

    print $ subN (S(S (S O))) (S O)
    print $ multN (S (S O)) (S (S (S O)))
    print $ addP (T I) (T I)
    print $ addP (T (T I)) (T I)
    print $ addP (T (T I)) (T (T (T I)))

    print $ multP (T (T I)) (T I)
    print $ addI (II (S (S O)) (S O))  (II (S (S (S O))) (S (S O)))

    print $ subtrI (II (S (S O)) (S O))  (II (S (S (S O))) (S (S O)))

    --print $ float_qq (addQ (QQ (ii_int i) (pp_int j)) (QQ (ii_int k) (pp_int l)))
    --print $ float_qq (multQ (QQ (ii_int i) (pp_int j)) (QQ (ii_int k) (pp_int l)))




    --multP O m = O
    --multP (S n) m = addN (multN n m) m
