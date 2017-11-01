{- Ciarán Ingle -}
{-   15319981   -}
module Ex01 where
import Data.List ((\\))

-- Datatypes -------------------------------------------------------------------

-- Do not change anything in this section !

type Id = String -- New type Id is considered the same as String

data Expr -- Algebraic data type Expr. Expr can hold a value, add, mul, sub, etc.
    = Val Double
    | Add Expr Expr
    | Mul Expr Expr
    | Sub Expr Expr
    | Dvd Expr Expr
    | Var Id
    | Def Id Expr Expr  -- Def x e1 e2 - x is in scope in e2, but not in e1. Compute value of e1, and assign it to x, then evaluate e2 as overall result.
    deriving (Eq, Show) -- Makes it possible to see values

type Dict k d = [(k, d)] -- Dict k d is a list of (key, datum value) pairs

define :: Dict k d -> k -> d -> Dict k d
define d s v = (s, v):d  -- Cons a new key, value pair to the head of the list.

find :: Eq k => Dict k d -> k -> Maybe d
find []          _    = Nothing                  -- Key is not in dictionary.
find ((s, v):ds) name | name == s = Just v       -- Key is found.
                      | otherwise = find ds name -- Search tail of list.

type EDict = Dict String Double

-- Part 1 : Evaluating Expressions -- (63 marks) -------------------------------

-- Implement the following function so all 'eval' tests pass:
    -- eval should return Nothing if:
    -- (1) A divide by zero operation was going to be performed;
    -- (2) The expression contains a variable not in the dictionary.

eval :: EDict -> Expr -> Maybe Double
eval d e = case simp d e of
                Val a     -> Just a
                otherwise -> Nothing

-- Part 2 : Simplifying Expressions -- (57 marks) ------------------------------

-- Given the following code :

simp :: EDict -> Expr -> Expr
simp d (Var v)       = simpVar d v
simp d (Add e1 e2)   = simpAdd (simp d e1) (simp d e2)
simp d (Sub e1 e2)   = simpSub (simp d e1) (simp d e2)
simp d (Mul e1 e2)   = simpMul (simp d e1) (simp d e2)
simp d (Dvd e1 e2)   = simpDvd (simp d e1) (simp d e2)
simp d (Def v e1 e2) = simpDef d v (simp d e1) (simp d e2)
simp _ e = e -- simplest case, Val, needs no special treatment.

-- Implement the following functions so all 'simp' tests pass:
    -- (1) See test scripts for most required properties
    -- (2) (Def v e1 e2) should simplify to e2,...
        -- (2a) .... if there is no mention of v in e2
        -- (2b) .... or any mention of v in e2 is inside another (Def v .. ..)

simpVar :: EDict -> Id -> Expr
simpVar d v = case find d v of
                  Nothing -> Var v -- Value not found - return variable name.
                  Just a  -> Val a -- Value found - return value.

simpAdd :: Expr -> Expr -> Expr
simpAdd (Val x) (Val y) = Val (x+y)
simpAdd (Val 0)  e2     = e2
simpAdd  e1     (Val 0) = e1
simpAdd  e1      e2     = Add e1 e2

simpSub :: Expr -> Expr -> Expr
simpSub (Val x) (Val y) = Val (x-y)
simpSub  e1     (Val 0) = e1
simpSub  e1      e2     = Sub e1 e2

simpMul :: Expr -> Expr -> Expr
simpMul (Val 0)  _      = Val 0
simpMul  _      (Val 0) = Val 0
simpMul (Val x) (Val y) = Val (x*y)
simpMul (Val 1)  e2     = e2
simpMul  e1     (Val 1) = e1
simpMul  e1      e2     = Mul e1 e2

simpDvd :: Expr -> Expr -> Expr
simpDvd  e1     (Val 0) = Dvd e1 (Val 0) -- Return the expression.
simpDvd (Val 0)  _      = Val 0
simpDvd (Val x) (Val y) = Val (x/y)
simpDvd  e1     (Val 1) = e1
simpDvd  e1      e2     = Dvd e1 e2

simpDef :: EDict -> Id -> Expr -> Expr -> Expr
simpDef d v (Val x) e2 = simp (define d v x) e2 -- e1 can be simplified. :D
simpDef d v  e1     e2 = Def v e1 e2            -- e1 cannot be simplified. D:
