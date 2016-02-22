import Parser
import Data.Char
import Data.List

data Reduction = Reduction { reductionTerm :: Term
                           , reductionType :: String
                           } deriving(Show, Eq)

-- main method
-- applies consecutive reductions to the given lambda term.
reduce term
    | term /= redTerm = Result (finalTerm result)
                               (1 + (reductionCount result))
                               (redTerm:(reductionTerms result))
                               (redType:(reductionTypes result))
    | otherwise = if redType == "" then Result term 0 [] []
                  else Result term 1 [term] [redType ++ " - infinite loop"]
    where reduction = visit term
          redTerm = reductionTerm reduction
          redType = reductionType reduction
          result = reduce redTerm

-- (isFreeIn vn t) returns true iff the var named vn is a free var in the term t
isFreeIn vn (Application x y) = isFreeIn vn x || isFreeIn vn y

isFreeIn vn (Var x) = x == vn

isFreeIn vn (Abstraction x y) = x /= vn && isFreeIn vn y

-- find a way to a_rename a variable by finding a var name that is not bound in the abstraction term
a_rename expr = [head ((['a'..'z'] ++ ['A'..'Z']) \\ (allFreeIn expr))]

-- find all free variables in an expression
allFreeIn (Application x y) = allFreeIn x ++ allFreeIn y

allFreeIn (Var v) = v

allFreeIn (Abstraction x y) = (allFreeIn y) \\ x

-- (replace t var rep) replaces in the term t all instances of the variable named var with the term rep
replaceVar (Application x y) var rep = Application (replaceVar x var rep) (replaceVar y var rep)

replaceVar (Var v) var rep = if v == var then rep else (Var v)

replaceVar (Abstraction x y) var rep
    | x == var = (Abstraction x y) -- the new abstraction binds the var we are trying to replace, so we stop here
    | (isFreeIn x rep) = -- in the term exists a free var with the same name which we need to a-rename first
        let newvar = a_rename y in
        (Abstraction newvar (replaceVar (replaceVar y x (Var newvar)) var rep))
    | otherwise = (Abstraction x (replaceVar y var rep)) -- replace normally

-- visit all terms depth-first until we find a function application, upon which we use the replace method
visit (Application (Abstraction x y) z) = Reduction (replaceVar y x z) "beta"

visit (Application x y)
    | xReductionTerm /= x = Reduction (Application xReductionTerm y) xReductionType
    | otherwise = Reduction (Application x yReductionTerm) yReductionType
    where xReduction = visit x
          xReductionTerm = reductionTerm xReduction
          xReductionType = reductionType xReduction
          yReduction = visit y
          yReductionTerm = reductionTerm yReduction
          yReductionType = reductionType yReduction

visit (Var v) = Reduction (Var v) ""

visit (Abstraction x (Application z (Var w)))
    | x == w && not (isFreeIn x z) = Reduction (z) "eta"
    | otherwise = Reduction (Abstraction x redTerm) redType
    where reduction = visit (Application z (Var w))
          redTerm = reductionTerm reduction
          redType = reductionType reduction

visit (Abstraction x y) = Reduction (Abstraction x yReductionTerm) yReductionType
    where yReduction = visit y
          yReductionTerm = reductionTerm yReduction
          yReductionType = reductionType yReduction
