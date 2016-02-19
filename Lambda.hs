import Parser
import Data.Char
import Data.List

-- main method - parse string, apply one b-reduction and pretty-print it again
pp s = (prettyprint.visit.myparse) s

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
visit (Application (Abstraction x y) z) = replaceVar y x z

visit (Application x y)
    | res /= x = (Application res y)
    | otherwise = (Application x (visit y))
    where res = visit x

visit (Var v) = Var v

visit (Abstraction x y) = Abstraction x (visit y)