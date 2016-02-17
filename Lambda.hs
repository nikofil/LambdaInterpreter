import Parser
import Data.Char
import Data.List

-- main method - parse string, apply one b-reduction and pretty-print it again
pp s = (prettyprint.visit.myparse) s

-- (freein str t) returns true iff the var named str is a free var in the term t
freein str (Application x y) = freein str x || freein str y

freein str (Var x) = x == str

freein str (Abstraction x y) = x /= str && freein str y

-- find a way to a-rename a variable by finding a var name that is not bound in the abstraction term
a_rename expr = [head ((['a'..'z'] ++ ['A'..'Z']) \\ (allfree expr))]

-- find all free variables in an expression
allfree (Application x y) = allfree x ++ allfree y

allfree (Var v) = v

-- (replace x y z) replaces in the term x all instances of the variable named y with the term z
replace (Application x y) rep with = Application (replace x rep with) (replace y rep with)

replace (Var v) rep with = if v == rep then with else (Var v)

replace (Abstraction x y) rep with =
    if x == rep then (Abstraction x y) -- the new abstraction binds the var we are trying to replace, so we stop here
    else if (freein x with) then -- in the term exists a free var with the same name which we need to a-rename first
        let newvar = a_rename y in
        (Abstraction newvar (replace (replace y x (Var newvar)) rep with))
    else (Abstraction x (replace y rep with)) -- replace normally

-- visit all terms depth-first until we find a function application, upon which we use the replace method
visit (Application (Abstraction x y) z) = replace y x z

visit (Application x y) = let res = visit x in
    if res /= x then (Application res y)
                else (Application x (visit y))

visit (Var v) = Var v

visit (Abstraction x y) = Abstraction x (visit y)