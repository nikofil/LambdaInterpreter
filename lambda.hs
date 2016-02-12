import Parser
import Data.Char

pp s = (prettyprint.visit.myparse) s

freein str (Application x y) = freein str x || freein str y

freein str (Var x) = x == str

freein str (Abstraction x y) = x /= str && freein str y

a_redux str = map toUpper str

replace (Application x y) rep with = Application (replace x rep with) (replace y rep with)

replace (Var v) rep with = if v == rep then with else (Var v)

replace (Abstraction x y) rep with = Abstraction (if (freein x with) then (a_redux x) else x) (
    if x == rep then y
    else if (freein x with) then (replace (replace y x (Var (a_redux x))) rep with)
    else (replace y rep with))

visit (Application (Abstraction x y) z) = replace y x z

visit (Application x y) = let res = visit x in
    if res /= x then (Application res y)
                else (Application x (visit y))

visit (Var v) = Var v

visit (Abstraction x y) = Abstraction x (visit y)