-- Vanda Azevedo 2019

import Prelude hiding (lookup, map) 
import Data.Map

type Var = String
type State = Map Var Int -- cada estado pode ter várias variáveis com valores atribuídos

data AExp = Add AExp AExp
    | Sub AExp AExp
    | Mul AExp AExp
    | Variable Var
    | Num Int
    deriving (Show)

data BExp = Eq AExp AExp
    | LEq AExp AExp
    | GEq AExp AExp
    | Less AExp AExp
    | Great AExp AExp
    | Dif AExp AExp
    | And BExp BExp
    | LOr BExp BExp
    | Not BExp
    | Truth Bool 
    deriving (Show)

data Command = Atrib Var AExp
    | Seq Command Command 
    | If BExp Command Command 
    | While BExp Command
    | Or Command Command
    | Skip 
    deriving (Show)

aexp :: AExp -> State -> Int
aexp (Add a1 a2) s = (aexp a1 s) + (aexp a2 s)
aexp (Sub a1 a2) s = (aexp a1 s) - (aexp a2 s)
aexp (Mul a1 a2) s = (aexp a1 s) * (aexp a2 s)
aexp (Variable v) s = case lookup v s of
    Just n -> n -- devolve o numero associado à variavél 
    Nothing -> error ("Variable " ++ v ++ " not defined")
aexp (Num n) _ = n 

bexp :: BExp -> State -> Bool
bexp (Eq e1 e2) s = (aexp e1 s) == (aexp e2 s)
bexp (Dif e1 e2) s = (aexp e1 s) /= (aexp e2 s)
bexp (LEq e1 e2) s = (aexp e1 s) <= (aexp e2 s)
bexp (GEq e1 e2) s = (aexp e1 s) >= (aexp e2 s)
bexp (Less e1 e2) s = (aexp e1 s) < (aexp e2 s)
bexp (Great e1 e2) s = (aexp e1 s) > (aexp e2 s)
bexp (And e1 e2) s = (bexp e1 s) && (bexp e2 s)
bexp (LOr e1 e2) s = (bexp e1 s) || (bexp e2 s)
bexp (Not e1) s = not (bexp e1 s)
bexp (Truth b) s = b

cmd :: Command -> State -> [State]
cmd (Atrib var a) s = [insert var (aexp a s) s]
-- aplicar c2 a todos os estados devolvidos por (cmd c1 s)
cmd (Seq c1 c2) s = mapCmd c2 (cmd c1 s)           
    where
        mapCmd c' [] = []
        mapCmd c' (x:xs) = (cmd c' x) ++ mapCmd c' xs 
cmd (If b c1 c2) s = if bexp b s then cmd c1 s else cmd c2 s        
cmd (While b c) s =  if bexp b s then cmd (Seq c (While b c)) s else [s]
cmd (Or c1 c2) s = (cmd c1 s) ++ (cmd c2 s)
cmd Skip s = [s]

-- Examples
x3 = Atrib "x" (Num 3)
y3 = Atrib "y" (Num 3)
y4 = Atrib "y" (Num 4)
xy = Atrib "z" (Add (Variable "x") (Variable "y"))
xz = Atrib "k" (Add (Num 3) (Num 4))

add1 = cmd (Seq x3 (Seq y3 xy)) empty
add2 = cmd (Seq x3 (Or y3 y4)) empty
add3 = cmd (Seq (Seq x3 (Or y3 y4)) xy) empty

-- Ciclo ou x=4
b = Truth True
x2 = Atrib "x" (Num 2)
xx2 = Atrib "x" (Add (Variable "x") (Num 2))
res = cmd (Or (Seq x2 xx2) (While b Skip)) empty

-- x=1 ou x=4
x1 = Atrib "x" (Num 1)
res2 = cmd (Or (Seq x2 xx2) x1) empty