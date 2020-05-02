module TypeCheck where
import Grammar

-- data Exp = Plus Exp Exp
--          | Assignment String Exp
--          | TypeAssignment String Type
--          | StmtList Exp Exp
--          | Minus Exp Exp
--          | Mult Exp Exp
--          | Div Exp Exp
--          | Mod Exp Exp
--          | LessThan Exp Exp
--          | LessOrEqThan Exp Exp
--          | BiggerThan Exp Exp
--          | BiggerOrEqThan Exp Exp
--          | IsEq Exp Exp
--          | And Exp Exp
--          | Or Exp Exp
--          | LanTrue
--          | LanFalse
--          | LanInt Int
--          | LanVar String
--          | IfStmt Exp StmtList
--          | IfElseStmt Exp StmtList StmtList
--          | WhileExp Exp StmtList
--     deriving (Show, Eq)
-- data Type = TypeInt | TypeBool 
--       deriving (Show, Eq)

type TypeEnvironment = [(String, Type)]

getBinding :: String -> TypeEnvironment -> Type
getBinding x [] = error "Varible binding not found"
getBinding x ((s, t):env) | x == s = t 
                          | otherwise = getBinding x env

isBinded :: String -> TypeEnvironment -> Bool
isBinded x [] = False
isBinded x ((s,t):env) | x == s = True 
                       | otherwise = isBinded x env

addBinding :: String -> Type -> TypeEnvironment -> TypeEnvironment
addBinding s t env = (s, t):env

updateBinding :: String -> Type -> TypeEnvironment -> TypeEnvironment -> TypeEnvironment
updateBinding s t [] newEnv = newEnv
updateBinding s t ((x,y): oldEnv) newEnv | s == x = (s,t) : newEnv ++ oldEnv
                                         | otherwise = updateBinding s t oldEnv ((x,y):newEnv)

updateEnv :: TypeEnvironment -> Exp -> TypeEnvironment 
-- Base cases
updateEnv e (LanInt x) = e
updateEnv e (LanVar x) = e
updateEnv e (LanTrue) = e
updateEnv e (LanFalse) = e
updateEnv e (EmptyList) = e

-- Multiple Expressions
updateEnv e (App e1 e2) = updateEnv (updateEnv e e1) e2
-- Type Assignment
updateEnv e (TypeAssignment str t) | isBinded str e == False = updateEnv (addBinding str t e) (TypeAssignment str t)
                                   | getBinding str e == t = e
                                   | otherwise = updateEnv (updateBinding str t e []) (TypeAssignment str t)
-- Assignment
updateEnv e (Assignment e1 e2) | (checker e e2) == (getBinding e1 e) = updateEnv e e2
updateEnv e _ = e

checker :: TypeEnvironment -> Exp -> Type
-- base cases
checker e (LanInt x) = TypeInt
checker e (LanVar x) = getBinding x e
checker e (LanTrue) = TypeBool
checker e (LanFalse) = TypeBool

-- emptyList
checker e (EmptyList) = TypeList
-- singleList
checker e (SingleList e1) | checker e e1 == TypeInt = TypeList
-- multilist
checker e (MultipleList e1 e2) | checker e e1 == TypeInt = checker e e2
-- checker StreamRead
checker e (StreamRead str) | isBinded str e == False = error "List has not been declared"
                           | getBinding str e == TypeList = TypeList
-- checker IndexOf
checker e (IndexOf str e1) | isBinded str e == False = error "List has not been declared"
                           | (checker e e1) == TypeInt = checker e e1
-- checker AddOneIndexOf
checker e (AddOneIndexOf str e1) | isBinded str e == False = error "List has not been declared"
                                 | getBinding str e == TypeList && (checker e e1) == TypeInt = checker e e1
-- checker AddOneIndexOf
checker e (AddManyIndexOf str e1 e2) | isBinded str e == False = error "List has not been declared"
                                     | getBinding str e == TypeList && (checker e e1) == TypeInt && (checker e e2) == TypeInt = checker e e1
-- streamAssignment 
checker e (IndexAssignment str index e1) | isBinded str e == False = error "List has not been declared"
                                         | getBinding str e == TypeList && (checker e e1) == (checker e index) = checker e e1

-- plus
checker e (Plus e1 e2) | checker e e1 == TypeInt && checker e e2 == TypeInt = TypeInt
-- minus
checker e (Minus e1 e2) | checker e e1 == TypeInt && checker e e2 == TypeInt = TypeInt
-- multiply
checker e (Mult e1 e2) | checker e e1 == TypeInt && checker e e2 == TypeInt = TypeInt
-- divide
checker e (Div e1 e2) | checker e e1 == TypeInt && checker e e2 == TypeInt = TypeInt
-- modulo
checker e (Mod e1 e2) | checker e e1 == TypeInt && checker e e2 == TypeInt = TypeInt
-- negate
checker e (Negate e1) | checker e e1 == TypeInt = TypeInt


-- less than
checker e (LessThan e1 e2) | checker e e1 == TypeInt && checker e e2 == TypeInt = TypeBool
-- bigger than
checker e (BiggerThan e1 e2) | checker e e1 == TypeInt && checker e e2 == TypeInt = TypeBool
-- less or equal than
checker e (LessOrEqThan e1 e2) | checker e e1 == TypeInt && checker e e2 == TypeInt = TypeBool
-- bigger or equal than
checker e (BiggerOrEqThan e1 e2) | checker e e1 == TypeInt && checker e e2 == TypeInt = TypeBool
-- equal
checker e (IsEq e1 e2) | checker e e1 == TypeInt && checker e e2 == TypeInt = TypeBool
-- not equal
checker e (NotEq e1 e2) | checker e e1 == TypeInt && checker e e2 == TypeInt = TypeBool
-- and
checker e (And e1 e2) | checker e e1 == TypeBool && checker e e2 == TypeBool = TypeBool
-- or
checker e (Or e1 e2) | checker e e1 == TypeBool && checker e e2 == TypeBool = TypeBool

-- sizeOf
checker e (SizeOf str) | isBinded str e == False = error "List has not been declared"
                       | getBinding str e == TypeList = TypeInt
-- output
checker e (Output exp) = TypeInt

-- type assignment 
checker e (TypeAssignment str t) | isBinded str e == False = checker (addBinding str t e) (TypeAssignment str t)
                                 | otherwise = t
-- assignment 
checker e (Assignment e1 e2) | (checker e e2) == (getBinding e1 e) = checker e e2

-- if statement
checker e (IfStmt e1 e2) | checker e e1 == TypeBool = checker e e2
                         | otherwise = error "Type error in if"
-- if else statement
checker e (IfElseStmt e1 e2 e3) | checker e e1 == TypeBool = checker e e2
                                | otherwise = error "Type error in if-else"

-- while statement
checker e (WhileExp e1 e2) | checker e e1 == TypeBool = checker e e2
                           | otherwise = error "Type error in while"

-- forEach statement
checker e (ForEach str str1 e1) | getBinding str e == TypeInt && getBinding str1 e == TypeList = checker e e1
                                | otherwise = error "Type error forEach"
-- App
checker e (App e1 e2 ) = checker (updateEnv e e1) e2

checker e (AddOne str) | getBinding str e == TypeInt = TypeInt
                       | otherwise = error "variable hasn't been declared"
                       
checker e (AddMany str e1) | getBinding str e == TypeInt && (checker e e1 == TypeInt) = TypeInt
                           | otherwise = error "variable hasn't been declared"
                       
-- error
checker e _ = error "Type Error"

-- function that prints the result of checking
printType :: Type -> String
printType TypeBool = "Bool"
printType TypeInt = "Int"
printType TypeList = "List"