module TypeCheck where
import Grammar

-- data StmtList = SingleExp Exp
--               | MultiExp StmtList StmtList
--              deriving (Show, Eq)
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

checker :: TypeEnvironment -> Exp -> Type

-- base cases
checker e (LanInt x) = TypeInt
checker e (LanVar x) = getBinding x e
checker e (LanTrue) = TypeBool
checker e (LanFalse) = TypeBool

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

-- type assignment 
checker e (TypeAssignment str t) | isBinded str e == False =  checker (addBinding str t e) (TypeAssignment str t)
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

-- error


-- stmChecker :: TypeEnvironment -> StmtList -> Type
-- stmChecker e (SingleExp e1) = checker e1
-- stmChecker e (MultiExp e1 e2) = 

-- function that prints the result of checking
printType :: Type -> String
printType TypeBool = "Bool"
printType TypeInt = "Int"