module Eval where
import Grammar

data Frame = PlusH Exp | HPlus Exp Environment 
           | MinusH Exp | HMinus Exp Environment
           | MultH Exp | HMult Exp Environment
           | DivH Exp | HDiv Exp Environment
           | ModH Exp | HMod Exp Environment
           | LessThanH Exp | HLessThan Exp Environment
           | LessOrEqThanH Exp | HLessOrEqThan Exp Environment
           | BiggerThanH Exp | HBiggerThan Exp Environment
           | BiggerOrEqThanH Exp | HBiggerOrEqThan Exp Environment
           | IsEqH Exp | HIsEq Exp Environment
           | NotEqH Exp | HNotEq Exp Environment
           | AndH Exp | HAnd Exp Environment
           | OrH Exp | HOr Exp Environment
           | HIfStmt Exp Environment
           | HIfElseStmt Exp Exp Environment
           | HAssignment String Environment
           | HTypeAssignment String Environment
           | HWhile Exp Exp Environment
           | WhileStmt Exp Exp Environment
           | HApp Exp 
           | HIndexAssignment String Exp
           | Neg

type Kontinuation = [ Frame ]
type State = (Exp, Environment, Kontinuation, [[Int]])

getValueBinding :: String -> Environment -> (Exp, Environment)
getValueBinding x [] = error "Variable binding not found"
getValueBinding x ((y,e):env) | x == y = (e,(y,e):env)
                              | otherwise = getValueBinding x env

-- updateEnvironment :: String -> Exp -> Environment -> Environment
-- updateEnvironment str t env = [(str, t)] ++ env

update :: Environment -> Environment -> String -> Exp -> Environment
update [] newEnv s e = (s,e) : newEnv
update ((x,y):oldEnv) newEnv s e | s == x = (s,e) : newEnv ++ oldEnv
                                 | otherwise = update oldEnv ((x,y):newEnv) s e 
                                 
isTerminated :: Exp -> Bool
isTerminated (LanInt _ ) = True
isTerminated LanTrue = True
isTerminated LanFalse = True
isTerminated (SingleList _) = True
isTerminated (MultipleList _ _) = True
isTerminated EmptyList = True
isTerminated _ = False

--Small step evaluation function
eval1 :: State -> State
eval1 ((LanVar x), env, k, xs) = (e', env', k, xs) 
                    where (e',env') = getValueBinding x env
                  
-- Rule for terminated evaluations
eval1 (v,env,[], xs) | isTerminated v = (v,env,[], xs)

-- Evaluation for negate operator
eval1 ((Negate e), env, k, xs) = ( e , env, (Neg):k, xs)
eval1 ((LanInt n), env, (Neg) : k, xs) = (LanInt (-n), env, k, xs)

-- Evaluation rules for plus operator
eval1 ((Plus e1 e2),env,k, xs) = (e1,env,(HPlus e2 env):k, xs)
eval1 ((LanInt n),env1,(HPlus e env2):k, xs) = (e,env2,(PlusH (LanInt n)) : k, xs)
eval1 ((LanInt m),env,(PlusH (LanInt n)):k, xs) = (LanInt (n + m),env,k, xs)

-- Evaluation rules for minus operator
eval1 ((Minus e1 e2),env,k , xs) = (e1,env,(HMinus e2 env):k, xs)
eval1 ((LanInt n),env1,(HMinus e env2):k, xs) = (e,env2,(MinusH (LanInt n)) : k, xs)
eval1 ((LanInt m),env,(MinusH (LanInt n)):k, xs) = (LanInt (n - m),env,k, xs)

-- Evaluation rules for times operator
eval1 ((Mult e1 e2),env,k, xs) = (e1,env,(HMult e2 env):k, xs)
eval1 ((LanInt n),env1,(HMult e env2):k, xs) = (e,env2,(MultH (LanInt n)) : k, xs)
eval1 ((LanInt m),env,(MultH (LanInt n)):k, xs) = (LanInt (n * m),env,k, xs)

-- Evaluation rules for divide operator
eval1 ((Div e1 e2),env,k, xs) = (e1,env,(HDiv e2 env):k, xs)
eval1 ((LanInt n),env1,(HDiv e env2):k, xs) = (e,env2,(DivH (LanInt n)) : k, xs)
eval1 ((LanInt m),env,(DivH (LanInt n)):k, xs) = (LanInt (n `div` m),env,k, xs)

-- Evaluation rules for mod operator
eval1 ((Mod e1 e2),env,k, xs) = (e1,env,(HMod e2 env):k, xs)
eval1 ((LanInt n),env1,(HMod e env2):k, xs) = (e,env2,(ModH (LanInt n)) : k, xs)
eval1 ((LanInt m),env,(ModH (LanInt n)):k, xs) = (LanInt (n `mod` m),env,k, xs)

-- Evaluation rules for less than operator
eval1 ((LessThan e1 e2), env, k, xs) = (e1, env, (HLessThan e2 env):k, xs)
eval1 ((LanInt n), env1, (HLessThan e env2):k, xs) = (e, env2, (LessThanH (LanInt n)):k, xs)
eval1 ((LanInt m), env, (LessThanH (LanInt n)):k, xs) | n < m = (LanTrue,env,k, xs)
                                                  | otherwise = (LanFalse,env,k, xs)

-- Evaluation rules for less or equal than operator
eval1 ((LessOrEqThan e1 e2), env, k, xs) = (e1, env, (HLessOrEqThan e2 env):k, xs)
eval1 ((LanInt n), env1, (HLessOrEqThan e env2):k, xs) = (e, env2, (LessOrEqThanH (LanInt n)):k, xs)
eval1 ((LanInt m), env, (LessOrEqThanH (LanInt n)):k, xs) | n <= m = (LanTrue,env,k, xs)
                                                      | otherwise = (LanFalse,env,k, xs)

-- Evaluation rules for bigger than operator
eval1 ((BiggerThan e1 e2), env, k, xs) = (e1, env, (HBiggerThan e2 env):k, xs)
eval1 ((LanInt n), env1, (HBiggerThan e env2):k, xs) = (e, env2, (BiggerThanH (LanInt n)):k, xs)
eval1 ((LanInt m), env, (BiggerThanH (LanInt n)):k, xs) | n > m = (LanTrue,env,k, xs)
                                                    | otherwise = (LanFalse,env,k, xs)

-- Evaluation rules for bigger or equal than operator
eval1 ((BiggerOrEqThan e1 e2), env, k, xs) = (e1, env, (HBiggerOrEqThan e2 env):k, xs)
eval1 ((LanInt n), env1, (HBiggerOrEqThan e env2):k, xs) = (e, env2, (BiggerOrEqThanH (LanInt n)):k, xs)
eval1 ((LanInt m), env, (BiggerOrEqThanH (LanInt n)):k, xs) | n >= m = (LanTrue,env,k, xs)
                                                        | otherwise = (LanFalse,env,k, xs)

-- Evaluation rules for equal operator
eval1 ((IsEq e1 e2), env, k, xs) = (e1, env, (HIsEq e2 env):k, xs)
eval1 ((LanInt n), env1, (HIsEq e env2):k, xs) = (e, env2, (IsEqH (LanInt n)):k, xs)
eval1 ((LanInt m), env, (IsEqH (LanInt n)):k, xs) | n == m = (LanTrue,env,k, xs)
                                              | otherwise = (LanFalse,env,k, xs)

-- Evaluation rules for not equal operator
eval1 ((NotEq e1 e2), env, k, xs) = (e1, env, (HNotEq e2 env):k, xs)
eval1 ((LanInt n), env1, (HNotEq e env2):k, xs) = (e, env2, (NotEqH (LanInt n)):k, xs)
eval1 ((LanInt m), env, (NotEqH (LanInt n)):k, xs) | n /= m = (LanTrue,env,k, xs)
                                               | otherwise = (LanFalse,env,k, xs)

-- Evaluation rules for and operator
eval1 ((And e1 e2), env, k, xs) = (e1, env, (HAnd e2 env):k, xs)
eval1 (LanTrue, env1, (HAnd e env2):k, xs) = (e, env2, (AndH LanTrue):k, xs)
eval1 (LanFalse, env1, (HAnd e env2):k, xs) = (LanFalse,env1,k, xs)
eval1 (LanTrue, env, (AndH LanTrue):k, xs) = (LanTrue,env,k, xs)
eval1 (LanTrue, env, (AndH LanFalse):k, xs) = (LanFalse,env,k, xs)

-- Evaluation rules for or operator
eval1 ((Or e1 e2), env, k, xs) = (e1, env, (HOr e2 env):k, xs)
eval1 (LanTrue, env1, (HOr e env2):k, xs) = (LanTrue,env1,k, xs)
eval1 (LanFalse, env1, (HOr e env2):k, xs) = (e, env2, (OrH LanFalse):k, xs)
eval1 (LanFalse, env, (OrH LanFalse):k, xs) = (LanFalse,env,k, xs)
            
-- Evaluation rules for if statement
eval1 ((IfStmt e1 e2), env, k, xs) = (e1, env, (HIfStmt e2 env):k, xs)
eval1 ((LanTrue), env1, (HIfStmt e2 env2):k, xs) = (e2, env2, k, xs)
eval1 ((LanFalse), env1, (HIfStmt e2 env2):k, xs) =  (LanFalse, env2, k, xs)

-- Evaluation rules for if-else statement
eval1 ((IfElseStmt e1 e2 e3), env, k, xs) = (e1, env, (HIfElseStmt e2 e3 env):k, xs)
eval1 ((LanTrue), env1, (HIfElseStmt e2 e3 env2):k, xs) = (e2, env2, k, xs) 
eval1 ((LanFalse), env1, (HIfElseStmt e2 e3 env2):k, xs) = (e3, env2, k, xs) 

-- Evaluation for Assignment
eval1 ((Assignment str e), env, k, xs) = (e, env, (HAssignment str env):k, xs)
eval1 (v, env1, (HAssignment str env2):k, xs) | isTerminated v = (v, update env2 [] str v, k, xs)

-- Evaluation for IndexOf
eval1 ((IndexOf str (LanInt x)), env, k, xs) = (LanInt value, env, k, xs)
    where value = getValueAtIndex x exp
          (exp, env1) = getValueBinding str env

eval1 ((IndexAssignment str index exp), env, k, xs) = (exp, env, (HIndexAssignment str index):k, xs)
eval1 ( v , env, (HIndexAssignment str (LanInt x):k), xs) | isTerminated v = ( v, update env [] str exp, k, xs)
                                    where exp = changeValueAtIndex x list v
                                          (list, env1) = getValueBinding str env    
    
-- Evaluation for type assignment
eval1 ((TypeAssignment str t), env, k, xs) = (LanTrue, env, k, xs)

-- Evaluation for APP
eval1 ((App e1 e2), env, k, xs) = (e1 , env , (HApp e2) : k, xs)
eval1 ( v , env, (HApp e):k , xs) | isTerminated v = (e, env, k, xs) 

-- Evaluation for while loop
eval1 ((WhileExp e1 e2), env, k, xs) = (e1, env, (HWhile e1 e2 env):k, xs)
eval1 ((LanTrue), env1, (HWhile e1 e2 env):k, xs) = (e2, env, (WhileStmt e1 e2 env):k, xs)
eval1 ((LanFalse), env1, ((HWhile e1 e2 env):k), xs) = (LanFalse, env, k, xs)
eval1 ( _ , env1, (WhileStmt e1 e2 env):k, xs) = (e1, env1, (HWhile e1 e2 env1):k, xs)

eval1 _ = error "BAG PULA DC NU STIU"

-- Function to iterate the small step reduction to termination
evalLoop :: Exp -> Exp -- GO BACK HEREEEE
evalLoop e = evalLoop' (e,[],[])
  where evalLoop' (e,env,k) = if (e' == e) && (isTerminated e') && (null k) then e' else evalLoop' (e',env',k')
                       where (e',env',k', []) = eval1 (e,env,k, []) 

-- evalWhile :: State -> 
-- evalWhile ((While e1 e2), env, k) | evalLoop e1 == LanTrue = evalWhile ((While e1 e2), env, k)

-- Function to unparse underlying values from the AST term
unparse :: Exp -> String 
unparse (LanInt n) = show n
unparse (LanTrue) = "true"
unparse (LanFalse) = "false"
unparse _ = "Unknown"

listToExp :: [Int] -> Exp 
listToExp [] = EmptyList
listToExp (x:[]) = SingleList (LanInt x)
listToExp (x:xs) = MultipleList (LanInt x) $ listToExp xs 

getValueAtIndex :: Int -> Exp -> Int
getValueAtIndex x _ | x < 1 = error "Out of bounds"
getValueAtIndex 1 ( SingleList (LanInt x)) = x
getValueAtIndex 1 ( MultipleList (LanInt x) e) = x
getValueAtIndex x ( SingleList e) = error "Out of bounds"
getValueAtIndex x ( MultipleList e1 e2) = getValueAtIndex (x-1) e2

changeValueAtIndex :: Int -> Exp -> Exp -> Exp
changeValueAtIndex x _ _ | x < 1 = error "Out of bounds"
changeValueAtIndex 1 (SingleList x) value = (SingleList value)
changeValueAtIndex 1 (MultipleList e1 e2) value = (MultipleList value e2)
changeValueAtIndex x (SingleList e) value = error "Out of bounds"
changeValueAtIndex x (MultipleList e1 e2) value = MultipleList e1 (changeValueAtIndex (x-1) e2 value) 
