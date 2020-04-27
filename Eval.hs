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
           | Neg

type Kontinuation = [ Frame ]
type State = (Exp, Environment, Kontinuation)

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
isTerminated _ = False

--Small step evaluation function
eval1 :: State -> State
eval1 ((LanVar x),env,k) = (e',env',k) 
                    where (e',env') = getValueBinding x env
                  
-- Rule for terminated evaluations
eval1 (v,env,[]) | isTerminated v = (v,env,[])

-- Evaluation for negate operator
eval1 ((Negate e), env, k) = ( e , env, (Neg):k)
eval1 ((LanInt n), env, (Neg) : k) = (LanInt (-n), env, k)

-- Evaluation rules for plus operator
eval1 ((Plus e1 e2),env,k) = (e1,env,(HPlus e2 env):k)
eval1 ((LanInt n),env1,(HPlus e env2):k) = (e,env2,(PlusH (LanInt n)) : k)
eval1 ((LanInt m),env,(PlusH (LanInt n)):k) = (LanInt (n + m),env,k)

-- Evaluation rules for minus operator
eval1 ((Minus e1 e2),env,k) = (e1,env,(HMinus e2 env):k)
eval1 ((LanInt n),env1,(HMinus e env2):k) = (e,env2,(MinusH (LanInt n)) : k)
eval1 ((LanInt m),env,(MinusH (LanInt n)):k) = (LanInt (n - m),env,k)

-- Evaluation rules for times operator
eval1 ((Mult e1 e2),env,k) = (e1,env,(HMult e2 env):k)
eval1 ((LanInt n),env1,(HMult e env2):k) = (e,env2,(MultH (LanInt n)) : k)
eval1 ((LanInt m),env,(MultH (LanInt n)):k) = (LanInt (n * m),env,k)

-- Evaluation rules for divide operator
eval1 ((Div e1 e2),env,k) = (e1,env,(HDiv e2 env):k)
eval1 ((LanInt n),env1,(HDiv e env2):k) = (e,env2,(DivH (LanInt n)) : k)
eval1 ((LanInt m),env,(DivH (LanInt n)):k) = (LanInt (n `div` m),env,k)

-- Evaluation rules for mod operator
eval1 ((Mod e1 e2),env,k) = (e1,env,(HMod e2 env):k)
eval1 ((LanInt n),env1,(HMod e env2):k) = (e,env2,(ModH (LanInt n)) : k)
eval1 ((LanInt m),env,(ModH (LanInt n)):k) = (LanInt (n `mod` m),env,k)

-- Evaluation rules for less than operator
eval1 ((LessThan e1 e2), env, k) = (e1, env, (HLessThan e2 env):k)
eval1 ((LanInt n), env1, (HLessThan e env2):k) = (e, env2, (LessThanH (LanInt n)):k)
eval1 ((LanInt m), env, (LessThanH (LanInt n)):k) | n < m = (LanTrue,env,k)
                                                  | otherwise = (LanFalse,env,k)

-- Evaluation rules for less or equal than operator
eval1 ((LessOrEqThan e1 e2), env, k) = (e1, env, (HLessOrEqThan e2 env):k)
eval1 ((LanInt n), env1, (HLessOrEqThan e env2):k) = (e, env2, (LessOrEqThanH (LanInt n)):k)
eval1 ((LanInt m), env, (LessOrEqThanH (LanInt n)):k) | n <= m = (LanTrue,env,k)
                                                      | otherwise = (LanFalse,env,k)

-- Evaluation rules for bigger than operator
eval1 ((BiggerThan e1 e2), env, k) = (e1, env, (HBiggerThan e2 env):k)
eval1 ((LanInt n), env1, (HBiggerThan e env2):k) = (e, env2, (BiggerThanH (LanInt n)):k)
eval1 ((LanInt m), env, (BiggerThanH (LanInt n)):k) | n > m = (LanTrue,env,k)
                                                    | otherwise = (LanFalse,env,k)

-- Evaluation rules for bigger or equal than operator
eval1 ((BiggerOrEqThan e1 e2), env, k) = (e1, env, (HBiggerOrEqThan e2 env):k)
eval1 ((LanInt n), env1, (HBiggerOrEqThan e env2):k) = (e, env2, (BiggerOrEqThanH (LanInt n)):k)
eval1 ((LanInt m), env, (BiggerOrEqThanH (LanInt n)):k) | n >= m = (LanTrue,env,k)
                                                        | otherwise = (LanFalse,env,k)

-- Evaluation rules for equal operator
eval1 ((IsEq e1 e2), env, k) = (e1, env, (HIsEq e2 env):k)
eval1 ((LanInt n), env1, (HIsEq e env2):k) = (e, env2, (IsEqH (LanInt n)):k)
eval1 ((LanInt m), env, (IsEqH (LanInt n)):k) | n == m = (LanTrue,env,k)
                                              | otherwise = (LanFalse,env,k)

-- Evaluation rules for not equal operator
eval1 ((NotEq e1 e2), env, k) = (e1, env, (HNotEq e2 env):k)
eval1 ((LanInt n), env1, (HNotEq e env2):k) = (e, env2, (NotEqH (LanInt n)):k)
eval1 ((LanInt m), env, (NotEqH (LanInt n)):k) | n /= m = (LanTrue,env,k)
                                               | otherwise = (LanFalse,env,k)

-- Evaluation rules for and operator
eval1 ((And e1 e2), env, k) = (e1, env, (HAnd e2 env):k)
eval1 (LanTrue, env1, (HAnd e env2):k) = (e, env2, (AndH LanTrue):k)
eval1 (LanFalse, env1, (HAnd e env2):k) = (LanFalse,env1,k)
eval1 (LanTrue, env, (AndH LanTrue):k) = (LanTrue,env,k)
eval1 (LanTrue, env, (AndH LanFalse):k) = (LanFalse,env,k)

-- Evaluation rules for or operator
eval1 ((Or e1 e2), env, k) = (e1, env, (HOr e2 env):k)
eval1 (LanTrue, env1, (HOr e env2):k) = (LanTrue,env1,k)
eval1 (LanFalse, env1, (HOr e env2):k) = (e, env2, (OrH LanFalse):k)
eval1 (LanFalse, env, (OrH LanFalse):k) = (LanFalse,env,k)
            
-- Evaluation rules for if statement
eval1 ((IfStmt e1 e2), env, k) = (e1, env, (HIfStmt e2 env):k)
eval1 ((LanTrue), env1, (HIfStmt e2 env2):k) = (e2, env2, k)
eval1 ((LanFalse), env1, (HIfStmt e2 env2):k) =  (LanFalse, env2, k)

-- Evaluation rules for if-else statement
eval1 ((IfElseStmt e1 e2 e3), env, k) = (e1, env, (HIfElseStmt e2 e3 env):k)
eval1 ((LanTrue), env1, (HIfElseStmt e2 e3 env2):k) = (e2, env2, k) 
eval1 ((LanFalse), env1, (HIfElseStmt e2 e3 env2):k) = (e3, env2, k) 

-- Evaluation for Assignment
eval1 ((Assignment str e), env, k) = (e, env, (HAssignment str env):k)
eval1 (v, env1, (HAssignment str env2):k) | isTerminated v = (v, update env2 [] str v, k)

-- Evaluation for type assignment
eval1 ((TypeAssignment str t), env, k) = (LanTrue, env, k)

-- Evaluation for APP
eval1 ((App e1 e2), env, k) = (e1 , env , (HApp e2) : k)
eval1 ( v , env, (HApp e):k ) | isTerminated v = (e, env, k) 

-- Evaluation for while loop
eval1 ((WhileExp e1 e2), env, k) = (e1, env, (HWhile e1 e2 env):k)
eval1 ((LanTrue), env1, (HWhile e1 e2 env):k) = (e2, env, (WhileStmt e1 e2 env):k)
eval1 ((LanFalse), env1, ((HWhile e1 e2 env):k)) = (LanFalse, env, k)
eval1 ( _ , env1, (WhileStmt e1 e2 env):k) = (e1, env1, (HWhile e1 e2 env1):k)

eval1 _ = error "BAG PULA DC NU STIU SI OR"

-- Function to iterate the small step reduction to termination
evalLoop :: Exp -> Exp
evalLoop e = evalLoop' (e,[],[])
  where evalLoop' (e,env,k) = if (e' == e) && (isTerminated e') && (null k) then e' else evalLoop' (e',env',k')
                       where (e',env',k') = eval1 (e,env,k) 

-- evalWhile :: State -> 
-- evalWhile ((While e1 e2), env, k) | evalLoop e1 == LanTrue = evalWhile ((While e1 e2), env, k)

-- Function to unparse underlying values from the AST term
unparse :: Exp -> String 
unparse (LanInt n) = show n
unparse (LanTrue) = "true"
unparse (LanFalse) = "false"
unparse _ = "Unknown"

listToExp :: [Int] -> Exp 
listToExp (x:[]) = SingleList (LanInt x)
listToExp (x:xs) = MultipleList (LanInt x) $ listToExp xs 
