module Eval where
import Grammar

data Frame = PlusH Exp | HPlus Exp Environment 
           | LessThanH Exp | HLessThan Exp Environment
           | LessOrEqThanH Exp | HLessOrEqThan Exp Environment
           | BiggerThanH Exp | HBiggerThan Exp Environment
           | BiggerOrEqThanH Exp | HBiggerOrEqThan Exp Environment
           | IsEqH Exp | HIsEq Exp Environment
           | NotEqH Exp | HNotEq Exp Environment
           | AndH Exp | HAnd Exp Environment
           | OrH Exp | HOr Exp Environment

type Kontinuation = [ Frame ]
type State = (Exp, Environment, Kontinuation)

getValueBinding :: String -> Environment -> (Exp, Environment)
getValueBinding x [] = error "Variable binding not found"
getValueBinding x ((y,e):env) = getValueBinding x env

update :: Environment -> String -> Exp ->Environment
update env x e = (x,e) : env

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

-- Evaluation rules for plus operator
eval1 ((Plus e1 e2),env,k) = (e1,env,(HPlus e2 env):k)
eval1 ((LanInt n),env1,(HPlus e env2):k) = (e,env2,(PlusH (LanInt n)) : k)
eval1 ((LanInt m),env,(PlusH (LanInt n)):k) = (LanInt (n + m),[],k)

-- Evaluation rules for less than operator
eval1 ((LessThan e1 e2), env, k) = (e1, env, (HLessThan e2 env):k)
eval1 ((LanInt n), env1, (HLessThan e env2):k) = (e, env2, (LessThanH (LanInt n)):k)
eval1 ((LanInt m), env, (LessThanH (LanInt n)):k) | n < m = (LanTrue,[],k)
                                                  | otherwise = (LanFalse,[],k)

-- Evaluation rules for less or equal than operator
eval1 ((LessOrEqThan e1 e2), env, k) = (e1, env, (HLessOrEqThan e2 env):k)
eval1 ((LanInt n), env1, (HLessThan e env2):k) = (e, env2, (LessOrEqThanH (LanInt n)):k)
eval1 ((LanInt m), env, (LessOrEqThanH (LanInt n)):k) | n <= m = (LanTrue,[],k)
                                                      | otherwise = (LanFalse,[],k)

-- Evaluation rules for bigger than operator
eval1 ((BiggerThan e1 e2), env, k) = (e1, env, (HBiggerThan e2 env):k)
eval1 ((LanInt n), env1, (HBiggerThan e env2):k) = (e, env2, (BiggerThanH (LanInt n)):k)
eval1 ((LanInt m), env, (BiggerThanH (LanInt n)):k) | n > m = (LanTrue,[],k)
                                                    | otherwise = (LanFalse,[],k)

-- Evaluation rules for bigger or equal than operator
eval1 ((BiggerOrEqThan e1 e2), env, k) = (e1, env, (HBiggerOrEqThan e2 env):k)
eval1 ((LanInt n), env1, (HBiggerOrEqThan e env2):k) = (e, env2, (BiggerOrEqThanH (LanInt n)):k)
eval1 ((LanInt m), env, (BiggerOrEqThanH (LanInt n)):k) | n >= m = (LanTrue,[],k)
                                                        | otherwise = (LanFalse,[],k)

-- Evaluation rules for equal operator
eval1 ((IsEq e1 e2), env, k) = (e1, env, (HIsEq e2 env):k)
eval1 ((LanInt n), env1, (HIsEq e env2):k) = (e, env2, (IsEqH (LanInt n)):k)
eval1 ((LanInt m), env, (IsEqH (LanInt n)):k) | n == m = (LanTrue,[],k)
                                              | otherwise = (LanFalse,[],k)

-- Evaluation rules for not equal operator
eval1 ((NotEq e1 e2), env, k) = (e1, env, (HNotEq e2 env):k)
eval1 ((LanInt n), env1, (HNotEq e env2):k) = (e, env2, (NotEqH (LanInt n)):k)
eval1 ((LanInt m), env, (NotEqH (LanInt n)):k) | n != m = (LanTrue,[],k)
                                               | otherwise = (LanFalse,[],k)

-- Evaluation rules for and operator
eval1 ((And e1 e2), env, k) = (e1, env, (HAnd e2 env):k)
eval1 ((LanBool n), env1, (HAnd e env2):k) = (e, env2, (AndH (LanBool n)):k)
eval1 ((LanBool m), env, (AndH (LanBool n)):k) | n && m = (LanTrue,[],k)
                                              | otherwise = (LanFalse,[],k)

-- Evaluation rules for or operator
eval1 ((Or e1 e2), env, k) = (e1, env, (HOr e2 env):k)
eval1 ((LanBool n), env1, (HOr e env2):k) = (e, env2, (OrH (LanBool n)):k)
eval1 ((LanBool m), env, (OrH (LanBool n)):k) | n || m = (LanTrue,[],k)
                                            | otherwise = (LanFalse,[],k)



eval1 _ = error "I only know how to add and less"

-- Function to iterate the small step reduction to termination
evalLoop :: Exp -> Exp
evalLoop e = evalLoop' (e,[],[])
  where evalLoop' (e,env,k) = if (e' == e) && (isTerminated e') && (null k) then e' else evalLoop' (e',env',k')
                       where (e',env',k') = eval1 (e,env,k) 

-- Function to unparse underlying values from the AST term
unparse :: Exp -> String 
unparse (LanInt n) = show n
unparse (LanTrue) = "true"
unparse (LanFalse) = "false"
unparse _ = "Unknown"