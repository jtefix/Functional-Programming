module Eval where
import Grammar

data Frame = PlusH Exp | HPlus Exp Environment 

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

eval1 _ = error "I only know how to add"

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