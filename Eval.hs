module Eval where
import Grammar

data Frame = AddH Exp | HAdd Exp Environment 

type Kontinuation = [ Frame ]
type State = (Exp, Environment, Kontinuation)

--Small step evaluation function
eval1 :: State -> State
eval1 ((LanVar x),env,k) = (e',env',k) 
                    where (e',env') = getValueBinding x env
                  
-- Rule for terminated evaluations
eval1 (v,env,[]) | isValue v = (v,env,[])

-- Evaluation rules for plus operator
eval1 ((Add e1 e2),env,k) = (e1,env,(HAdd e2 env):k)
eval1 ((LanInt n),env1,(HAdd e env2):k) = (e,env2,(AddH (LanInt n)) : k)
eval1 ((LanInt m),env,(AddH (LanInt n)):k) = (LanInt (n + m),[],k)
