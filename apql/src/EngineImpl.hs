-- Put your Engine implementation in this file
module EngineImpl where

import Types
import qualified Data.Set as S  
import qualified Data.Map as M
import Data.List



-- type Env = [(VName, [Data])]

execute :: IDB -> [[PSpec]] -> EDB -> Either ErrMsg EDB
execute = undefined

-- stratum stratas = undefined


-- ggg (Atom pname terms) = case lookup (pname, length terms) edb of
--     Just x -> 
--     Nothing -> error






-- apply :: EDB -> [Clause] -> PSpec -> EDB
-- apply edb [] spec = edb
-- apply edb ((Clause (Atom pname terms) [pos] tests) : cs) spec
--     | spec == (pname, length terms) = let e = positives edb pos in constrain edb e tests
--     | spec /= (pname, length terms) = edb 

-- contribute :: EDB -> Env -> Atom -> EDB
-- contribute edb env (Atom pname terms) = undefined

-- crow env ((TData d) : terms) = d : rrr env terms
-- crow env ((TVar vname) : terms) = lookup env vname 
--     | Just (b : binds) -> x : rrr (insert env vname binds) terms

-- positives :: EDB -> Atom -> 
-- positives edb (Atom pname terms) = case lookup (pname, length terms) edb of
--     Just x ->

-- union :: [[Data]] -> [[Data]] -> [[Data]] 
-- union table table' =  [[x ++ y ] | x <- table, y <- table']

-- andIt :: EDB -> Env -> Atom -> [(VName, [Data])]
-- andIt edb env (Atom pname terms) = case lookup (pname, length terms) edb of
--     Just x -> undefined
--     Nothing -> [] 


-- bind :: EDB -> Atom -> [(VName, [Data])]
-- bind edb (Atom pname terms) = case lookup (pname, length terms) edb of
--     Just x -> undefined
--     Nothing -> []

-- q :: [Term] -> Row -> [(VName, Data)] -> [(VName, Data)]
-- q  _ [] acc = acc 
-- q ((TVar vname) : terms) (r : row) acc = q terms row ((vname, r) : acc)
-- q ((TData dat) : terms) (r : row) acc 
--     | dat == r = q terms row acc
--     | dat /= r = []

-- constrain :: EDB -> Env -> [Test] -> Env
-- constrain edb env tests = foldl (te edb ) env tests 

-- te :: EDB -> Env -> EDB -> Test -> Env
-- te edb env (TNot atom) = let y = bind edb atom in undefined
-- te edb env (TEq (TVar vname) (TVar vname')) = case lookup env vname of
--     Just x -> case lookup env vname' of 
--         Just x' -> let y = x `intersect` x' in let e = insert env y vname in insert e y vname'

-- notBindings :: Env -> [(VName, [Data])] -> Env
-- notBindings env [] = env
-- notBindings env ((vname, bs) : bindings) = case lookup vname env of
--     Just x -> notBindings (insert env x \\ bs) bindings
--     Nothing -> env 

-- insert :: Env -> VName -> [Data] -> Env 
-- insert ((vnam, d): e) (vname, daa)
--     | vname == vname = (vname, daa)
--     | vname != vname = insert (vnam, d) (vname, daa) : e
    