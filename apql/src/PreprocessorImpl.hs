-- Put your Preprocessor implementation in this file.
module PreprocessorImpl where

import Types
import Data.List

clausify :: Program -> Either ErrMsg IDB
clausify p = case map t p of 
    x -> Right (IDB (specs x) x)     

specs :: [Clause] -> [(PName, Int)]
specs p = nub $ foldl s [] p

s :: [(PName, Int)] -> Clause -> [(PName, Int)]
s names (Clause (Atom n vars) _ _) = (n, length vars) : names 

t :: Rule -> [Clause]
t rule = map clause (transform rule)

clause (Rule atom cond) = Clause atom (positive cond) (tests cond) 

first :: Cond -> Cond
first cond = case cond of
    CNot (CAnd c1 c2) -> first $ COr (CNot c1) (CNot c2)
    CNot (COr c1 c2) -> first $ CAnd (CNot c1) (CNot c2)
    CNot (CNot c) -> first c
    CAnd c1 c2 -> CAnd (first c1) (first c2)
    COr c1 c2 -> COr (first c1) (first c2)
    CNot c -> CNot (first c)
    c -> c

second :: Cond -> Cond
second cond = case cond of 
    CAnd _ (CNot CTrue) -> CNot CTrue
    CAnd (CNot CTrue) _ -> CNot CTrue
    CAnd c1 (COr c2 c3) -> second $ COr (CAnd c1 c2) (CAnd c1 c3)
    CAnd (COr c1 c2) c3 -> second $ COr (CAnd c1 c3) (CAnd c2 c3)
    CAnd c1 c2 -> CAnd (second c1) (second c2)
    COr c1 c2 -> COr (second c1) (second c2)
    CNot c -> CNot (second c)
    c -> c

third :: (Atom, Cond) -> [Rule]
third r = case r of 
    (_,  CNot CTrue) -> []
    (a, COr c1 c2) -> [Rule a c1, Rule a c2]
    (a,  r) -> [Rule a r]

positive :: Cond -> [Atom]
positive cond = case cond of
    CTrue -> []
    CAnd c1 c2 -> positive c1 ++ positive c2
    CAtom atom -> [atom]
    _ -> []

tests :: Cond -> [Test]
tests cond = case cond of 
    CNot (CAtom atom) -> [TNot atom]
    CEq t1 t2 -> [TEq t1 t2]
    CNot (CEq t1 t2) ->  [TNeq t1 t2]

transform :: Rule -> [Rule]
transform rule = case rule of 
    Rule atom cond -> 
        let c = second $ first cond in
            third (atom, c)




stratify :: IDB -> [PSpec] -> Either ErrMsg [[PSpec]]
stratify = undefined
