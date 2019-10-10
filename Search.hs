{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE MultiParamTypeClasses #-}

module Search where

import ProblemState
import Data.Maybe
import Data.List

import qualified Data.Set as S

{-
    *** TODO ***

    Tipul unei nod utilizat în procesul de căutare. Recomandăm reținerea unor
    informații legate de:

    * stare;
    * acțiunea care a condus la această stare;
    * nodul părinte, prin explorarea căruia a fost obținut nodul curent;
    * adâncime
    * copiii, ce vor desemna stările învecinate
-}

data Node s a = NodeC { state :: s
                      , action :: Maybe a
                      , parent :: Maybe (Node s a)
                      , depth :: Int
                      , children :: [(a,s)]
                      }

{-
    *** TODO ***

    Întoarce starea stocată într-un nod.
-}

nodeState :: Node s a -> s
nodeState n = (state n)

{-
    *** TODO ***

    Generarea întregului spațiu al stărilor 
    Primește starea inițială și creează nodul corespunzător acestei stări, 
    având drept copii nodurile succesorilor stării curente.
-}

createStateSpace :: (ProblemState s a) => s -> Node s a
createStateSpace state = (NodeC state Nothing Nothing 0 (successors state)) 

{-
    *** TODO PENTRU BONUS ***

    Ordonează întreg spațiul stărilor după euristica din ProblemState. 
    Puteți folosi `sortBy` din Data.List.
-}

orderStateSpace :: (ProblemState s a) => Node s a -> Node s a
sortHelper :: (ProblemState s a) => (a, s) -> (a, s) -> Ordering
sortHelper (a1, s1) (a2, s2) | (heuristic s1) > (heuristic s2) = GT 
                             | (heuristic s1) < (heuristic s2) = LT
                             | otherwise = EQ
orderStateSpace (NodeC state action parent depth children) = (NodeC state action parent depth (sortBy sortHelper children))


{-
    *** TODO ***

    Întoarce lista nodurilor rezultate prin parcurgerea limitată în adâncime
    a spațiului stărilor, pornind de la nodul dat ca parametru.

    Pentru reținerea stărilor vizitate, recomandăm Data.Set. Constrângerea
    `Ord s` permite utilizarea tipului `Set`.
-}
limitedDfs :: (ProblemState s a, Ord s)
           => Node s a    -- Nodul stării inițiale
           -> Int         -- Adâncimea maximă de explorare
           -> [Node s a]  -- Lista de noduri



helperDfs _ [] _ res = res
helperDfs set (x : xs) max res | (depth x) > max = helperDfs set xs max res
                               | S.notMember (state x) set = helperDfs (S.insert (state x) set) (cList ++ xs) max (res ++ [x])
                               | otherwise = helperDfs set xs max res

                               where
                               cList = map (\ c -> (orderStateSpace (NodeC (snd c) (Just (fst c)) (Just x) ((depth x) + 1) (successors (snd c))))) (children x)
 

limitedDfs root max = helperDfs S.empty [root] max []



{-
    *** TODO ***

    Explorează în adâncime spațiul stărilor, utilizând adâncire iterativă,
    pentru determinarea primei stări finale întâlnite.

    Întoarce o perche între nodul cu prima stare finală întâlnită și numărul
    de stări nefinale vizitate până în acel moment.
-}

iterativeDeepening :: (ProblemState s a, Ord s)
    => Node s a         -- Nodul stării inițiale
    -> (Node s a, Int)  -- (Nod cu prima stare finală,
                        --  număr de stări nefinale vizitate)
findPosInList [] = []
findPosInList list | isGoal (state (head list)) = list
                   | otherwise = findPosInList (tail list)

helper :: (ProblemState s a, Ord s) => Node s a -> Int -> Int -> (Node s a, Int)
helper root max sum | (length result) /= 0 =  ((head result), (sum + (length dfs) - (length (findPosInList dfs))))
                    | otherwise = (helper root (max + 1) (sum + length dfs))

                    where
                    result = filter (\ y -> isGoal (state y)) dfs
                    dfs =  (limitedDfs root max)

iterativeDeepening root = helper root 0 0

{-
    *** TODO ***

    Pornind de la un nod, reface calea către nodul inițial, urmând legăturile
    către părinți.

    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

extractPath :: Node s a -> [(a, s)]
extractPath node = fst (foldl (\ acc x -> ([(fromJust (action (snd acc)), state (snd acc))] ++ (fst acc), fromJust (parent (snd acc)))) ([], node) [1..(depth node)])

{-
    *** TODO ***

    Pornind de la o stare inițială, se folosește de iterativeDeepening pentru 
    a găsi prima stare finală și reface calea către nodul inițial folosind 
    extractPath. 
  
    Întoarce o listă de perechi (acțiune, stare), care se încheie în starea
    finală, dar care EXCLUDE starea inițială.
-}

solve :: (ProblemState s a, Ord s)
      => s          -- Starea inițială de la care se pornește 
      -> Bool       -- Dacă să folosească sau nu euristica dată 
      -> [(a, s)]   -- Lista perechilor
solve stare eur | eur == False =  extractPath node
                | otherwise = extractPath newNode
                    where
                    node = fst (iterativeDeepening (createStateSpace stare))
                    newNode =  fst (iterativeDeepening (orderStateSpace (createStateSpace stare)))
{-
    Poate fi utilizată pentru afișarea fiecărui element al unei liste
    pe o linie separată.
-}

printSpacedList :: Show a => [a] -> IO ()
printSpacedList = mapM_ (\a -> print a >> putStrLn (replicate 20 '*'))