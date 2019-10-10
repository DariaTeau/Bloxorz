{-# OPTIONS_GHC -Wall #-}
{-# LANGUAGE EmptyDataDecls, MultiParamTypeClasses,
             TypeSynonymInstances, FlexibleInstances,
             InstanceSigs #-}


module Bloxorz where

import ProblemState


import qualified Data.Array as A


{-
    Caracterele ce vor fi printate pentru fiecare tip de obiect din joc 
    Puteți înlocui aceste caractere cu orice, în afară de '\n'.
-}

hardTile :: Char
hardTile = '▒'

softTile :: Char
softTile = '='

block :: Char
block = '▓'

switch :: Char
switch = '±'

emptySpace :: Char
emptySpace = ' '

winningTile :: Char
winningTile = '*'

{-
    Sinonim de tip de date pentru reprezetarea unei perechi (int, int)
    care va reține coordonatele de pe tabla jocului
-}

type Position = (Int, Int)
{-
    Direcțiile în care se poate mișcă blocul de pe tablă
-}

data Directions = North | South | West | East
    deriving (Show, Eq, Ord)

{-
    *** TODO ***

    Tip de date care va reprezenta plăcile care alcătuiesc harta și switch-urile
-}

data Cell = Cell
			{ tile :: Char
			, position :: Position
			, isSwitch :: Bool
			, hasBlock :: Bool
			, positions :: [Position]
			}
		deriving (Eq, Ord)


instance Show Cell where
    show (Cell t p isS hasB pos) | hasB == True =  [block]
    							 | otherwise = [t]

{-
    *** TODO ***

    Tip de date pentru reprezentarea nivelului curent
-}

data Level = Level
			{matrix :: A.Array Int [Cell]
			, blockPos :: [Position]
			, m :: Int
			, n :: Int
			}
    deriving (Eq, Ord)

{-
    *** Opțional *** 
  
    Dacă aveți nevoie de o funcționalitate particulară, 
    instantiati explicit clasele Eq și Ord pentru Level. 
    În cazul acesta, eliminați deriving (Eq, Ord) din Level. 
-}

-- instance Eq Level where
--     (==) = undefined

-- instance Ord Level where
--     compare = undefined

{-
    *** TODO ***

    Instantiati Level pe Show. 

    Atenție! String-ul returnat va fi urmat și precedat de un rând nou. 
    În cazul în care jocul este câștigat, la sfârșitul stringului se va mai
    concatena mesajul "Congrats! You won!\n". 
    În cazul în care jocul este pierdut, se va mai concatena "Game Over\n". 
-}

tileAt pos list = tile (head (drop pos list))
-- face show pe o lista ce cells
doItOnLists :: [Cell] -> String
doItOnLists list = (foldl (\ acc c@(Cell t p isS hasB pos)-> acc ++ (show c)) "" list)

instance Show Level where
    show  (Level mat blPos m n) | ((length blPos) == 1) && ((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == softTile)
    								= "\n" ++ (foldl (\ acc x -> acc ++ x ++ "\n") "" (map (\ x -> (doItOnLists x)) (A.elems mat)))
    								++ "Game Over\n"
                                | ((length blPos) == 2) && (((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == winningTile)
    								|| ((tileAt (snd (head (tail blPos))) (mat A.! (fst (head (tail blPos))))) == winningTile))
    								= "\n" ++ (foldl (\ acc x -> acc ++ x ++ "\n") "" (map (\ x -> (doItOnLists x)) (A.elems mat)))
    									++ "Congrats! You won!\n"
                                | ((length blPos) == 1) && ((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == winningTile)
    								= "\n" ++ (foldl (\ acc x -> acc ++ x ++ "\n") "" (map (\ x -> (doItOnLists x)) (A.elems mat)))
    								++ "Congrats! You won!\n"
    							| ((length blPos) == 1) && ((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == emptySpace)
									= "\n" ++ (foldl (\ acc x -> acc ++ x ++ "\n") "" (map (\ x -> (doItOnLists x)) (A.elems mat)))
    									++ "Game Over\n"
    							| ((length blPos) == 2) && (((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == emptySpace)
    								|| ((tileAt (snd (head (tail blPos))) (mat A.! (fst (head (tail blPos))))) == emptySpace))
    								= "\n" ++ (foldl (\ acc x -> acc ++ x ++ "\n") "" (map (\ x -> (doItOnLists x)) (A.elems mat)))
    									++ "Game Over\n"
    							| otherwise = "\n" ++ (foldl (\ acc x -> acc ++ x ++ "\n") "" (map (\ x -> (doItOnLists x)) (A.elems mat)))

{-
    *** TODO ***

    Primește coordonatele colțului din dreapta jos a hârtii și poziția inițială a blocului.
    Întoarce un obiect de tip Level gol.
    Implicit, colțul din stânga sus este (0, 0).
-}

createCellList i n blPos = map (\ x -> if ((fst blPos) == i) && ((snd blPos) == x)
						 then (Cell block (i, x) False True []) else (Cell emptySpace (i, x) False False [])) [0..n]
emptyLevel :: Position -> Position -> Level
emptyLevel mapPos blPos = Level (A.array (0, (fst mapPos)) [(i, (createCellList i (snd mapPos) blPos)) | i <- [0..(fst mapPos)]]) [blPos] (fst mapPos) (snd mapPos)

{-
    *** TODO ***

    Adaugă o celulă de tip Tile în nivelul curent.
    Parametrul char descrie tipul de tile adăugat: 
        'H' pentru tile hard 
        'S' pentru tile soft 
        'W' pentru winning tile 
-}
condition x atPos = ((fst (position x)) == (fst atPos)) && ((snd (position x)) == (snd atPos))
--modifica tile-ul unei celule
reconstruct tile x listOfPos | tile == switch = (Cell tile (position x) True (hasBlock x) listOfPos)
							 | otherwise = (Cell tile (position x) (isSwitch x) (hasBlock x) listOfPos)

--modifica tile-ul celulei de la pozitia atPos
replaceCell cells atPos tile listOfPos = foldl (\ acc x -> if (condition x atPos) then (acc ++ [(reconstruct tile x listOfPos)]) else (acc ++ [x])) [] cells
addTile :: Char -> Position -> Level -> Level

translateChar c | c == 'H' = hardTile
				| c == 'S' = softTile
				| c == 'W' = winningTile
				| otherwise = undefined

addTile tile pos (Level mat blPos m n) = Level (mat A.// [(i, (replaceCell (mat A.! (fst pos)) pos (translateChar tile) [])) | i <- [(fst pos)]]) blPos m n 

{-
    *** TODO ***

    Adaugă o celulă de tip Swtich în nivelul curent.
    Va primi poziția acestuia și o listă de Position
    ce vor desemna pozițiile în care vor apărea sau 
    dispărea Hard Cells în momentul activării/dezactivării
    switch-ului.
-}
--lista o sa aiba pe prima pozitie -1 daca e inactiv
addSwitch :: Position -> [Position] -> Level -> Level
addSwitch pos listOfPos (Level mat blPos m n) = Level (mat A.// [(i, (replaceCell (mat A.! (fst pos)) pos switch (listOfPos))) | i <- [(fst pos)]]) blPos m n

{-
    === MOVEMENT ===
-}

{-
    *** TODO ***

    Activate va verifica dacă mutarea blocului va activa o mecanică specifică. 
    În funcție de mecanica activată, vor avea loc modificări pe hartă. 
-}

--verifica daca un emptySpace e inlocuit cu un hardTile si invers
correctCell cells pos | (tile (head (drop ((snd pos)) cells))) == emptySpace = hardTile
					  |	(tile (head (drop ((snd pos)) cells))) == hardTile = emptySpace
					  | otherwise = (tile (head (drop ((snd pos)) cells)))
activate :: Cell -> Level -> Level
activate cell (Level mat blPos m n) = (Level (A.listArray (0, m) (foldl (\ acc x -> (take ((fst x)) acc) ++ ((replaceCell (head (drop ((fst x)) acc)) x 
										(correctCell (head (drop ((fst x)) acc)) x) []) : (drop ((fst x) + 1) acc))) 
										(A.elems mat) (positions cell))) blPos m n)



{-
    *** TODO ***

    Mișcarea blocului în una din cele 4 direcții 
    Hint: Dacă jocul este deja câștigat sau pierdut, puteți lăsa nivelul neschimbat.
-}
--primeste lista de pos ca sa o updateze
directionToPosition direction positions | (direction == North) && ((length positions) == 2) = if ((fst (head positions)) == (fst (head (tail positions)))) then (map (\ pos -> (((fst pos) - 1),(snd pos))) positions)
									  	                                                      else [(((fst (head positions)) - 1), (snd (head positions)))]

									  	| (direction == North) && ((length positions) == 1) = [(((fst (head positions)) - 1), (snd (head positions))),  
									  														  (((fst (head positions)) - 2), (snd (head positions)))]

									  	| (direction == South) && ((length positions) == 2) = if ((fst (head positions)) == (fst (head (tail positions)))) then map (\ pos -> (((fst pos) + 1),(snd pos))) positions
									  														  else [(((fst (head positions)) + 2), (snd (head positions)))]

									  	| (direction == South) && ((length positions) == 1) = (((fst (head positions)) +1), (snd (head positions)))
									  														: [(((fst (head positions)) +2), (snd (head positions)))]

									  	| (direction == East) && ((length positions) == 2) = if ((snd (head positions)) == (snd (head (tail positions)))) then (map (\ pos -> ((fst pos),((snd pos) + 1))) positions)
									  														else [((fst (head (tail positions))), ((snd (head (tail positions))) + 1))]

									  	| (direction == East) && ((length positions) == 1) = ((fst (head positions)), ((snd (head positions)) + 1))
									  														: [((fst (head positions)), ((snd (head positions)) + 2))]

									  	| (direction == West) && ((length positions) == 2) = if ((snd (head positions)) == (snd (head (tail positions)))) then (map (\ pos -> ((fst pos),((snd pos) - 1))) positions)
									  		                                                  else [((fst (head positions)), ((snd (head positions)) - 1))]

									  	| (direction == West) && ((length positions) == 1) = ((fst (head positions)), ((snd (head positions)) - 2))
									  														: [((fst (head positions)), ((snd (head positions)) - 1))]
									  	| otherwise = positions

putBlockAtPos cells pos = map (\ cell -> if (condition cell pos) then (Cell (tile cell) (position cell) (isSwitch cell) True (positions cell))
										else cell) cells
removeBlockAtPos cells pos = map (\ cell -> if (condition cell pos) then (Cell (tile cell) (position cell) (isSwitch cell) False (positions cell))
										else cell) cells

checkIfActivate l@(Level mat blPos m n) | length blPos == 2 = 
											if(tile (head (drop ((snd (head blPos))) (mat A.! ((fst (head blPos)))))) == switch)
											then (activate (head (drop ((snd (head blPos))) (mat A.! (fst (head blPos))))) l)
											else if (tile (head (drop ((snd (head (tail blPos)))) (mat A.! (fst (head (tail blPos)))))) == switch)
												then (activate  (head (drop ((snd (head (tail blPos)))) (mat A.! (fst (head (tail blPos)))))) l)
												else l
										| length blPos == 1 = 
											if(tile (head (drop ((snd (head blPos))) (mat A.! (fst (head blPos))))) == switch)
											then (activate (head (drop ((snd (head blPos))) (mat A.! (fst (head blPos))))) l)
											else l
										| otherwise = l
move :: Directions -> Level -> Level
move direction l@(Level mat blPos m n) | continueGame l == False = l
											
									   |otherwise =
										 if ((length d) == 2) then checkIfActivate (Level
											(updatedMat1 A.// [(i, (putBlockAtPos (updatedMat1 A.! i) pos)) | i <- [(fst (head (tail d)))], pos <- [(head (tail d))]])
											d m n)
									 	else checkIfActivate (Level updatedMat1 d m n)
									 
									where 
									d = directionToPosition direction blPos
									newMatrix | length blPos == 1 = (mat A.// [(i, (removeBlockAtPos (mat A.! i) atPos)) | i <- (map fst blPos), atPos <- blPos])
										   	  | length blPos == 2 = (updated A.// [(i, (removeBlockAtPos (updated A.! i) atPos)) |
										   						 i <- [fst (head (tail blPos))], atPos <- [head (tail blPos)]])

										 	  				where
										   	  				updated = (mat A.// [(i, (removeBlockAtPos (mat A.! i) atPos)) | i <- [fst (head blPos)], atPos <- [head blPos]])

									updatedMat1 = (newMatrix A.// [(i, (putBlockAtPos (newMatrix A.! i) pos)) | i <- [fst (head d)], pos <- [head d]]) 
									

{-
    *** TODO ***

    Va returna True dacă jocul nu este nici câștigat, nici pierdut.
    Este folosită în cadrul Interactive.
-}

continueGame :: Level -> Bool
continueGame (Level mat blPos m n )	| ((length blPos) == 1) && ((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == softTile) = False
    								
                                	| ((length blPos) == 2) && (((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == winningTile)
    									|| ((tileAt (snd (head (tail blPos))) (mat A.! (fst (head (tail blPos))))) == winningTile)) = False
    								
                                	| ((length blPos) == 1) && ((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == winningTile) = False
    								
    								| ((length blPos) == 1) && ((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == emptySpace) = False
									
    								| ((length blPos) == 2) && (((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == emptySpace)
    									&& ((tileAt (snd (head (tail blPos))) (mat A.! (fst (head (tail blPos))))) == emptySpace)) = False
    								
    								| otherwise = True

{-
    *** TODO ***

    Instanțiați clasa `ProblemState` pentru jocul nostru. 
  
    Hint: Un level câștigat nu are succesori! 
    De asemenea, puteți ignora succesorii care 
    duc la pierderea unui level.
-}

instance ProblemState Level Directions where
    successors l | isGoal l = []
		         |otherwise = filter (\ x -> continueGame (snd x) || isGoal (snd x)) [(North, move North l), (South, move South l), (East, move East l), (West, move West l)]

    isGoal (Level mat blPos m n) | ((length blPos) == 2) && (((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == winningTile)
    									|| ((tileAt (snd (head (tail blPos))) (mat A.! (fst (head (tail blPos))))) == winningTile)) = True
    								
                                | ((length blPos) == 1) && ((tileAt (snd (head blPos)) (mat A.! (fst (head blPos)))) == winningTile) = True
                                | otherwise = False

    -- Doar petru BONUS

    heuristic level = (abs ((fst best) - (fst w))) + (abs ((snd best) - (snd w))) - 1
                        where
                        b = (blockPos level)
                        w = position (head (winPos (A.elems (matrix level))))
                        best | length b == 2 = if (abs ((fst (head b)) - (fst w))) > (abs ((fst (head (tail b))) - (fst w))) then head (tail b)
                                             else if (abs ((snd (head b)) - (snd w))) > (abs ((snd (head (tail b))) - (snd w))) then head (tail b)
                                             else head b
                             | otherwise = head b
                        winPos list = head (filter (\y -> (filter (\x -> (tile x) == winningTile) y) /= []) list)
