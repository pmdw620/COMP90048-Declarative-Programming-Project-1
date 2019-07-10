-- File       : Proj1.hs
-- Author     : Ming Pan
-- Start Date : 28/03/2019
-- Purpose    : COMP90048 Declarative Programming Project 1 submission

-- Last Modified Date: 03/04/2019
module Proj1 (Pitch, toPitch, feedback,
    GameState, initialGuess, nextGuess) where
import Data.Char
import Data.List

-- A Pitch is composed by a Note (Char) and a Octave (Int)
-- The GameState coomposed by the different guesses (3-pitches chords)
-- The  first  guess  was concluded by numouros practice and it  can be 
-- considered as the 'best' first guess
data Pitch     = Pitch { note :: Char, octave :: Int }deriving (Eq)
type GameState = [[Pitch]]

-- Define the consts
-- allPitches is the collection of all the valid pitches for gaming
-- allChords is the list of all the subsets of allPitches with length==3
-- firstGuess is conducted by numerous tests on the server and performs
--   the best (lowest average guess)
allPitches :: [Pitch]
allPitches = [Pitch note octave | note <- ['A'..'G'], 
              octave <- [1..3]]
allChords  :: [[Pitch]]
allChords  = [chord | chord <- subsequences allPitches, 
              length chord == 3]
firstGuess :: [Pitch]
firstGuess = [Pitch 'A' 1, Pitch 'B' 2, Pitch 'C' 2]


-- This is the function that takes String args and determine  whether it 
-- is a valid input for generating a Pitch since a Pitch can be represe-
-- nted by 2-character String, the note of a Pitch ranged 'A' to 'G' and
-- octave ranged 1 to 3.
toPitch :: String -> Maybe Pitch
toPitch str 
    | strlen == 2 && (note >= 'A' && note <= 'G') 
           && (octave >= 1 && octave <= 3) = Just (Pitch note octave)
    | otherwise                            = Nothing
    where 
        strlen = length str
        note   = head str
        octave = digitToInt (str!!1)

-- toString function that convert a pitch to a coresponded string
toString :: Pitch -> String
toString pitch = [note pitch] ++ [(intToDigit (octave pitch))] 

-- Instance declaration for Pitch in the Show class
instance Show Pitch where
    show pitch = toString pitch

-- The feedback function takes the guess that provided by the  composer 
-- and generate  the  result  of  correctness. The  rules  are shown as 
-- follows:
-- 1. how many pitches in the guess are included in the target (correct
--    pitches).
-- 2. how many pitches have the right note but the wrong octave (correct
--    notes). Fomula: cn = |cp - sn|
-- 3. how many pitches have the right octave but the wrong note (correct
--    octaves) Fomula: co = |cp - so|
feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess  = (nCorrPitches, nCorrNotes, nCorrOctaves)
    where nCorrPitches = length (intersect target guess)
          sameNotes    = findSame (eNotes target) (eNotes guess)
          sameOctaves  = findSame (eOctaves target) (eOctaves guess)
          nCorrNotes   = absMinus sameNotes nCorrPitches
          nCorrOctaves = absMinus sameOctaves nCorrPitches

-- eNotes is the  function that extract the  notes  of given pitches  and
-- combine them to a char list 
eNotes :: [Pitch] -> [Char]
eNotes []     = []
eNotes (x:xs) = (note x) : eNotes xs

-- eOctaves is the  function that extract the notes of given pitches  and 
-- combine them to a integer list 
eOctaves :: [Pitch] -> [Int]
eOctaves []     = []
eOctaves (x:xs) = (octave x) : eOctaves xs

-- find the number of same elements of 2 given lists
findSame :: Eq a =>[a] ->  [a] -> Int
findSame [] e      = 0
findSame (x:xs) ys = if x `elem` ys
                     then 1 + findSame xs (delete x ys)
                     else
                        findSame xs ys

-- absolute result value from minus
absMinus :: Int -> Int -> Int
absMinus a b
    | a - b <= 0 = b - a
    | otherwise  = a - b 

-- initialGuess fuction that take all the subsets for 3-pitch chords, and
-- remove the first guess from  the  game  state.  
initialGuess :: ([Pitch], GameState)
initialGuess      = (guess, gameState)
    where
        guess     = firstGuess
        gameState = delete guess allChords

-- nextGuess function test all the candidates left in the game state with
-- the last guess made and save only the ones which is consistent to  the
-- correct answer. A possible target is inconsistent  with an answer  you 
-- have received for a previous guess if the answer you would receive for 
-- that guess and that (possible) target is different from the answer you 
-- actually received  for  that  guess.  Once  the   game  state has been 
-- updated, use the information to get the best option for next guess
nextGuess :: ([Pitch],GameState) -> (Int,Int,Int) -> ([Pitch],GameState)
nextGuess (lastGuess, gameState) score = (nextGuess, newState)
    where
        sameScoreCands = [target | target <- gameState, 
                          feedback target lastGuess == score]
        newState       = delete lastGuess sameScoreCands
        nextGuess      = bestGuess newState

-- The bestGuess function return the best guess (the possible target that
-- will leave the  maximum  number  of  possible  targets if we guess it) 
-- which is the first element (with the greatest number) of  the  sorted
-- tuple list [(possTarget, numOfTarLeft)].
bestGuess :: GameState -> [Pitch]
bestGuess gameState     = fst (head sortedTup)
    where
        tupNumOfTarLeft = getTup gameState gameState
        sortedTup       = sortBy sortRules tupNumOfTarLeft

-- This is the function that customized the ordering rules. The  function 
-- compares  the  second element of a tuple and decide the order. In this
-- case, it is the possible target left. The greater the  number is,  the
-- more chance that target is the actual target.
-- This can be explained as:
--   totalTarget = scoreCase1 * m(1) + scoreCase2 * m(2) + ..
--               + scoreCasen * m(n)
--   1. totalTarget is the total possible target in a game state (const),
--   2. scoreCase1 to scoreCasen are different score types  from  testing
--   3. the m(1) to m(n) are the numbers those relate to their score type
--      respectively.
--   let's suppose scoreCase m is the correct answer.Then we can conclude 
--   that the more scoreCases, the smaller m (x) from probability study.
sortRules :: (Ord b) => (a, b) -> (a, b) -> Ordering
sortRules x y
    | (snd x) < (snd y)   = GT
    | otherwise           = LT

-- The getTup returns a tuple  composed by  2 elements. The first element
-- represents the possible target and the second  part shows  the   total
-- number of score types leave if we guess with it.
-- The process is described as follows:
-- 1. Get the possible target (possTar).
-- 2. Test each other chord (in the game state) with that possible target 
--    (possTar) and store the score into a score list.
-- 3. Sort the score list so that we can group the same result.
-- 4. The length of the groupted list is the number of total different 
--    score types of which corresponding to its' target  (possTar)
getTup :: [[Pitch]] ->  GameState -> [([Pitch], Int)]
getTup [] _             = []
getTup (x:xs) gameState = (x, scoreTypes) : getTup xs gameState
    where 
        newState        = delete x gameState
        scoreTypes      = length (group (sort [score | guess <- newState,
                          let score = feedback x guess]))

