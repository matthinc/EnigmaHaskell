#!/usr/bin/env runghc

-- Programmed after watching "The Immitation Game"
-- Just to clarify: I HATE Nazis but I like cryptography

import Data.Maybe
import Data.List
import Data.Sequence (empty, insertAt)
import Data.Foldable (toList)
import Data.Char (toUpper)
import Data.List.Split (chunksOf)
import System.Environment (getArgs)
import Text.ParserCombinators.Parsec

import Config (parseConfig, Rotor, ukwB, alphabet)

-- | Returns position of letter in a char-array
findPositionOfLetterInList :: Char -> [Char] -> Int
findPositionOfLetterInList letter arr = fromMaybe (-1) $ findIndex (== letter) arr

-- | Reverses a 'rotor' permutation
-- Example: Input: a -> c, b -> d
--          Output: c -> a, d -> b
-- Or written as array [c, d, ..., ...] --> [..., ..., a, d]
-- Algorithm:
-- 1) if position is 0, search for alphabet[0] == a in rotor
-- 2) Take this position and lookup letter at this position in the alphabet
-- 3) Insert this letter at position 0 in the new rotor
-- Execute the steps 1-3 for every index [0..(length alphabet) - 1]
-- Start with an empty sequence
reverseRotor :: [Char] -> [Char]
reverseRotor r = toList $ foldl processPosition empty [0..length alphabet - 1]
  where processPosition newRotor position = let posOfLetter = findPositionOfLetterInList (alphabet !! position) r
                                                alphaLetterAtPos = alphabet !! posOfLetter in
                                              insertAt position alphaLetterAtPos newRotor
-- | Apply rotor permutation to a character                          
applyRotor :: Char -> [Char] -> Char
applyRotor letter rotor = let index =  findPositionOfLetterInList letter alphabet in
                            rotor !! index

-- | Apply multiple rotor permutations to a character
applyRotors :: Char -> [[Char]] -> Char
applyRotors letter rotors = foldl applyRotor letter rotors

-- | 'Rotate' rotor
rotateRotor :: Int -> [Char] -> [Char]
rotateRotor amount rotor = shiftOutput $ take (length rotor) $ drop realAmount $ cycle rotor
  where realAmount = amount `mod` 26 -- allows negative amounts (useful when working with rings > rotation)
        shiftOutput r = map shiftLetter r
        shiftLetter l = (cycle alphabet) !! ((findPositionOfLetterInList l alphabet) + (26 - realAmount))

-- | [1, 1, 2] -> [0, 0, 1]
startWith1 :: [Int] -> [Int]
startWith1 arr = zipWith (-) arr (cycle [1])

-- | Simulates the whole enigma rotor block
applyRotorBlock :: Char     -- ^ The input letter
                -> [[Char]] -- ^ The rotors
                -> [Int]    -- ^ The rotation
                -> [Char]   -- ^ The UKW rotor
                -> Char     -- ^ The encrypted result
applyRotorBlock letter rotors rotations ukw = (applyBackward . applyUkw . applyForward) letter
  where modifiedRotors = zipWith rotateRotor rotations rotors
        applyForward letter = applyRotors letter modifiedRotors 
        applyUkw letter = applyRotor letter ukw
        applyBackward letter = applyRotors letter $ reverse (map reverseRotor modifiedRotors)

-- | Encrypt text in the rotor block
encryptTextInRotorBlock :: [Char] -- ^ The input text
            -> [Rotor]            -- ^ The rotors
            -> [Int]              -- ^ The initial position
            -> [Int]              -- ^ The rings
            -> [Char]             -- ^ The UKW rotor
            -> [Char]             -- ^ The encrypted result
encryptTextInRotorBlock text rotors initial rings ukw = zipWith encryptNextLetter preparedText [1..length text]
  where encryptNextLetter letter n = applyRotorBlock letter rotorEncodings (calculateRotorPositions n) ukw
        preparedText = filter (\a -> elem a ['A'..'Z']) $ map toUpper text
        rotorEncodings = map fst rotors
        initialRotorPositions = map (\a -> a `mod` 26) $ zipWith (-) initial rings
        rotorPositionChange rotorPos index
          | (visibleRings rotorPos) !! 1  == (snd (rotors !! 1)) = [1, 1, 1]
          | (visibleRings rotorPos) !! 0  == (snd (rotors !! 0)) = [1, 1, 0]
          | otherwise                                            = [1, 0, 0]
        visibleRings rotorPos = map (\a -> a `mod` 26) $ zipWith (+) rings (zipWith (-) rotorPos initial)
        calculateRotorPositions max = foldl applyPositionChangeToPosition initialRotorPositions [1..max]
        applyPositionChangeToPosition position index = zipWith (+) position (rotorPositionChange position index)

-- | Apply plugboard permutatios to a text
plugboard :: [Char] -- ^ Input text
          -> [(Char, Char)] -- ^ Plugboard settings
          -> [Char] -- ^ Output text
plugboard text permutations = map applyAllPermutations text
  where applyAllPermutations char = foldl applyPermutation char permutations
        applyPermutation char perm
          | fst perm == char = snd perm
          | snd perm == char = fst perm
          | otherwise        = char

-- | Runs the enigma machine for a given text
runEnigma :: [Char]            -- ^ The input text
          -> [Rotor]           -- ^ The rotors
          -> [Int]             -- ^ The initial position
          -> [Int]             -- ^ The rings
          -> [Char]            -- ^ The UKW rotor
          -> [(Char, Char)]    -- ^ Plugboard setting
          -> [Char]            -- ^ The encrypted result
runEnigma text rotors initial rings ukw plugboardPerm = plugboard outputFromRotors plugboardPerm
  where outputFromRotors = encryptTextInRotorBlock pbText (reverse $ rotors) (reverse $ startWith1 initial) (reverse $ startWith1 rings) ukw
        pbText = plugboard text plugboardPerm

-- | ABCDEFGHIJKL -> ABCDE FGHIJ KL
formatOutput :: [Char] -> [Char]
formatOutput text = take (length partitionedResult - 1) partitionedResult
  where partitionedResult = foldl (\a b -> a ++ b ++ " ") "" $ chunksOf 5 text

main :: IO ()
main = do
  args <- getArgs

  let (rotors, grundstellung, rings, plugboard) = parsedArgs args in
    putStrLn $ show $ formatOutput $ runEnigma (args !! 1) rotors grundstellung rings ukwB plugboard

  where parsedArgs args = case parse parseConfig "" (args !! 0) of
          Right arg -> arg
          Left err  -> error $ show err
