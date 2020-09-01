#!/usr/bin/env runghc

-- Programmed after watching "The Immitation Game"
-- Just to clarify: I HATE Nazis but I like cryptography

import Data.Maybe
import Data.List
import Data.Sequence (empty, insertAt)
import Data.Foldable (toList)
import Data.Char (toUpper)

-- | Just the alphabet (Uppercase only)
alphabet :: [Char]
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- | Rotor 1
rotorI     :: ([Char], Int) -- ^ Encoding and turnover position
rotorI =   ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", 17)

-- | Rotor 2
rotorII    :: ([Char], Int) -- ^ Encoding and turnover position
rotorII =  ("AJDKSIRUXBLHWTMCQGZNPYFVOE", 5)

-- | Rotor 3
rotorIII   :: ([Char], Int) -- ^ Encoding and turnover position
rotorIII = ("BDFHJLCPRTXVZNYEIWGAKMUSQO", 22)

-- | Rotor 4
rotorIV    :: ([Char], Int) -- ^ Encoding and turnover position
rotorIV =  ("ESOVPZJAYQUIRHXLNFTGKDCMWB", 10)

-- | Rotor 5
rotorV     :: ([Char], Int) -- ^ Encoding and turnover position
rotorV =   ("VZBRGITYUPSDNHLXAWMJQOFECK", 26)

-- | UkwB "Umkehrwalze B" (reflector)
ukwB :: [Char] -- ^ Encoding (symmetric)
ukwB = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

-- | Returns position of letter in a char-array
findPositionOfLetterInList :: Char -> [Char] -> Int
findPositionOfLetterInList letter arr = fromMaybe (-1) (findIndex (\a -> a == letter) arr)

-- | Reverses a 'rotor' permutation
-- Example: Input: a -> c, b -> d
--          Output: c -> a, d -> b
-- Or written as array [c, d, ..., ...] --> [..., ..., a, d]
-- Algorithm:
-- 1) if position is 0, search for alphabet[0] == a in rotor
-- 2) Take this position and lookup letter at this position in the alphabet
-- 3) Insert this letter at position 0 in the new rotor
-- Execute the steps 1-3 for every index [0..((length alphabet) - 1)]
-- Start with an empty sequence
reverseRotor :: [Char] -> [Char]
reverseRotor r = toList $ foldl processPosition empty [0..((length alphabet) - 1)]
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
rotateRotor amount rotor = shiftOutput $ take (length rotor) $ drop amount $ cycle rotor
  where shiftOutput r = map shiftLetter r
        shiftLetter l = (cycle alphabet) !! ((findPositionOfLetterInList l alphabet) + (26 - amount))

-- | Simulates the whole enigma rotor block
applyRotorBlock :: Char     -- ^ The input letter
                -> [[Char]] -- ^ The rotors
                -> [Int]    -- ^ The rotation
                -> [Int]    -- ^ The ring position
                -> [Char]   -- ^ The UKW rotor
                -> Char     -- ^ The encrypted result
applyRotorBlock letter rotors rotations rings ukw = (applyBackward . applyUkw . applyForward) letter
  where modifiedRotors= zipWith (\a b -> rotateRotor b a) rotors (zipWith (+) rotations rings)
        applyForward letter = applyRotors letter modifiedRotors 
        applyUkw letter = applyRotor letter ukw
        applyBackward letter = applyRotors letter $ reverse (map reverseRotor modifiedRotors)

-- | Encrypt text in the rotor block
encryptTextInRotorBlock :: [Char]          -- ^ The input text
            -> [([Char], Int)] -- ^ The rotors
            -> [Int]           -- ^ The ring position
            -> [Char]          -- ^ The UKW rotor
            -> [Char]          -- ^ The encrypted result
encryptTextInRotorBlock text rotors rings ukw = zipWith encryptNextLetter (prepareText text) [1..((length text))]
  where encryptNextLetter letter position = applyRotorBlock letter rotorEncodings (calculateRotorPositions position) rings ukw
        prepareText text = filter (\a -> elem a ['A'..'Z']) (map toUpper text)
        rotorEncodings = map fst rotors
        rotorPositionChange pos rotorPos
          | (rotorPos !! 1) == (snd (rotors !! 1)) = [1, 1, 1]
          | (rotorPos !! 0) == (snd (rotors !! 0)) = [1, 1, 0]
          | otherwise                              = [1, 0, 0]
        calculateRotorPositions max = foldl (\a b -> zipWith (+) a (rotorPositionChange b a)) [0, 0, 0] [1..max]

main :: IO ()
main = putStrLn $ show $ encryptedText
  where encryptedText = encryptTextInRotorBlock plaintext rotorConfig ringConfig ukw
        plaintext = "THISISATEST"
        -- expected: ZPJJSVSPGBW
        rotorConfig = [rotorI, rotorII, rotorIII]
        ringConfig = [0, 0, 0]
        ukw = ukwB
