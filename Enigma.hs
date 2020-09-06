#!/usr/bin/env runghc

-- Programmed after watching "The Immitation Game"
-- Just to clarify: I HATE Nazis but I like cryptography

import Data.Maybe
import Data.List
import Data.Sequence (empty, insertAt)
import Data.Foldable (toList)
import Data.Char (toUpper)
import Data.List.Split (chunksOf)
import Text.ParserCombinators.Parsec
import System.Environment (getArgs)
import Data.Either
  
type Rotor = ([Char], Int)

-- | Just the alphabet (Uppercase only)
alphabet :: [Char]
alphabet = "ABCDEFGHIJKLMNOPQRSTUVWXYZ"

-- | Rotor 1
rotorI     :: Rotor -- ^ Encoding and turnover position
rotorI =   ("EKMFLGDQVZNTOWYHXUSPAIBRCJ", 17)

-- | Rotor 2
rotorII    :: Rotor -- ^ Encoding and turnover position
rotorII =  ("AJDKSIRUXBLHWTMCQGZNPYFVOE", 5)

-- | Rotor 3
rotorIII   :: Rotor -- ^ Encoding and turnover position
rotorIII = ("BDFHJLCPRTXVZNYEIWGAKMUSQO", 22)

-- | Rotor 4
rotorIV    :: Rotor -- ^ Encoding and turnover position
rotorIV =  ("ESOVPZJAYQUIRHXLNFTGKDCMWB", 10)

-- | Rotor 5
rotorV     :: Rotor -- ^ Encoding and turnover position
rotorV =   ("VZBRGITYUPSDNHLXAWMJQOFECK", 26)

-- | UkwB "Umkehrwalze B" (reflector)
ukwB :: [Char] -- ^ Encoding (symmetric)
ukwB = "YRUHQSLDPXNGOKMIEBFZCWVJAT"

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

--------------- Argument parsing ---------------

parseRotors :: Parser [Rotor]
parseRotors = do
  rotor1 <- rotor
  char ','
  rotor2 <- rotor
  char ','
  rotor3 <- rotor
  return [rotor1, rotor2, rotor3]
  where rotor = (do
                  try (do
                    string "III"
                    pure rotorIII)
                  <|>
                  try (do
                    string "II"
                    pure rotorII)
                  <|>
                  try (do
                    string "IV"
                    pure rotorIV)
                  <|>
                  try (do
                    string "I"
                    pure rotorI)
                  <|>
                  try (do
                    string "V"
                    pure rotorV))

parseRingsAndGrundstellung :: Parser [Int]
parseRingsAndGrundstellung = do
  n1 <- number
  char ','
  n2 <- number
  char ','
  n3 <- number
  return [n1, n2, n3]
  where number = (do
                   value <- read <$> many1 digit
                   pure value)

parsePlugboard :: Parser [(Char, Char)]
parsePlugboard = do
  plugs <- many plug
  return plugs
  where plug = (do
                 c1 <- upper
                 c2 <- upper
                 spaces
                 pure (c1, c2))

parseConfig :: Parser ([Rotor], [Int], [Int], [(Char, Char)])
parseConfig = do
  rotors <- parseRotors
  spaces
  grundstellung <- parseRingsAndGrundstellung
  spaces
  rings <- parseRingsAndGrundstellung
  spaces
  plugboard <- parsePlugboard
  return (rotors, grundstellung, rings, plugboard)

--------------- Main ---------------

main :: IO ()
main = do
  args <- getArgs

  let (rotors, grundstellung, rings, plugboard) = parsedArgs args in
    putStrLn $ show $ formatOutput $ runEnigma (args !! 1) rotors grundstellung rings ukwB plugboard


  where parsedArgs args = case parse parseConfig "" (args !! 0) of
          Right arg -> arg
          Left err  -> error $ show err
  
-- main = putStrLn $ show $ formatOutput encryptedText
--   where encryptedText = runEnigma plaintext rotorConfig initialRotation rings ukw plubgoardConfig
--         -- Example taken from https://de.wikipedia.org/wiki/Enigma_(Maschine)#Bedienung
--         -- "Das Oberkommando der Wehrmacht gibt bekannt: Aachen ist gerettet."
--         plaintext = "DASOBERKOMMANDODERWEHRMAQTGIBTBEKANNTXAACHENXAACHENXISTGERETTET"
--         rotorConfig = [rotorI, rotorIV, rotorIII]
--         --  A  B  C  D  E  F  G  H  I  J  K  L  M  N  O  P  Q  R  S  T  U  V  W  X  Y  Z
--         -- 01 02 03 04 05 06 07 08 09 10 11 12 13 14 15 16 17 18 19 20 21 22 23 24 25 26
--         initialRotation = [18, 20, 26]
--         rings = [16, 26, 8]
--         ukw = ukwB
--         plubgoardConfig = [('A', 'D'), ('C', 'N'), ('E', 'T'), ('F', 'L'), ('G', 'I'), ('J', 'V'), ('K', 'Z'), ('P', 'U'), ('Q', 'Y'), ('W', 'X')]
