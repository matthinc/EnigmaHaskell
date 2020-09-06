module Config (parseConfig, Rotor, alphabet, ukwB) where

import Text.ParserCombinators.Parsec

type Rotor = ([Char], Int)

--------------- Constants ---------------              

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

--------------- Config parsing ---------------

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
