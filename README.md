# Haskell Enigma Simulator

Simulates an _Enigma-I_ machine in Haskell.

I developed this after watching the great movie _"The Imitation Game"_ to gain a deeper knowledge of how the _Enigma-I_ machine worked.
This project is **NOT** meant to glorify Nazi-technology.

> The Enigma machine is an encryption device developed and used in the early- to mid-20th century to protect commercial, diplomatic and military communication.
> It was employed extensively by Nazi Germany during World War II, in all branches of the German military. 
> <cite>https://en.wikipedia.org/wiki/Enigma_machine</cite>

## Usage

```
./Enigma.hs "[Rotors] [Grundstellung] [Rings] [Plugboard]" "Text_to_encrypt/decrypt"
```

Example:

From [https://de.wikipedia.org/wiki/Enigma_(Maschine)](https://de.wikipedia.org/wiki/Enigma_(Maschine))

```bash
./Enigma.hs "I,IV,III 18,20,26 16,26,8 AD CN ET FL GI JV KZ PU QY WX" \
"DASOBERKOMMANDODERWEHRMAQTGIBTBEKANNTXAACHENXAACHENXISTGERETTET"

> "LJPQH SVDWC LYXZQ FXHIU VWDJO BJNZX RCWEO TVNJC IONTF QNSXW ISXKH JDAGD JVA"

./Enigma.hs "I,IV,III 18,20,26 16,26,8 AD CN ET FL GI JV KZ PU QY WX" \
"LJPQH SVDWC LYXZQ FXHIU VWDJO BJNZX RCWEO TVNJC IONTF QNSXW ISXKH JDAGD JVA" 

> "DASOB ERKOM MANDO DERWE HRMAQ TGIBT BEKAN NTXAA CHENX AACHE NXIST GERET TET"
```



