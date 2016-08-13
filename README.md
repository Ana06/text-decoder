# Text decoder

A text decoder written in Haskell. Developed in May 2015. Comments are in Spanish.

The coding used is a biyection between characters in a way that each character of the original text has been replaced by other. The range os characters is `A..Z`, `a..z`, dots, commas, spaces and line breaks. There are not accents, numbers nor `ñ`. The coding change capital letters by capital letters and lower case letters by lower case letters, being consistent: if `a` is changed by `d` then `A` is changed by `D` and vice versa. The coding does not change dots, commas, spaces nor line breaks. The text to be decoded is in the file [texto.txt](https://github.com/Ana06/text-decoder/blob/master/texto.txt), although any other file that follows the previous rules can be used.

[frecuencias.txt](https://github.com/Ana06/text-decoder/blob/master/frecuencias.txt) and [quijote.txt](https://github.com/Ana06/text-decoder/blob/master/quijote.txt) are auxiliary files to get the frecuency of the words. [prAna.hs](https://github.com/Ana06/text-decoder/blob/master/prAna.hs) is the text decoder itself. Lastly, [textoDescifrado.txt](https://github.com/Ana06/text-decoder/blob/master/textoDescifrado.txt) is the decoded text got from [texto.txt](https://github.com/Ana06/text-decoder/blob/master/texto.txt) using the program.

## Authors

This project was developed by Ana María Martínez Gómez.


## Licence

Code published under MIT License (see [LICENSE](LICENSE)).

