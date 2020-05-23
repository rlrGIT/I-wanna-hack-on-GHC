{-
Russell Rivera
File: jLex.hs
Desc: A Java Lexer
-}

module Main where

import Data.Char
import System.Environment
import System.Exit
import System.FilePath

main :: IO()
main = do
 args <- getArgs -- returns a list of command line arguments
 filename <- checkArgs args -- generates a monadic FilePath (type of string) with names that are OS dependent.
 input <- readFile filename -- read the file of names, bind to a variable.
 let result = lexJava input -- set result to the value(s) produced by lexJava
 writeFile (takeBaseName filename <.> "lex") (unlines result)

 -- Checks the command-line arguments.
 -- Returns the filename to lex upon success.
checkArgs :: [String] -> IO FilePath
checkArgs [path] = pure path
checkArgs _other = do
 putStrLn "Usage: ./Lexer <filename>.java"
 putStrLn "Writes to <filename>.lex"
 exitFailure

-- Takes Java code as input and returns a list of Strings.
-- Each string in the output list is one Java token.
-- Comments and whitespace are discarded.
lexJava :: String -> [String]
lexJava s = lexNext (findToken s)

lexNext :: String -> [String]
lexNext ""     = []
lexNext (x:xs) = [firstToken] ++ lexJava restOfTokens
 where
   lexed = lexSingle x xs
   firstToken = fst lexed
   restOfTokens = snd lexed

-- assemble the largest token possible
lexSingle :: Char -> String -> (String, String)
lexSingle char str
 | validFirst char = buildIdentifier char str
 | otherwise       = ("", (char:str))

 -- findToken and comment helper functions
findToken :: String -> String -- remove comments and white spaces from a string
findToken "" = ""
findToken (s:sx)
  | isSpace s = findToken sx
  | otherwise = rComment (s:sx)

-- identify and create the longest identifier possible
validFirst :: Char -> Bool
validFirst c = isAlpha c
        || c == '_'
        || c == '$'

validNext :: Char -> Bool
validNext c = isAlpha c
       || isDigit c
       || c == '_'
       || c == '$'

buildIdentifier :: Char -> String -> (String, String)
buildIdentifier '\n' "\n" = ("", "")
buildIdentifier x xs
 | validFirst x = buildIdHelper (x:xs)
 | otherwise    = ("", (x:xs))

buildIdHelper :: String -> (String, String)
buildIdHelper "\n" = ("", "")
buildIdHelper [x]
 | validNext x = ([x], "") -- can probably get rid of some of this
 | otherwise = ("", [x])
buildIdHelper (x:xs)
 | validNext x = (x:a, b)
 | otherwise = ("", (x:xs))
    where
       (a, b) = buildIdHelper xs -- format output for recursion

-- remove comments, newlines
rComment :: String -> String
rComment "" = ""
rComment [x]
 | x == '\n' = ""
 | otherwise = [x]
rComment (x:xs)
 | x == '\n' = rComment xs
rComment (x:y:xs)
 | (x:y:xs) == ('/':'/':xs) = waitNewLine xs
 | (x:y:xs) == ('/':'*':xs) = waitComment xs
 | (x:y:xs) == (x:y:"\n")   = (x:[y])
 | otherwise =  x: rComment (y:xs)

-- return text after a new line if any
waitNewLine :: String -> String
waitNewLine "" = ""
waitNewLine ('\n':xs) = xs
waitNewLine (_:xs) = waitNewLine xs

-- find the close comment token
waitComment :: String -> String
waitComment "" = ""
waitComment ('*':'/':xs) = xs
waitComment (_:xs) = waitComment xs

lexSepOrOp :: String -> (String, String) -- lex the first operator or separator
lexSepOrOp ""  = ("", "")
lexSepOrOp (w:x:y:z:rest)
 | (w:x:y:[z]) == ">>>=" = (">>>=", rest)
 | otherwise = ("", (w:x:y:z:rest))
lexSepOrOp (x:y:z:rest)
 | (x:y:[z]) == "<<<" = ("<<<", rest)
 | (x:y:[z]) == ">>>" = (">>>", rest)
 | (x:y:[z]) == "..." = ("...", rest)
 | (x:y:[z]) == ">>=" = (">>=", rest)
 | (x:y:[z]) == "<<=" = ("<<=", rest)
 | otherwise = ("", (x:y:z:rest))
lexSepOrOp (x:y:zs)
 | (x:[y]) == "->" = ("->", zs)
 | (x:[y]) == "==" = ("==", zs)
 | (x:[y]) == ">=" = (">=", zs)
 | (x:[y]) == "<=" = ("<=", zs)
 | (x:[y]) == "!=" = ("!=", zs)
 | (x:[y]) == "&&" = ("&&", zs)
 | (x:[y]) == "||" = ("||", zs)
 | (x:[y]) == "++" = ("++", zs)
 | (x:[y]) == "--" = ("--", zs)
 | (x:[y]) == "+=" = ("+=", zs)
 | (x:[y]) == "-=" = ("-=", zs)
 | (x:[y]) == "*=" = ("*=", zs)
 | (x:[y]) == "/=" = ("/=", zs)
 | (x:[y]) == "&=" = ("&=", zs)
 | (x:[y]) == "|=" = ("|=", zs)
 | (x:[y]) == "^=" = ("^=", zs)
 | (x:[y]) == "%=" = ("%=", zs)
 | otherwise = ("", (x:y:zs))
lexSepOrOp (x:xs)
 | [x] == ":" = (";", xs)
 | [x] == "~" = ("~", xs)
 | [x] == "?" = ("?", xs)
 | [x] == "(" = ("(", xs)
 | [x] == ")" = (")", xs)
 | [x] == "{" = ("{", xs)
 | [x] == "}" = ("}", xs)
 | [x] == "[" = ("[", xs)
 | [x] == "]" = ("]", xs)
 | [x] == ";" = (";", xs)
 | [x] == "," = (",", xs)
 | [x] == "." = (".", xs)
 | [x] == "@" = ("@", xs)
 | [x] == "+" = ("+", xs)
 | [x] == "-" = ("-", xs)
 | [x] == "*" = ("*", xs)
 | [x] == "/" = ("/", xs)
 | [x] == "&" = ("&", xs)
 | [x] == "%" = ("%", xs)
 | [x] == "^" = ("^", xs)
 | [x] == "!" = ("!", xs)
 | [x] == "=" = ("=", xs)
 | [x] == ">" = (">", xs)
 | [x] == "<" = ("<", xs)
 | [x] == "|" = ("|", xs)
 | otherwise = ("", (x:xs))
