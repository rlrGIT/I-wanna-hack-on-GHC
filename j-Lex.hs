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
lexJava s = [s]

lexSingle :: Char -> String -> (String, String)
lexSingle char str = ([char], str)

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


-- remove whitespace and comments from a string
ignoreWS :: String -> String
ignoreWS "\n" = ""
ignoreWS [x]  = [x]

ignoreWS (x:y:"\n")   = (x:[y])
ignoreWS ('/':'/':xs) = waitNewLine xs
ignoreWS ('/':'*':xs) = waitComment xs

ignoreWS (x:y:zs)
 | isSpace x = ignoreWS (y:zs)
 | otherwise = x: ignoreWS (y:zs)


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
lexSepOrOp [x] = ([x], "")



-- getSepOrOp str
--  | isQuad firstFour    = (firstFour, drop 4 str)
--  | isTriple firstThree = (firstThree, drop 3 str)
--  | isDouble firstTwo   = (firstTwo, drop 2 str)
--  | isSingle first      = (first, tail str)
--  | otherwise           = ("", str)
--   where
--    firstFour  = take 4 str
--    firstThree = take 3 str 
--    firstTwo   = take 2 str
--    first      = take 1 str

-- -- determine whether the string has an operator
-- isSingle :: String -> Bool
-- isSingle str = str `elem` [":", "~", "?", "(", ")", "{", "}", "[", "]", ";", ",", ".", "@", "+", "-", "*", "/", "&", "%", "^", "!", "=", ">", "<", "|", ":" ]

-- isDouble :: String -> Bool
-- isDouble str = str `elem` ["->", "==", ">=", "<=", "!=", "&&", "||", "++", "--", "+=", "-=", "*=", "/=", "&=", "|=", "^=", "%=", "<<", ">>"]

-- isTriple :: String -> Bool
-- isTriple str = str `elem` ["<<<", ">>>", "...", ">>=", "<<="]

-- isQuad :: String -> Bool
-- isQuad str = str == ">>>="






















