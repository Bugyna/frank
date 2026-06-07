module Token where

import Control.Exception
import Error
import Pos

alphabet :: [Char]
alphabet = ['a'..'z'] ++ ['A' .. 'Z']
numeric :: [Char]
numeric = ['0' .. '9']
alphanum :: [Char]
alphanum = alphabet ++ numeric :: [Char]
symbol_chars :: [Char]
symbol_chars = alphanum ++ "<>=+-*&^%$#@!/_~?"

symbol_starter_chars :: [Char]
symbol_starter_chars = alphabet ++ "<>=+-*&^%$#@!/_~?"

whitespace :: [Char]
whitespace = " \n\r\t"

separators :: [Char]
separators = whitespace ++ "()"


readLine :: String -> String
readLine = takeWhile (\c -> c /= '\n')




data LexUtilState = LexUtilState {token_curr_offset :: Int, token_start_offset :: Int, line_start_offset :: Int, start_pos :: Pos, curr_pos :: Pos, text :: String} deriving (Show)

lexAdvance :: LexUtilState -> Char -> LexUtilState
lexAdvance lexer@(LexUtilState _read _read_start _line_offset spos cpos _) c
  | c == '\n' = lexer {token_curr_offset=_read+1, token_start_offset=_read_start+1, line_start_offset=_read+1, start_pos=incrementLine(spos), curr_pos=incrementLine(cpos)}
  | otherwise = lexer {token_curr_offset=_read+1, token_start_offset=_read_start+1, start_pos=incrementColumn(spos), curr_pos=incrementColumn(cpos)}

lexAdvanceInToken :: LexUtilState -> Char -> LexUtilState
lexAdvanceInToken lexer@(LexUtilState _read _line_offset _ _ cpos _) c
  | c == '\n' = lexer {token_curr_offset=_read+1, line_start_offset=_read+1, curr_pos=incrementLine(cpos)}
  | otherwise = lexer {token_curr_offset=_read+1, curr_pos=incrementColumn(cpos)}

lexTokenAck :: LexUtilState -> LexUtilState
lexTokenAck lexer@(LexUtilState _read _read_start _line_offset _ cpos _) = 
  lexer {token_curr_offset=_read, token_start_offset=_read, start_pos=cpos}



data TokenKind = TNil | TSym | TNum | TStr | TLParen | TRParen deriving (Show, Eq)

data Token = Token {kind :: TokenKind, content :: String, start :: Pos, end :: Pos}
  deriving (Eq, Show)

showToken :: Token -> String
showToken (Token k t s e) = (show k) ++ " '" ++ t ++ "' (" ++ (show s) ++ " - " ++ show(e) ++ ")"


emptyToken :: Token
emptyToken = (Token TNil "" (Pos (-1) (-1)) (Pos (-1) (-1)))


createToken :: TokenKind -> String -> Pos -> Pos -> Token
createToken state text startPos endPos =
  (Token state text startPos endPos)


printTokens :: [Token] -> String
printTokens [] = ""
printTokens (h:[]) = showToken h
printTokens (h:t) = showToken h ++ ", " ++ printTokens t


