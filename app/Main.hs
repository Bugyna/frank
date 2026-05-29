module Main (main) where

import Control.Exception


data Pos = Pos {line :: Int, column :: Int} deriving (Eq, Ord)
instance Show Pos where
  show pos = show (line pos) ++ ":" ++ show (column pos)

incrementLine :: Pos -> Pos
incrementLine pos = pos {line = (line pos) + 1}
incrementColumn :: Pos -> Pos
incrementColumn pos = pos {column = (column pos) + 1}


readLine :: String -> String
readLine = takeWhile (\c -> c /= '\n')

data FrankError = FrankError String Pos String

instance Show FrankError where
  show (FrankError reason pos text) =
    let base_msg = (concat [reason, " ", show pos, "\n", text])
    in base_msg++"\n"++(replicate (column pos) ' ')++"^ here"


instance Exception FrankError


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

data Token =
    Nil Pos
  | Sym String Pos Pos
  | Num String Pos Pos
  | Str String Pos Pos
  | LParen Pos
  | RParen Pos
    deriving (Show, Eq)


emptyToken :: Token
emptyToken = Nil (Pos (-1) (-1))

createToken :: TokenKind -> String -> Pos -> Pos -> Token
createToken state text startPos endPos =
  constructor state text startPos endPos
  where
    constructor TSym = Sym
    constructor TNum = Num
    constructor TStr = Str
    constructor _    = throw (FrankError "Noo:" startPos "Trying to create invalid token type")



printTokens :: [Token] -> String
printTokens [] = ""
printTokens (h:[]) = show h
printTokens (h:t) = show h ++ " " ++ printTokens t


parseToken :: TokenKind -> [Char] -> LexUtilState -> [Char] -> [Token] -> [Token]
parseToken state constr lexer "" ret
  | state /= TNil = ret ++ [createToken state constr (start_pos lexer) (curr_pos lexer)]
  | otherwise    = ret

parseToken TNil constr lexer text@(peek:rest) ret
  | elem peek symbol_starter_chars = parseToken TSym constr lexer text ret
  | elem peek numeric              = parseToken TNum constr lexer text ret
  | peek == '"'                    = parseToken TStr constr lexer rest ret
  | peek == '('                    = parseToken TNil constr (lexAdvance lexer peek) rest (ret ++ [LParen (curr_pos lexer)])
  | peek == ')'                    = parseToken TNil constr (lexAdvance lexer peek) rest (ret ++ [RParen (curr_pos lexer)])
  | otherwise                      = parseToken TNil constr (lexAdvance lexer peek) rest ret


parseToken TStr constr lexer (peek:rest) ret
  | peek == '"'     = ret++[createToken TStr constr (start_pos lexer) (curr_pos lexer)]
  -- | peek == '\n'    = throw (FrankError "Unexpected EOL in string: " (start_pos lexer) constr)
  | otherwise       = parseToken TStr (constr++[peek]) (lexAdvanceInToken lexer peek) rest ret


parseToken state constr lexer@(LexUtilState curr_offset _ line_offset pos cpos orig_text) text@(peek:rest) ret
  | elem peek (matching_chars state) = parseToken state (constr++[peek]) (lexAdvanceInToken lexer peek) rest ret
  | elem peek separators             = parseToken TNil "" (lexTokenAck lexer) text (ret++[createToken state constr pos cpos])
  | otherwise                        = throw (FrankError "Invalid Char: " cpos (readLine $ drop line_offset orig_text))
  where matching_chars TSym   = symbol_chars
        matching_chars TNum   = numeric
        matching_chars _     = ""


eval :: [Token] -> Token
eval toks@(curr:rest) = 

main :: IO ()
main = do
  let text = "(let test-123 22a \"string test\")\n\
  \ (let another-expr (lambda (a b c) (body of sorts)))\n\
  \ " 
  putStrLn $ (printTokens (parseToken TNil "" (LexUtilState 0 0 0 (Pos 1 0) (Pos 1 0) text) text []))


