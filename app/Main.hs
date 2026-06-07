module Main (main) where

import Pos
import Token as T
import Error as Err
import Control.Exception


parseText :: TokenKind -> [Char] -> LexUtilState -> [Char] -> [Token] -> [Token]
parseText state constr lexer "" ret
  | state /= TNil = ret ++ [createToken state constr (start_pos lexer) (curr_pos lexer)]
  | otherwise    = ret

parseText TNil constr lexer text@(peek:rest) ret
  | elem peek symbol_starter_chars = parseText TSym constr lexer text ret
  | elem peek numeric              = parseText TNum constr lexer text ret
  | peek == '"'                    = parseText TStr constr lexer rest ret
  | peek == '('                    = parseText TNil constr (lexAdvance lexer peek) rest
      (ret ++ [Token TLParen "(" (curr_pos lexer) (curr_pos lexer)])
  | peek == ')'                    = parseText TNil constr (lexAdvance lexer peek) rest
      (ret ++ [Token TRParen ")" (curr_pos lexer) (curr_pos lexer)])
  | otherwise                      = parseText TNil constr (lexAdvance lexer peek) rest ret


parseText TStr constr lexer (peek:rest) ret
  | peek == '"'     = ret++[createToken TStr constr (start_pos lexer) (curr_pos lexer)]
  -- | peek == '\n'    = throw (FrankError "Unexpected EOL in string: " (start_pos lexer) constr)
  | otherwise       = parseText TStr (constr++[peek]) (lexAdvanceInToken lexer peek) rest ret


parseText state constr lexer@(LexUtilState curr_offset _ line_offset pos cpos orig_text) text@(peek:rest) ret
  | elem peek (matching_chars state) = parseText state (constr++[peek]) (lexAdvanceInToken lexer peek) rest ret
  | elem peek separators             = parseText TNil "" (lexTokenAck lexer) text (ret++[createToken state constr pos cpos])
  | otherwise                        = throw (FrankError "Invalid Char: " cpos (readLine $ drop line_offset orig_text))
  where matching_chars TSym   = symbol_chars
        matching_chars TNum   = numeric
        matching_chars _     = ""


-- data ParserState = ParserState {offset :: Int, prev :: Token, curr :: Token}

data ObjType =
    TAtom
  | TList
    deriving (Show)

data Obj =
    Nil    {               token :: Token}
  | Num    {num :: Int,    token :: Token}
  | Str    {str :: String, token :: Token}
  | Symbol {sym :: String, token :: Token}
  | Expr   {list :: [Obj]}
    deriving (Show)


car :: Obj -> Obj
car (Expr list) = head list
car o = throw (FrankError ("Invalid type! Expected Cons got Atom [" ++ (show o) ++ "]\n") (start (token o)) (show o))


parseTokens :: Obj -> [Token] -> Obj
parseTokens o [] = o

parseTokens (Main.Nil _) (peek:rest)
  | (kind peek) == TLParen = parseTokens (Expr []) rest
  | (kind peek) == TRParen = Main.Nil peek
  | (kind peek) == TStr    = Str (content peek) peek
  | (kind peek) == TSym    = Symbol (content peek) peek
  | (kind peek) == TNum    = Num (read (content peek):: Int) peek
  | otherwise = throw (FrankError "Trying to parse invalid state" (start peek) (content peek))


parseTokens o@(Expr e) (peek:rest)
  | (kind peek) == TLParen = (Expr $ e ++ [parseTokens (Expr []) rest])
  | (kind peek) == TRParen = o
  | (kind peek) == TStr    = parseTokens (Expr $ e ++ [Str (content peek) peek]) rest
  | (kind peek) == TSym    = parseTokens (Expr $ e ++ [Symbol (content peek) peek]) rest
  | (kind peek) == TNum    = parseTokens (Expr $ e ++ [Num (read (content peek):: Int) peek]) rest
  | otherwise = throw (FrankError "Trying to parse invalid state" (start peek) (content peek))


-- parseTokens (Num _ _) (peek:rest) = throw (FrankError "Trying to parse invalid state" (start peek) (content peek))
-- parseTokens (Str _ _) (peek:rest) = throw (FrankError "Trying to parse invalid state" (start peek) (content peek))
-- parseTokens (Symbol _ _) (peek:rest) = throw (FrankError "Trying to parse invalid state" (start peek) (content peek))
-- parseTokens (Expr _) (peek:rest) = throw (FrankError "Trying to parse invalid state" (start peek) (content peek))





main :: IO ()
main = do
  let text = "(let test-123 22 \"string test\")\n\
  \ (let another-expr (lambda (a b c) (body of sorts)))\n\
  \ " 
  let toks = (parseText TNil "" (LexUtilState 0 0 0 (Pos 1 0) (Pos 1 0) text) text [])
  putStrLn $ (printTokens toks)
  putStrLn "   "
  putStrLn $ (show $ parseTokens (Main.Nil (Token TNil "" (Pos 1 0) (Pos 1 0))) toks)


