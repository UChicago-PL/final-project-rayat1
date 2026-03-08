module Main where

import System.Environment
import Data.Char
import Text.Read

import qualified Data.Map as M

data Op
  = Plus
  | Minus
  | Times
  | Div

data AST
  = Var String
  | BinOp (Op, AST, AST)
  | Numb Integer

data Token
  = TVar String
  | TBinOp Op
  | TNum Integer

type Post = [Token]

type IToken = (Integer, Token) -- Mark the character of the token

type Post' = [IToken]

data Rep
  = Tree AST
  | List Post


data NotationType
  = NTPrefix
  | NTInfix
  | NTPostfix

type CompResult = Either String Integer
type ParseResult = Either String Rep
type ConstructResult = Either String String
type NTResult = Either String NotationType


constructPostfix :: Rep -> String
constructPostfix _ = ""

constructPrefix :: Rep -> String
constructPrefix _ = ""

constructInfix :: Rep -> String
constructInfix _ = ""

detectNotation :: String -> NTResult
detectNotation [] = Left "Cannot decipher what notation was attempted"
detectNotation (c : rest)
  | c == '(' = detectInOrPre rest
  | isDigit c = detectInOrPost rest False
  | otherwise = detectNotation rest

detectInOrPost :: String -> Bool -> NTResult
detectInOrPost [] _ = Left "Cannot decipher what notation was attempted"
detectInOrPost (c : rest) seenSpace
  | isDigit c = if seenSpace then Right NTPostfix else detectInOrPost rest False
  | isOp c = Right NTInfix
  | otherwise = detectInOrPost rest seenSpace

detectInOrPre :: String -> NTResult
detectInOrPre [] = Left "Cannot decipher what notation was attempted"
detectInOrPre (c : rest)
  | isOp c = Right NTPrefix
  | isDigit c = Right NTInfix
  | otherwise = detectInOrPre rest

isOp :: Char -> Bool
isOp c = c == '+' || c == '*' || c == '/' || c == '-'

parseFull :: String -> ParseResult
parseFull s =
  let
    ntype = detectNotation s
  in
    case ntype of
      Right NTPrefix -> Right $ Tree (parsePrefix s)
      Right NTInfix -> Right $ Tree (parseInfix s)
      Right NTPostfix -> Right $ List (parsePostfix s)
      Left err -> Left err

toTok :: String -> Token
toTok token =
  let
    intopt :: Maybe Integer
    intopt = readMaybe token
  in
    case intopt of
      Just n -> TNum n
      Nothing ->
        case token of
          ['+'] -> TBinOp Plus
          ['*'] -> TBinOp Times
          ['-'] -> TBinOp Minus
          ['/'] -> TBinOp Div
          _ -> TVar token

parsePostfix :: String -> Post
parsePostfix s =
  let
    tokenized = words s
  in
    map toTok tokenized

computePostfix :: Post -> [Integer] -> M.Map String Rep -> CompResult
computePostfix lst stk vmap =
  case lst of
    [] ->
      case stk of
        [] -> Left "Computation does not work, stack is empty (too many operations)"
        [n] -> Right n
        _ -> Left "Computation does not work, stack too full (too many integers)"
    (TBinOp op : rest) ->
      case stk of
        [] -> Left "Computation does not work, stack is empty (too many operations)"
        [_] -> Left "Computation does not work, stack is empty (too many operations)"
        (n1 : n2 : more) ->
          let
            res = evaluate op n1 n2
          in
            case res of
              Left err -> Left err
              Right n -> computePostfix rest (n : more) vmap
    (TNum n : rest) -> computePostfix rest (n : stk) vmap
    (TVar v : rest) ->
      let
        vres = vmap M.!? v
      in
        case vres of
          Nothing -> Left $ "Variable " ++ v ++ " used but not defined (or has a parse error in its definition)"
          Just repr ->
            case computeGeneral' (Right repr) vmap of
              Left err -> Left err
              Right n -> computePostfix rest (n : stk) vmap

computeGeneral' :: ParseResult -> M.Map String Rep -> CompResult
computeGeneral' repr vmap =
  case repr of
    Right (Tree a) -> computeAST a vmap
    Right (List ls) -> computePostfix ls [] vmap
    Left err -> Left err

evaluate :: Op -> Integer -> Integer -> CompResult
evaluate op n1 n2 =
  case op of
    Plus -> Right (n1 + n2)
    Minus -> Right (n1 - n2)
    Times -> Right (n1 * n2)
    Div ->
      case n2 of
        0 -> Left "Division by 0"
        _ -> Right (n1 `div` n2)


parseInfix :: String -> AST
parseInfix _ = Var "TODO"

parsePrefix :: String -> AST
parsePrefix _ = Var "TODO"

help :: String
help = "TODO: explain arguments"


computeAST :: AST -> M.Map String Rep -> CompResult
computeAST ast vmap =
  case ast of
    Numb n -> Right n
    BinOp (op, a1, a2) ->
      let
        a1val = computeAST a1 vmap
        a2val = computeAST a2 vmap
      in
        case (a1val, a2val) of
          (Right n1, Right n2) -> evaluate op n1 n2
          (_, Left n2) -> Left n2
          (Left n1, _) -> Left n1
    Var v ->
      let
        vres = vmap M.!? v
      in
        case vres of
          Nothing -> Left $ "Variable " ++ v ++ " used but not defined (or has a parse error in its definition)"
          Just repr -> computeGeneral' (Right repr) vmap



computeGeneral :: Rep -> String
computeGeneral repr =
  case repr of
    Tree a -> show $ computeAST a M.empty
    List ls -> 
      let
        res = computePostfix ls [] M.empty
      in
        case res of
          Right n -> show n
          Left err -> err

parseInput :: (Rep -> String) -> String -> String
parseInput outputFn input = unlines $ parseHelper outputFn (lines input) M.empty 0

parseHelper :: (Rep -> String) -> [String] -> M.Map String Rep -> Integer -> [String]
parseHelper _ [] _ _ = []
-- parseHelper outFn (line : rest) vmap lnum


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--to", "--postfix"] -> interact $ parseInput constructPostfix
    ["--to", "--prefix"] -> interact $ parseInput constructPrefix
    ["--to", "--infix"] -> interact $ parseInput constructInfix
    ["--compute"] -> interact $ parseInput computeGeneral
    _ -> putStrLn help
