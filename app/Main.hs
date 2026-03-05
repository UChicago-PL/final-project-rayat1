module Main where

import System.Environment
-- import Data.Char
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
  | Num Integer

data Token
  = TVar String
  | TBinOp Op
  | TNum Integer

type Post = [Token]

data Rep
  = Tree AST
  | List Post


data NotationType
  = NTPrefix
  | NTInfix
  | NTPostfix

constructPostfix :: Rep -> String
constructPostfix _ = ""

constructPrefix :: Rep -> String
constructPrefix _ = ""

constructInfix :: Rep -> String
constructInfix _ = ""

detectNotation :: String -> NotationType
detectNotation _ = NTPostfix

parseFull :: String -> Rep
parseFull s =
  let
    ntype = detectNotation s
  in
    case ntype of
      NTPrefix -> Tree (parsePrefix s)
      NTInfix -> Tree (parseInfix s)
      NTPostfix -> List (parsePostfix s)

parseAll :: String -> [Rep]
parseAll m = map parseFull $ lines m

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

computePostfix :: Post -> [Integer] -> M.Map String Rep  -> Integer
computePostfix lst stk _ =
  --
  case lst of
    [] ->
      case stk of
        [] -> error "Computation does not work, stack is empty (too many operations)"
        [n] -> n
        _ -> error "Computation does not work, stack too full (too many integers)"
    (TBinOp op : rest) ->
      case stk of
        [] -> error "Computation does not work, stack is empty (too many operations)"
        [_] -> error "Computation does not work, stack is empty (too many operations)"
        (n1 : n2 : more) -> computePostfix rest (evaluate op n1 n2 : more) M.empty
    _ -> 0

evaluate :: Op -> Integer -> Integer -> Integer
evaluate _ _ _ = 0


parseInfix :: String -> AST
parseInfix _ = Var "TODO"

parsePrefix :: String -> AST
parsePrefix _ = Var "TODO"

help :: String
help = "TODO: explain arguments"


computeAST :: AST -> Integer
computeAST _ = 0

computeList :: Post -> Integer
computeList _ = 0

computeGeneral :: Rep -> String
computeGeneral repr =
  case repr of
    Tree a -> show $ computeAST a
    List ls -> show $ computeList ls

main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--to", "--postfix"] -> interact $ constructPostfix . parseFull
    ["--to", "--prefix"] -> interact $ constructPrefix . parseFull
    ["--to", "--infix"] -> interact $ constructInfix . parseFull
    ["--compute"] -> interact $ computeGeneral . parseFull
    _ -> putStrLn help
