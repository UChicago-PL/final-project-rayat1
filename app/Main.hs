module Main where

import System.Environment
import Data.Char

import qualified Data.Map as M

data Op
  = Plus
  | Minus
  | Times
  | Div deriving Show

opString :: Op -> String
opString Plus = "+"
opString Minus = "-"
opString Times = "*"
opString Div = "/"

data AST
  = Var (String, Int)
  | BinOp (Op, AST, AST, Int)
  | Numb (Int, Int)


data Token
  = TVar String
  | TBinOp Op
  | TNum Int
  | TOPar
  | TCPar deriving Show

type IToken = (Token, Int) -- Mark the column number of the token

type Post = [IToken]

data Rep
  = Tree AST
  | List Post


data NotationType
  = NTPrefix
  | NTInfix
  | NTPostfix

data OutputMode
  = ToPrefix
  | ToInfix
  | ToPostfix
  | Compute deriving Show


-- Key type
type Failure a = Either String a


dispF :: Show a => Failure a -> String
dispF a =
  case a of
    Left err -> err
    Right w -> show w

-- Scanning
scanInput :: String -> Int -> [IToken]
scanInput "" _ = []
scanInput (c : rest) n
  | isSpace c = scanInput rest (n + 1)
  | isAlpha c =
    let
      (var, remainder) = scanVar rest [c]
    in
      (TVar var, n) : scanInput remainder (n + length var)
  | isNumber c =
    let
      (number, len, remainder) = scanNum rest [c]
    in
      (TNum number, n) : scanInput remainder (n + len)
  | c == '+' = (TBinOp Plus, n) : scanInput rest (n + 1)
  | c == '-' = (TBinOp Minus, n) : scanInput rest (n + 1)
  | c == '*' = (TBinOp Times, n) : scanInput rest (n + 1)
  | c == '/' = (TBinOp Div, n) : scanInput rest (n + 1)
  | c == '(' = (TOPar, n) : scanInput rest (n + 1)
  | c == ')' = (TCPar, n) : scanInput rest (n + 1)
  | otherwise = scanInput rest (n + 1) --All characters we don't understand are skipped

scanNum :: [Char] -> [Char] -> (Int, Int, String)
-- read is guaranteed to work since we only add numeric characters to the string
scanNum [] acc = (read $ reverse acc, length acc, "")
scanNum (c : rest) acc
  | isNumber c = scanNum rest (c : acc)
  | otherwise = (read $ reverse acc, length acc, c : rest)

scanVar :: String -> String -> (String, String)
scanVar [] acc = (reverse acc, "")
scanVar (c : rest) acc
  | isAlpha c = scanVar rest (c : acc)
  | otherwise = (reverse acc, c : rest)


-- Notation detection
isOp :: Char -> Bool
isOp c = c == '+' || c == '*' || c == '/' || c == '-'

detectNotation :: String -> Failure NotationType
detectNotation [] = Left "Cannot decipher what notation was attempted"
detectNotation (c : rest)
  | c == '(' = detectInOrPre rest
  | isDigit c || isAlpha c = detectInOrPost rest False
  | otherwise = detectNotation rest

detectInOrPost :: String -> Bool -> Failure NotationType
detectInOrPost [] _ = Left "Cannot decipher what notation was attempted"
detectInOrPost (c : rest) seenSpace
  | isDigit c || isAlpha c = if seenSpace then Right NTPostfix else detectInOrPost rest False
  | isOp c = Right NTInfix
  | isSpace c = detectInOrPost rest True
  | otherwise = detectInOrPost rest seenSpace

detectInOrPre :: String -> Failure NotationType
detectInOrPre [] = Left "Cannot decipher what notation was attempted"
detectInOrPre (c : rest)
  | isOp c = Right NTPrefix
  | isDigit c || isAlpha c = Right NTInfix
  | otherwise = detectInOrPre rest



-- Notation conversions
constructPostfix :: Rep -> String
constructPostfix repr =
  case repr of
    List p -> listToPostfix p
    Tree t -> astToPostfix t

constructPrefix :: Rep -> String
constructPrefix repr =
  case repr of
    List p -> listToPrefix p
    Tree t -> astToPrefix t

constructInfix :: Rep -> String
constructInfix repr =
  case repr of
    List p -> listToInfix p
    Tree t -> astToInfix t


astToPostfix :: AST -> String
astToPostfix ast =
  case ast of
    Var (s, _) -> s ++ " "
    BinOp (op, a1, a2, _) -> astToPostfix a1 ++ astToPostfix a2 ++ opString op ++ " "
    Numb (n, _) -> show n ++ " "

listToPostfix :: Post -> String
listToPostfix [] = ""
listToPostfix ((tok, _) : rest) =
  case tok of
    TVar s -> s ++ " " ++ listToPostfix rest
    TBinOp op -> opString op ++ " " ++ listToPostfix rest
    TNum n -> show n ++ " " ++ listToPostfix rest
    TOPar -> "("
    TCPar -> ")"


listToInfix :: Post -> String
listToInfix lst = lHelper lst [] where
  lHelper [] [c] = c
  lHelper [] _ = "Error in postfix notation (too many items left on the stack)"
  lHelper (tok : rest) stk =
    case tok of
      (TVar s, _) -> lHelper rest (s : stk)
      (TNum n, _) -> lHelper rest (show n : stk)
      (TBinOp op, _) ->
        case stk of
          [] -> "(__ " ++ opString op ++ " __) " ++ lHelper rest []
          [n1] -> "( " ++ n1 ++ " " ++ opString op ++ " __) " ++ lHelper rest []
          (n1 : n2 : stk') ->
            let
              oper = "(" ++ n1 ++ " " ++ opString op ++ " " ++ n2 ++ ")"
            in
              lHelper rest (oper : stk')
      _ -> lHelper rest stk

astToPrefix :: AST -> String
astToPrefix ast =
  case ast of
    Var (s, _) -> s ++ " "
    Numb (n, _) -> show n ++ " "
    BinOp (op, a1, a2, _) -> "(" ++ opString op ++ " " ++ astToPrefix a1 ++ astToPrefix a2 ++ ") "

astToInfix :: AST -> String
astToInfix ast =
  case ast of
    Var (s, _) -> s
    Numb (n, _) -> show n
    BinOp (op, a1, a2, _) -> "(" ++ astToInfix a1 ++ " " ++ opString op ++ " " ++ astToInfix a2 ++ ") "


listToPrefix :: Post -> String
listToPrefix lst =
  let
    infString = listToInfix lst
    pOpt = parseInfix $ scanInput infString 0
  in
    case pOpt of
      Right (ast, []) -> astToPrefix ast
      Right (_, _) -> "Error after conversion to infix, likely was an inaccuracy in that conversion (This is likely the programmer's fault, not yours!)"
      Left err -> "The following error ocurred while converting to infix: " ++ err



-- Parsing token lists
parseMonadHelper :: (AST, [IToken]) -> Failure (AST, [IToken])
parseMonadHelper (parseRes, []) = Right (parseRes, [])
parseMonadHelper (_, (_, col) : _) = Left $ "Found additional tokens at column " ++ show col ++ " after finishing parsing"

parseLine :: String -> Int -> Failure Rep
parseLine s n =
  let
    ntype = detectNotation s
  in
    case ntype of
      Right NTPrefix -> fmap (Tree . fst) $ parsePrefix (scanInput s n) >>= parseMonadHelper
      Right NTInfix -> fmap (Tree . fst) $ parseInfix (scanInput s n) >>= parseMonadHelper
      Right NTPostfix -> Right $ List (scanInput s n)
      Left err -> Left err

parsePrefix :: [IToken] -> Either String (AST, [IToken])
parsePrefix [] = Left "Parse failed, reached end of line"
parsePrefix ((TVar s, col) : rest) = Right (Var (s, col), rest)
parsePrefix ((TNum n, col) : rest) = Right (Numb (n, col), rest)
parsePrefix ((TOPar, col) : (TBinOp op, col') : rest) =
  case parsePrefix rest of
    Left err -> Left err
    Right (t1, rest') ->
      case parsePrefix rest' of
        Left err -> Left err
        Right (t2, (TCPar, _) : rest'') -> Right (BinOp (op, t1, t2, col'), rest'')
        Right (_, (_, col'') : _) -> Left $ "Missing `)` for `(` located at column " ++ show col ++ ", expected at col " ++ show col''
        Right (_, []) -> Left $ "Missing `)` for `(` located at column " ++ show col ++ "Reached end of line"
parsePrefix ((_, col) : _) = Left $ "Parse failed at column " ++ show col ++
  " . Might be missing an opening `(` before an operation, or incorrectly placed one before a number"


parseInfix :: [IToken] -> Failure (AST, [IToken])
parseInfix toklist =
  let
    nextAddExpHelper :: Failure (AST, [IToken]) -> Failure (AST, [IToken])
    nextAddExpHelper term_w_tlist =
        case term_w_tlist of
          Right (term, (TBinOp Plus, col) : rest) ->
            let
              nextTerm = nextMulExp rest
            in
              case nextTerm of
                Left err -> Left err
                Right (nTerm, nToks) -> nextAddExpHelper $ Right (BinOp (Plus, term, nTerm, col), nToks)
          Right (term, (TBinOp Minus, col) : rest) ->
            let
              nextTerm = nextMulExp rest
            in
              case nextTerm of
                Left err -> Left err
                Right (nTerm, nToks) -> nextAddExpHelper $ Right (BinOp (Minus, term, nTerm, col), nToks)
          _ -> term_w_tlist

    nextAddExp :: [IToken] -> Failure (AST, [IToken])
    nextAddExp tlist = nextAddExpHelper (nextMulExp tlist)

    nextMulExpHelper :: Failure (AST, [IToken]) -> Failure (AST, [IToken])
    nextMulExpHelper term_w_tlist =
        case term_w_tlist of
          Right (term, (TBinOp Times, col) : rest) ->
            let
              nextTerm = nextFactor rest
            in
              case nextTerm of
                Left err -> Left err
                Right (nTerm, nToks) -> nextMulExpHelper $ Right (BinOp (Times, term, nTerm, col), nToks)
          Right (term, (TBinOp Div, col) : rest) ->
            let
              nextTerm = nextMulExp rest
            in
              case nextTerm of
                Left err -> Left err
                Right (nTerm, nToks) -> nextMulExpHelper $ Right (BinOp (Div, term, nTerm, col), nToks)
          _ -> term_w_tlist

    nextMulExp :: [IToken] -> Failure (AST, [IToken])
    nextMulExp tlist = nextMulExpHelper (nextFactor tlist)

    nextFactor :: [IToken] -> Either String (AST, [IToken])
    nextFactor tlist =
      case tlist of
        ((TOPar, col) : rest) ->
          case nextAddExp rest of
            Left err -> Left err
            Right (a, (TCPar, _) : rest') ->  Right (a, rest')
            _ -> Left $ "Missing ) for ( at column " ++ show col
        ((TNum n, col) : rest) -> Right (Numb (n, col), rest)
        ((TVar s, col) : rest) -> Right (Var (s, col), rest)
        ((_, col) : _) -> Left $ "Parse error at column " ++ show col ++ ". Unexpected token"
        [] -> Left "Parse error at end of line"
  in
    nextAddExp toklist >>= parseMonadHelper


-- Computation
computeGeneral :: Failure Rep -> M.Map String Rep -> Failure Int
computeGeneral repr vmap =
  case repr of
    Right (Tree a) -> computeAST a vmap
    Right (List ls) -> computePostfix ls [] vmap
    Left err -> Left err


computePostfix :: Post -> [Int] -> M.Map String Rep -> Failure Int
computePostfix lst stk vmap =
  case lst of
    [] ->
      case stk of
        [] -> Left "Computation does not work, stack is empty (too many operations)"
        [n] -> Right n
        _ -> Left "Computation does not work, stack too full (too many integers)"
    ((TBinOp op, col) : rest) ->
      case stk of
        [] -> Left $ "Computation does not work, stack is empty (too many operations)" ++
                  "Error with " ++ opString op ++ "at column " ++ show col
        [_] -> Left $ "Computation does not work, stack is empty (too many operations)" ++
                "Error with " ++ opString op ++ "at column " ++ show col
        (n1 : n2 : more) ->
          let
            res = evaluate op n2 n1
          in
            case res of
              Left err -> Left $ err ++ " at column " ++ show col
              Right n -> computePostfix rest (n : more) vmap
    ((TNum n, _) : rest) -> computePostfix rest (n : stk) vmap
    ((TVar v, col) : rest) ->
      let
        vres = vmap M.!? v
      in
        case vres of
          Nothing -> Left $ "Variable `" ++ v ++ "` used but not defined (or had a parse error in its definition)" ++
            " at column " ++ show col
          Just repr ->
            case computeGeneral (Right repr) vmap of
              Left err -> Left err
              Right n -> computePostfix rest (n : stk) vmap
    ((_, _) : rest) -> computePostfix rest stk vmap

computeAST :: AST -> M.Map String Rep -> Failure Int
computeAST ast vmap =
  case ast of
    Numb (n, _) -> Right n
    BinOp (op, a1, a2, col) ->
      let
        a1val = computeAST a1 vmap
        a2val = computeAST a2 vmap
      in
        case (a1val, a2val) of
          (Right n1, Right n2) ->
            case evaluate op n1 n2 of
              Right ans -> Right ans
              Left err -> Left $ err ++ " at column " ++ show col
          (Left n1, _) -> Left n1
          (_, Left n2) -> Left n2
    Var (v, col) ->
      let
        vres = vmap M.!? v
      in
        case vres of
          Nothing -> Left $ "Variable " ++ v ++ " used but not defined (or has a parse error in its definition) " ++
            "at column " ++ show col
          Just repr ->
            case computeGeneral (Right repr) vmap of
              Left err -> Left $ "In declaration of variable `" ++ v ++ "`: " ++ err
              Right n -> Right n


evaluate :: Op -> Int -> Int -> Failure Int
evaluate op n1 n2 =
  case op of
    Plus -> Right (n1 + n2)
    Minus -> Right (n1 - n2)
    Times -> Right (n1 * n2)
    Div ->
      case n2 of
        0 -> Left "Division by 0"
        _ -> Right (n1 `div` n2)



-- Interaction
handleInput :: OutputMode -> M.Map String Rep -> String -> String
handleInput outputFn vmap input = unlines $ inputHelper outputFn (lines input) vmap


handleLine :: OutputMode -> String -> M.Map String Rep -> (String, M.Map String Rep)
handleLine _ ('!' : rest) vmap =
  let
    (vname, rest') = scanVar rest ""
    vexpr = parseLine rest' (2 + length vname)
  in
    case vexpr of
      Left err -> ("Variable " ++ vname ++ " not parsed due to " ++ err, vmap)
      Right expr -> ("Variable `" ++ vname ++ "` added to map!", M.insert vname expr vmap)
handleLine out ln vmap =
  let
    expr = parseLine ln 1
  in
    case expr of
      Left err -> ("Line not parsed due to " ++ err, vmap)
      Right repr ->
        case out of
          ToPostfix -> (constructPostfix repr, vmap)
          ToPrefix -> (constructPrefix repr, vmap)
          ToInfix -> (constructInfix repr, vmap)
          Compute -> (dispF $ computeGeneral (Right repr) vmap, vmap)

inputHelper :: OutputMode -> [String] -> M.Map String Rep -> [String]
inputHelper _ [] _ = []
inputHelper out (ln : rest) vmap =
  let
    (result, updatedMap) = handleLine out ln vmap
  in
    result : inputHelper out rest updatedMap

repl :: M.Map String Rep -> OutputMode -> IO ()
repl vmap mode = do
  putStrLn $ "Currently in mode: " ++ show mode
  line <- getLine
  case line of
    "quit" -> putStrLn "Have a nice day!"
    "Compute" -> do
        putStrLn "Mode changed to computation! Will now compute non-assignment lines"
        repl vmap Compute
    "Postfix" -> do
        putStrLn "Mode changed to postfix! Will now convert non-assignment lines to postfix"
        repl vmap ToPostfix
    "Prefix" -> do
        putStrLn "Mode changed to prefix! Will now convert non-assignment lines to prefix"
        repl vmap ToPrefix
    "Infix" -> do
        putStrLn "Mode changed to infix! Will now convert non-assignment lines to infix"
        repl vmap ToInfix
    "help" -> do
      putStrLn helpRepl
      repl vmap mode
    _ -> do
      let (result, updatedMap) = handleLine mode line vmap
      putStrLn result
      repl updatedMap mode

helpRepl :: String
helpRepl = "The REPL will parse variable lines and store the expression associated with the variable.\n" ++
  "Lines which do not assign variables will be handled according to the current mode. The available modes are:\n" ++
  "Compute, Postfix, Prefix, or Infix. Type the mode name at the REPL to change the mode\n" ++
  "Compute mode will compute the expression, the other modes will convert the expression into the desired notation type\n" ++
  "Tip: call the program with `rlwrap` for arrow keys and stuff in the REPL"

help :: String
help =
  "--postfix, --prefix, and --infix convert the non-assignment standard input lines to that notation form\n" ++
  "--compute computes the non-assignment standard input lines, which can be expressions in prefix, infix, or postfix form\n" ++
  "variables are assigned as: !name expr, the name must be all alphanumeric characters, the expression is any arithmetic expression\n" ++
  "variables need to be defined before they are used.\n" ++
  "Pass the --repl flag to start a REPL where you can use the calculator and define variables. Type `help` in the REPL for more help"


main :: IO ()
main = do
  args <- getArgs
  case args of
    ["--postfix"] -> interact $ handleInput ToPostfix M.empty
    ["--prefix"] -> interact $ handleInput ToPrefix M.empty
    ["--infix"] -> interact $ handleInput ToInfix M.empty
    ["--compute"] -> interact $ handleInput Compute M.empty
    ["--repl"] -> do
      putStrLn "Type `help` for help"
      repl M.empty Compute
    _ -> putStrLn help



