-- Assignment for Week 3 of AP
-- Martin Jorgensen, tzk173
-- Casper B. Hansen, fvx507

module CurvySyntax2 where

import CurveAST
import Text.ParserCombinators.ReadP
import Data.Char

-- Lets keep error simple shall we.
type Error = String


-- Convenience and basic things :)

-- Parses chars
chrToken :: Char -> ReadP ()
chrToken c = do
  skipSpaces
  _ <- char c
  skipSpaces

-- Parses strings
strToken :: String -> ReadP ()
strToken s = do
  skipSpaces
  _ <- string s
  skipSpaces

-- Unify floats and ints under "number"
number :: ReadP Expr
number = getFloat <++ getInt

-- Parse signed/unsiged integers
getInt :: ReadP Expr
getInt = do
  s <- sign
  d <- digits
  return $ Const (s * read d)

-- Parse signed/unsiged floats
getFloat :: ReadP Expr
getFloat = do
  s <- sign
  i <- digits
  _ <- char '.'
  f <- digits
  return $ Const (s * read (i ++ "." ++ f))

-- Parse "signs" using miltiplication with -1 as minus sign.
sign :: ReadP Number
sign = (do
           _ <- char '-'
           return $ -1)
       +++ return 1

-- Terms in expressions
term :: ReadP Expr
term = do
  f <- factor
  termop f

-- Term parsing
termop :: Expr -> ReadP Expr
termop val = (do
                 _ <- chrToken '*'
                 f <- factor
                 termop(Mult val f)
             ) <++ return val

-- Factors, subparts of multiplications
factor :: ReadP Expr
factor = number +++ parens expr

-- Parse expressions in paranthesis
parens :: ReadP a -> ReadP a
parens e = do
  chrToken '('
  ex <- e
  chrToken ')'
  return ex

-- Well, yes. digits.
digits :: ReadP String
digits = munch1 isDigit
-- No more convenience stuff

-- Parse a program
prog :: ReadP [Def]
prog = do
  d <- defs
  eof
  return d

-- Parse list of Defs
defs :: ReadP [Def]
defs = many def

-- Parse single Def
def :: ReadP Def
def = do
  iden <- ident
  _ <- chrToken '='
  ct <- curve
  defop (Def iden ct [])

-- Parse "operators" in Defs
defop :: Def -> ReadP Def
defop inval@(Def iden ct _) = (do
      _ <- strToken "where"
      _ <- chrToken '{'
      dv <- defs
      _ <- chrToken '}'
      return $ Def iden ct dv)
  <++ return inval

-- Parse Curve
curve :: ReadP Curve
curve = (do
            iden <- ident
            curveop (Id iden)
        ) <++
        (do
            p <- point
            curveop (Single p)
        ) <++ parens curve

-- Parse "operators" in curves.
curveop :: Curve -> ReadP Curve
curveop val = (do
                  skipSpaces
                  _ <- string "rot"
                  munch1 isSpace
                  ex <- expr
                  curveop(Rot val ex)
              ) <++
              (do
                  skipSpaces
                  _ <- string "refh"
                  munch1 isSpace
                  ex <- expr
                  curveop(Refh val ex)
              ) <++
              (do
                  skipSpaces
                  _ <- string "refv"
                  munch1 isSpace
                  ex <- expr
                  curveop(Refv val ex)
              ) <++
              (do
                  _ <- strToken "**"
                  ex <- expr
                  curveop(Scale val ex)
              ) <++
              (do
                  _ <- strToken "->"
                  p <- point
                  curveop(Translate val p)
              ) <++
              (do
                  _ <- strToken "^"
                  c <- curve
                  curveop(Over val c)
              ) <++
              (do
                  _ <- strToken "++"
                  c <- curve
                  curveop(Connect val c)
              ) <++ return val

-- Parse point
point :: ReadP Point
point = do
  _ <- chrToken '('
  ex1 <- expr
  _ <- chrToken ','
  ex2 <- expr
  _ <- chrToken ')'
  return $ Point ex1 ex2

-- Parse expressions.
expr :: ReadP Expr
expr = (do
           t <- term
           exprop t
       ) +++
       (do
           skipSpaces
           _ <- string "width"
           munch1 isSpace
           c <- curve
           return $ Width c
       ) +++
       (do
           skipSpaces
           _ <- string "height"
           munch1 isSpace
           c <- curve
           return $ Height c
       )

-- Parse expression operators.
-- Notice that multiplication is handled in "term" further up.
exprop :: Expr -> ReadP Expr
exprop val = (do
                 _ <- chrToken '+'
                 t <- term
                 exprop(Add val t)
             ) <++ return val

-- Parse identifications.
ident :: ReadP Ident
ident = do
  iden <- munch1 (\x -> isDigit x || isLetter x)
  if iden `elem` ["rot","refv","refh","width","height"]
    then pfail
    else do
    munch1 isSpace
    return iden


-- Use the methods to access the parser

-- Parses a string into a program.
parseString :: String -> Either Error Program
parseString s = case opt of
  [] -> Left "Parser error."
  (x:_) -> Right (fst x)
  where opt = readP_to_S prog s

-- Reads and parses a file to a program.
parseFile :: FilePath -> IO (Either Error Program)
parseFile filename = fmap parseString $ readFile filename


-- Let the testing begin!
runTests :: IO ()
runTests = do
  -- The test lines will get a bit long, sorry about that :/
  test 0 "c = (0,0)" (Right [Def "c" (Single (Point (Const 0.0) (Const 0.0)))[]])

  test 1 "c = a where {a = (0,0.5) ++ (1,1)}" (Right [Def "c" (Id "a") [Def "a" (Connect (Single (Point (Const 0.0) (Const 0.5))) (Single (Point (Const 1.0) (Const 1.0)))) []]])

  test 2 "c = (0,0) ++ (5, 42.5)" (Right [Def "c" (Connect (Single (Point (Const 0.0) (Const 0.0))) (Single (Point (Const 5.0) (Const 42.5)))) []])

  test 3 "c = (1,1) rot (2+3) -> (4,5)" (Right [Def "c" (Translate (Rot (Single (Point (Const 1.0) (Const 1.0))) (Add (Const 2.0) (Const 3.0))) (Point (Const 4.0) (Const 5.0))) []])

  test 4 "c = (1+1, 2) ++ (3*3+3,3)" (Right [Def "c" (Connect (Single (Point (Add (Const 1.0) (Const 1.0)) (Const 2.0))) (Single (Point (Add (Mult (Const 3.0) (Const 3.0)) (Const 3.0)) (Const 3.0)))) []])

  test 5 "a = (1,2) ++ (3,4) ^ (5,6)" (Right [Def "a" (Connect (Single (Point (Const 1) (Const 2))) (Over (Single (Point (Const 3) (Const 4))) (Single (Point (Const 5) (Const 6))))) []])
    where
      test i inp exp =
        if testParse inp exp
        then putStrLn(show(i) ++ "=OK")
        else putStrLn(show(i) ++ "=FAIL")
        where
          testParse inp exp = (parseString inp) == (exp)
