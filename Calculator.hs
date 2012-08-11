module Calculator where

import Control.Monad
import Control.Monad.Error
import Test.HUnit
import Test.QuickCheck
import System.IO
import Text.ParserCombinators.Parsec


data Expr a = Number a
            | Add (Expr a) (Expr a)
            | Mul (Expr a) (Expr a)
            | Sub (Expr a) (Expr a)
            | Div (Expr a) (Expr a)
            | Negate (Expr a)
            deriving (Show, Eq)

data AddOp = Plus | Minus
data MulOp = Times | Over

evaluate :: (Fractional a, Monad m , Eq a, Show a) => Expr a -> m a
evaluate (Negate x) = liftM negate (evaluate x)
evaluate (Div x y)  = do x' <- (evaluate x)
                         y' <- (evaluate y)
                         if y' == 0 then fail "Division by zero"
                                    else return $ x' / y'
evaluate (Sub x y)  = liftM2 (-) (evaluate x) (evaluate y)
evaluate (Mul x y)  = liftM2 (*) (evaluate x) (evaluate y)
evaluate (Add x y)  = liftM2 (+) (evaluate x) (evaluate y)
evaluate (Number x) = return x



pDigit = oneOf ['0'..'9']
pSign = option '+' $ oneOf "-+"
pDigits = many1 pDigit
pDecimalPoint = char '.'
pFracPart = option "0" (pDecimalPoint >> pDigits)

number = do sign <- pSign
            integerPart <- pDigits
            fracPart <- pFracPart
            expPart <- pExp
            let i = read integerPart
            let f = read fracPart
            let e = expPart
            let value = (i + (f / 10^(length fracPart))) * 10 ^^ e
            return $ Number $ case sign of
                 '+' -> value
                 '-' -> negate value
         where pExp = option 0 $ do
                             oneOf "eE"
                             sign <- pSign
                             num <- pDigits
                             let n = read num
                             return $ if sign == '-' then negate n else n

whitespace = many $ oneOf "\n\t\r\v "

term = do t <- term'
          whitespace
          return t
       where term' = (try number) <|> negTerm <|> parenExpr

negTerm = do
             char '-'
             whitespace
             e <- term
             return $ Negate e

expr :: GenParser Char () (Expr Double)
expr = do
          first <- mulTerm
          ops <- addOps
          return $ foldl buildExpr first ops
       where buildExpr acc (Plus, x) = Add acc x
             buildExpr acc (Minus, x) = Sub acc x

addOp = do operator <- oneOf "+-"
           whitespace
           t <- mulTerm
           return $ case operator of
                         '-' -> (Minus, t)
                         '+' -> (Plus, t)

addOps = many addOp


parenExpr = do char '('
               e <- expr
               char ')'
               return e

mulTerm = do first <- term
             ops <- mulOps
             return $ foldl buildExpr first ops
          where buildExpr acc (Times, x) = Mul acc x
                buildExpr acc (Over, x) = Div acc x

mulOp = do operator <- oneOf "*/"
           whitespace
           t        <- term
           return $ case operator of
                         '*' -> (Times, t)
                         '/' -> (Over, t)

mulOps = many mulOp
calculation = do whitespace
                 e <- expr
                 eof
                 return e

eval :: String -> String
eval s = case (parse calculation "" s) of
                   Right x -> case evaluate x of
                                   Right v -> show v
                                   Left err -> "Error"
                   Left err -> "Parse Error"


