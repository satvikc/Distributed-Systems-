module Calculator where

import Control.Monad
import Control.Monad.Error
import Test.HUnit
import Test.QuickCheck
import System.IO
import Text.ParserCombinators.Parsec


data expr a = number a
            | add (expr a) (expr a)
            | mul (expr a) (expr a)
            | sub (expr a) (expr a)
            | div (expr a) (expr a)
            | negate (expr a)
            deriving (show, eq)

data addop = plus | minus
data mulop = times | over

evaluate :: (fractional a, monad m , eq a, show a) => expr a -> m a
evaluate (negate x) = liftm negate (evaluate x)
evaluate (div x y)  = do x' <- (evaluate x)
                         y' <- (evaluate y)
                         if y' == 0 then fail "division by zero"
                                    else return $ x' / y'
evaluate (sub x y)  = liftm2 (-) (evaluate x) (evaluate y)
evaluate (mul x y)  = liftm2 (*) (evaluate x) (evaluate y)
evaluate (add x y)  = liftm2 (+) (evaluate x) (evaluate y)
evaluate (number x) = return x



pdigit = oneof ['0'..'9']
psign = option '+' $ oneof "-+"
pdigits = many1 pdigit
pdecimalpoint = char '.'
pfracpart = option "0" (pdecimalpoint >> pdigits)

number = do sign <- psign
            integerpart <- pdigits
            fracpart <- pfracpart
            exppart <- pexp
            let i = read integerpart
            let f = read fracpart
            let e = exppart
            let value = (i + (f / 10^(length fracpart))) * 10 ^^ e
            return $ number $ case sign of
                 '+' -> value
                 '-' -> negate value
         where pexp = option 0 $ do
                             oneof "ee"
                             sign <- psign
                             num <- pdigits
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
