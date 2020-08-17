module Lambda (
    V, Term (..), Parser, try, parseLambda, reduce, normalForm, rename, parseReduce, term
) where

import Text.Parsec
import Text.Parsec.String (Parser)
import Text.Parsec.Language (emptyDef)
import qualified Text.Parsec.Token as Tok

type V = String
data Term = Var V | Lambda V Term | App Term Term

-- beta reduction
rename :: String -> Term -> Term -> Term
rename from to t@(Var v) = if v == from then to else t
rename from to t@(Lambda v term) = if v == from then t else Lambda v (rename from to term)
rename from to (App a b) = App (rename from to a) (rename from to b)

normalForm :: Term -> Bool
normalForm (Var v) = True
normalForm (Lambda _ term) = normalForm term
normalForm (App (Lambda _ _) b) = False
normalForm (App a b) = normalForm a && normalForm b

reduce :: Term -> Term
reduce (Var v) = Var v
reduce (Lambda x term) = Lambda x (reduce term)
reduce (App (Lambda x term) b) = rename x b term
reduce (App a b) = App (reduce a) (reduce b)

-- from AST to list
instance Show Term where
    show (Var v) = v
    show (Lambda v term) = "(lambda " ++ v ++ " " ++ show term ++ ")"
    show (App a b) = "(" ++ show a ++ " " ++ show b ++ ")"

-- the following is a parser
-- lexer
lexer :: Tok.TokenParser ()
lexer = Tok.makeTokenParser emptyDef {Tok.reservedNames = ["lambda"]}

parens :: Parser a -> Parser a
parens a = try (Tok.parens lexer a) <|> Tok.braces lexer a

reserved :: String -> Parser ()
reserved = Tok.reserved lexer

-- parser

var :: Parser String
var = try (Tok.identifier lexer) <|> Tok.operator lexer

abstraction :: Parser Term
abstraction = parens $ do
    reserved "lambda"
    x <- var
    Lambda x <$> term

app :: Parser Term
app = parens $ do
    a <- term
    App a <$> term

term :: Parser Term
term = try (Var <$> var) <|> try abstraction <|> app

parseLambda :: String -> Either ParseError Term
parseLambda = parse term "stdin"

parseReduce :: String -> Either ParseError Term
parseReduce s = do
    l <- parseLambda s
    return $ reduce l
