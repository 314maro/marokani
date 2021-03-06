{-# LANGUAGE GeneralizedNewtypeDeriving, TupleSections #-}

module Language.MaroKani.Parser
  ( parseIO
  ) where

import Language.MaroKani.Types

import Control.Applicative
import qualified Text.Trifecta as T
import qualified Text.Parser.Token.Style as TS
import qualified Data.HashSet as HashSet
import Data.Monoid (mempty)

import Control.Monad.Trans (MonadIO)
import Control.Monad.Catch (MonadCatch, throwM)

cStyle :: TS.CommentStyle
cStyle = TS.CommentStyle "/*" "*/" "" True

newtype Parser a = Parser { runParser :: T.Parser a }
  deriving (Functor,Applicative,Alternative,Monad,T.Parsing,T.CharParsing)

instance T.TokenParsing Parser where
  nesting (Parser m) = Parser (T.nesting m)
  someSpace = TS.buildSomeSpaceParser (Parser T.someSpace) cStyle
  semi = Parser T.semi
  highlight h (Parser m) = Parser (T.highlight h m)

idStyle :: T.IdentifierStyle Parser
idStyle = TS.emptyIdents
  { T._styleReserved = HashSet.fromList
    ["_","if","then","else","while","for","do","namespace","import"]
  }

opStyle :: T.IdentifierStyle Parser
opStyle = (TS.emptyOps :: T.IdentifierStyle Parser)
  { T._styleStart = T._styleLetter opStyle
  , T._styleLetter = T.oneOf ":!#$%&*+./<=>?@^|-~"
  , T._styleReserved = HashSet.fromList ["=",":=","::=",".","/*","*/"] }

addParens :: String -> String
addParens s = "(" ++ s ++ ")"

addBrackets :: String -> String
addBrackets s = "[" ++ s ++ "]"

var :: Parser String
var = T.ident idStyle
  <|> (addParens <$> T.parens (T.ident opStyle))
  <|> (addBrackets <$> T.brackets (T.ident opStyle))
  T.<?> "variable"

reservedId :: String -> Parser String
reservedId s = T.token $ T.string s <* T.notFollowedBy (T.ident idStyle :: Parser String)

mk2ArgsOper :: String -> Parser (Expr -> Expr -> Expr)
mk2ArgsOper name = return $ \x y -> Var (addParens name) `App` x `App` y

oper :: Char -> Parser (Expr -> Expr -> Expr)
oper c = do
  name <- T.ident opStyle { T._styleStart = T.char c }
  mk2ArgsOper name

opers :: String -> Parser (Expr -> Expr -> Expr)
opers = foldr (<|>) empty . map oper

operName :: String -> Parser ()
operName s = T.token $ T.string s *> T.notFollowedBy (T.ident opStyle :: Parser String)

unaryOper :: Parser Expr
unaryOper = do
  name <- T.ident opStyle
  e <- expr
  return $ App (Var $ addBrackets name) e

declConst :: Parser ()
declConst = operName "::="

isDeclConst :: Parser (Expr -> Expr)
isDeclConst = (id <$ declConst) <|> (App (Var "newMutable") <$ operName ":=")

value :: Parser Value
value = (either VInt VDouble <$> T.naturalOrDouble)
  <|> (VString <$> T.stringLiteral)
  <|> (VBool False <$ T.symbol "()")
  T.<?> "value"

fact :: Parser Expr
fact = (EValue <$> value)
  <|> T.try (Var <$> var)
  <|> (T.brackets $ T.try (mk2ArgsOper "--->" <*> expr <* T.symbol ",," <*> expr) <|> (EArray <$> T.commaSep expr))
  <|> (T.braces $ EObject <$> (T.commaSep $ (,) <$> var <*> (isDeclConst <*> expr)))
  <|> (EFun <$ T.symbol "\\" <*> var' <*> many var' <*> T.braces exprs)
  <|> (Multi <$> T.parens exprs)
  T.<?> "factor"
  where
    var' = var <|> reservedId "_"

objectRef :: Parser Expr
objectRef = foldl ObjectRef <$> fact <*> many (operName "." *> var)

app :: Parser Expr
app = foldl App <$> (objectRef <|> unaryOper) <*> many objectRef

dotSharp :: Parser Expr
dotSharp = app `T.chainr1` opers ".#"

power :: Parser Expr
power = dotSharp `T.chainr1` opers "^"

mulDiv :: Parser Expr
mulDiv = power `T.chainl1` opers "*/%"

addSub :: Parser Expr
addSub = mulDiv `T.chainl1` opers "+-:@"

backQuote :: Parser Expr
backQuote = T.chainl1 addSub $ do
  e <- T.between (T.symbol "`") (T.symbol "`") addSub
  return $ \a b -> e `App` a `App` b

comp :: Parser Expr
comp = backQuote `T.chainl1` opers "=!<>~"

parseAnd :: Parser Expr
parseAnd = comp `T.chainl1` opers "&"

parseOr :: Parser Expr
parseOr = parseAnd `T.chainl1` opers "|"

dollar :: Parser Expr
dollar = parseOr `T.chainr1` opers "$?"

asgn :: Parser Expr
asgn = dollar `T.chainr1` (mk2ArgsOper name <* operName name)
  where
    name = "="

decl :: Parser Expr
decl = T.try (Decl <$> var <*> (isDeclConst <*> expr))
  <|> asgn

namespace :: Parser Expr
namespace = ENamespace <$ reservedId "namespace" <*> var
  <*> T.braces (T.commaSep $ (,) <$> var <* declConst <*> expr)

objectChain :: Parser (Expr,String)
objectChain = (,) <$> fact <* operName "." <*> var

import' :: Parser Expr
import' = uncurry Import <$ reservedId "import"
  <*> (((\(x,y) -> (Just x,y)) <$> T.try objectChain) <|> ((Nothing,) <$> var))

if' :: Parser Expr
if' = If <$ reservedId "if" <*> expr
  <* reservedId "then" <*> expr
  <*> optional (reservedId "else" *> expr)

while :: Parser Expr
while = While <$ reservedId "while" <*> expr
  <* reservedId "do" <*> expr

for :: Parser Expr
for = For <$ reservedId "for" <*> optional expr
  <* T.symbol ";" <*> optional expr
  <* T.symbol ";" <*> optional expr
  <* reservedId "do" <*> expr

expr :: Parser Expr
expr = namespace <|> import' <|> if' <|> while <|> for <|> decl
  T.<?> "expr"

exprs :: Parser [Expr]
exprs = T.whiteSpace *> T.sepEndBy expr T.semi T.<?> "exprs"

parse :: String -> T.Result [Expr]
parse s = T.parseString (runParser $ exprs <* T.eof) mempty s

parseIO :: (MonadIO m, MonadCatch m) => String -> m [Expr]
parseIO code = case parse code of
  T.Failure doc -> throwM $ ParserError doc
  T.Success es -> return es
