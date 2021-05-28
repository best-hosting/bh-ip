{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Text.Megaparsec.Pos
import qualified Control.Monad.State.Strict as S
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Combinators.Expr
import qualified Data.Char as C

main :: IO ()
main = return ()

type Parser = Parsec Void Text

mySequence :: Parser (Char, Char, Char)
mySequence = do
  a <- char 'a'
  b <- char 'b'
  c <- char 'c'
  return (a, b, c)



mySequence2 :: Parser (Char, Char, Char)
mySequence2 =
  (,,) <$> char 'a'
       <*> char 'b'
       <*> char 'c'

data Scheme
  = SchemeData
  | SchemeFile
  | SchemeFtp
  | SchemeHttp
  | SchemeHttps
  | SchemeIrc
  | SchemeMailto
  deriving (Eq, Show)

pScheme :: Parser Scheme
pScheme = choice
  [ SchemeData   <$ string "data"
  , SchemeFile   <$ string "file"
  , SchemeFtp    <$ string "ftp"
  , SchemeHttps  <$ string "https"
  , SchemeHttp   <$ string "http"
  , SchemeIrc    <$ string "irc"
  , SchemeMailto <$ string "mailto" ]

data Authority = Authority
  { authUser :: Maybe (Text, Text) -- (user, password)
  , authHost :: Text
  , authPort :: Maybe Int
  } deriving (Eq, Show)


data Uri = Uri
  { uriScheme :: Scheme
  , uriAuthority :: Maybe Authority
  } deriving (Eq, Show)

pUri :: Parser Uri
pUri = do
  uriScheme <- dbg "scheme" pScheme <?> "valid scheme"
  void (char ':')
  uriAuthority <- optional $ do            -- (1)
    void (string "//")
    authUser <- dbg "auth" . optional . try $ do              -- (2)
      user <- T.pack <$> some alphaNumChar <?> "username"
      void (char ':')
      password <- T.pack <$> some alphaNumChar <?> "password"
      void (char '@')
      return (user, password)
    authHost <- (dbg "host" $ T.pack <$> some (alphaNumChar <|> char '.')) <?> "host"
    authPort <- dbg "port" $ optional (char ':' *> (L.decimal <?> "port"))
    return Authority {..}                        -- (5)
  return Uri {..}                                -- (6)

alternatives :: Parser (Char, Char)
alternatives = try foo <|> bar

foo = (,) <$> char 'a' <*> char 'b'
bar = (,) <$> char 'a' <*> char 'c'


--type ParserS = ParsecT Void Text (S.State String)
type ParserS = S.StateT String (ParsecT Void Text Identity)

parser0 :: ParserS String
parser0 = a <|> b
  where
    a = "foo" <$ S.put "branch A"
    b = S.get <* S.put "branch B"

parser1 :: ParserS String
parser1 = a <|> b
  where
    a = "foo" <$ S.put "branch A" <* empty
    b = S.get <* S.put "branch B"

runS :: IO ()
runS = do
{-  let run p          = S.runState (runParserT p "" "") "initial"
      (Right a0, s0) = run parser0
      (Right a1, s1) = run parser1-}

  let run p   = runIdentity $ runParserT (S.runStateT p "initial") "" ""
      Right (a0, s0) = run parser0
      Right (a1, s1) = run parser1

  putStrLn  "Parser 0"
  putStrLn ("Result:      " ++ show a0)
  putStrLn ("Final state: " ++ show s0)

  putStrLn  "Parser 1"
  putStrLn ("Result:      " ++ show a1)
  putStrLn ("Final state: " ++ show s1)

sc :: Parser ()
sc = L.space
  space1                         -- (2)
  (L.skipLineComment "//")       -- (3)
  (L.skipBlockComment "/*" "*/") -- (4)

lexeme :: Parser a -> Parser a
lexeme = L.lexeme sc

symbol :: Text -> Parser Text
symbol = L.symbol sc

charLiteral :: Parser Char
charLiteral = between (char '\'') (char '\'') L.charLiteral

stringLiteral :: Parser String
stringLiteral = char '\"' *> manyTill L.charLiteral (char '\"')

integer :: Parser Integer
integer = lexeme L.decimal

float :: Parser Double
float = lexeme L.float

signedInteger :: Parser Integer
signedInteger = L.signed sc integer

signedFloat :: Parser Double
signedFloat = L.signed (pure ()) float

pKeyword :: Text -> Parser Text
pKeyword keyword = lexeme (string keyword <* notFollowedBy alphaNumChar)

withPredicate1
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate1 f msg p = do
  r <- lookAhead p
  if f r
    then p
    else fail msg

withPredicate2
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate2 f msg p = do
  o <- getOffset
  r <- p
  if f r
    then return r
    else do
      setOffset o
      fail msg

data Expr
  = Var String
  | Int Int
  | Negation Expr
  | Sum      Expr Expr
  | Subtr    Expr Expr
  | Product  Expr Expr
  | Division Expr Expr
  deriving (Eq, Ord, Show)

pVariable :: Parser Expr
pVariable = Var <$> lexeme
  ((:) <$> letterChar <*> many alphaNumChar <?> "variable")

pInteger :: Parser Expr
pInteger = Int <$> lexeme L.decimal


parens :: Parser a -> Parser a
parens = between (symbol "(") (symbol ")")

pTerm :: Parser Expr
pTerm = choice
  [ pVariable
  , pInteger
  , parens pExpr
  ]

pExpr :: Parser Expr
pExpr = makeExprParser pTerm operatorTable

operatorTable :: [[Operator Parser Expr]]
operatorTable =
  [ [ prefix "-" Negation
    , prefix "+" id
    ]
  , [ binary "*" Product
    , binary "/" Division
    ]
  , [ binary "+" Sum
    , binary "-" Subtr
    ]
  ]

binary :: Text -> (Expr -> Expr -> Expr) -> Operator Parser Expr
binary  name f = InfixL  (f <$ symbol name)


prefix, postfix :: Text -> (Expr -> Expr) -> Operator Parser Expr
prefix  name f = Prefix  (f <$ symbol name)
postfix name f = Postfix (f <$ symbol name)



lineComment :: Parser ()
lineComment = L.skipLineComment "#"

scn2 :: Parser ()
--scn2 = L.space space1 lineComment empty
scn2 = void $ dbg "scn2" $ (many (char ' ' <|> char '\t' <|> char '\n') <?> "scn2")

sc2 :: Parser ()
sc2 = L.space (void $ some (char ' ' <|> char '\t')) lineComment empty

lexeme2 :: Parser a -> Parser a
lexeme2 = L.lexeme sc2

pItem :: Parser String
pItem = dbg "pItem" (lexeme2 (some (alphaNumChar <|> char '-')) <?> "list item")

pItemList :: Parser (String, [String]) -- header and list items
pItemList = dbg "top" $ L.nonIndented scn2 pItemListBlock

pItemListBlock :: Parser (String, [String])
pItemListBlock = dbg "block" $ L.indentBlock scn2 p
  where
    p = do
      header <- dbg "header" pItem
      return (L.IndentMany Nothing (return . (header, )) (dbg "item" pItem))

