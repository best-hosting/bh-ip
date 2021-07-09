{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE TypeApplications  #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import Text.Megaparsec.Pos
import qualified Control.Monad.State.Strict as S
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void
import Data.List
import Control.Monad
import Control.Monad.Identity
import Control.Monad.Combinators.Expr
import qualified Data.Char as C
import qualified Data.List.NonEmpty as NE
import qualified Data.Set as Set
import Data.IORef
import System.IO.Unsafe

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
      --return (L.IndentMany Nothing (return . (header, )) (dbg "item" pItem))
      return (L.IndentMany Nothing (return . (header, )) (dbg "item" pItem))

pItemListNested :: Parser (String, [(String, [String])])
pItemListNested = L.nonIndented scn2 (L.indentBlock scn2 p)
  where
    p = do
      header <- pItem
      return (L.IndentSome Nothing (return . (header, )) pItemListBlock)

pItemListNested2 :: Parser (String, [String])
pItemListNested2 = L.nonIndented scn2 (L.indentBlock scn2 p)
  where
    p = do
      header <- pItem
      return (L.IndentSome Nothing (return . (header, )) pLineFold)

pLineFold :: Parser String
pLineFold = L.lineFold scn2 $ \sc' ->
  let ps = some (alphaNumChar <|> char '-') `sepBy1` try sc'
  in unwords <$> ps <* scn2 -- (1)

unfortunateParser :: Parser ()
unfortunateParser = failure (Just EndOfInput) (Set.fromList es)
  where
    es = [Tokens (NE.fromList "a"), Tokens (NE.fromList "b")]

data Custom = NotKeyword Text
  deriving (Eq, Show, Ord)

instance ShowErrorComponent Custom where
  showErrorComponent (NotKeyword txt) = T.unpack txt ++ " is not a keyword"

type ParserC = Parsec Custom Text

notKeyword :: Text -> ParserC a
notKeyword = customFailure . NotKeyword

data CustomO
  = TrivialWithLocation
    [String] -- position stack
    (Maybe (ErrorItem Char))
    (Set.Set (ErrorItem Char))
  | FancyWithLocation
    [String] -- position stack
    (ErrorFancy Void) -- Void, because we do not want to allow to nest Customs
  deriving (Eq, Ord, Show)

instance ShowErrorComponent CustomO where
  showErrorComponent (TrivialWithLocation stack us es) =
    parseErrorTextPretty (TrivialError @Text @Void undefined us es)
      ++ showPosStack stack
  showErrorComponent (FancyWithLocation stack cs) =
    parseErrorTextPretty (FancyError @Text undefined (Set.singleton cs))
      ++ showPosStack stack

showPosStack :: [String] -> String
showPosStack = intercalate ", " . fmap ("in " ++)

type ParserO = Parsec CustomO Text

inside :: String -> ParserO a -> ParserO a
inside location p = do
  r <- observing p
  case r of
    Left (TrivialError _ us es) ->
      fancyFailure . Set.singleton . ErrorCustom $
        TrivialWithLocation [location] us es
    Left (FancyError _ xs) -> do
      let f :: ErrorFancy CustomO -> ErrorFancy CustomO
          f (ErrorFail msg) = ErrorCustom $
            FancyWithLocation [location] (ErrorFail msg)
          f (ErrorIndentation ord rlvl alvl) = ErrorCustom $
            FancyWithLocation [location] (ErrorIndentation ord rlvl alvl)
          f (ErrorCustom (TrivialWithLocation ps us es)) = ErrorCustom $
            TrivialWithLocation (location:ps) us es
          f (ErrorCustom (FancyWithLocation ps cs)) = ErrorCustom $
            FancyWithLocation (location:ps) cs
      fancyFailure (Set.map f xs)
    Right x -> return x

inside' :: String -> ParserO a -> ParserO a
inside' location = do
    region go
  where
    go :: ParseError T.Text CustomO -> ParseError T.Text CustomO
    go (TrivialError p us es)   = FancyError p . Set.singleton . ErrorCustom
                                    $ TrivialWithLocation [location] us es
    go (FancyError p xs) = FancyError p (Set.map f xs)
      where
        f :: ErrorFancy CustomO -> ErrorFancy CustomO
        f (ErrorFail msg) = ErrorCustom $
              FancyWithLocation [location] (ErrorFail msg)
        f (ErrorIndentation ord rlvl alvl) = ErrorCustom $
              FancyWithLocation [location] (ErrorIndentation ord rlvl alvl)
        f (ErrorCustom (TrivialWithLocation ps us es)) = ErrorCustom $
              TrivialWithLocation (location:ps) us es
        f (ErrorCustom (FancyWithLocation ps cs)) = ErrorCustom $
              FancyWithLocation (location:ps) cs

myParser :: ParserO String
myParser = some (char 'a') *> some (char 'b')

runParserO :: IO ()
runParserO = do
  parseTest (inside "foo" myParser) "aaacc"
  parseTest (inside "foo" $ inside "bar" myParser) "aaacc"

runParserO' :: IO ()
runParserO' = do
  parseTest (inside' "foo" myParser) "aaacc"
  parseTest (inside' "foo" $ inside' "bar" myParser) "aaacc"

withPredicate3
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate3 f msg p = do
  o <- getOffset
  r <- p
  if f r
    then return r
    else region (setErrorOffset o) (fail msg)

withPredicate3'
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate3' f msg p = do
  o <- getOffset
  region (setErrorOffset o) $ do
    r <- p
    if f r
      then return r
      else fail msg

withPredicate4
  :: (a -> Bool)       -- ^ The check to perform on parsed input
  -> String            -- ^ Message to print when the check fails
  -> Parser a          -- ^ Parser to run
  -> Parser a          -- ^ Resulting parser that performs the check
withPredicate4 f msg p = do
  o <- getOffset
  r <- p
  if f r
    then return r
    else parseError (FancyError o (Set.singleton (ErrorFail msg)))

data T a = T a
  deriving (Show)

defT :: Monoid a => T a
defT = T mempty

tRef :: Monoid a => IORef (T a)
tRef = unsafePerformIO (newIORef defT)

{-f :: IO ()
f = atomicWriteIORef tRef defT-}

tRef2 :: T a -> IORef (T a)
tRef2 s = unsafePerformIO (newIORef s)

f2 :: Monoid a => T a -> IO ()
f2 v = atomicWriteIORef (tRef2 v) v

f4 :: Monoid a => T a -> IO (T a)
f4 v = atomicModifyIORef (tRef2 v) (\_ -> (v, v))

{-f5 :: Monoid a => IO (T a)
f5 = atomicModifyIORef (tRef2 defT) (\_ -> (defT, defT))-}

f3 :: Monoid a => T a -> ()
f3 (T v) = let x = T (v <> mempty) in ()
