{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

import Text.Megaparsec
import Text.Megaparsec.Char
import Text.Megaparsec.Debug
import qualified Control.Monad.State.Strict as S
import qualified Text.Megaparsec.Char.Lexer as L
import qualified Data.Text as T
import Data.Text (Text)
import Data.Void
import Control.Monad
import Control.Monad.Identity

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

