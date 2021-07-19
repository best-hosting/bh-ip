{-# LANGUAGE GADTs #-}

import qualified Options.Applicative as O

data Option a = Option
  { optName :: String
  , optParser :: String -> Maybe a -- ^ How to parse option argument.
  }

instance Functor Option where
  fmap f (Option name p) = Option name (fmap f . p)

optMatches :: Option a -> String -> Bool
optMatches opt s = s == '-' : '-' : optName opt

data Parser a where
  NilP :: a -> Parser a
  ConsP :: Option (a -> b)
        -> Parser a -> Parser b

instance Functor Parser where
  fmap f (NilP x) = NilP (f x)
  fmap f (ConsP opt rest) = ConsP (fmap (f.) opt) rest

instance Applicative Parser where
  pure = NilP
  NilP f <*> p = fmap f p
  ConsP opt rest <*> p =
    ConsP (fmap uncurry opt) ((,) <$> rest <*> p)

-- | Create Parser for option.
option :: String -> (String -> Maybe a) -> Parser a
option name p = ConsP (fmap const (Option name p)) (NilP ())

-- | Create Parser for option using 'read' as argument parser.
optionR :: Read a => String -> Parser a
optionR name = option name p
  where
    p arg = case reads arg of
      [(r, "")] -> Just r
      _       -> Nothing

data User = User
  { userName :: String
  , userId :: Integer
  } deriving Show

parser :: Parser User
parser = User <$> option "name" Just <*> optionR "id"

runParser :: Parser a -> [String] -> Maybe (a, [String])
runParser (NilP x) args = Just (x, args)
runParser (ConsP _ _) [] = Nothing
runParser p (arg : args) =
  case stepParser p arg args of
    Nothing -> Nothing
    Just (p', args') -> runParser p' args'

stepParser :: Parser a -> String -> [String] -> Maybe (Parser a, [String])
stepParser p arg args = case p of
  NilP _ -> Nothing
  ConsP opt rest
    | optMatches opt arg -> case args of
        [] -> Nothing
        (value : args') -> do
          f <- optParser opt value
          return (fmap f rest, args')
    | otherwise -> do
        (rest', args') <- stepParser rest arg args
        return (ConsP opt rest', args')

main :: IO ()
main = undefined