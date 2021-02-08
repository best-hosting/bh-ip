{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}

module Main where

import Data.Char
import Network.HTTP
import Text.HTML.TagSoup
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void
import Control.Monad.Reader
import System.IO
import Data.Time.Clock
import Data.List
import Data.Foldable
import qualified Data.Map as M
import Data.Monoid
import qualified Data.Set as S

openURL :: String -> IO String
openURL x = getResponseBody =<< simpleHTTP (getRequest x)

haskellLastModifiedDateTime :: IO ()
haskellLastModifiedDateTime = do
    src <- readFile "huy"
    let nmapReport = parseTags src
    print $ fromReport nmapReport
    --writeFile "t.html" src
{-    src <- openURL "http://wiki.haskell.org/Haskell"
    let lastModifiedDateTime = fromFooter $ parseTags src
    putStrLn $ "wiki.haskell.org was last modified on " ++ lastModifiedDateTime
    where fromFooter = unwords . drop 6 . words . innerText . take 2 . dropWhile (~/= "<li id=footer-info-lastmod>")-}
  where
    fromReport :: [Tag String] -> [Host]
    fromReport  =
        map parseHost
        . sections (~== "<status state=\"up\" reason=\"arp-response\">")

data Host   = Host {ip :: String, mac :: String}
  deriving (Show)

parseHost :: [Tag String] -> Host
parseHost xs = case filter (~== "<address>") . takeWhile (~/= "</host>") $ xs of
    [x, y]  -> Host {ip = fromAttrib "addr" x, mac = fromAttrib "addr" y}
    _       -> undefined

{-main :: IO ()
main = haskellLastModifiedDateTime-}


newtype Printer a   = Printer {runPrinter :: a -> String}

string :: Printer String
string = Printer id

konst :: String -> Printer a
konst s = Printer (const s)

showP :: Show a => Printer a
showP = Printer show

int :: Printer Int
int = showP

newline :: Printer ()
newline = konst "\n"

instance Contravariant Printer where
    contramap f (Printer pb) = Printer (pb . f)

instance Divisible Printer where
    divide f (Printer py) (Printer pz) = Printer $ \x -> let (y, z) = f x in py y <> pz z
    conquer = konst ""

instance Decidable Printer where
    choose f (Printer py) (Printer pz) = Printer $ \x -> either py pz (f x)
    lose f = Printer $ \x -> absurd (f x)

infixr 4 >*<
(>*<) :: Divisible f => f a -> f b -> f (a, b)
(>*<) = divide id

infixr 4 >*
(>*) :: Divisible f => f a -> f () -> f a
(>*)  = divide (\x -> (x, ()))

infixr 4 *<
(*<) :: Divisible f => f () -> f a -> f a
(*<)  = divide (\y -> ((), y))

infixr 3 >|<
(>|<) :: Decidable f => f a -> f b -> f (Either a b)
(>|<) = choose id

data Car = Car {make :: String, model :: String, engine :: Engine}
  deriving (Show)

data Engine = Pistons Int | Rocket
  deriving (Show)

toyota :: Car
toyota = Car "Toyota" "Corolla" (Pistons 4)

engineToEither :: Engine -> Either Int ()
engineToEither (Pistons n)  = Left n
engineToEither Rocket       = Right ()

enginePrint :: Printer Engine
enginePrint = engineToEither >$< (konst "Pistons: " *< int >|< konst "Rocket")

carToTuple :: Car -> (String, (String, Engine))
carToTuple c = (make c, (model c, engine c))

carPrint :: Printer Car
carPrint = carToTuple >$<
            (   (konst "Make: " *< string >* newline)
            >*< (konst "Model: " *< string >* newline)
            >*< enginePrint
            )

newtype LogAction m msg = LogAction
    { unLogAction :: msg -> m ()
    }

instance Applicative m => Semigroup (LogAction m a) where
    LogAction action1 <> LogAction action2 =
        LogAction $ \a -> action1 a *> action2 a

instance Applicative m => Monoid (LogAction m a) where
    mempty = LogAction $ \_ -> pure ()

logStringStdout :: LogAction IO String
logStringStdout = LogAction putStrLn

instance Contravariant (LogAction m) where
    contramap f (LogAction action) = LogAction (action . f)

instance (Applicative m) => Divisible (LogAction m) where
    conquer = mempty
    divide f (LogAction actionB) (LogAction actionC) =
        LogAction $ \x -> let (b, c) = f x in actionB b *> actionC c

instance (Applicative m) => Decidable (LogAction m) where
    lose f = LogAction (absurd . f)
    choose f (LogAction actionB) (LogAction actionC) =
        LogAction (either actionB actionC . f)

cfilter :: Applicative m => (msg -> Bool) -> LogAction m msg -> LogAction m msg
cfilter p (LogAction action) = LogAction $ \a -> when (p a) (action a)

cmapM :: Monad m => (a -> m b) -> LogAction m b -> LogAction m a
cmapM f (LogAction action) = LogAction (action <=< f)

infix 5 <&
(<&) :: LogAction m msg -> msg -> m ()
(<&) = unLogAction

stringL :: LogAction IO String
stringL = logStringStdout

-- Combinator that allows to log any showable value
showL :: Show a => LogAction IO a
showL   = show >$< stringL

-- Returns a log action that logs a given string ignoring its input.
constL :: String -> LogAction IO a
--constL s = const s >$< stringL
constL s = s >$ stringL

intL :: LogAction IO Int
intL    = showL

newtype LoggerT msg m a = LoggerT
    { runLoggerT :: ReaderT (LogAction (LoggerT msg m) msg) m a
    } deriving ( Functor, Applicative, Monad, MonadIO
               , MonadReader (LogAction (LoggerT msg m) msg)
               )

class HasLog env msg m where
    getLogAction :: env -> LogAction m msg
    setLogAction :: LogAction m msg -> env -> env

instance HasLog (LogAction m msg) msg m where
    getLogAction = id
    setLogAction = const

type WithLog env msg m = (MonadReader env m, HasLog env msg m)

liftLA :: Monad m => LogAction m msg -> LogAction (LoggerT msg m) msg
liftLA la = LogAction $ \msg -> LoggerT (lift (unLogAction la msg))

usingLoggerT :: Monad m => LogAction m msg -> LoggerT msg m a -> m a
usingLoggerT la lt = runReaderT (runLoggerT lt) (liftLA la)

logMsg :: forall msg env m . WithLog env msg m => msg -> m ()
logMsg msg = do
    LogAction log <- asks getLogAction
    log msg

example :: WithLog env String m => m ()
example = do
    logMsg "Starting application..."
    logMsg "Finishing application..."

{-main :: IO ()
main = usingLoggerT logStringStdout example-}

logStringStderr :: LogAction IO String
logStringStderr = LogAction $ hPutStrLn stderr

logStringBoth :: LogAction IO String
logStringBoth = logStringStdout <> logStringStderr

data Severity = Debug | Info | Warning | Error
    deriving (Eq, Ord, Show)

data Message = Message
    { messageSeverity :: Severity
    , messageText     :: String
    }
  deriving (Show)

fmtMessage :: Message -> String
fmtMessage (Message sev txt) = "[" ++ show sev ++ "] " ++ txt

logM :: WithLog env Message m => Severity -> String -> m ()
logM sev txt = logMsg (Message sev txt)

exampleM :: WithLog env Message m => m ()
exampleM = do
    logM Debug "Starting application..."
    logM Info  "Finishing application..."

{-main :: IO ()
main = usingLoggerT (contramap fmtMessage logStringStdout) exampleM-}

{-main :: IO ()
main = usingLoggerT
    ( cfilter (\(Message sev _) -> sev > Debug)
    $ contramap fmtMessage logStringStdout
    )
    exampleM-}

data RichMessage = RichMessage
    { richMessageMsg  :: Message
    , richMessageTime :: UTCTime
    }

fmtRichMessage :: RichMessage -> String
fmtRichMessage (RichMessage msg t) = "[" ++ show t ++ "] " ++ fmtMessage msg

makeRich :: LogAction IO RichMessage -> LogAction IO Message
makeRich = cmapM toRichMessage
  where
    toRichMessage :: Message -> IO RichMessage
    toRichMessage msg = do
        time <- getCurrentTime
        pure $ RichMessage msg time

{-main :: IO ()
main = usingLoggerT
    (makeRich $ contramap fmtRichMessage logStringStdout)
    exampleM-}

carL :: LogAction IO Car
carL = carToTuple >$<
        (   (constL "Logging make.." *< stringL >* constL "Finished logging make..")
        >*< (constL "Logging model.." *< stringL >* constL "Finished logging model..")
        >*< (constL "Logging pistons.." *< (engineToEither >$< (intL >|< constL "Rocket")) >* constL "Finished logging pistons..")
        )

main :: IO ()
main = usingLoggerT carL (logMsg toyota)

{-data Stream a = Cons a (Stream a)
  deriving (Show)

headS :: Stream a -> a
headS (Cons x _ ) = x

tailS :: Stream a -> Stream a
tailS (Cons _ xs) = xs

tailsS :: Stream a -> Stream (Stream a)
tailsS xs = Cons xs (tailsS (tailS xs))

tS :: Stream Int
tS  = Cons 1 (Cons 2 (Cons 3 undefined))-}

data Stream a = a :> Stream a
  deriving (Functor, Foldable, Show)

fromList :: [a] -> Stream a
fromList xs = go (cycle xs)
  where
    go (a:rest) = a :> go rest

class Functor w => Comonad w where
    extract :: w a -> a
    duplicate :: w a -> w (w a)
    extend :: (w a -> b) -> w a -> w b
    extend f = fmap f . duplicate

instance Comonad Stream where
    extract (a :> _) = a
    duplicate w@(a :> rs) = w :> duplicate rs
    --extend f = fmap f . duplicate
    extend f w@(a :> rs) = f w :> extend f rs

infixl 4 =>>
(=>>) :: Comonad w => w a -> (w a -> b) -> w b
wx =>> f = extend f wx

countStream :: Stream Int
countStream = fromList [0..]

evens :: Stream Int
evens = fromList [0, 2..]

ix :: Int -> Stream a -> a
ix n _ | n < 0 = error "whoops"
ix 0 (a :> _) = a
ix n (_ :> rest) = ix (n - 1) rest
--ix n = extract . dropS n

dropS :: Int -> Stream a -> Stream a
dropS n = ix n . duplicate

takeS :: Int -> Stream a -> [a]
takeS n = take n . toList

rollingAvg :: Int -> Stream Int -> Stream Double
rollingAvg  = extend . windowedAvg
{-rollingAvg wn = extend go
  where
    go :: Stream Int -> Double
    go = (/ fromIntegral wn) . fromIntegral . sum . takeS wn-}

windowedAvg :: Int -> Stream Int -> Double
windowedAvg windowSize = avg . takeS windowSize
  where
    avg :: [Int] -> Double
    avg xs = fromIntegral (sum xs) / fromIntegral (length xs)

data Store s a = Store (s -> a) s
  deriving (Functor)

instance Comonad (Store s) where
    extract (Store f s0) = f s0
    duplicate (Store f s0) = Store (\s -> Store f s) s0
    --extend wf wx@(Store g s0) = Store (\s -> wf (Store g s)) s0

data StoreT s w a = StoreT (w (s -> a)) s
  deriving (Functor)

instance Comonad w => Comonad (StoreT s w) where
    extract (StoreT fw s0)  = extract fw s0
    --duplicate (StoreT fw s0) = 

inventory :: M.Map Int String
inventory = M.fromList  [ (0, "Fidget spinners")
                        , (1, "Books")
                        , (2, "Guitars")
                        , (3, "Laptops")
                        ]

warehouse :: Store Int (Maybe String)
warehouse = Store (\shelf -> M.lookup shelf inventory) 1

pos :: Store s a -> s
pos (Store f s0) = s0

peek :: s -> Store s a -> a
peek s1 (Store f s0) = f s1

peeks :: (s -> s) -> Store s a -> a
peeks g (Store f s0) = f (g s0)

seek :: s -> Store s a -> Store s a
seek s1 (Store f _) = Store f s1

seeks :: (s -> s) -> Store s a -> Store s a
seeks g (Store f s0) = Store f (g s0)

squared :: Store Int Int
squared = Store (\x -> x^2) 10

experiment :: Functor f => (s -> f s) -> Store s a -> f a
experiment g (Store f s0) = f <$> g s0

aboveZero :: Int -> Maybe Int
aboveZero n | n > 0     = Just n
            | otherwise = Nothing

withN :: Store Int (String, Int)
withN = squared =>> experiment (\n -> (show n, n))

shifted :: Store Int (String, Int)
shifted = undefined

startingGrid :: Store (Sum Int, Sum Int) Bool
startingGrid = Store checkAlive (0, 0)
  where
    checkAlive :: (Sum Int, Sum Int) -> Bool
    checkAlive coord = S.member coord livingCells

livingCells :: S.Set (Sum Int, Sum Int)
livingCells = S.fromList [(1, 0), (2, 1), (0, 2), (1, 2), (2, 2)]

drawLine :: Int -> S.Set (Sum Int, Sum Int) -> Int -> String
drawLine m ss l = map go [0..m - 1]
  where
    go :: Int -> Char
    go n    = if (Sum l, Sum n) `S.member` ss then '#' else '.'

drawSet :: Int -> S.Set (Sum Int, Sum Int) -> String
drawSet k ss = concatMap ((++ "\n") . drawLine k ss) [0..k - 1]

drawGrid :: Int -> Store (Sum Int, Sum Int) Bool -> String
drawGrid = undefined

neighbourLocations :: (Sum Int, Sum Int) -> [(Sum Int, Sum Int)]
neighbourLocations loc = mappend loc <$>
    [ (-1, -1), (-1, 0), (-1, 1)
    ,  (0, -1),           (0, 1)
    ,  (1, -1),  (1, 0),  (1, 1)
    ]

numLivingNeighbours :: Store (Sum Int, Sum Int) Bool -> Int
numLivingNeighbours = getSum . foldMap toCount . experiment neighbourLocations
  where
    toCount :: Bool -> Sum Int
    toCount True  = Sum 1
    toCount False = Sum 0

checkCellAlive :: Store (Sum Int, Sum Int) Bool -> Bool
checkCellAlive w = case (extract w, numLivingNeighbours w) of
    (True, 2) -> True
    (_, 3)    -> True
    _         -> False

step :: Store (Sum Int, Sum Int) Bool -> Store (Sum Int, Sum Int) Bool
step = extend checkCellAlive

data Env e a = Env e a
  deriving (Eq, Show, Functor)

instance Comonad (Env e) where
    extract (Env _ x) = x
    duplicate w@(Env e x) = Env e w
    extend f w@(Env e x) = Env e (f w)

askE :: Env e a -> e
askE (Env e _) = e

asksE :: (e -> e') -> Env e a -> e'
asksE f (Env e _) = f e

data Settings = Settings
                { padAmount :: Int
                , maxLength :: Int
                , padChar :: Char
                }
  deriving (Show)

getPadChar :: Env Settings a -> Char
getPadChar  = asksE padChar

context :: Env Settings String
context = Env (Settings {padAmount = 3, maxLength = 5, padChar = '*'}) "Hello world"

trunc :: Env Settings [a] -> [a]
trunc w = let mxLngth = asksE maxLength w in take mxLngth (extract w)

pad :: Env Settings String -> String
pad w = let padAmt = asksE padAmount w
            c      = asksE padChar w
        in     replicate padAmt c
            <> extract w
            <> replicate padAmt c

pad' :: Env Settings String -> String
pad' = do
    padAmt <- asksE padAmount
    c      <- asksE padChar
    txt    <- extract
    let padding = replicate padAmt c
    --return $ padding <> txt <> padding
    const padding <> extract <> const padding


infixl 5 =>=
(=>=) :: Comonad w => (w a -> b) -> (w b -> c) -> (w a -> c)
--f =>= g = \wx -> g (extend f wx)
f =>= g = \wx -> g (f <$> duplicate wx)

pipeline :: Env Settings String -> String
pipeline = trunc =>= pad

pipeline2 :: Env Settings String -> String
pipeline2 = pad =>= trunc

localE :: (e -> e') -> Env e a -> Env e' a
localE f (Env e x) = Env (f e) x

pipeline3 :: Env Settings String -> String
pipeline3 = trunc =>= pad . localE (\s -> s{padChar = '_'}) =>= pad

