{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

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

