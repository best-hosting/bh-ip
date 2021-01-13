module Main where

import Data.Char
import Network.HTTP
import Text.HTML.TagSoup
import Data.Functor.Contravariant
import Data.Functor.Contravariant.Divisible
import Data.Void

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

main :: IO ()
main = haskellLastModifiedDateTime


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
    lose f  = Printer $ \x -> absurd (f x)

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

car :: Car
car = Car "Toyota" "Corolla" (Pistons 4)

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

