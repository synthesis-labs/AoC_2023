module Handy where

import           Control.Monad              (join)
import qualified Data.Set as Set
import qualified Data.ByteString.Char8      as Char8 (pack)
import qualified Data.ByteString.Lazy.Char8 as LChar8 (unpack)
import           Data.String.Interpolate    (i)
import           Network.HTTP.Client        (httpLbs, newManager, parseRequest,
                                             requestHeaders, responseBody,
                                             responseStatus)
import           Network.HTTP.Client.TLS    (tlsManagerSettings)
import           Network.HTTP.Types.Header  (hCookie)
import           Network.HTTP.Types.Status  (statusCode)
import           System.Directory           (createDirectory,
                                             doesDirectoryExist, doesFileExist)
import           System.IO                  (IOMode (ReadMode), hGetContents,
                                             openFile)

-- read contents from a file, applying a simple transform
read_data :: (String -> a) -> String -> IO a
read_data f filename = do
  pure f <*> (openFile filename ReadMode >>= hGetContents)

-- take last elements
takeLast :: Int -> [a] -> [a]
takeLast n xs = drop (length xs - n) xs

-- drop last elements
dropLast :: (Eq t, Num t) => t -> [a] -> [a]
dropLast 0 xs = xs
dropLast n xs = dropLast (n - 1) $ init xs

-- remove all whitespace from a string
trim :: String -> String
trim = unwords . words

-- merge a list into the middle of a list
insert :: [a] -> Int -> [a] -> [a]
insert es idx l = take idx l ++ es ++ drop (idx + 1) l

-- fmap inside fmap
(<$$>) :: (Functor g, Functor f) => (a -> b) -> g (f a) -> g (f b)
(<$$>) f gfa = (\fa -> f <$> fa) <$> gfa

infixl 8 <$$>

-- Always rounds closest to the second argument
div_round :: Int -> Int -> Int
div_round l r =
  if even total
    then total `div` 2
    else if l > r
           then (total - 1) `div` 2
           else (total + 1) `div` 2
  where
    total = l + r

-- Flat map!
(<$->) :: Monad m => (a -> m b) -> m a -> m b
(<$->) f ma = join $ f <$> ma

-- A range generator, which supports going from high to low
-- (which default [a .. b] doesn't)
range :: Int -> Int -> [Int]
range a b =
  if a < b
    then [a .. b]
    else [a,pred a .. b]

-- Take a list and chunk it into smaller lists
-- e.g. chunk 2 [1,2,3,4] => [[1,2], [3,4]]
chunk :: Int -> [a] -> [[a]]
chunk _ [] = []
chunk size l =
  let first = take size l
   in [first] <> chunk size (drop size l)

-- My own cool monad combinator which runs the function until we
-- hit a predicate. This should probably be adjusted to not be specific
-- to [a] but rather t a, where t is a monoid i guess? (i am on a plane
-- so can't google ... )
takeUntilM :: Monad m => (b -> Bool) -> (a -> m b) -> [a] -> m ([b])
takeUntilM _ _ [] = pure mempty
takeUntilM predicate f (a:as) = do
  b <- f a
  if not $ predicate b
    then do
      r <- takeUntilM predicate f as
      pure $ b : r
    else pure mempty

unique :: (Eq a, Ord a) => [a] -> [a]
unique = Set.toList . Set.fromList

-- Make parameters nicer
type Year = Int

type Day = Int

data WhichPuzzleInput = Example1 | Example2 | Example3 | Mine

-- Get the puzzle input, either from disk, or from http first time
--
get_puzzle_input :: WhichPuzzleInput -> Year -> Day -> IO String
get_puzzle_input which_input year day = do
  let local_path = "data/"
      local_file = case which_input of
        Example1 -> [i|input_#{year}_#{day}_example_1|]
        Example2 -> [i|input_#{year}_#{day}_example_2|]
        Example3 -> [i|input_#{year}_#{day}_example_3|]
        Mine -> [i|input_#{year}_#{day}|]
      download_url = [i|https://adventofcode.com/#{year}/day/#{day}/input|]
      downloadFile :: IO ()
      downloadFile = do
        putStrLn $
          [i|Downloading input for year #{year} day #{day} (will be cached)|]
        cookie <- readFile "cookie.txt"
        req <- parseRequest download_url
        let req0 = req {requestHeaders = [(hCookie, Char8.pack cookie)]}
        manager <- newManager tlsManagerSettings
        resp <- httpLbs req0 manager
        if statusCode (responseStatus resp) /= 200
          then do
              let body :: String = LChar8.unpack $ responseBody resp
              error $
                [i|Failed to download input for year #{year} day #{day} => #{body}|]
          else do
            let body :: String = LChar8.unpack $ responseBody resp
            writeFile (local_path <> local_file) body
            pure ()
  -- Ensure the directory exists
  _ <-
    do exists <- doesDirectoryExist local_path
       if not exists
         then createDirectory local_path
         else pure ()
  -- Does the file exist?
  _ <-
    do exists <- doesFileExist (local_path <> local_file)
       if not exists
         then downloadFile
         else pure ()
  openFile (local_path <> local_file) ReadMode >>= hGetContents



-- A filter
(-|) :: [a] -> (a -> Bool) -> [a]
(-|) = flip filter

-- map
(-+) :: [a] -> (a -> b) -> [b]
(-+) = flip (<$>)

-- flatmap
(-*) :: [a] -> (a -> [b]) -> [b]
(-*) = flip concatMap



data SeekResult a
  = Skip -- Region is continuous, so skip it
  | Continue -- Region is non-continuous, so keep searching
  | Found (Int, a) -- Found the spot!

-- A magical binary search that will search the entire space looking for
-- interesting features (such as jumps in continuous space)
seeker ::
     Int
  -> Int
  -> (Int -> a)
  -> ((Int, a) -> (Int, a) -> SeekResult a)
  -> [(Int, a)]
seeker start stop produce continuous =
  let (startv, stopv) = (produce start, produce stop)
   in case continuous (start, startv) (stop, stopv) of
        Skip -> []
        Found v -> [v]
        Continue ->
          let mid = (start + stop) `div` 2
           -- Split the region and fork the search
           in seeker start mid produce continuous ++
              seeker mid stop produce continuous