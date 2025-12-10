module Main where

import System.Environment (getArgs)

ex1 :: IO ()
ex1 = putStrLn "hello, world!"

ex2 :: IO ()
ex2 = getLine >>= \str -> putStrLn str

ex3 :: IO ()
ex3 = do
  str <- getLine
  putStrLn str

ex4 :: IO ()
ex4 = do
  number <- fmap (read :: String -> Int) getLine
  print $ number + 1

encrypt' :: String -> String
encrypt' str = unwords $ map reverse $ words str

encrypt :: FilePath -> FilePath -> IO ()
encrypt fin fout = do
  contents <- readFile fin
  let encrypted = encrypt' contents
  writeFile fout encrypted

type WordsCount = Int

type LinesCount = Int

type BytesCount = Int

wc' :: String -> IO (LinesCount, WordsCount, BytesCount)
wc' contents = do
  let wordsCnt = length $ words contents
  let linesCnt = length $ lines contents
  let bytesCnt = length contents

  pure (linesCnt, wordsCnt, bytesCnt)

wc :: FilePath -> IO ()
wc fin = do
  contents <- readFile fin
  (lcnt, wcnt, bcnt) <- wc' contents
  putStrLn $ show lcnt ++ " " ++ show wcnt ++ " " ++ show bcnt

main :: IO ()
main = do
  -- fin <- getLine
  -- fout <- getLine

  -- encrypt fin fout
  args <- getArgs
  if length args /= 1
    then putStrLn "expected one argument"
    else wc $ head args
