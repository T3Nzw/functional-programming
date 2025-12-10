# Входно-изходни операции. Монада за състояние

## Входно-изходни операции

- изходни операции

  - `putChar :: Char -> IO ()`;
  - `putStr :: String -> IO ()`;
  - `putStrLn :: String -> IO ()`;
  - `print :: Read a => a -> IO ()` (еквивалентно на `putStrLn . show`);
  - `return :: a -> IO a`.

- входни операции

  - `getChar :: IO Char`;
  - `getLine :: IO String`;
  - `getContents :: IO String`.

- работа с файлове:

  - `readFile :: FilePath -> IO String`;
  - `writeFile :: FilePath -> String -> IO ()`;
  - `withFile :: FilePath -> IOMode -> (Handle -> IO r) -> IO r`;
  - `hGetLine :: Handle -> IO String`;
  - `hPutStrLn :: Handle -> String -> IO ()`;
  - `hFlush :: Handle -> IO ()`.

  ```hs
  data IOMode
    = ReadMode
    | WriteMode
    | AppendMode
    | ReadWriteMode
  ```

Други полезни:

- `read :: Read a => String -> a`;
- `readMaybe :: Read a => String -> Maybe a`;
- `getArgs :: IO [String]` - аргументи от командния ред;
- `try :: IO a -> IO (Either IOException a)`;
- `catch :: IO a -> (IOException -> IO a) -> IO a`.
