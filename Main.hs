-- | Програма обробки тексту підручника з програмування.
-- Використання:
--   prog <вхідний_файл> <довжина_слів> [<вихідний_файл>]
-- Якщо довжину не вказано, програма запитує її з клавіатури.
module Main where

import System.Environment (getArgs)
import System.Exit (exitFailure)
import System.IO
import Text.Read (readMaybe)

import TextParser (розібратиТекст, зібратиТекст)
import TextProcessor (видалитиСловаЗаДовжиною)

main :: IO ()
main = do
  hSetEncoding stdout utf8
  args <- getArgs
  case args of
    (шлях : nRow : вихід : _) -> запустити шлях nRow (Just вихід)
    [шлях, nRow]              -> запустити шлях nRow Nothing
    [шлях] -> do
      putStr "Введіть довжину слова, яке потрібно видалити: "
      hFlush stdout
      nRow <- getLine
      запустити шлях nRow Nothing
    _ -> do
      putStrLn "Використання: prog <вхідний_файл> <довжина_слів> [<вихідний_файл>]"
      exitFailure

запустити :: FilePath -> String -> Maybe FilePath -> IO ()
запустити шлях nRow вихід =
  case readMaybe nRow :: Maybe Int of
    Just n | n > 0 -> do
      сирийТекст <- читатиФайл шлях
      let текст = розібратиТекст сирийТекст
          результат = зібратиТекст (видалитиСловаЗаДовжиною n текст)
      putStrLn "--- Результат обробки ---"
      putStr результат
      case вихід of
        Just p  -> записатиФайл p результат >> putStrLn ("Збережено у файл: " ++ p)
        Nothing -> pure ()
    _ -> do
      putStrLn "Помилка: довжина слова має бути цілим числом, більшим за 0."
      exitFailure

читатиФайл :: FilePath -> IO String
читатиФайл шлях = do
  h <- openFile шлях ReadMode
  hSetEncoding h utf8
  вміст <- hGetContents h
  length вміст `seq` hClose h -- повністю прочитати файл до закриття дескриптора
  pure вміст

записатиФайл :: FilePath -> String -> IO ()
записатиФайл шлях вміст = do
  h <- openFile шлях WriteMode
  hSetEncoding h utf8
  hPutStr h вміст
  hClose h
