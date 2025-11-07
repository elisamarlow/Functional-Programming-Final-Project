import Data.Char
import System.IO
import Control.Exception
import Control.Monad (forM_)

import LTypes
import Lambek
import Parser
import PySupport
import LExp 
-- Capitalization normalizer
stdize (c:cs) = toUpper c : map toLower cs


repl :: Lexicon PythonCode -> PythonHandle -> IO ()
repl elx phandle = do
  putStr "> "
  line <- getLine
  let ws = map stdize $ words line

  let derivs = getAllDerivationsWithEnv elx ws

  case derivs of
    [] -> putStrLn "That is not a well-formed sentence"
    [single] -> do
      -- putStrLn "I can derive your sentence as follows:"
      let (env, lexp) = single
      -- putStrLn ("   " ++ prettyLExp lexp)
      let pyExpr = interpretLExp env lexp
      val <- runPythonCode phandle pyExpr
      if not (null val) && head val == '[' then
        if val == "[]" then
          putStrLn "No one"
        else
          putStrLn (val)
      else
          putStrLn val

    many -> do
      putStrLn "That sentence is ambiguous"
      forM_ many $ \(env, lexp) -> do
        let pyExpr = interpretLExp env lexp
        val <- runPythonCode phandle pyExpr
        putStrLn ("Sense [" ++ prettyLExp lexp ++ "]: " ++ val)

  repl elx phandle


main = do
  hSetBuffering stdout NoBuffering
  elx <- getLex
  db  <- getDB
  phandle <- launchPython db
  putStrLn "Starting chat..."
  repl elx phandle
  where
    getDB :: IO PythonCode
    getDB = do
      putStr "Facts about the world: "
      dname <- getLine
      mdb <- try (readFile dname) :: IO (Either IOError String)
      case mdb of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getDB
        Right db -> return (PCode db)

    getLex :: IO (Lexicon PythonCode)
    getLex = do
      putStr "Lexicon file: "
      lname <- getLine
      res <- parseFile lexicon lname
      case res of
        Left err -> putStrLn ("Error loading file " ++ show err) >> getLex
        Right (Left err) -> putStrLn ("parse error at " ++ lname ++ ":" ++ show err) >> getLex
        Right (Right lex) -> return lex