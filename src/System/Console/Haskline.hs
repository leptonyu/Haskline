module System.Console.Haskline(
    haskline
    ) where


import Control.Monad(forM_)
import Data.Map.Strict as M
import System.Console.Haskline.Action
import System.Console.Haskline.Readline


type Haskline = IO Int

haskline :: String -> Actions -> Haskline
haskline prompt actions = runHaskline $ mkEnv prompt
    where actions' = appendAction actions defaultActions
          runHaskline env = do 
            (cmdline,env') <- readline env
            case words cmdline of
                []         -> runHaskline env'
                (cmd:args) -> case findAction cmd actions' of
                    Nothing -> runHaskline env'
                    Just action -> runAction action args env'
          runAction act args env = case lineAct act of
            S ac -> do
                status <- ac actions' env args
                case status of
                    Next env' -> runHaskline env'
                    Exit code  -> return code
            A ac -> do
                status <- ac env args
                case status of
                    Next env' -> runHaskline env'
                    Exit code  -> return code

