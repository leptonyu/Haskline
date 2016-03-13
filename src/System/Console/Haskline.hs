module System.Console.Haskline(
    haskline
    ) where


import Control.Monad(forM_)
import Data.Map.Strict as M
import System.Console.Haskline.Readline


type Haskline = IO Int

haskline :: String -> Actions -> Haskline
haskline prompt actions = runHaskline $ mkEnv prompt actions'
    where actions' = appendAction actions defaultActions
          runHaskline env = do 
            (cmdline,env') <- readline env
            case words cmdline of
                []         -> runHaskline env'
                (cmd:args) -> case findAction cmd actions' of
                    Nothing -> do 
                        putStrLn $ cmd ++ ": command not found"
                        runHaskline env'
                    Just action -> do
                        status <- lineAct action env' args
                        case status of 
                            Next env'' -> runHaskline env''
                            Exit code  -> return code

