module System.Console.Haskline.Readline(
     readline
    ,mkEnv
    ,history
    ) where


import System.Console.Haskline.Env
import System.Console.Haskline.Buffer as B
import System.Console.Haskline.Command


import Control.Exception (finally)
import System.IO(
     hSetEcho
    ,hSetBuffering
    ,hGetEcho
    ,hGetBuffering
    ,hPutStr
    ,hFlush
    ,stdin
    ,stdout
    ,BufferMode(NoBuffering)
    )


readline :: Env -> IO (String, Env)
readline env = do
    hPutStr stdout $ prompt env
    hFlush stdout
    env' <- withNoBuffOrEcho $ readLoop $ resetEnv env
    return (getBuffer env', env')

withNoBuffOrEcho :: IO a -> IO a
withNoBuffOrEcho m = do
    oldInBuf  <- hGetBuffering stdin
    oldOutBuf <- hGetBuffering stdout
    oldEcho   <- hGetEcho stdout
    hSetBuffering stdin  NoBuffering
    hSetBuffering stdout NoBuffering
    hSetEcho      stdout False
    finally m $ do
        hSetBuffering stdin  oldInBuf
        hSetBuffering stdout oldOutBuf
        hSetEcho      stdout oldEcho