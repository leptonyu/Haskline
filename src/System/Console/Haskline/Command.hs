module System.Console.Haskline.Command where

import System.Console.Haskline.Env(
     Env
    ,getBuffer
    ,getNextBuffer
    ,updateBuffer
    ,buffer
    ,addHistory
    ,hisind
    ,hisbuff
    ,history
    )
import System.Console.Haskline.Buffer(
     Buff
    ,mkBuff
    ,getBuff
    ,getNext
    ,insertPrev
    ,deletePrev
    ,deleteNext
    ,dropPrev
    ,dropNext
    ,lenPrev
    ,lenNext
    ,hasPrev
    ,hasNext
    ,movePrev
    ,moveNext
    ,moveStart
    ,moveEnd
    )

import System.IO(
     hGetChar
    ,stdin
    )
import Data.Map.Strict as M


data Command 
    = Move Cursor
    | Accept
    | DeletePrev
    | DeleteCurr
    | DeleteToStart
    | DeleteToEnd
    | Char Char
    | HistoryPrev
    | HistoryNext
    | Complete
    | EndOfFile
    deriving (Eq, Ord, Show, Read)

data Cursor 
    = Previous
    | Next
    | Home
    | End
      deriving (Eq, Ord, Show, Read)

type Commands = [(String,Command)]


prevStr = "\ESC[D"
nextStr = "\ESC[C"

moveLeft  :: Int -> IO ()
moveLeft  = putStr . concat . flip replicate prevStr

moveRight :: Int -> IO ()
moveRight = putStr . concat . flip replicate nextStr

replaceLine :: String -> String -> IO ()
replaceLine old new = 
    do moveLeft (length old)
       let sp = replicate (length old - length new) ' '
       putStr (new ++ sp)
       moveLeft (length sp)


commands :: Commands
commands = 
    [("\n",      Accept),        -- Enter
     ("\ESC[D",  Move Previous), -- left arrow
     ("\STX",    Move Previous), -- C-b
     ("\ESC[C",  Move Next),     -- right arrow
     ("\ACK",    Move Next),     -- C-f
     ("\SOH",    Move Home),     -- C-a
     ("\ESC[H",  Move Home),     -- Home
     ("\ENQ",    Move End),      -- C-e
     ("\ESC[F",  Move End),      -- End
     ("\DEL",    DeletePrev),    -- Backspace
     ("\ESC[3~", DeleteCurr),    -- Del
     ("\v",      DeleteToEnd),   -- C-k
     ("\NAK",    DeleteToStart), -- C-u
     ("\ESC[A",  HistoryPrev),   -- up arrow
     ("\ESC[B",  HistoryNext),   -- down arrow
     ("\t",      Complete)      -- tab
    ]

readCommand :: Commands -> IO Command
readCommand cs = hGetChar stdin >>= execCmd
    where execCmd c = ec c [(ss, command) | ((s:ss), command) <- cs, s == c]
          ec c [] = return $ Char c
          ec _ [("",command)] = return command
          ec _ cs = readCommand cs

readLoop :: Env -> IO Env
readLoop env = readCommand commands >>= runCommand
    where runCommand :: Command -> IO Env
          runCommand (Char c)      = do
                putChar c
                let ys = getNextBuffer env
                putStr ys
                moveLeft $ length ys
                readLoop $ updateBuffer env (insertPrev c)
          runCommand (Accept)      = do
                putChar '\n' 
                return $ addHistory (getBuffer env) env
          runCommand (Move Previous) 
            | hasPrev $ buffer env = do
                moveLeft  1
                readLoop $ updateBuffer env movePrev
          runCommand (Move Next)     
            | hasNext $ buffer env = do
                moveRight 1 
                readLoop $ updateBuffer env moveNext
          runCommand (Move Home)   = do
                let n = lenPrev $ buffer env
                moveLeft  n
                readLoop $ updateBuffer env moveStart
          runCommand (Move End)    = do
                let n = lenNext $ buffer env
                moveRight  n
                readLoop $ updateBuffer env moveEnd
          runCommand (HistoryPrev) = moveHistory env 1
          runCommand (HistoryNext) = moveHistory env (-1)
          runCommand (DeletePrev)    
            | hasPrev $ buffer env = do
                moveLeft 1
                let ys = getNextBuffer env ++ " "
                putStr ys
                moveLeft $ length ys
                readLoop $ updateBuffer env deletePrev
          runCommand (DeleteCurr)    
            | hasNext $ buffer env = do
                let ys = tail $ getNextBuffer env ++ " "
                putStr ys
                moveLeft $ length ys
                readLoop $ updateBuffer env deleteNext
          runCommand (DeleteToStart) = do
                let n  = lenPrev $ buffer env
                    ys = getNextBuffer env ++ replicate n ' '
                moveLeft n
                putStr ys
                moveLeft $ length ys
                readLoop $ updateBuffer env dropPrev
          runCommand (DeleteToEnd)   = do
                let n = lenNext $ buffer env
                putStr $ replicate n ' '
                moveLeft n
                readLoop $ updateBuffer env dropNext
          runCommand _             = readLoop env

updateHistory :: Env -> Int -> String -> Int -> String -> Env
updateHistory env ind old ind' new = env {
     buffer  = mkBuff new
    ,hisind  = ind'
    ,hisbuff = M.insert ind old $ hisbuff env
    }


moveHistory :: Env -> Int -> IO Env
moveHistory env step = do
    let ind = hisind env
        ind' = ind + step
    case M.lookup ind' (hisbuff env) of
        Just newLine -> do
            let line = getBuffer env
            replaceLine line newLine
            readLoop $ updateHistory env ind line ind' newLine
        Nothing -> do
            let histories = history env
            if ind' <= length histories && ind' > 0
            then do
                let line = getBuffer env
                    newLine = histories !! (ind'-1)
                replaceLine line newLine
                readLoop $ updateHistory env ind line ind' newLine
            else readLoop env


