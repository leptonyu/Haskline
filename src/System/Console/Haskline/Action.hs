module System.Console.Haskline.Action where


import Control.Monad(forM_)
import System.Console.Haskline.Env
import Data.Map.Strict as M

import System.Directory(
    getCurrentDirectory
    ,getDirectoryContents
    ,setCurrentDirectory
    ,doesDirectoryExist
    ,getHomeDirectory
    )

data Status = Exit Int | Next Env
type Action = Env -> [String] -> IO Status
type SystemAction = Actions -> Action
data BaseAction = A Action | S SystemAction

data ActionLine = ActionLine {
     lineName  :: String
    ,lineAlias :: [String]
    ,lineDesp  :: String
    ,lineAct   :: BaseAction
    }

type Actions = [ActionLine]

next :: Env -> IO Status
next = return . Next

exit :: Int -> IO Status
exit = return . Exit 

exitAction :: Action
exitAction _ _ = exit 0

helpAction :: SystemAction
helpAction actions env _ = do
    forM_ actions $ \action -> putStrLn $ lineName action ++ " \t" ++ lineDesp action
    next env 

histAction :: Action
histAction env _ = do
    forM_ (reverse $ history env) putStrLn
    next env

pwdAction :: Action
pwdAction env _ = do 
    getCurrentDirectory >>= putStrLn
    next env

cdAction :: Action
cdAction env [] = next env
cdAction env ["-"] = case getString "old_pwd" env of
    Just v -> cdAction env [v]
    Nothing -> do
        putStrLn "cd: OLDPWD not set"
        next env
cdAction env (x:_) = do
    x' <- fullpath x
    check <- doesDirectoryExist x'
    case check of
        True -> do
            old <- getCurrentDirectory
            setCurrentDirectory x'
            next $ setProperty "old_pwd" (P old) env
        False -> do
            putStrLn $ "cd: " ++ x' ++ ": No such file or directory"
            next env

fullpath :: FilePath -> IO FilePath
fullpath path = if head path == '~' 
    then do
        home <- getHomeDirectory 
        return $ home ++ tail path
    else return path

lsAction :: Action
lsAction env [] = lsAction env ["."]
lsAction env (x:_) = do
    x' <- fullpath x
    dirs <- getDirectoryContents x'
    forM_ dirs putStrLn
    next env

expoAction :: Action
expoAction env []  = do
    forM_ (M.assocs $ envMap env) $ \(k,v) -> putStrLn $ k ++ "=" ++ show v
    next env
expoAction env [k] = do
    case getProperty k env of
        Just v -> putStrLn $ k ++ "=" ++ show v
        Nothing -> return ()
    next env
expoAction env (k:v:[])   = expoAction env (k:v:"str":[])
expoAction env (k:v:p:_)  = case p of
    "int"  -> next $ setProperty k (P (read v::Int)) env
    "bool" -> next $ setProperty k (P (read v::Bool)) env
    _      -> next $ setProperty k (P v) env

defaultActions :: Actions
defaultActions = 
    [ActionLine "exit" ["quit","q"] "Exit the InShell"              $ A exitAction
    ,ActionLine "help" []           "Display helpful information"   $ S helpAction
    ,ActionLine "history" []        "Display history information"   $ A histAction
    ,ActionLine "pwd" []            "Print the current working directory"  $ A pwdAction
    ,ActionLine "cd"  []            "Change the current directory to DIR"  $ A  cdAction
    ,ActionLine "ls"  []            "List directory contents"       $ A lsAction
    ,ActionLine "export"  []        "Set environment"               $ A expoAction
    ]

findAction :: String -> Actions -> Maybe ActionLine
findAction _ [] = Nothing
findAction cmd (al:xs)
    | lineName al == cmd || elem cmd (lineAlias al) = Just al
    | otherwise = findAction cmd xs

appendAction :: Actions -> Actions -> Actions
appendAction = (++)