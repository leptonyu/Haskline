{-# LANGUAGE ExistentialQuantification #-}
module System.Console.Haskline.Env where


import System.Console.Haskline.Buffer


import System.FilePath(splitFileName,(</>),pathSeparator)
import Control.Monad(forM_,mapM)
import Data.Typeable(Typeable,typeOf,cast)
import Data.Maybe(fromJust)
import Data.List(isPrefixOf,concatMap)
import Data.Map.Strict as M
import System.Directory(
    getCurrentDirectory
    ,getDirectoryContents
    ,setCurrentDirectory
    ,doesDirectoryExist
    ,getHomeDirectory
    ,canonicalizePath
    )

data Property = forall a. (Typeable a, Show a) => P a 

instance Show Property where
    show (P a) = "<" ++ show (typeOf a) ++ ">:" ++ show a

data Env = Env 
    { prompt  :: String
    , history :: [String]
    , hisind  :: Int
    , hisbuff :: M.Map Int String
    , buffer  :: BuffChar
    , envMap  :: M.Map String Property
    , complete:: [Completion]
    , actions :: Actions
    }

type CompletionSearch = Env -> String -> IO (String,[String]) 
type Completion = (Int -> Bool , CompletionSearch)

data Status = Exit Int | Next Env
type Action = Env -> [String] -> IO Status
data ActionLine = ActionLine {
     lineName  :: String
    ,lineAlias :: [String]
    ,lineDesp  :: String
    ,lineAct   :: Action
    }

instance Show ActionLine where
    show al = lineName al ++ " \t" ++ lineDesp al

type Actions = [ActionLine]


mkEnv :: String -> Actions -> Env
mkEnv p = Env p [] 0 M.empty (mkBuff []) M.empty [((==0),commandCompletion),(\_ -> True,fileCompletion)]


-- FIX-ME
fileCompletion :: CompletionSearch
fileCompletion env str = do
    let (root,file) = splitFileName str
    dirs <- getDirectoryContents root
    dirs' <- mapM (abc root) dirs
    return $ findNames dirs' file
    where abc :: FilePath -> FilePath -> IO FilePath
          abc root fl = do
            ex <- doesDirectoryExist (root ++ fl)
            return $ if ex then fl ++ [pathSeparator] else fl

commandCompletion :: CompletionSearch
commandCompletion env str = return $ findNames (fmap lineName $ actions env) str


findNames :: [String] -> String -> (String,[String])
findNames [] _ = ("",[])
findNames a x = let a' = Prelude.filter (isPrefixOf x) a in (longestCommonPrefix a',a')

longestCommonPrefix :: Eq a => [[a]] -> [a]
longestCommonPrefix [] = []
longestCommonPrefix lists = foldr1 commonPrefix lists

commonPrefix :: Eq a => [a] -> [a] -> [a]
commonPrefix (x:xs) (y:ys)
    | x == y    = x : commonPrefix xs ys
commonPrefix _ _ = []

resetEnv :: Env -> Env
resetEnv env = env { buffer = (mkBuff []), hisbuff = M.empty , hisind=0}

getProperty :: String -> Env -> Maybe Property
getProperty key env = M.lookup key (envMap env)

getString :: String -> Env -> Maybe String
getString key env = case getProperty key env of
    Nothing -> Nothing
    Just (P a) -> cast a

setProperty :: String -> Property -> Env -> Env
setProperty k v env = env { envMap = M.insert k v (envMap env)}

updatePrompt :: Env -> (String -> String) -> Env
updatePrompt env f = env { prompt = f $ prompt env }

updateBuffer :: Env -> (BuffChar -> BuffChar) -> Env
updateBuffer env f = env { buffer = f $ buffer env }

updateCompletion :: Env -> [Completion] -> Env
updateCompletion env cps = env { complete = cps ++ complete env}

addHistory :: String -> Env -> Env
addHistory "" env = env { 
      hisbuff = M.empty
    , hisind = 0
    }
addHistory cmd env = env { 
      history = cmd:history env
    , hisbuff = M.empty
    , hisind = 0
    }

getBuffer :: Env -> String
getBuffer = getBuff . buffer

getNextBuffer :: Env -> String
getNextBuffer = getNext . buffer

getPrevBuffer :: Env -> String
getPrevBuffer = getPrev . buffer

runComplete :: Env -> [String] -> String -> Maybe Char -> IO (String,[String]) 
runComplete env cmds prev mc= case find (complete env) cmds of
    Nothing -> return ("",[])
    Just cp -> cp env prev
    where find [] _ = Nothing
          find ((f,c):xs) cmd
            | f (value cmd prev mc) = Just c
            | otherwise  = find xs cmd
          value _ _     Nothing   = 0
          value cmds _ (Just ' ') = length cmds
          value cmds _ _          = length cmds - 1


-- Action

next :: Env -> IO Status
next = return . Next

exit :: Int -> IO Status
exit = return . Exit 

exitAction :: Action
exitAction _ _ = exit 0

helpAction :: Action
helpAction env _ = do
    let acts = actions env
    forM_ acts $ putStrLn . show
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
    forM_ (beautifulPrint 80 dirs) putStrLn
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
    [ActionLine "exit" ["quit","q"] "Exit the InShell"              exitAction
    ,ActionLine "help" ["?"]           "Display helpful information"   helpAction
    ,ActionLine "history" []        "Display history information"   histAction
    ,ActionLine "pwd" []            "Print the current working directory"  pwdAction
    ,ActionLine "cd"  []            "Change the current directory to DIR"   cdAction
    ,ActionLine "ls"  []            "List directory contents"       lsAction
    ,ActionLine "export"  []        "Set environment"               expoAction
    ]

findAction :: String -> Actions -> Maybe ActionLine
findAction _ [] = Nothing
findAction cmd (al:xs)
    | lineName al == cmd || elem cmd (lineAlias al) = Just al
    | otherwise = findAction cmd xs

appendAction :: Actions -> Actions -> Actions
appendAction = (++)



beautifulPrint :: Int -> [String] -> [String]
beautifulPrint width xs = doPrint width line mlen xs
    where mlen = maxLen xs
          line = floor $ fromIntegral width / (fromIntegral mlen + 1)

doPrint :: Int -> Int -> Int -> [String] -> [String]
doPrint width _ _ []    = []
doPrint width line mlen xs = let (as,bs)=splitAt line xs in fmt mlen as : doPrint width line mlen bs

maxLen :: [String] -> Int
maxLen [] = 0
maxLen (x:xs) = max (length x) (maxLen xs)

fmt :: Int -> [String] -> String
fmt width = concatMap (align ' ' $ width + 1) 


align :: a -> Int -> [a] -> [a]
align _ 0 _  = []
align a n [] = a : align a (n-1) []
align a n (x:xs) = x : align a (n-1) xs

