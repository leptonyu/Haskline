{-# LANGUAGE ExistentialQuantification #-}
module System.Console.Haskline.Env where


import System.Console.Haskline.Buffer

import Data.Typeable(Typeable,typeOf,cast)
import Data.Maybe(fromJust)
import Data.Map.Strict as M

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
    } deriving Show


mkEnv :: String -> Env
mkEnv p = Env p [] 0 M.empty (mkBuff []) M.empty

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


addHistory :: String -> Env -> Env
addHistory cmd env = env { 
      history = if length cmd == 0 then history env else cmd : history env
    , hisbuff = M.empty
    , hisind = 0
    }

getBuffer :: Env -> String
getBuffer = getBuff . buffer

getNextBuffer :: Env -> String
getNextBuffer = getNext . buffer