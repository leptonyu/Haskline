module System.Console.Haskline.Buffer where

type Buff a = ([a],[a])
type BuffChar = Buff Char
type BuffStr  = Buff String

mkBuff :: [a] -> Buff a
mkBuff xs = (reverse xs,[])

lenBuff :: Buff a -> Int
lenBuff (xs,ys) = length xs + length ys

reverseBuff :: Buff a -> Buff a
reverseBuff (xs,ys) = (ys,xs)

getBuff :: Buff a -> [a]
getBuff (xs,ys) = foldl (flip (:)) ys xs

peekPrev :: Buff a -> Maybe a
peekPrev (x:_,_) = Just x
peekPrev _       = Nothing

peekNext :: Buff a -> Maybe a
peekNext (_,y:_) = Just y
peekNext _       = Nothing

movePrev :: Buff a -> Buff a
movePrev (x:xs,ys) = (xs,x:ys)
movePrev b         = b

moveNext :: Buff a -> Buff a
moveNext (xs,y:ys) = (y:xs,ys)
moveNext b         = b

moveStart :: Buff a -> Buff a
moveStart bf = ([], getBuff bf)

moveEnd :: Buff a -> Buff a
moveEnd (xs,ys) = (foldl (flip (:)) xs ys,[])

insertNext :: a -> Buff a -> Buff a
insertNext y (xs,ys) = (xs,y:ys)

insertPrev :: a -> Buff a -> Buff a
insertPrev x (xs,ys) = (x:xs,ys)

insertsPrev :: [a] -> Buff a -> Buff a
insertsPrev as (xs,ys) = (reverse as ++xs,ys)

deleteNext :: Buff a -> Buff a
deleteNext (xs,_:ys) =(xs,ys)
deleteNext b = b 

deletePrev :: Buff a -> Buff a
deletePrev (_:xs,ys) =(xs,ys)
deletePrev b = b 

dropPrev :: Buff a -> Buff a
dropPrev (_,ys) = ([],ys)

dropNext :: Buff a -> Buff a
dropNext (xs,_) = (xs,[])

hasPrev :: Buff a -> Bool
hasPrev (_:_,_) = True
hasPrev _ = False

hasNext :: Buff a -> Bool
hasNext (_,_:_) = True
hasNext _ = False

lenPrev :: Buff a -> Int
lenPrev (xs,_) = length xs

lenNext :: Buff a -> Int
lenNext (_,ys) = length ys

getPrev :: Buff a -> [a]
getPrev = reverse . fst

getNext :: Buff a -> [a]
getNext = snd

getPrevWord :: BuffChar -> String
getPrevWord (xs,_) = reverse $ lastWord xs ""
    where lastWord ""      ys = ys
          lastWord (' ':_) ys = ys
          lastWord (x:xs)  ys = x:lastWord xs ys

getNextWord :: BuffChar -> String
getNextWord (_,ys) = firstWord ys ""
    where firstWord ""      ys = ys
          firstWord (' ':_) ys = ys
          firstWord (x:xs)  ys = x:firstWord xs ys