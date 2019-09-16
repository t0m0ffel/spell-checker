module Main where

import Data.Tree
import Data.Maybe
import Data.List hiding (insert)
import Data.Char
import Prelude hiding (min)
import Data.Ord hiding (min)
import System.IO
import System.Environment
import Control.Monad
import System.IO.Unsafe


-- | The beginning of everything.
main::IO() 
main = do   
                        args <- getArgs
                        case args of
                                [dic,txt,b] -> do
                                                                dicf <- readFile dic
                                                                txtf <- readFile txt
                                                                writeFile "output.txt" ("")
                                                                ui (words txtf) 3 (insertAll (words $ dicf)) ("t"==b)
                                [dic,txt] -> do
                                                                dicf <- readFile dic
                                                                txtf <- readFile txt
                                                                writeFile "output.txt" ("")
                                                                ui (words txtf) 3 (insertAll (words $ dicf)) False
                                _ -> do
                                                putStrLn ("Please use your text, wordlist and t(if you want to work with the probality matrices) as arguments.")
ui::[String] -- ^List with words from text
    ->Int    -- ^Indicates how many words should be displayed
    ->Tree (Char,Bool) -- ^Wordlist as a Trie
    ->Bool -- ^Indicates whether to use matrices or not
    ->IO()
ui [] _ _ _  = putStrLn "Finished"
ui (s:ss) count t b = do
                                        putStrLn ("Input: "++s)
                                        if checkWord s t
                                        then do
                                                        appendFile  "output.txt" (s++" ")
                                                        ui ss count t b
                                                else do
                                                        go count
                                                         where
                                                                cw = getCorrectedWords s t b
                                                                go count' =do
                                                                                        putStrLn ("Suggestions:")
                                                                                        mapM_ print $ takeWithoutDups count' cw
                                                                                        putStrLn ("Type m for more suggestions")
                                                                                        word <- getLine
                                                                                        if word == "m"
                                                                                        then  go (count'+3)
                                                                                        else
                                                                                                if elemIndex word (takeWithoutDups count'  cw) /= Nothing
                                                                                                then do
                                                                                                                appendFile  "output.txt" (word++" ")
                                                                                                                ui (ss) 3 t b
                                                                                                else do
                                                                                                         putStrLn "Try again"
                                                                                                         go count'

-- |Gives a list of suggestions for a typo.
getCorrectedWords::String -- ^Typo
                                        ->Tree (Char,Bool) -- ^Wordlist as a Trie
                                        ->Bool -- ^indicates whether to use matrices or not
                                        ->[String] -- ^List with suggestions for typo
getCorrectedWords s  t b  = map fst  									
                                                        $ sortBy (comparing  snd)
                                                        $ readOff (insertWord (init s) t)
 where
                init::String->[(Char,Int)]
                init s  = (go   [(' ',0)] s 0 '@')
                 where
                        go  m [] _ _ = m
                        go  m (s:ss) i di10 = go (m++[(s,i+del di10 s)]) ss  (i+del di10 s) s


                insertWord::[(Char,Int)]->Tree (Char,Bool)->Tree (Char,Int,Bool)
                insertWord  m t = go m ' ' t
                 where
                                go::[(Char,Int)]->Char->Tree (Char,Bool)->Tree (Char,Int,Bool)
                                go m lastchar (Node (x,y) xs)
                                                        | x == ' ' = (Node (x,lev,y) (map (go (insertChar  x lastchar m) ' ') xs))
                                                        | mincost > 3= (Node (x,lev,y) [])
                                                        | True =   (Node ( x,lev,y) (map (go array x) xs))
                                                                where
                                                                                mincost = snd $ min array
                                                                                array =  insertChar  x lastchar m
                                                                                lev= snd $ last array

                insertChar:: Char->Char->[(Char,Int)]->[(Char,Int)]
                insertChar c lastchar mms =  go 0  mms (' ',0)
                 where
                                go::Int->[(Char,Int)]->(Char,Int)->[(Char,Int)]
                                go  i m di1jt
                                                 |i==length m =  []
                                                 |i==0 =  if c==dij1c
                                                                 then (dij1c, dij1):(go (i+1)  m (dij1c, dij1))
                                                                 else  (dij1c,( dij1)+ins lastchar c ):(go (i+1)  m (dij1c,dij1+ins lastchar c ))
                                                 |True =  if c==dij1c
                                                                   then   (dij1c,di1j1):(go (i+1)  m (dij1c,di1j1))
                                                                   else (dij1c,dij):(go (i+1)  m (dij1c,dij))
                                                        where

                                                                        di1j1t = (m!!(i-1))
                                                                        dij1t = m!!i

                                                                        dij1c = fst (m!!i)
                                                                        di1j1c = fst (m!!(i-1))

                                                                        dij = cost c dij1c lastchar di1j1c [di1j, di1j1,dij1]

                                                                        di1j = snd di1jt
                                                                        dij1 = snd dij1t

                                                                        di1j1= snd di1j1t


                cost::Char->Char->Char->Char->[(Int)]->Int
                cost c1 c2 lci lcd a= go  a
                 where go (d:s:i:[])  = minimum  [d+del lcd c2 ,
                                                                                        s+sub c2 c1,
                                                                                        i+ins lci c1  ]
                del::Char->Char->Int
                del c1 c2 = if b then gv else 1
                 where gv = getValue ((\x-> if x== ' ' then '@' else x) $ toLower c1 ) (toLower c2)  delCM

                sub::Char->Char->Int
                sub c1 c2 =  if b then gv
                                        else 1--
                 where gv =  getValue   (toLower c1)  (toLower c2)  subCM

                ins::Char->Char->Int
                ins c1 c2 = if b then gv else 1
                 where gv = getValue ((\x-> if x== ' ' then '@' else x) $ toLower c1 ) (toLower c2) addCM

                getValue::Char->Char->[(Char,[(Char,Int)])]->Int
                getValue c1 c2 arr =  (fromMaybe 99999 $ lookup c2 $ fromMaybe [] $ lookup c1 arr)


                delCM::[(Char,[(Char,Int)])]
                delCM= getMatrixWithCosts $ unsafePerformIO delM
                 where
                                delM = do
                                                        x <- readFile "confusion-matrix-del.txt"
                                                        return  $ stringArray $ lines x

                addCM::[(Char,[(Char,Int)])]
                addCM = getMatrixWithCosts $ unsafePerformIO addM
                         where
                                        addM = do
                                                x <- readFile "confusion-matrix-add.txt"
                                                return  $ stringArray $ lines x

                subCM::[(Char,[(Char,Int)])]
                subCM = getMatrixWithCosts $ unsafePerformIO subM
                 where
                                subM =  do
                                                x <- readFile "confusion-matrix-sub.txt"
                                                return  $ stringArray $ lines x

-- |Transforms all word list from a file to a two dimensional array.
stringArray::[String]->[(Char,[(Char,Int)])]
stringArray (a:arr) =  map (\(x:xs,y)->(x,zip (filter (/=' ') a) y)) 
                                        $ map ((\(x:xs)->(x,map (\x-> read x ::Int) xs)))
                                        $ map words arr



-- |Change matrix with absolute frequency to matrix with costs
getMatrixWithCosts::[(Char,[(Char,Int)])]->[(Char,[(Char,Int)])]
getMatrixWithCosts m = go m (getColumnSums m)
 where 
                go::[(Char,[(Char,Int)])]->[Int]->[(Char,[(Char,Int)])]
                go xs arr = map (\(x1,y1)->(x1,map (\((x,y),z)->(x,transformToCosts y z  )) (zip (y1) arr))) xs

                getColumnSums::[(Char,[(Char,Int)])]->[Int]
                getColumnSums arr = map sum $ map (map snd)
                                                                        $ transpose
                                                                        $ map snd arr


-- |Transforms a values for the frequency of yx and x to costs.
transformToCosts::Int->Int->Int
transformToCosts y z  = if y==0 then 9999 else (round $ -log ((fromIntegral y) / (fromIntegral z)))


-- |Checking whether a word is already in the trie .
checkWord::String->Tree (Char,Bool)->Bool
checkWord (s:ss) t = go (toLower s :ss) t || go (s:ss) t
        where
                        go::String->Tree (Char,Bool)->Bool
                        go (s:[]) (Node (x,y) xs) = ((lookupT s xs) /= Nothing) && (\(Node (_,x) _) ->x) (xs!!(fromJust i))
                                where i =  lookupT s xs
                        go (s:ss) (Node (x,y) xs) = if i==Nothing
                                                                                then False
                                                                                else go ss (xs!!(fromJust i))
                                where i =  lookupT s xs


-- |Insert a list of words into the trie.
insertAll::[String]->Tree (Char,Bool)
insertAll xs = go (stringToTree " ") xs
 where 
        go::Tree (Char,Bool)->[String]->Tree (Char,Bool)
        go t [] = t
        go t (x:xs) = go (insertT x t) xs

-- |insert a word into the trie
insertT::String->Tree (Char,Bool)->Tree (Char,Bool)
insertT (x:xs) (Node (y,b) ys)
                                        |e == Nothing = (Node (y,b)  (stringToTree (x:xs):ys))
                                        |xs == "" = (Node (y,b) (updateL ys (fromJust e) changeN))
                                        |True = (Node (y,b) (updateL ys (fromJust e) (insertT xs)))
                where
                                e = lookupT x ys


-- |Returns Just the index of a char in a root node or nothing.
lookupT::Char->[Tree (Char,Bool)]->Maybe Int
lookupT c ts = go ts 0
 where
                go [] _ = Nothing
                go  (t:ts) i =  if ((\(Node (l,_) _)->l) t) == c
                                                then Just i
                                                else go ts (i+1)


-- |Uses a function on the nth element in a list of tries.
updateL::[Tree (Char,Bool)]->Int->(Tree (Char,Bool)->Tree (Char,Bool))->[Tree (Char,Bool)]
updateL ts i f = replaceNth i (f (ts!!i))  (ts)
 where
        replaceNth::Int->Tree (Char,Bool)->[Tree (Char,Bool)]->[Tree (Char,Bool)]
        replaceNth n newVal (x:xs)
                | n == 0 = newVal:xs
                | otherwise = x:replaceNth (n-1) newVal xs


-- |Converts a word in to a list trie.
stringToTree::String->Tree (Char,Bool)
stringToTree (x:xs) 
                | xs == "" = (Node (x,True) [])
                | True = (Node (x,False) [stringToTree xs])

-- |Returns all words in a trie. (with duplicates)
readOff::Tree (Char,Int,Bool)->[(String,Int)]
readOff t =   concat  $ go t "" 
 where
                go::Tree (Char,Int,Bool)->String->[[(String,Int)]]
                go (Node (x,y,z) []) s =  [[(s++[x],y)]]
                go (Node (' ',_,_) xs) s = [summary | t <- xs , summary <- go t ""]
                go (Node (x,y,z) xs) s =  if z  then
                                                                                [summary++[(s++[x],y)] | t <- xs , summary <- go t (s++[x])]
                                                                                else
                                                                                [summary | t <- xs , summary <- go t (s++[x])]

-- |Sets the root node of a trie to true.
changeN:: Tree (Char,Bool)->Tree (Char,Bool)
changeN (Node (x,_) ts) = (Node (x,True) ts) 

-- |Returns the minimal tuple of a list of tuples
min :: Ord a => [(t, a)] -> (t, a)
min (xs) = head $  sortBy (comparing snd) xs

-- |Take the first n elements from a list without duplicates
takeWithoutDups::Eq a=> Int->[a]->[a]
takeWithoutDups i xs = go i xs []
 where  
        go 0 _ ys = ys
        go i (x:xs) ys = if elemIndex x ys == Nothing
                                         then  go (i-1) xs (ys++[x])
                                         else  go (i) xs (ys)
        go _ _ ys = ys


