module Diccionario (Dict, mkDict, insertDict, inDict, delDict) where

newtype Dict a = Dicc [a] deriving Show

mkDict:: (Ord a)=> Dict a
mkDict = Dicc []

insertDict :: (Ord a)=> a -> Dict a -> Dict a
insertDict x (Dicc t) = Dicc x t

inDict::(Ord a) => a -> Dict a -> Bool
inDict x (Dicc t) =  x t

delDict::(Ord a)=> a -> Dict a -> Dict a
delDict x (Dicc t) = Dicc t