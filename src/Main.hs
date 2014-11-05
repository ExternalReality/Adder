module Main where

import Prelude hiding (and, or)

main :: IO ()
main = undefined

data Bit = High | Low
  deriving (Show)

halfAdder :: Bit -> Bit -> (Bit, Bit)
halfAdder inputOne inputTwo = (inputOne `xor` inputTwo , inputOne `and` inputTwo)

xor :: Bit -> Bit -> Bit
xor High Low = High
xor Low High = High
xor _      _        = Low  

and :: Bit -> Bit -> Bit
and High High = High
and _        _       = Low 

fullAdder :: Bit -> Bit -> Bit -> (Bit, Bit)
fullAdder inputOne inputTwo carryIn = 
   (sumOutputTwo,  carryOutput `or` carryOutputTwo)     
  where 
    (sumOutput, carryOutput)              = halfAdder inputOne inputTwo
    (sumOutputTwo, carryOutputTwo) = halfAdder sumOutput carryIn

or  :: Bit -> Bit -> Bit
or  Low Low = Low
or _        _      = High 
