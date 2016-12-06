module Main where

import System.Environment

main :: IO ()
main = do
    args <- getArgs
    val1 <- return $ read $ args !! 0
    val2 <- return $ read $ args !! 1
    putStrLn $ (++) "+ :" $ show $ val1 + val2
    putStrLn $ (++) "- :" $ show $ val1 - val2
    putStrLn $ (++) "* :" $ show $ val1 * val2
    putStrLn $ (++) "/ :" $ show $ val1 / val2

