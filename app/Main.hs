import System.Environment (getArgs)

import MyPandoc

main :: IO ()
main = do
    args <- getArgs
    myPandoc args