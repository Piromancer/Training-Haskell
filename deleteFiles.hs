import Data.List
import System.Directory

main' :: IO ()
main' = do
            putStr "Substring: "
            substr <- getLine
            if (substr == "") then
                putStrLn "Canceled"
            else do
                    content <- getDirectoryContents "."
                    goodContent <- return $ filter (isInfixOf substr) content
                    mapM (\x -> putStrLn ("Removing file: " ++ x)) goodContent
                    --mapM (removeFile) goodContent
                    return ()
