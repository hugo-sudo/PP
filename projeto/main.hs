import System.IO
import System.Environment
main = do
    args <- getArgs
    filePercurso <- readFile (args !! 0)
    fileInteresse <- readFile (args !! 1)
    fileJson <- openFile (args !! 2) WriteMode
    contentLines <- lines (filePercurso)
    putStrLn contentLines

    
--mapM (putStrLn . show) contentLines


{- getLatitude :: FilePath -> IO String
getLatitude file = (readFile (file)) -}

