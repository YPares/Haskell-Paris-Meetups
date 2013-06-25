import Control.Concurrent.Async
import Data.Functor

compter_mots f = do
    str <- readFile f  -- Je sais, j'ai dit "pas de lazy I/O"
                       -- mais vous voulez vraiment que vous
                       -- ressorte _conduit_ ?
    return $! length $ words str

moyenne = uncurry (/) . foldl (\(s,t) i -> (s+i, t+1)) (0,0)
main = do
    putStrLn "Je vais faire des trucs _en_ concurrence"
    let fichiers = ["f1.txt", "f2.txt", "f3.txt"]
    moy <- moyenne . map fromIntegral <$>
             mapConcurrently compter_mots fichiers
    putStrLn $ "La moyenne est de " ++ show moy ++ " mots"

