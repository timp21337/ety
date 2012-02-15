import Language.English.EtymologyOnline

main :: IO ()
main = do
  (title,entry) <- getRandomEntry
  putStrLn title
  putStrLn entry
