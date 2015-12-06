module Handler where 

import System.IO.Error

handler :: IOError -> IO ()
handler ex
  | isAlreadyInUseError  ex = putStrLn "File already in use"
  | isDoesNotExistError ex = putStrLn "File not found"
  | isPermissionError ex = putStrLn "Access to file is denied"
  | otherwise = ioError ex