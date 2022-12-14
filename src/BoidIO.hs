module BoidIO (loadFlock, saveFlock) where

import Boid (Boid, newBoid)
import System.IO (Handle, IOMode (ReadMode, WriteMode), hClose, hGetLine, hIsEOF, hPrint, withFile)

loadFlock :: String -> IO [Boid]
loadFlock file = withFile file ReadMode readFlockFile

readFlockFile :: Handle -> IO [Boid]
readFlockFile hdl = do
  isEOF <- hIsEOF hdl
  ( if isEOF
      then return []
      else
        ( do
            line <- hGetLine hdl
            bs <- readFlockFile hdl
            case newBoid $ map read (words line) of
              Just b -> return (b : bs)
              Nothing -> return bs
        )
    )

saveFlock :: Maybe String -> Int -> [Boid] -> IO Int
saveFlock outDir n = case outDir of
  Just dn -> \bs -> do
    withFile file WriteMode $ writeFlockFile bs
    return (n + 1)
    where
      file = dn ++ "/" ++ show n ++ ".txt"
  Nothing -> \_ -> return 0

writeFlockFile :: [Boid] -> Handle -> IO ()
writeFlockFile [] hdl = hClose hdl
writeFlockFile (b : bs) hdl = do
  hPrint hdl b
  writeFlockFile bs hdl