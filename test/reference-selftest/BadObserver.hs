module BadObserver (main) where
import Control.Monad (forever)
import System.IO (BufferMode(LineBuffering), hSetBuffering, stdout)
main :: IO ()
main = do
    hSetBuffering stdout LineBuffering
    forever (getLine >> putStrLn "not a valid observation")
