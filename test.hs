{-# LANGUAGE OverloadedStrings #-}

import Data.Typeable
import System.IO (hPutStr, hClose)
import System.Process.Typed
import Data.ByteString.Lazy.Char8
import System.Exit (ExitCode)
import Data.ByteString.Lazy (ByteString)

data Command = Command {
    template :: String,
    nParameters :: Int,
    stdout :: ByteString,
    stderr :: ByteString,
    code :: ExitCode
} deriving (Show)

-- A function with no input (date)
-- need to use () to indicate it doesnt receive parameters
-- need to use 'return' to extract stuff from the monad that comes out of readProcess, i think
-- this scarily seems like imperative code, but isn't. i think im getting it
-- got the type signature from ghci, :load this file, :t runDate
--      runDate :: Control.Monad.IO.Class.MonadIO m => () -> m Command
-- but it fails...
runDate () = do
    (exitCode, out, err) <- readProcess "date"
    let c = Command {
        template="date", 
        nParameters=0,
        code=exitCode,
        stdout=out,
        stderr=err
    }
    return c

runCat file = do
    (exitCode, out, err) <- readProcess (proc "cat" [file])
    let c = Command {
        template="cat [parameter]", 
        nParameters=1,
        code=exitCode,
        stdout=out,
        stderr=err
    }
    return c

main :: IO ()
main = do
    out1 <- runDate ()
    out2 <- runCat "test.txt"
    print out1
    print out2