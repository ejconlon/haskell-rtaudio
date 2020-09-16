import Control.Concurrent.MVar (MVar, newEmptyMVar, takeMVar, tryPutMVar)
import Control.Exception (finally)
import Control.Monad (void)
import Sound.RtAudio (Api (..), ErrorCallback, Format (..), OutputStreamCallback, closeStream, createAudio,
                      getDefaultOutputDevice, openOutputStream)
import System.Timeout (timeout)

outCb :: OutputStreamCallback IO Double
outCb ob st = undefined

errCb :: MVar () -> ErrorCallback IO
errCb closeVar errCode errMsg = do
  putStrLn ("Error: " ++ show errCode ++ errMsg)
  void (tryPutMVar closeVar ())

main :: IO ()
main = do
  audio <- createAudio UnspecifiedApi
  defOut <- getDefaultOutputDevice audio
  closeVar <- newEmptyMVar
  stream <- openOutputStream audio defOut FormatFloat64 outCb (errCb closeVar)
  -- 1 second in us
  let delay = 100000
  finally (void (timeout delay (takeMVar closeVar))) (closeStream audio)
