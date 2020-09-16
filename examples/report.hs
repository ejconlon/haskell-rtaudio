import Sound.RtAudio.Report (buildReport)
import Text.Pretty.Simple (pPrint)

main :: IO ()
main = buildReport >>= pPrint
