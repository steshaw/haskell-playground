--
-- Inspired by:
--   http://channel9.msdn.com/Shows/Going+Deep/Erik-Meijer-and-Matthew-Podwysocki-Perspectives-on-Functional-Programming
--

type Action a = IO a
type Void = ()

main :: Action Void
main = printMsg "hello" "Steve"

printMsg :: String -> String -> IO ()
printMsg = \ msg1 msg2 -> do putStrLn msg1
                             putStrLn msg2
