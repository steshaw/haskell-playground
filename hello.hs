--
-- Inspired by:
--   http://channel9.msdn.com/Shows/Going+Deep/Erik-Meijer-and-Matthew-Podwysocki-Perspectives-on-Functional-Programming
--

import Control.Monad ((>=>))

type Action a = IO a
type Void = ()

main :: Action Void
main = printMsg "hello" "Steve"

printMsg :: String -> String -> IO ()
printMsg = \ msg1 msg2 -> do putStrLn msg1
                             putStrLn msg2

type Parser a = String -> [(a, String)]

recChar :: Char -> Parser ()
recChar char = \s -> case s of
  (first : rest) | first == char -> [((), rest)]
  otherwise -> []

data Employee
data Department
data Country
data Currency

employeesDepartment :: Employee -> Maybe Department
employeesDepartment = error "todo"

departmentsCountry :: Department -> Maybe Country
departmentsCountry = error "todo"

countriesCurrency :: Country -> Maybe Currency
countriesCurrency = error "todo"

employeeCurrency :: Employee -> Maybe Currency
employeeCurrency emp =
  employeesDepartment emp >>= \dept ->
  departmentsCountry dept >>= \country ->
  countriesCurrency country

employeeCurrency' :: Employee -> Maybe Currency
employeeCurrency' emp = do
  dept <- employeesDepartment emp
  country <- departmentsCountry dept
  countriesCurrency country

employeeCurrency'' :: Employee -> Maybe Currency
employeeCurrency'' = employeesDepartment >=> departmentsCountry >=> countriesCurrency
