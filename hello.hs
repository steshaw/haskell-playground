--
-- Inspired by:
--   http://channel9.msdn.com/Shows/Going+Deep/Erik-Meijer-and-Matthew-Podwysocki-Perspectives-on-Functional-Programming
--

import Control.Monad ((>=>))
import Data.Map as M hiding (map)

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

type Employee = String
data Department = Accounts | Products
  deriving (Show, Eq, Ord)
data Country = US | UK | Australia
  deriving (Show, Eq, Ord)
data Currency = USD | GBP | AUD
  deriving (Show, Eq, Ord)

emp2dept :: M.Map Employee Department
emp2dept = M.fromList [("Fred", Accounts), ("Bob", Products), ("Wilma", Accounts)]

dept2country :: M.Map Department Country
dept2country = M.fromList [(Accounts, UK), (Products, US)]

country2currency :: M.Map Country Currency
country2currency = M.fromList [(UK, GBP), (US, USD)]

employeesDepartment :: Employee -> Maybe Department
employeesDepartment emp = M.lookup emp emp2dept

departmentsCountry :: Department -> Maybe Country
departmentsCountry dept = M.lookup dept dept2country

countriesCurrency :: Country -> Maybe Currency
countriesCurrency country = M.lookup country country2currency

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

eg1 :: [(Employee, Maybe Currency)]
eg1 = currenciesFor ["Fred", "Wilhelm", "Wilma", "Bob", "Rob"]
  where
    currenciesFor emps = zip emps $ map employeeCurrency'' emps
