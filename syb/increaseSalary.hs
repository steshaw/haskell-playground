--
-- Example derived from http://research.microsoft.com/en-us/um/people/simonpj/papers/hmap/Boilerplate%20v3.ppt
--

{-# LANGUAGE DeriveDataTypeable, NoMonomorphismRestriction #-}

import Data.Generics

data Company = Company [Department]
  deriving (Show, Data, Typeable)
data Department = Department String Manager [Employee]
  deriving (Show, Data, Typeable)
data Manager = Manager {rank :: Float, manName :: String, manSalary :: Salary}
  deriving (Show, Data, Typeable)
data Employee = Employee {empName :: String, empSalary :: Salary}
  deriving (Show, Data, Typeable)
data Salary = Salary Float
  deriving (Show, Data, Typeable)

-- small, one department retail company
company1 = Company [Department "Sales" (Manager 1 "Deborah" (Salary 75000)) [Employee "Jane" (Salary 35000)]]

increaseSalary percentage (Salary salary) = Salary $ (1 + percentage / 100) * salary

-- General function operating on Salary
increaseSalaryEverywhere percentage = everywhere (mkT $ increaseSalary percentage)

-- As above but limited to Company
increaseSalaryCompanyWide :: Float -> Company -> Company
increaseSalaryCompanyWide = increaseSalaryEverywhere

salaryIncreasePercentage = 3.5

result = increaseSalaryCompanyWide salaryIncreasePercentage company1
