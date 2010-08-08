doubleMe x = x + x

doubleUs x y = x*2 + y*2

doubleSmallNumber n = if n > 100 then n else n * 2

conanO'Brien = "Hello Conan!"

xs = [1..1000 * 1000]
try = maximum xs

lucky 7 = "Lucky Seven!"
lucky n = "Nope!"

bmiTell weight height = let bmi = weight / height ^ 2 in
  case () of
    _ | bmi <= 18.5 -> "skinny!"
    _ | bmi <= 25.0 -> "normal"
    _ | bmi <= 30.0 -> "overweight!"
    _ | otherwise   -> "obese!"

data Person = Person {name::String, age::Int} deriving (Show)
