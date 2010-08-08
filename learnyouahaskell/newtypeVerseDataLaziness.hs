
newtype Foo = Foo Int

fooHello = case undefined of Foo _ -> "Hello"

data Bar = Bar Int

barHello = case undefined of Bar _ -> "Hello"

main = do
  print fooHello
  print barHello
