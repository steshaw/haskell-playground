import Control.Monad

data Status = Open | Closed
  deriving (Eq, Show)
data Issue = Issue {ident :: Int, status :: Status}
  deriving (Eq, Show)

openIssues issues =
  unless (null (filter (\it -> status it == Open) issues)) $
    putStrLn "There are some open issues"

main = do
  let issues =
        [ Issue 1 Open
        , Issue 2 Closed
        , Issue 3 Open
        ]

  putStrLn "Some open:"
  openIssues issues -- Prints "There are some open issues".
  let allClosed = (flip fmap) issues $ \it ->
        if status it == Open then it {status = Closed} else it
  putStrLn "All closed:"
  openIssues allClosed -- Prints nothing.
