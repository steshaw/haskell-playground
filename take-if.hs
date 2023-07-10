import Control.Monad (when)

data Status = Open | Closed
  deriving (Eq, Show)
data Issue = Issue {id :: Int, status :: Status}
  deriving (Eq, Show)

openIssues issues =
  when (any (\it -> status it == Open) issues) $
    putStrLn "There are some open issues"

main = do
  let issues =
        [ Issue 1 Open
        , Issue 2 Closed
        , Issue 3 Open
        ]

  putStrLn "Some open:"
  openIssues issues -- Prints "There are some open issues".
  let for = flip fmap
  let allClosed = for issues $ \it ->
        if status it == Open then it {status = Closed} else it
  putStrLn "All closed:"
  openIssues allClosed -- Prints nothing.
