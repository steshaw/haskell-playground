headOr =
  \a -> \xs ->
    case xs of
      [] -> a
      (h : _) -> h
