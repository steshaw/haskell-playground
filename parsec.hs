import Text.ParserCombinators.Parsec

parens = do 
  { char '('
  ; level1 <- parens
  ; char ')'
  ; level2 <- parens
  ; return (max (level1+1) level2)
  } <|> return 0
