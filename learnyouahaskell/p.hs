data Node t = Leaf t | Branch [(Node t)]

-- this is the emitter
emit :: Node Char -> [Char]
emit (Leaf n)    = [n]
emit (Branch ns) = "(" ++ (concat (map emit ns)) ++ ")"

main = putStrLn (emit (Branch [
                          Leaf 'A',
                          Leaf 'B',
                          Leaf 'C',
                          Branch [
                              Leaf 'D',
                              Leaf 'E',
                              Branch [
                                  Leaf 'F',
                                  Leaf 'G',
                                  Leaf 'H'
                              ],
                              Leaf 'I'
                          ],
                          Leaf 'J'
                      ]))
