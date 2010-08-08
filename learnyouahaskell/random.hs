import System.Random

randomStream g =
  let (r, g') = random g
  in (r, g'):randomStream g'

randoms' g = map fst $ randomStream g

finiteRandoms 0 g = ([], g)
finiteRandoms n g =
  let rs = take n $ randomStream g
  in (map fst rs, last $ map snd rs)
