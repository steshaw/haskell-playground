
-- From The Gentle Art of Levitation video. See https://vimeo.com/16534403

data List z = Nil | Cons z (List z)

data ListF z list = NilF | ConsF z list

newtype Mu f = In { inject :: f (Mu f) }

type List' z = Mu (ListF z)
