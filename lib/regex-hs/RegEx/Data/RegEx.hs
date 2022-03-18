{-# OPTIONS_GHC -Wno-missing-methods #-}

module RegEx.Data.RegEx where

import Data.Bifunctor (Bifunctor (bimap))
import Data.Function (fix, (&))
import Data.String (IsString (fromString))

{- Notes for lottaquestions
   To load up this program in a REPL, go to the project directory of the file and run:
   nix develop -c cabal repl
   To load up the program directly from its git repo run:
   nix develop 'gitlab:smunix.talks/regex-hs' -c cabal repl
   Checking the type of a regex:
   :t "ab(c)?" :: RegEx
-}

{- RegEx : regular expression data structure -}
{-
data RegEx
  = None -- 'matches no string'
  | Eps -- 'Epsilon, matches empty strings'
  | Sym Char -- 'a'
  | Then RegEx RegEx -- 'ab'
  | Or RegEx RegEx -- 'a|b'
  | Star RegEx -- '(ab)*'
  | Plus RegEx -- 'a+'
  deriving (Show, Eq)
-}

data RegEx where
  None :: RegEx
  Eps :: RegEx
  Sym :: Char -> RegEx
  Then :: RegEx -> RegEx -> RegEx
  Choice :: RegEx -> RegEx -> RegEx
  Star :: RegEx -> RegEx
  Dot :: Char -> RegEx
  -- Plus :: RegEx -> RegEx
  deriving (Show, Eq)

instance Num RegEx where
  fromInteger 0 = None
  fromInteger 1 = Eps
  fromInteger _ = error "not supported"

  None + a = a
  a + None = a
  a + b = Choice a b

  None * a = None
  a * None = None
  Eps * a = a
  a * Eps = a
  a * b = Then a b

-- >>> [0 :: RegEx, 1, sym 'a', sym 'a' * sym 'b']
-- [None,Eps,Sym 'a',Then (Sym 'a') (Sym 'b')]

-- >>> "(ab" :: RegEx
-- Then (Sym 'a') (Sym 'b')

-- (def)
-- abc(def)
-- abc(de(fg)?hijkl)
-- a(de(fg)?)

-- >>> "nick" :: RegEx
-- Then (Then (Then (Sym 'n') (Sym 'i')) (Sym 'c')) (Sym 'k')

-- >>> "n(ic)(oa)k" :: RegEx
-- Then (Then (Then (Sym 'n') (Then (Sym 'i') (Sym 'c'))) (Then (Sym 'o') (Sym 'a'))) (Sym 'k')

-- >>> "(nick)" :: RegEx
-- Then (Then (Then (Sym 'n') (Sym 'i')) (Sym 'c')) (Sym 'k')

-- >>> "n(ic)(oa)k" :: RegEx
-- Then (Sym 'n') (Then (Then (Sym 'i') (Sym 'c')) (Then (Then (Sym 'o') (Sym 'a')) (Sym 'k')))

-- >>> "n?" :: RegEx
-- Choice (Sym 'n') Eps

-- >>> "n+" :: RegEx
-- Then (Sym 'n') (Star (Sym 'n'))

-- >>> "n*" :: RegEx
-- Star (Sym 'n')

-- >>> "n(ab)?" :: RegEx
-- Then (Sym 'n') (Choice (Then (Sym 'a') (Sym 'b')) Eps)

-- >>> "n(ab)+" :: RegEx
-- Then (Sym 'n') (Then (Then (Sym 'a') (Sym 'b')) (Star (Then (Sym 'a') (Sym 'b'))))

-- >>> "(ab)?" :: RegEx
-- Choice (Then (Sym 'a') (Sym 'b')) Eps

-- >>> "[ab]" :: RegEx
-- Choice (Sym 'a') (Sym 'b')

-- >>> "([ab])?" :: RegEx
-- Choice (Choice (Sym 'a') (Sym 'b')) Eps

-- >>> "[ab]?" :: RegEx
-- Choice (Choice (Sym 'a') (Sym 'b')) Eps

-- >>> "[ab]+" :: RegEx
-- Then (Choice (Sym 'a') (Sym 'b')) (Star (Choice (Sym 'a') (Sym 'b')))

-- >>> "[ab]*" :: RegEx
-- Star (Choice (Sym 'a') (Sym 'b'))

-- >>> "[a(cd)+b]*" :: RegEx
-- Star (Choice (Sym 'a') (Choice (Then (Then (Sym 'c') (Sym 'd')) (Star (Then (Sym 'c') (Sym 'd')))) (Sym 'b')))

-- >>> "a|b" :: RegEx
-- Choice (Sym 'a') (Sym 'b')

-- >>> "(a|bc)+" :: RegEx
-- Then (Choice (Sym 'a') (Then (Sym 'b') (Sym 'c'))) (Star (Choice (Sym 'a') (Then (Sym 'b') (Sym 'c'))))

-- >>> "[a|bc]" :: RegEx
-- Choice (Sym 'a') (Choice (Sym 'b') (Sym 'c'))

-- >>> "[abc]" :: RegEx
-- Choice (Sym 'a') (Choice (Sym 'b') (Sym 'c'))

instance IsString RegEx where
  fromString = fst . parse 1
    where
      parse :: RegEx -> [Char] -> (RegEx, [Char])
      parse r (parseThen r -> o@(_, [])) = o
      parse r (parseThen r -> (r', s')) = parse r' s'

      parseThen :: RegEx -> [Char] -> (RegEx, [Char])
      parseThen r [] = (r, [])
      parseThen r (')' : cs) = (r, cs)
      parseThen r ('(' : (parseThen 1 -> (r', ('?' : (bimap ((r *) . ((r' + 1) *)) id . parseThen 1 -> o))))) = o
      parseThen r ('(' : (parseThen 1 -> (r', ('+' : (bimap ((r *) . ((r' & plus) *)) id . parseThen 1 -> o))))) = o
      parseThen r ('(' : (parseThen 1 -> (r', ('*' : (bimap ((r *) . ((r' & star) *)) id . parseThen 1 -> o))))) = o
      parseThen r ('(' : (parseThen 1 -> (r', bimap ((r *) . (r' *)) id . parseThen 1 -> o))) = o
      parseThen r ('[' : (parseChoice 0 -> (r', ('?' : (bimap ((r *) . ((r' + 1) *)) id . parseThen 1 -> o))))) = o
      parseThen r ('[' : (parseChoice 0 -> (r', ('+' : (bimap ((r *) . ((r' & plus) *)) id . parseThen 1 -> o))))) = o
      parseThen r ('[' : (parseChoice 0 -> (r', ('*' : (bimap ((r *) . ((r' & star) *)) id . parseThen 1 -> o))))) = o
      parseThen r ('[' : (parseChoice 0 -> (r', bimap ((r *) . (r' *)) id . parseThen 1 -> o))) = o
      parseThen r (c : '?' : (bimap ((r *) . ((sym c + 1) *)) id . parseThen 1 -> o)) = o
      parseThen r (c : '*' : (bimap ((r *) . ((sym c & star) *)) id . parseThen 1 -> o)) = o
      parseThen r (c : '+' : (bimap ((r *) . ((sym c & plus) *)) id . parseThen 1 -> o)) = o
      parseThen r (c : '|' : (bimap ((r *) . (sym c +)) id . parseThen 1 -> o)) = o
      parseThen r (c : (bimap ((r *) . (sym c *)) id . parseThen 1 -> o)) = o

      parseChoice :: RegEx -> String -> (RegEx, String)
      parseChoice r (']' : cs) = (r, cs)
      parseChoice r ('(' : (parseThen 1 -> (r', ('*' : (bimap ((r +) . ((r' & star) +)) id . parseChoice 0 -> o))))) = o
      parseChoice r ('(' : (parseThen 1 -> (r', ('+' : (bimap ((r +) . ((r' & plus) +)) id . parseChoice 0 -> o))))) = o
      parseChoice r ('(' : (parseThen 1 -> (r', ('?' : (bimap ((r +) . ((r' + 0) +)) id . parseChoice 0 -> o))))) = o
      parseChoice r ('(' : (parseThen 1 -> (r', bimap ((r +) . (r' +)) id . parseChoice 0 -> o))) = o
      parseChoice r ('|' : (bimap (r +) id . parseChoice 0 -> o)) = o
      parseChoice r (c : (bimap ((r +) . (sym c +)) id . parseChoice 0 -> o)) = o

-------------------------
-- Smart constructors  --
-------------------------
sym :: Char -> RegEx
sym a
  | a == '.' = Dot a
  | otherwise = Sym a

plus :: RegEx -> RegEx
plus a = a * star a

star :: RegEx -> RegEx
star None = 0
star Eps = 1
star (Star (Then a (Star ((== a) -> True)))) = star a
star a@(Star {}) = a
star a = Star a
