
import Prelude hiding (lookup)
import System.IO
import Data.Maybe (fromMaybe)
import Data.Map (Map, fromList, lookup)


data PLSentence =
  PLAtomic Int |
  PLNot PLSentence |
  PLAnd PLSentence PLSentence |
  PLOr PLSentence PLSentence |
  PLIf PLSentence PLSentence |
  PLIff PLSentence PLSentence
  deriving (Eq, Show)

-- Running this function as 'plSize (PLAnd (PLAtomic 2) (PLNot (PLOr (PLAtomic 2) (PLAtomic 2))))'
-- returns a value of 3 nodes.

plSize :: PLSentence -> Int
plSize (PLAtomic i) = 0
plSize (PLNot sentence) = 1 + plSize sentence
plSize (PLAnd sentence1 sentence2) = 1 + plSize sentence1 + plSize sentence2
plSize (PLOr sentence1 sentence2) = 1 + plSize sentence1 + plSize sentence2
plSize (PLIf sentence1 sentence2) = 1 + plSize sentence1 + plSize sentence2
plSize (PLIff sentence1 sentence2) = 1 + plSize sentence1 + plSize sentence2

-- Interpretation will be provided to the function in the form (fromList [(Int, Bool),..])

type Interpretation = Map Int Bool

-- 'evaluate' function was run with following values: 'evaluate (PLAnd (PLAtomic 5) (PLOr (PLAtomic 2) (PLAtomic 3))) (fromList [(2, True), (3, False), (5, False)]) '
-- giving value: False

evaluate :: PLSentence -> Interpretation -> Bool
evaluate (PLAtomic i) ls = fromMaybe False (lookup i ls)
evaluate (PLNot sentence) ls = not (evaluate sentence ls)
evaluate (PLAnd sentence1 sentence2) ls = evaluate sentence1 ls && evaluate sentence2 ls
evaluate (PLOr sentence1 sentence2) ls = evaluate sentence1 ls || evaluate sentence2 ls
evaluate (PLIf sentence1 sentence2) ls = not (evaluate sentence1 ls) || evaluate sentence2 ls
evaluate (PLIff sentence1 sentence2) ls = evaluate sentence1 ls == evaluate sentence2 ls


data NORSentence =
  NORAtomic Int |
  NOR NORSentence NORSentence

-- Running the function as 'norSize (NOR (NOR (NORAtomic 3) (NORAtomic 3)) (NORAtomic 3))'
-- returns a value of 2 nodes.

norSize :: NORSentence -> Int
norSize (NORAtomic i) = 0
norSize (NOR sentence1 sentence2) = 1 + norSize sentence1 + norSize sentence2

-- Running the function as 'evaluateNOR (NOR (NORAtomic 2) (NORAtomic 3)) (fromList [(2, True), (3, True)])'
-- returns a value of 'False'.

evaluateNOR :: NORSentence -> Interpretation -> Bool
evaluateNOR (NORAtomic i) ls = fromMaybe False (lookup i ls)
evaluateNOR (NOR sentence1 sentence2) ls = not (evaluateNOR sentence1 ls || evaluateNOR sentence2 ls)
