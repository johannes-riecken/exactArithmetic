-- Execution proceeds from right to left, except that when a right parenthesis is encountered, the segment enclosed by it and its matching left parenthesis is executed, and its result replaces the entire segment and its enclosing parentheses.
-- 2. 	Adverbs and conjunctions are executed before verbs; the phrase ,"2-a is equivalent to (,"2)-a , not to ,"(2-a) . Moreover, the left argument of an adverb or conjunction is the entire verb phrase that precedes it. Thus, in the phrase +/ . */b , the rightmost adverb / applies to the verb derived from the phrase +/ . * , not to the verb * .
-- 3. 	A verb is applied dyadically if possible; that is, if preceded by a noun that is not itself the right argument of a conjunction.
-- 4. 	Certain trains form verbs and adverbs, as described in § F.
-- 5. 	To ensure that these summary parsing rules agree with the precise parsing rules prescribed below, it may be necessary to parenthesize an adverbial or conjunctival phrase that produces anything other than a noun or verb.
--
--
-- Nouns are atoms or arrays. All nouns have a noun rank (the number of dimensions it has), a shape (the sizes/lengths of each of its dimensions) and a type (numeric, character, boxed, symbol).

import Data.Tree
import qualified Data.List.NonEmpty as NE
import Data.List.NonEmpty (NonEmpty (..))

data Token a = Noun a | Verb a | Adverb a | Conjunction a | ParenOpen | ParenClose deriving (Show, Eq)

-- 1+2
expr00 :: [Token String]
expr00 = [Noun "2", Verb "+", Noun "1"]

want00 :: Tree (Token String)
want00 = Node (Verb "+") [pure (Noun "1"), pure (Noun "2")]

-- Right to left, so actually ,"2-a, a.k.a. (,"2)-a
expr0 :: [Token String]
expr0 = [Noun "a", Verb "-", Noun "2", Adverb "\"", Verb ","]

want0 :: Tree (Token String)
want0 = Node (Verb "-") [Node (Adverb "\"") [pure (Verb ","), pure (Noun "2")], pure (Noun "a")]

expr1 :: [Token String]
-- +/ . */b a.k.a. (+/ . *)/ b
expr1 = [Noun "b", Adverb "/", Verb "*", Conjunction ".", Adverb "/", Verb "+"]

-- Each pass takes a list of trees and returns a list of trees. The list of
-- trees is in reverse order because evaluation proceeds backwards, but the
-- trees themselves have their nodes ordered conventionally.

makePassable :: NE.NonEmpty (Token String) -> [Tree (Token String)]
makePassable = fmap pure . NE.toList

processAdverbs :: [Tree (Token String)] -> [Tree (Token String)]
processAdverbs [] = [] -- can't happen
processAdverbs [x] = [x]
processAdverbs [x, y] = [x, y]
processAdverbs [Node (Noun x) xs:Node (Adverb y) []:Node (Verb z) zs:xxs] = Node (Adverb y) [Node (Verb z) zs, Node (Noun x) xs] : processAdverbs xxs
processAdverbs (x:xs) = x : processAdverbs xs

processVerbs :: [Tree (Token String)] -> [Tree (Token String)]
processVerbs [] = [] -- can't happen
processVerbs [x] = [x]
processVerbs [Node (Noun x) xs,Node (Verb y) []] = [Node (Verb y) [Node (Noun x)



processVerbs :: NE.NonEmpty (Tree (Token String)) -> NE.NonEmpty (Tree (Token String))
processVerbs (x :| []) = x :| []
processVerbs (Node (Noun x) [] :| [Node (Verb y) []]) = Node (Verb y) [Node (Noun x) []] :| []
-- processVerbs (Node (Noun x) [] :| (Node (Verb y) [] : Node (Noun z) [] : as)) = Node (Verb y) [Node (Noun x) [], Node (Noun z) []] :| processVerbs (NE.fromList as)

toTree :: NE.NonEmpty (Token String) -> Tree (Token String)
toTree (x :| []) = Node x []
-- e.g. +: 7
toTree (Noun x :| [Verb y]) = Node (Verb y) [Node (Noun x) []]
-- e.g. 3 + 7
toTree (Noun x :| [Verb y, Noun z]) = Node (Verb y) [Node (Noun z) [], Node (Noun x) []]

main :: IO ()
main = do
    print $ want00 == toTree (NE.fromList expr00)
    -- print $ want0 == toTree (NE.fromList expr0)
