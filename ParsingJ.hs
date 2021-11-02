-- Execution proceeds from right to left, except that when a right parenthesis is encountered, the segment enclosed by it and its matching left parenthesis is executed, and its result replaces the entire segment and its enclosing parentheses.
-- 2. 	Adverbs and conjunctions are executed before verbs; the phrase ,"2-a is equivalent to (,"2)-a , not to ,"(2-a) . Moreover, the left argument of an adverb or conjunction is the entire verb phrase that precedes it. Thus, in the phrase +/ . */b , the rightmost adverb / applies to the verb derived from the phrase +/ . * , not to the verb * .
-- 3. 	A verb is applied dyadically if possible; that is, if preceded by a noun that is not itself the right argument of a conjunction.
-- 4. 	Certain trains form verbs and adverbs, as described in § F.
-- 5. 	To ensure that these summary parsing rules agree with the precise parsing rules prescribed below, it may be necessary to parenthesize an adverbial or conjunctival phrase that produces anything other than a noun or verb.

data Token a = Noun a | Verb a | Adverb a | Conjunction a | ParenOpen | ParenClose deriving (Show, Eq)

-- Right to left, so actually ,"2-a, a.k.a. (,"2)-a
expr0 :: [Token String]
expr0 = [Noun "a", Verb "-", Noun "2", Adverb "\"", Verb ","]

expr1 :: [Token String]
-- +/ . */b a.k.a. (+/ . *)/ b
expr1 = [Noun "b", Adverb "/", Verb "*", Conjunction ".", Adverb "/", Verb "+"]

-- parenthesize :: [Token String] -> [[Token String]]

main :: IO ()
main = pure ()
