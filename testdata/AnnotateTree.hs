import Data.Tree

t0 = Node "A" [
        Node "B" [
            Node "D" [],
            Node "E" [
                Node "F" [],
                Node "G" [],
                Node "H" []
            ]
        ],
        Node "C" []
    ]

annotateRows :: Tree a -> Tree (Int, a)
annotateRows (Node x []) = Node (0,x) []
annotateRows (Node x [y]) = Node (n,x) [y']
    where y'@(Node (n,_) _) = annotateRows y
annotateRows (Node x [y,z]) = Node (n0 + n1 + 1,x) [y', Node (n0 + n1 + 1,z'') zs] where
    y'@(Node (n0,y'') ys) = annotateRows y
    z'@(Node (n1,z'') zs) = annotateRows z
annotateRows (Node x [a,b,c]) = Node (n0 + n1 + 1,x) [a', Node (n0 + n1 + 1,b'') bs, Node (n0 + n1 + n2 + 2,c'') cs] where
    a'@(Node (n0,a'') as) = annotateRows a
    b'@(Node (n1,b'') bs) = annotateRows b
    c'@(Node (n2,c'') cs) = annotateRows c

t1 = Node "A" []
t2 = Node "A" [
        Node "B" []
    ]
t3 = Node "A" [
        Node "B" [],
        Node "C" []
    ]
t4 = Node "A" [
        Node "B" [],
        Node "C" [],
        Node "D" []
    ]
t5 = Node "A" [
        Node "B" [
            Node "D" [],
            Node "E" []
        ],
        Node "C" []
    ]

main :: IO ()
main = do
    putStrLn . drawTree . fmap show . annotateRows $ t1
    putStrLn . drawTree . fmap show . annotateRows $ t2
    putStrLn . drawTree . fmap show . annotateRows $ t3
    putStrLn . drawTree . fmap show . annotateRows $ t4
    putStrLn . drawTree . fmap show . annotateRows $ t5
