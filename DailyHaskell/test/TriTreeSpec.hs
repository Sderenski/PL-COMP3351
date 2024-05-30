module TriTreeSpec where

    import Test.Hspec

    import TriTree 

    main :: IO ()
    main = hspec spec

    spec :: Spec
    spec = do
        describe "insert" $ do 
            context "insert 5 Empty" $
                it "should be Leaf 5" $
                    (insert 5 Empty) `shouldBe` Leaf 5

            context "insert 10 (Leaf 5)" $
                it "should be Node 5 10 Empty Empty Empty" $
                    (insert 10 (Leaf 5)) `shouldBe` Node 5 10 Empty Empty Empty
            
            context "insert 14 (Node 5 10 Empty Empty Empty)" $
                it "should be Node 5 10 Empty Empty (Leaf 14)" $
                    (insert 14 (Node 5 10 Empty Empty Empty)) `shouldBe` Node 5 10 Empty Empty (Leaf 14)
        
        describe "insertList" $ do
            context "insertList [15,13,11,16,12,18,9,17,10] Empty" $
                it "should be Node 10 17 (Leaf 9) (Node 12 16 (Leaf 11) (Node 13 15 Empty Empty Empty) Empty) (Leaf 18)" $
                    (insertList [15,13,11,16,12,18,9,17,10] Empty) `shouldBe` Node 10 17 (Leaf 9) (Node 12 16 (Leaf 11) (Node 13 15 Empty Empty Empty) Empty) (Leaf 18)

            context "insertList [43,39,20,3,41,37] Empty" $
                it "should be Node 37 41 (Node 3 20 Empty Empty Empty) (Leaf 39) (Leaf 43)" $
                    (insertList [43,39,20,3,41,37] Empty) `shouldBe` Node 37 41 (Node 3 20 Empty Empty Empty) (Leaf 39) (Leaf 43)
            
            context "insertList [28,25,23,22,1,24,21] Empty" $
                it "should be Node 21 24 (Leaf 1) (Node 22 23 Empty Empty Empty) (Node 25 28 Empty Empty Empty)" $
                    (insertList [28,25,23,22,1,24,21] Empty) `shouldBe` Node 21 24 (Leaf 1) (Node 22 23 Empty Empty Empty) (Node 25 28 Empty Empty Empty)

        describe "search" $ do 
            context "search 10 (Node 5 10 Empty Empty Empty)" $
                it "should be True" $
                    (search 10 (Node 5 10 Empty Empty Empty)) `shouldBe` True
            
            context "search 12 (Node 5 10 Empty Empty (Leaf 14))" $
                it "should be False" $
                    (search 12 (Node 5 10 Empty Empty (Leaf 14))) `shouldBe` False
            
            context "search 7 (Node 5 10 (Leaf 2) (Node 6 9 Empty (Leaf 7) Empty) (Leaf 12))" $
                it "should be True" $
                    (search 7 (Node 5 10 (Leaf 2) (Node 6 9 Empty (Leaf 7) Empty) (Leaf 12))) `shouldBe` True

        describe "identical" $ do
            context "identical (Leaf 2) (Leaf 2)" $
                it "should be True" $
                    (identical (Leaf 2) (Leaf 2)) `shouldBe` True

            context "identical (Node 8 10 Empty (Leaf 5) Empty) (Node 8 10 Empty (Leaf 5) Empty)" $
                it "should be True" $
                    (identical (Node 8 10 Empty (Leaf 5) Empty) (Node 8 10 Empty (Leaf 5) Empty)) `shouldBe` True
            
            context "identical (Node 8 10 Empty (Leaf 5) Empty) (Node 4 15 (Leaf 3) Empty Empty)" $
                it "should be False" $
                    (identical (Node 8 10 Empty (Leaf 5) Empty) (Node 4 15 (Leaf 3) Empty Empty)) `shouldBe` False

        describe "treeMap" $ do 
            context "treeMap (2*) (Leaf 2)" $
                it "should be Leaf 4" $
                    (treeMap (2*) (Leaf 2)) `shouldBe` Leaf 4
            
            context "treeMap (2*) (Node 7 14 (Leaf 3) (Node 8 12 Empty Empty Empty) (Leaf 22))" $
                it "should be Node 14 28 (Leaf 6) (Node 16 24 Empty Empty Empty) (Leaf 44)" $
                    (treeMap (2*) (Node 7 14 (Leaf 3) (Node 8 12 Empty Empty Empty) (Leaf 22))) `shouldBe` Node 14 28 (Leaf 6) (Node 16 24 Empty Empty Empty) (Leaf 44)

            context "treeMap (2*) Empty" $
                it "should be Empty" $
                    (treeMap (2*) Empty) `shouldBe`Empty

        describe "treeFoldPreOrder" $ do
            context "treeFoldPreOrder (\acc x -> acc + x) 0 (Node 10 17 (Leaf 9) (Node 12 16 (Leaf 11) (Node 13 15 Empty Empty Empty) Empty) (Leaf 18))" $
                it "should be 121" $
                    (treeFoldPreOrder (\acc x -> acc + x) 0 (Node 10 17 (Leaf 9) (Node 12 16 (Leaf 11) (Node 13 15 Empty Empty Empty) Empty) (Leaf 18))) `shouldBe` 121
            
            context "treeFoldPreOrder (\acc x -> acc + x) 0 (Node 37 41 (Node 3 20 Empty Empty Empty) (Leaf 39) (Leaf 43))" $
                it "should be 121" $
                    (treeFoldPreOrder (\acc x -> acc + x) 0 (Node 37 41 (Node 3 20 Empty Empty Empty) (Leaf 39) (Leaf 43))) `shouldBe` 183
            
            context "treeFoldPreOrder (\acc x -> acc + x) 0 (Node 21 24 (Leaf 1) (Node 22 23 Empty Empty Empty) (Node 25 28 Empty Empty Empty))" $
                it "should be 121" $
                    (treeFoldPreOrder (\acc x -> acc + x) 0 (Node 21 24 (Leaf 1) (Node 22 23 Empty Empty Empty) (Node 25 28 Empty Empty Empty))) `shouldBe` 144

        
        describe "treeFoldInOrder" $ do
            context "treeFoldInOrder (\acc x -> acc + x) 0 (Node 10 17 (Leaf 9) (Node 12 16 (Leaf 11) (Node 13 15 Empty Empty Empty) Empty) (Leaf 18))" $
                it "should be 121" $
                    (treeFoldInOrder (\acc x -> acc + x) 0 (Node 10 17 (Leaf 9) (Node 12 16 (Leaf 11) (Node 13 15 Empty Empty Empty) Empty) (Leaf 18))) `shouldBe` 121

            context "treeFoldInOrder (\acc x -> acc + x) 0 (Node 37 41 (Node 3 20 Empty Empty Empty) (Leaf 39) (Leaf 43))" $
                it "should be 183" $
                    (treeFoldInOrder (\acc x -> acc + x) 0 (Node 37 41 (Node 3 20 Empty Empty Empty) (Leaf 39) (Leaf 43))) `shouldBe` 183
            
            context "treeFoldInOrder (\acc x -> acc + x) 0 (Node 21 24 (Leaf 1) (Node 22 23 Empty Empty Empty) (Node 25 28 Empty Empty Empty))" $
                it "should be 144" $
                    (treeFoldInOrder (\acc x -> acc + x) 0 (Node 21 24 (Leaf 1) (Node 22 23 Empty Empty Empty) (Node 25 28 Empty Empty Empty))) `shouldBe` 144

            
        describe "treeFoldPostOrder" $ do
            context "treeFoldPostOrder (\acc x -> acc + x) 0 (Node 10 17 (Leaf 9) (Node 12 16 (Leaf 11) (Node 13 15 Empty Empty Empty) Empty) (Leaf 18))" $
                it "should be 121" $
                    (treeFoldPostOrder (\acc x -> acc + x) 0 (Node 10 17 (Leaf 9) (Node 12 16 (Leaf 11) (Node 13 15 Empty Empty Empty) Empty) (Leaf 18))) `shouldBe` 121

            context "treeFoldPostOrder (\acc x -> acc + x) 0 (Node 37 41 (Node 3 20 Empty Empty Empty) (Leaf 39) (Leaf 43))" $
                it "should be 183" $
                    (treeFoldPostOrder (\acc x -> acc + x) 0 (Node 37 41 (Node 3 20 Empty Empty Empty) (Leaf 39) (Leaf 43))) `shouldBe` 183
            
            context "treeFoldPostOrder (\acc x -> acc + x) 0 (Node 21 24 (Leaf 1) (Node 22 23 Empty Empty Empty) (Node 25 28 Empty Empty Empty))" $
                it "should be 144" $
                    (treeFoldPostOrder (\acc x -> acc + x) 0 (Node 21 24 (Leaf 1) (Node 22 23 Empty Empty Empty) (Node 25 28 Empty Empty Empty))) `shouldBe` 144