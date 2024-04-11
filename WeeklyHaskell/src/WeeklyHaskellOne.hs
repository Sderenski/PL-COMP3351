module WeeklyHaskellOne where

    --RemoveChar
    --  Consumes a single character and a string
    --  Produces a new string
    --     Removes all instances of the character removed from the OG string
    removeChar :: Char -> [Char] -> [Char]
    removeChar c [] = []
    removeChar c (q:qs) = if c == q 
                            then removeChar c qs 
                            else q : removeChar c qs 

    --RemoveWhitespace
    --   Consumes String
    --   Produces a new string
    --      removes all spaces, tabs, newlines, and carriage returns
    -- ' ' - Space, '\t' - tab, '\n' - new lines, '\r' - carriage return
    removeWhitespace :: [Char] -> [Char]
    removeWhitespace [] = []
    -- removeWhitespace (q:qs) 








    --RemovePunctuation
    --   Consumes a string
    --   Produces new string
    --      removes all commas, periods, parentheses, brackets




    --CharsToAscii
    --   consumes a string
    --   produces a new list containing ASCII values of the characters in the given string
    --      Can use the fromEnum function
    charsToAscii :: [Char] -> [Int]
    charsToAscii [] = []
    charsToAscii (q:qs) = fromEnum q : charsToAscii qs



    --asciiToChars
    --   Consumes a list of integer
    --   Produces a new list of characters created from ASCII values
    --      can use the toEnum function
    asciiToChars :: [Int] -> [Char]
    asciiToChars [] = []
    asciiToChars (q:qs) = toEnum q : asciiToChars qs



    --shiftInts
    --   Consume an integer(shift value) and list of integers(ASCII values)
    --   Produces a new list of integers where each value in the list has been shifted
    
    shiftInts :: (Ord a, Num a) => a -> [a] -> [a]
    shiftInts i [] = []
    shiftInts i (q:qs) = if (q + i) > 127 
                            then (q + i) - 128 : shiftInts i qs 
                            else q + i : shiftInts i qs

    --shiftMessage
    --    Consumes an integer (the shift value) and a string (the message)
    --    Produces a new string which is the encrypted messaged
    shiftMessage :: Int -> [Char] -> [Char]
    shiftMessage i [] = []
    shiftMessage i message = asciiToChars (shiftInts i (charsToAscii message))