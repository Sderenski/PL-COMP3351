module DailyOne where
    
    -- Function that takes four parameters: a, b, c, x. 
    -- Should return the value of a + b*x + c*x^2.
    quadratic :: Floating a => a -> a -> a -> a -> a
    quadratic a b c x = a + (b*x) + c*x**2

    -- Function takes a single number along with a 2-tuple which represents 
    --  a dimensional vector. Function returns a 2-tuple that is vector scaled
    scaleVector :: Num b => b -> (b, b) -> (b, b)
    scaleVector x (a,b) = (a*x, b*x)

    -- takes 2 3-tuples which are three dimensional points and find the cartesian
    -- distance between them
    tripleDistance :: Floating a => (a, a, a) -> (a, a, a) -> a
    tripleDistance (x1, y1, z1) (x2, y2, z2) = sqrt ((x2 - x1)**2 + (y2 - y1)**2 + (z2 - z1)**2)