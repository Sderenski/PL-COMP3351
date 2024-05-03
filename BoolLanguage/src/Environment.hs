module Environment where
    import BoolDefinitions
    -- Maintain Association between names and values
    -- *** Data Structure Idea Behind it ***

    -- Create an Empty Environment
    emptyEnv :: [(String, ValueType)]
    emptyEnv = [] 

    -- Extend an Environment with a new Name and Value pair
    -- **  Make this faster at some point
    extendEnv :: (String, ValueType) -> [(String, ValueType)] -> [(String, ValueType)]
    extendEnv (key, val) env = (key, val) : env

    -- Apply the Environment 
    --   (Lookup the name in the Association)
    applyEnv :: String -> [(String, ValueType)] -> ValueType
    applyEnv _ [] = error "Variable is Undefined"
    applyEnv key ((k, val) : remain) = if key == k
                                        then val
                                        else applyEnv key remain

    -- *** Functional Approach to it ***

    -- Functions can take functions as parameters and return them as well

    -- The \key is a lambda function, it is creating annous function like () => {} in JS
    emptyEnv' = \key -> error "Variable Undefined"

    extendEnv' (key, value) fenv = \k -> if k == key 
                                                    then value
                                                    else (fenv k)

    applyEnv' k fenv = fenv k

