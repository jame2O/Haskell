import Language.Haskell.TH (safe)
-- Functor: A type class that represents things that can be mapped over.
-- Applicative: A type class that can apply function wrapped in context to values wrapped in context.
-- Monad: something idk

safeDiv :: Int -> Int -> Maybe Int
safeDiv x y
    | y == 0 = Nothing
    | otherwise = Just (x `div` y)

twoSafeDiv :: Int -> Int -> Int -> Maybe Int
twoSafeDiv a b c = safeDiv a b >>= (\x -> safeDiv x c)


getName :: IO String
getName = do
    putStrLn "What is your name?"
    name <- getLine
    putStrLn ("Your name is " ++ name)
    return name

getAge :: IO String
getAge = do
    putStrLn "How old are you?"
    age <- getLine
    case maybeRead age of
        Just age -> do
            putStrLn ("You were born in " ++ show (2024 - age))
            return (show age)
        Nothing -> do
            putStrLn "Invalid age. Please enter a valid age"
            getAge


maybeRead :: Read a => String -> Maybe a
maybeRead s = case reads s of
    [(x, "")] -> Just x
    _ -> Nothing

getInfo :: IO String
getInfo = do
    name <- getName
    age <- getAge
    return (name ++ " " ++ age)


-- Defining a new monad
newtype Logger a = Logger { runLogger :: (a, [String])} deriving (Show) --a is the computatiom, [String] is the list of log messages
-- Now we need to define the Functor, Applicative, and Monad instances for Logger

instance Functor Logger where
    fmap f (Logger (a, log)) = Logger (f a, log) -- fmap applies the function to the computation and leaves the log unchanged

instance Applicative Logger where
    pure a = Logger (a, [])
    (Logger (f, log1)) <*> (Logger (a, log2)) = Logger (f a, log1 ++ log2)

instance Monad Logger where
    return = pure
    Logger (a, log) >>= f = let Logger (b, log') = f a in Logger (b, log ++ log')

logMsg :: String -> Logger ()
logMsg m = Logger ((), [m])

logDivide :: Show a => Fractional a => a -> a -> Logger ()
logDivide x y = do
    logMsg "Starting divide computation"
    let z = x / y
    logMsg ("The result of " ++ show x ++ " divided by " ++ show y ++ " is: " ++ show z)
    return 