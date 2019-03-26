module GameMaster where

    import Control.Monad (ap, liftM)
    
    import GameMasterDef
    
    -- Question 1.
    
    -- The game master for the guessing game.  The parameter is the secret number
    -- the player has to guess.
    guessingGame :: MonadGameMaster m => Integer -> m Ending
    guessingGame secret
        | secret < 1 || secret > 100 = error "invalid game"
        | otherwise = guessingGameImp secret 1 100
    
    guessingGameImp secret lo hi = do
        req1 <- gmAction lo hi
        case req1 of
            Surrender -> return (Lose secret)
            Guess i
                | i < lo || i > hi -> guessingGameImp secret lo hi
                | i == secret -> return Win
                | i < secret -> guessingGameImp secret (lo+1) hi
                | otherwise -> guessingGameImp secret lo (hi-1)

    -- Question 2.
    
    instance Functor FreeGameMaster where
        -- fmap :: (a -> b) -> FreeGameMaster a -> FreeGameMaster b
        -- If you are confident with your Monad instance, you can just write
        fmap = liftM
    
    instance Applicative FreeGameMaster where
        -- pure :: a -> FreeGameMaster a
        -- If you are confident with your Monad instance, you can just write
        pure = return
    
        -- (<*>) :: FreeGameMaster (a -> b) -> FreeGameMaster a -> FreeGameMaster b
        -- If you are confident with your Monad instance, you can just write
        (<*>) = ap
    
    instance Monad FreeGameMaster where
        -- return :: a -> FreeGameMaster a
        return a = Pure a
        -- (>>=) :: FreeGameMaster a -> (a -> FreeGameMaster b) -> (FreeGameMaster b)
        Pure a >>= k = k a
        GMAction i j f >>= k = 
            GMAction i j (\msg -> (f msg >>= k))
    instance MonadGameMaster FreeGameMaster where
        -- gmAction :: Integer -> Integer -> FreeGameMaster PlayerMsg
        gmAction i j = GMAction i j (\msg -> Pure msg)
    
    -- Question 3.
    
    testGame :: (Integer -> FreeGameMaster Ending) -> Bool
    testGame somegame =
        -- surrender right away
        bisimulates
            [Surrender]
            (somegame 10)
            (GMAction 1 100 (\_ -> Pure (Lose 10)))
        &&
        -- guess the correct number right away 
        bisimulates
            [Guess 10]
            (somegame 10)
            (GMAction 1 100 (\_ -> Pure Win))
        &&
        bisimulates
            [Guess 60]
            (somegame 60)
            (GMAction 1 100 (\_ -> Pure Win))
        &&
        -- guess smaller, then guess larger, then guess correctly
        bisimulates
            [Guess 50, Guess 70, Guess 60]
            (somegame 60)
            (GMAction 1 100 (\_ -> 
                (GMAction 2 100 (\_ -> 
                    (GMAction 2 99 (\_ -> Pure Win
                    ))))))
        &&
        -- guess out of range, then surrender
        bisimulates
            [Guess 101, Guess 0, Surrender]
            (somegame 60)
            (GMAction 1 100 (\_ -> 
                (GMAction 1 100 (\_ -> 
                    (GMAction 1 100 (\_ -> Pure (Lose 60)
                    )))))) 
        &&
        -- guess smaller, guess larger, guess out of range, guess correctly
        bisimulates
            [Guess 30, Guess 70, Guess 1, Guess 100, Guess 60]
            (somegame 60)
            (GMAction 1 100 (\_ -> 
                (GMAction 2 100 (\_ -> 
                    (GMAction 2 99 (\_ -> 
                        (GMAction 2 99 (\_ ->
                            GMAction 2 99 (\_ -> Pure Win
                            )))))))))                   

    -- rewrote test code from testGameMaster.hs
    bisimulates ps prog expected = go ps prog expected
        where
            go _ (Pure a) (Pure ex) = ex == a
            go _ gm@(Pure _) ex@(GMAction _ _ _) = False
            go (p:ps) gm@(GMAction lo1 hi1 next1) ex@(GMAction lo2 hi2 next2)
                | lo1 == lo2, hi1 == hi2 = go ps (next1 p) (next2 p)
                | otherwise = False
            go _ gm@(GMAction _ _ _) ex@(Pure _) = False