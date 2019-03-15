module GameMaster where

    import Control.Monad (ap, liftM)
    
    import GameMasterDef
    
    -- Question 1.
    
    -- The game master for the guessing game.  The parameter is the secret number
    -- the player has to guess.
    guessingGame :: MonadGameMaster m => Integer -> m Ending
    guessingGame secret
        | secret < 1 || secret > 100 = error "invalid game"
        | otherwise = error "TODO"
    
    
    -- Question 2.
    
    instance Functor FreeGameMaster where
        -- fmap :: (a -> b) -> FreeGameMaster a -> FreeGameMaster b
        -- If you are confident with your Monad instance, you can just write
        -- fmap = liftM
    
    instance Applicative FreeGameMaster where
        -- pure :: a -> FreeGameMaster a
        -- If you are confident with your Monad instance, you can just write
        -- pure = return
    
        -- (<*>) :: FreeGameMaster (a -> b) -> FreeGameMaster a -> FreeGameMaster b
        -- If you are confident with your Monad instance, you can just write
        -- (<*>) = ap
    
    instance Monad FreeGameMaster where
        -- return :: a -> FreeGameMaster a
    
        -- (>>=) :: FreeGameMaster a -> (a -> FreeGameMaster b) -> (FreeGameMaster b)
    
    instance MonadGameMaster FreeGameMaster where
        -- gmAction :: Integer -> Integer -> FreeGameMaster PlayerMsg
    
    
    -- Question 3.
    
    testGame :: (Integer -> FreeGameMaster Ending) -> Bool
    testGame = error "TODO"