module Data.Either.Utils where

unsafeFromEither :: (Show s) => Either s a -> a
unsafeFromEither (Right a) = a
unsafeFromEither (Left s)  = error (show s)
