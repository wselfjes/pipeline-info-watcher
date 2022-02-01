{-# LANGUAGE DataKinds           #-}
{-# LANGUAGE DeriveFunctor       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE KindSignatures      #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeApplications    #-}
{-# LANGUAGE TypeFamilies        #-}

module Data.List.FixedList where

import           Data.Either.Utils
import           Data.Proxy
import           Data.String       (IsString (..))
import           GHC.TypeLits

newtype FixedList (n :: Nat) a = FixedList
  { getFixedList :: [a]
  }
  deriving (Eq, Functor, Show)

instance (KnownNat n) => IsString (FixedList n Char) where
  fromString = unsafeFromEither . listToFixedList

listToFixedList
  :: forall n a.  KnownNat n
  => [a]
  -> Either String (FixedList n a)
listToFixedList xs
  | length xs /= n = Left ("expected list size " <> show n <> " but input list is of size " <> show (length xs))
  | otherwise = Right (FixedList xs)
  where
    n = fromIntegral (natVal (Proxy :: Proxy n))

take
  :: forall n1 n2 a.  (KnownNat n1, KnownNat n2, (<=) n2 n1)
  => FixedList n1 a
  -> FixedList n2 a
take = FixedList . Prelude.take n2 . getFixedList
  where
    n2 = fromIntegral (natVal (Proxy :: Proxy n2))
