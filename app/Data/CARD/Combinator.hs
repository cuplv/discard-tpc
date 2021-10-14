{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE TypeFamilies #-}

module Data.CARD.Combinator where

import Data.Aeson
import Data.CARD
import Data.Maybe (catMaybes)
import GHC.Generics
import Lens.Micro.Platform

counter :: Lens' Counter Int
counter = lens (\(Counter x) -> x) (\(Counter _) x -> Counter x)

runEf s e = defineEffect e s

-- | The "empty CARD" which simply allows setting a new value and
-- provides no domain-specific conrefs. This could be replaced by
-- making Set a generic effect, in the same way that EQV is a generic
-- conref.
data CUnit a = CUnit { _cUnit :: a }
  deriving (Show,Read,Eq,Ord,Generic)

makeLenses ''CUnit

instance (Show a, Read a, Eq a, Ord a, Generic a) => CARD (CUnit a) where
  data Ef (CUnit a) = SSet a deriving (Show,Read,Eq,Ord,Generic)
  defineEffect _ (SSet a) = CUnit a
  data Cr (CUnit a) deriving (Show,Read,Eq,Ord,Generic)
  defineConflict = undefined
  defineLe = undefined

data CProduct a b = CProduct { _cFst :: a, _cSnd :: b}
  deriving (Show,Read,Eq,Ord,Generic)

type CProduct2 a = CProduct a a

type CProduct3 a = CProduct a (CProduct2 a)

type CProduct4 a = CProduct a (CProduct3 a)

makeLenses ''CProduct

instance (CARD a, CARD b) => CARD (CProduct a b) where
  data Ef (CProduct a b)
    = OnFst (Ef a)
    | OnSnd (Ef b)
  data Cr (CProduct a b)
    = OfFst (Cr a)
    | OfSnd (Cr b)
  defineEffect x = \case
    OnFst e0 -> over cFst (runEf e0) x
    OnSnd e1 -> over cSnd (runEf e1) x
  defineConflict c e = case (c,e) of
    (OfFst c0,OnFst e0) -> defineConflict c0 e0
    (OfSnd c1,OnSnd e1) -> defineConflict c1 e1
    _ -> False
  defineLe c (Effect es1) (Effect es2) = case c of
    OfFst c0 ->
      let f (OnFst e0) = Just e0
          f _ = Nothing
      in defineLe
           c0
           (Effect . catMaybes . map f $ es1)
           (Effect . catMaybes . map f $ es2)
    OfSnd c1 ->
      let f (OnSnd e1) = Just e1
          f _ = Nothing
      in defineLe
           c1
           (Effect . catMaybes . map f $ es1)
           (Effect . catMaybes . map f $ es2)

deriving instance (Show (Ef a), Show (Ef b)) => Show (Ef (CProduct a b))
deriving instance (Read (Ef a), Read (Ef b)) => Read (Ef (CProduct a b))
deriving instance (Eq (Ef a), Eq (Ef b)) => Eq (Ef (CProduct a b))
deriving instance (Ord (Ef a), Ord (Ef b)) => Ord (Ef (CProduct a b))
deriving instance (Generic (Ef a), Generic (Ef b)) => Generic (Ef (CProduct a b))

deriving instance (Show (Cr a), Show (Cr b)) => Show (Cr (CProduct a b))
deriving instance (Read (Cr a), Read (Cr b)) => Read (Cr (CProduct a b))
deriving instance (Eq (Cr a), Eq (Cr b)) => Eq (Cr (CProduct a b))
deriving instance (Ord (Cr a), Ord (Cr b)) => Ord (Cr (CProduct a b))
deriving instance (Generic (Cr a), Generic (Cr b)) => Generic (Cr (CProduct a b))
