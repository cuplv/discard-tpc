{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE RankNTypes #-}

module TPC.State where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.CARD
import GHC.Generics
import Lens.Micro.Platform

type CustomerId = String

type ItemId = String

type OrderId = Int

type CarrierId = String

type WarehouseId = String

-- | The "empty CARD" which simply allows setting a new value and
-- provides no domain-specific conrefs. This could be replaced by
-- making Set a generic effect, in the same way that EQV is a generic
-- conref.
data Setable a = Setable a
  deriving (Show,Read,Eq,Ord,Generic)

instance (Show a, Read a, Eq a, Ord a, Generic a) => CARD (Setable a) where
  data Ef (Setable a) = SSet a deriving (Show,Read,Eq,Ord,Generic)
  defineEffect (Setable _) (SSet a) = Setable a
  data Cr (Setable a) deriving (Show,Read,Eq,Ord,Generic)
  defineConflict = undefined
  defineLe = undefined

-- | A generic CARD for a maybe value, providing a guard for
-- confirming that the value is Nothing.
data Justable a = Justable (Maybe a)
  deriving (Show,Read,Eq,Ord,Generic)

instance (Show a, Read a, Eq a, Ord a, Generic a) => CARD (Justable a) where
  data Ef (Justable a) 
    = SetJust a
    | SetNothing
    deriving (Show,Read,Eq,Ord,Generic)
  defineEffect (Justable _) (SetJust a) = Justable (Just a)
  defineEffect (Justable _) SetNothing = Justable Nothing
  data Cr (Justable a) = IsNothing deriving (Show,Read,Eq,Ord,Generic)
  defineConflict IsNothing e = case e of
                                 SetJust _ -> True
                                 SetNothing -> False
  defineLe IsNothing e1 e2 =
    let hasJust (Effect es) = length (filter (\case
                                                 SetJust _ -> True
                                                 SetNothing -> False) es) > 0
    in hasJust e1 && not (hasJust e2)

class ProdC a d

-- data Lensable a = Lensable a
--   deriving (Show,Read,Eq,Ord,Generic)

-- instance (Show a, Read a, Eq a, Ord a, Generic a) => CARD (Lensable a) where

data OrderLine
  = OrderLine { olItemId :: ItemId
              , olQuantity :: Int
              , olAmount :: Int
              }
  deriving (Show,Read,Eq,Ord,Generic)

data Order
  = Order { oCId :: CustomerId
          , oDate :: String
          , oCaId :: Justable CarrierId
          , oLines :: [OrderLine]
          }
  deriving (Show,Read,Eq,Ord,Generic)

instance CARD Order where
  data Ef Order
    = OnCaId (Ef (Justable CarrierId))
    deriving (Show,Read,Eq,Ord,Generic)
  data Cr Order
    = OfCaId (Cr (Justable CarrierId))
    deriving (Show,Read,Eq,Ord,Generic)

data Item
  = Item { iName :: String
         , iPrice :: Int
         }
  deriving (Show,Read,Eq,Ord,Generic)

data Stock
  = Stock { sQuantity :: Counter
          , sYtd :: Counter
          , sOrderCount :: Counter
          , sRemoteCount :: Counter
          }
  deriving (Show,Read,Eq,Ord,Generic)

data Customer
  = Customer { cBalance :: Counter }
  deriving (Show,Read,Eq,Ord,Generic)

data TPCC
  = TPCC { tpccStock :: Map (WarehouseId,ItemId) Stock
         , tpccItems :: Map ItemId Item
         , tpccCustomers :: Map CustomerId Customer
         , tpccOrders :: Map OrderId Order
         }
  deriving (Show,Read,Eq,Ord,Generic)
