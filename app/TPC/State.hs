{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TPC.State where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.CARD
import GHC.Generics
import Lens.Micro.Platform

type CustomerId = String

type ItemId = String

type ReplicaId = Int

type OrderId = (ReplicaId,Int)

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

-- | A CARD for maps.
data SimpleKV k v = SimpleKV { simpleKV :: Map k v }
  deriving (Show,Read,Eq,Ord,Generic)

instance (Show k, Read k, Eq k, Ord k, Generic k, Show v, Read v, Eq v, Ord v, Generic v) => CARD (SimpleKV k v) where
  data Ef (SimpleKV k v) = KVAdd k v deriving (Show,Read,Eq,Ord,Generic)
  defineEffect (SimpleKV m) (KVAdd k v) = SimpleKV (Map.insert k v m)
  data Cr (SimpleKV k v) deriving (Show,Read,Eq,Ord,Generic)

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

data OrderLine
  = OrderLine { _olItemId :: ItemId
              , _olQuantity :: Int
              , _olAmount :: Int
              }
  deriving (Show,Read,Eq,Ord,Generic)

makeLenses ''OrderLine

data Order
  = Order { _oCId :: CustomerId
          , _oDate :: String
          , _oCaId :: Justable CarrierId
          , _oLines :: [OrderLine]
          }
  deriving (Show,Read,Eq,Ord,Generic)

runEf s e = defineEffect e s

makeLenses ''Order

instance CARD Order where
  data Ef Order
    = OnOCaId { onOCaId :: Ef (Justable CarrierId) }
    deriving (Show,Read,Eq,Ord,Generic)
  defineEffect s (OnOCaId e) = over oCaId (runEf e) s
  data Cr Order
    = OfOCaId (Cr (Justable CarrierId))
    deriving (Show,Read,Eq,Ord,Generic)
  defineConflict (OfOCaId c) (OnOCaId e) = defineConflict c e
  defineLe (OfOCaId c) (Effect es1) (Effect es2) =
    defineLe c (Effect $ map onOCaId es1) (Effect $ map onOCaId es2)

data Item
  = Item { _iName :: String
         , _iPrice :: Int
         }
  deriving (Show,Read,Eq,Ord,Generic)

makeLenses ''Item

data Stock
  = Stock { _sQuantity :: Counter
          , _sYtd :: Counter
          , _sOrderCount :: Counter
          , _sRemoteCount :: Counter
          }
  deriving (Show,Read,Eq,Ord,Generic)

makeLenses ''Stock

data StockTag = SQuantity | SYtd | SOrderCount | SRemoteCount
  deriving (Show,Read,Eq,Ord,Generic)

stockLens :: StockTag -> Lens' Stock Counter
stockLens = \case
  SQuantity -> sQuantity
  SYtd -> sYtd
  SOrderCount -> sOrderCount
  SRemoteCount -> sRemoteCount

instance CARD Stock where
  data Ef Stock
    = EfSCounter StockTag (Ef Counter)
    deriving (Show,Read,Eq,Ord,Generic)
  defineEffect s (EfSCounter t e) = over (stockLens t) (runEf e) s
  data Cr Stock
    = CrSCounter StockTag (Cr Counter)
    deriving (Show,Read,Eq,Ord,Generic)
  defineConflict (CrSCounter t1 c) (EfSCounter t2 e) | t1 == t2 = 
                                                       defineConflict c e
                                                     | otherwise = False
  defineLe (CrSCounter t c) (Effect es1) (Effect es2) = 
    let f (EfSCounter t1 e1) | t1 == t = Just e1
                             | otherwise = Nothing
    in defineLe c (Effect $ catMaybes (map f es1))
                  (Effect $ catMaybes (map f es2))

data Customer
  = Customer { _cBalance :: Counter }
  deriving (Show,Read,Eq,Ord,Generic)

makeLenses ''Customer

instance CARD Customer where
  data Ef Customer
    = EfCustomer { efCustomer :: Ef Counter }
    deriving (Show,Read,Eq,Ord,Generic)
  defineEffect s (EfCustomer e) = over cBalance (runEf e) s
  data Cr Customer
    = CrCustomer (Cr Counter)
    deriving (Show,Read,Eq,Ord,Generic)
  defineConflict (CrCustomer c) (EfCustomer e) = defineConflict c e
  defineLe (CrCustomer c) (Effect es1) (Effect es2) =
    defineLe c (Effect $ map efCustomer es1) (Effect $ map efCustomer es2)

data TPCC
  = TPCC { _tpccStock :: Map (WarehouseId,ItemId) Stock
         , _tpccItems :: Map ItemId Item
         , _tpccCustomers :: Map CustomerId Customer
         , _tpccOrders :: SimpleKV OrderId Order
         }
  deriving (Show,Read,Eq,Ord,Generic)

makeLenses ''TPCC

instance CARD TPCC where
  data Ef TPCC
    = EfTStock (WarehouseId, ItemId) (Ef Stock)
    | EfTCustomers CustomerId (Ef Customer)
    | EfTOrders (Ef (SimpleKV OrderId Order))
    deriving (Show,Read,Eq,Ord,Generic)
  defineEffect s (EfTStock i e) = 
    over (tpccStock . at i . _Just) (runEf e) s
  defineEffect s (EfTCustomers i e) =
    over (tpccCustomers . at i . _Just) (runEf e) s
  defineEffect s (EfTOrders e) =
    over tpccOrders (runEf e) s
  data Cr TPCC
    = CrTStock (WarehouseId, ItemId) (Cr Stock)
    | CrTCustomers CustomerId (Cr Customer)
    deriving (Show,Read,Eq,Ord,Generic)
  defineConflict c e = case (c,e) of
    (CrTStock i1 c, EfTStock i2 e) | i1 == i2 -> defineConflict c e
    (CrTCustomers i1 c, EfTCustomers i2 e) | i1 == i2 -> defineConflict c e
    _ -> False
  defineLe c (Effect es1) (Effect es2) = case c of
    CrTStock i c ->
      let f (EfTStock i1 e1) | i1 == i = Just e1
          f _ = Nothing
      in defineLe c (Effect $ catMaybes (map f es1)) 
                    (Effect $ catMaybes (map f es2))
    CrTCustomers i c ->
      let f (EfTCustomers i1 e1) | i1 == i = Just e1
          f _ = Nothing
      in defineLe c (Effect $ catMaybes (map f es1)) 
                    (Effect $ catMaybes (map f es2))
