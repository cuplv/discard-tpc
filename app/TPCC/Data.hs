{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE EmptyDataDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}

module TPCC.Data where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe (catMaybes)
import Data.CARD
import Data.CARD.Counter
import Data.CARD.Map
import Data.CARD.Maybe
import GHC.Generics
import Lens.Micro.Platform

type UC = UniversalC

type CustomerId = String

type ItemId = String

type ReplicaId = Int

type OrderId = (ReplicaId,Int)

type CarrierId = String

type WarehouseId = String

data OrderLine
  = OrderLine { _olItemId :: ItemId
              , _olQuantity :: Int
              , _olAmount :: Int
              }
  deriving (Show,Read,Eq,Ord,Generic)

makeLenses ''OrderLine

data OrderInfo
  = OrderInfo { _oiCId :: CustomerId
              , _oiDate :: String
              , _oiLines :: [OrderLine]
              }
  deriving (Show,Read,Eq,Ord,Generic)

makeLenses ''OrderInfo

type Order = (OrderInfo, Maybe CarrierId)

mkOrder :: CustomerId -> String -> [OrderLine] -> Order
mkOrder a b c = (OrderInfo a b c,Nothing)

oCId :: Lens' Order CustomerId
oCId = _1 . oiCId

oDate :: Lens' Order String
oDate = _1 . oiDate

oLines :: Lens' Order [OrderLine]
oLines = _1 . oiLines

oCarrierId :: Lens' (a,b) b
oCarrierId = _2

type OrderE = ((), MaybeE () CarrierId)

type OrderC = (UC (), UC (MaybeC ()))

setCarrierId :: CarrierId -> OrderE
setCarrierId i = mempty & oCarrierId .~ (insertE i)

data Item
  = Item { _iName :: String
         , _iPrice :: Int
         }
  deriving (Show,Read,Eq,Ord,Generic)

makeLenses ''Item

type Stock = (Int,Int,Int,Int)

sQuantity :: Lens' (a,b,c,d) a
sQuantity = _1

sYtd :: Lens' (a,b,c,d) b
sYtd = _2

sOrderCount :: Lens' (a,b,c,d) c
sOrderCount = _3

sRemoteCount :: Lens' (a,b,c,d) d
sRemoteCount = _4

type IntE = CounterE Int
type IntC = CounterC Int

type StockE 
  = ( IntE
    , IntE
    , IntE
    , IntE
    )

type StockC = (IntC, IntC, IntC, IntC)

data CustomerInfo
  = CustomerInfo { _ciName :: String }
  deriving (Show,Read,Eq,Ord,Generic)

makeLenses ''CustomerInfo

type Customer = (CustomerInfo, Int)

type CustomerE = ((), IntE)

type CustomerC = (UC (), IntC)

cName :: Lens' Customer String
cName = _1 . ciName

cBalance :: Lens' Customer Int
cBalance = _2

type Tpcc 
  = ( Map (WarehouseId, ItemId) Stock -- stock
    , Map ItemId Item -- items
    , Map CustomerId Customer -- customers
    , Map OrderId Order -- orders
    )

type TpccE
  = ( MapE (WarehouseId, ItemId) StockE Stock
    , MapE ItemId () Item
    , MapE CustomerId CustomerE Customer
    , MapE OrderId OrderE Order
    )

type TpccC
  = ( MapC (WarehouseId, ItemId) StockC
    , MapC ItemId (UC ())
    , MapC CustomerId CustomerC
    , MapC OrderId OrderC
    )

tpccStock :: Lens' Tpcc (Map (WarehouseId, ItemId) Stock)
tpccStock = _1

tpccItems :: Lens' Tpcc (Map ItemId Item)
tpccItems = _2

tpccCustomers :: Lens' Tpcc (Map CustomerId Customer)
tpccCustomers = _3

tpccOrders :: Lens' Tpcc (Map OrderId Order)
tpccOrders = _4
