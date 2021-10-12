{-# LANGUAGE DeriveGeneric #-}

module TPC.State where

import Data.Aeson
import Data.Map (Map)
import qualified Data.Map as Map
import Data.CARD
import GHC.Generics

type CustomerId = String

type ItemId = String

type OrderId = Int

type CarrierId = String

type WarehouseId = String

data OrderLine
  = OrderLine { olItemId :: ItemId
              , olQuantity :: Int
              , olAmount :: Int
              }
  deriving (Show,Read,Eq,Ord,Generic)

data Order
  = Order { oCId :: CustomerId
          , oDate :: String
          , oCaId :: Maybe CarrierId
          , oLines :: [OrderLine]
          }
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
  = Customer { cBalance :: Int }
  deriving (Show,Read,Eq,Ord,Generic)

data TPCC
  = TPCC { tpccStock :: Map (WarehouseId,ItemId) Stock
         , tpccItems :: Map ItemId Item
         , tpccCustomers :: Map CustomerId Customer
         , tpccOrders :: Map OrderId Order
         }
  deriving (Show,Read,Eq,Ord,Generic)
