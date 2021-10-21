{-# LANGUAGE RankNTypes #-}

module TPCC where

import qualified Data.Map as Map
import Lang.Carol
import Data.CARD
import Data.CARD.Counter
import Data.CARD.Map
import Data.CARD.Maybe
import Lens.Micro.Platform

import TPCC.Data

type TpccOp = Carol TpccC TpccE Tpcc

-- | Enter a new order into the order table, reducing stock quantities
-- accordingly.
newOrder :: ReplicaId -> CustomerId -> [OrderLine] -> TpccOp (Either String ())
newOrder rid cid ols = do
  -- Do an inconsistent query to be used for price information.
  items <- view tpccItems <$> query uniC
  -- Add up total cost (or "Left i" if i's price is missing).
  let olamt ol = (fmap (ol ^. olQuantity *))
                   (view iPrice <$> Map.lookup (ol^.olItemId) items)
  let m = foldr (\ol a -> case olamt ol of
                            Just a1 -> fmap (+ a1) a
                            Nothing -> Left (ol ^. olItemId))
                (Right 0)
                ols
  case m of
    -- If all prices were listed, continue with transaction.
    Right amt -> do 
      -- Add the total amount to the customer's balance.
      -- issue.ef $ EfTCustomers cid (EfCustomer (Add amt))
      issue' _3 $ mapE cid (adjustE (meml _2 $ addE amt))
      -- Make effects for reducing stock quantities.
      -- let qEfs = Effect $ map quantityEf ols
      let qEfs = foldr (\ol e -> e <> quantEf ol) idE ols
      -- Consume those effects, to ensure stock satisfies them.
      r <- consume qEfs
      case r of
        Just _ -> do
          -- Success.  Issue the stock-reducing effects.
          issue qEfs
          -- And now create the new order, marking carrier ID as
          -- "Nothing".
          let newO = mkOrder cid "today" ols
          -- Get a causal-consistent view of order ids.
          oids <- Map.keys . view tpccOrders <$> query uniC
          -- Create a new unique order id.
          let newOid = (rid, 1 + maximum (map snd oids))
          -- Add the new order under the new order id.
          issue' _4 $ mapE newOid (insertE newO)
          -- issue.ef $ EfTOrders (KVAdd newOid newO)
          return (Right ())
        Nothing -> do
          return (Left "Stock quantities were not sufficient.")

    -- Otherwise, report missing item listing.
    Left i -> return (Left $ "Item " ++ show i ++ " is not listed.")

-- | Accept payment from customer.
payment :: CustomerId -> Int -> TpccOp ()
payment cid amt = do
  issue' _3 $ mapE cid (adjustE (meml _2 $ subE amt))
  -- And add to the YTD of the warehouse and district...

orderStatus :: CustomerId -> TpccOp [String]
orderStatus = undefined

-- | Make a stock-quantity-reducing effect from an 'OrderLine'.
quantEf :: OrderLine -> TpccE
quantEf ol =
  let i = view olItemId ol
      n = view olQuantity ol
  in meml _1 (mapE ("main", i) 
                   (adjustE (meml sQuantity (subE n))))

meml :: (Monoid a, Monoid b) => Lens' a b -> b -> a
meml l v = mempty & l <>~ v


issue' :: (Monoid e1, Monoid e2) => Lens' e1 e2 -> e2 -> Carol c e1 s ()
issue' l e = issue (meml l e)
