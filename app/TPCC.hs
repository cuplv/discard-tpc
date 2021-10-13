module TPCC where

import qualified Data.Map as Map
import Lang.Carol
import Lens.Micro.Platform

import TPCC.Data

quantityEf :: OrderLine -> Ef TPCC
quantityEf ol = EfTStock
                  ("main",ol ^. olItemId)
                  (EfSCounter SQuantity (Sub $ ol ^. olQuantity))

-- | Enter a new order into the order table, reducing stock quantities
-- accordingly.
newOrder :: ReplicaId -> CustomerId -> [OrderLine] -> Carol TPCC (Either String ())
newOrder rid cid ols = do
  -- Do an inconsistent query to be used for price information.
  items <- view tpccItems <$> query crT
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
      issue.ef $ EfTCustomers cid (EfCustomer (Add amt))
      -- Make effects for reducing stock quantities.
      let qEfs = Effect $ map quantityEf ols
      -- Consume those effects, to ensure stock satisfies them.
      r <- consume qEfs
      case r of
        Just _ -> do
          -- Success.  Issue the stock-reducing effects.
          issue qEfs
          -- And now create the new order to the state.
          let newO = Order cid "today" (Justable Nothing) ols
          -- Get a causal-consistent view of order ids.
          oids <- Map.keys . simpleKV . view tpccOrders <$> query crT
          -- Create a new unique order id.
          let newOid = (rid, 1 + maximum (map snd oids))
          -- Add the new order under the new order id.
          issue.ef $ EfTOrders (KVAdd newOid newO)
          return (Right ())
        Nothing -> do
          return (Left "Stock quantities were not sufficient.")

    -- Otherwise, report missing item listing.
    Left i -> return (Left $ "Item " ++ show i ++ " is not listed.")
