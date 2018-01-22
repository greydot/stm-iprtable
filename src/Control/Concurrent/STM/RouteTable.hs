module Control.Concurrent.STM.RouteTable ( TRouteTable
                                         , newTRouteTable
                                         , insert
                                         ) where

import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Data.Functor (($>))
import Data.IP
import Data.IP.RouteTable (Routable(..))

data RouteEntry k a = Nil
                    | Branch !(AddrRange k) !k !(Maybe a) !(TRouteTable k a) !(TRouteTable k a)

data TRouteTable k a = TRouteTable (TVar (RouteEntry k a))

newTRouteTable :: Routable k => STM (TRouteTable k a)
newTRouteTable = TRouteTable <$> newTVar Nil

insert :: Routable k => TRouteTable k a -> AddrRange k -> a -> STM ()
insert (TRouteTable var) r val = do e <- readTVar var
                                    res <- insert' e r val
                                    maybe (pure ()) (writeTVar var) res

insert' :: Routable k => RouteEntry k a -> AddrRange k -> a -> STM (Maybe (RouteEntry k a))
insert' Nil r val = do b1 <- newTRouteTable
                       b2 <- newTRouteTable
                       return $ Just $ Branch r (keyToTestBit r) (Just val) b1 b2
insert' e@(Branch r2 tb2 mval b1 b2) r1 val | r1 == r2 = return $ Just $ Branch r2 tb2 (Just val) b1 b2
                                            | r2 >:> r1 && isLeft r1 tb2 = insert b1 r1 val $> Nothing
                                            | r2 >:> r1                  = insert b2 r1 val $> Nothing
                                            | r1 >:> r2 = do b1' <- TRouteTable <$> newTVar e
                                                             b2' <- newTRouteTable
                                                             return $ Just $ if isLeft r2 tb1
                                                                              then Branch r1 tb1 (Just val) b1' b2'
                                                                              else Branch r1 tb1 (Just val) b2' b1'
    where
      tb1 = keyToTestBit r1


keyToTestBit :: Routable k => AddrRange k -> k
keyToTestBit = intToTBit . mlen

isLeft :: Routable k => AddrRange k -> k -> Bool
isLeft adr = isZero (addr adr)
