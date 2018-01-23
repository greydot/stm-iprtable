module Control.Concurrent.STM.RouteTable ( TRouteTable
                                         , newTRouteTable
                                         , insert
                                         , toList
                                         , fromList
                                         , delete
                                         , lookup
                                         ) where

import Prelude hiding (lookup)
import Control.Concurrent.STM (STM)
import Control.Concurrent.STM.TVar (TVar, newTVar, readTVar, writeTVar)
import Data.Foldable (traverse_)
import Data.Functor (($>))
import Data.IP
import Data.IP.RouteTable (Routable(..))

data RouteEntry k a = Nil
                    | Branch !(AddrRange k) !k !(Maybe a) !(TRouteTable k a) !(TRouteTable k a)

data TRouteTable k a = TRouteTable (TVar (RouteEntry k a))

newTRouteTable :: Routable k => STM (TRouteTable k a)
newTRouteTable = newBranch Nil

newBranch :: Routable k => RouteEntry k a -> STM (TRouteTable k a)
newBranch e = TRouteTable <$> newTVar e

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
                                            | otherwise = do b1' <- newTRouteTable
                                                             b2' <- newTRouteTable
                                                             let b = Branch r1 tb1 (Just val) b1' b2'
                                                             Just <$> link b e
    where
      tb1 = keyToTestBit r1

link :: Routable k => RouteEntry k a -> RouteEntry k a -> STM (RouteEntry k a)
link s1@(Branch k1 _ _ _ _) s2@(Branch k2 _ _ _ _) = do b1 <- newBranch s1
                                                        b2 <- newBranch s2
                                                        pure $ if isLeft k1 tbg
                                                                  then Branch kg tbg Nothing b1 b2
                                                                  else Branch kg tbg Nothing b2 b1
  where
    kg = glue 0 k1 k2
    tbg = keyToTestBit kg
link _ _ = error "link"

glue :: Routable k => Int -> AddrRange k -> AddrRange k -> AddrRange k
glue n k1 k2
  | addr k1 `masked` mk == addr k2 `masked` mk = glue (n + 1) k1 k2
  | otherwise = makeAddrRange (addr k1) (n - 1)
  where
    mk = intToMask n

keyToTestBit :: Routable k => AddrRange k -> k
keyToTestBit = intToTBit . mlen

isLeft :: Routable k => AddrRange k -> k -> Bool
isLeft adr = isZero (addr adr)

toList :: Routable k => TRouteTable k a -> STM [(AddrRange k,a)]
toList (TRouteTable var) = do l <- f =<< readTVar var
                              pure l
  where
    f Nil = pure []
    f (Branch k _ Nothing lb rb) = do ll <- toList lb
                                      rl <- toList rb
                                      pure (ll ++ rl)
    f (Branch k _ (Just val) lb rb) = do ll <- toList lb
                                         rl <- toList rb
                                         pure $ (k,val):(ll ++ rl)

fromList :: Routable k => [(AddrRange k,a)] -> STM (TRouteTable k a)
fromList xs = do t <- newTRouteTable
                 traverse_ (uncurry (insert t)) xs
                 pure t

delete :: Routable k => TRouteTable k a -> AddrRange k -> STM ()
delete (TRouteTable var) range = do e <- readTVar var
                                    r <- del e
                                    case r of
                                      Nothing -> pure ()
                                      Just e' -> writeTVar var e'
  where
    del Nil = pure Nothing
    del (Branch k2 tb2 v2 lb rb) | range == k2 = pure (Just $ Branch k2 tb2 Nothing lb rb)
                                 | k2 >:> range && isLeft range tb2 = delete lb range $> Nothing
                                 | k2 >:> range                     = delete rb range $> Nothing
                                 | otherwise = pure Nothing

node :: Routable k => AddrRange k -> k -> Maybe a -> TRouteTable k a -> TRouteTable k a -> STM (RouteEntry k a)
node k tb v lt@(TRouteTable lvar) rt@(TRouteTable rvar) = do l <- readTVar lvar
                                                             r <- readTVar rvar
                                                             pure (node' v l r)
  where
    node' Nothing Nil r = r
    node' Nothing l Nil = l
    node' v l r = Branch k tb v lt rt

lookup :: Routable k => TRouteTable k a -> AddrRange k -> STM (Maybe a)
lookup rt r = search rt r Nothing

search :: Routable k => TRouteTable k a -> AddrRange k -> Maybe a -> STM (Maybe a)
search (TRouteTable var) r res = search' =<< readTVar var
  where
    search' Nil = pure res
    search' (Branch k tb Nothing lb rb)
            | k >:> r && isLeft r tb = search lb r res
            | k >:> r                = search rb r res
            | otherwise              = pure res
    search' (Branch k tb val lb rb)
            | k == r = pure val
            | k >:> r && isLeft r tb = search lb r val
            | k >:> r                = search rb r val
            | otherwise              = pure res
