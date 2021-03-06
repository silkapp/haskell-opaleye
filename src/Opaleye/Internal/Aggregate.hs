{-# LANGUAGE TupleSections #-}
module Opaleye.Internal.Aggregate where

import           Control.Applicative (Applicative, pure, (<*>))

import qualified Data.Profunctor as P
import qualified Data.Profunctor.Product as PP

import qualified Opaleye.Internal.PackMap as PM
import qualified Opaleye.Internal.PrimQuery as PQ
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.Column as C
import qualified Opaleye.Internal.Order as O

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ

{-|
An 'Aggregator' takes a collection of rows of type @a@, groups
them, and transforms each group into a single row of type @b@. This
corresponds to aggregators using @GROUP BY@ in SQL.
-}
newtype Aggregator a b = Aggregator
                         (PM.PackMap (Maybe (HPQ.AggrOp, [HPQ.OrderExpr]), HPQ.PrimExpr)
                                     HPQ.PrimExpr
                                     a b)

makeAggr' :: Maybe HPQ.AggrOp -> Aggregator (C.Column a) (C.Column b)
makeAggr' m = Aggregator (PM.PackMap
                          (\f (C.Column e) -> fmap C.Column (f (fmap (,[]) m, e))))

makeAggr :: HPQ.AggrOp -> Aggregator (C.Column a) (C.Column b)
makeAggr = makeAggr' . Just

orderAggregate :: O.Order c -> Aggregator c d -> Aggregator c d
orderAggregate o (Aggregator (PM.PackMap pm)) =
  Aggregator (PM.PackMap (\f c -> pm (f . P.first' (fmap (P.second' (++ O.orderExprs c o)))) c))

runAggregator :: Applicative f => Aggregator a b
              -> ((Maybe (HPQ.AggrOp, [HPQ.OrderExpr]), HPQ.PrimExpr) -> f HPQ.PrimExpr)
              -> a -> f b
runAggregator (Aggregator a) = PM.packmap a

aggregateU :: Aggregator a b
           -> (a, PQ.PrimQuery, T.Tag) -> (b, PQ.PrimQuery, T.Tag)
aggregateU agg (c0, primQ, t0) = (c1, primQ', T.next t0)
  where (c1, projPEs) =
          PM.run (runAggregator agg (extractAggregateFields t0) c0)

        primQ' = PQ.Aggregate projPEs primQ

extractAggregateFields :: T.Tag -> (Maybe (HPQ.AggrOp, [HPQ.OrderExpr]), HPQ.PrimExpr)
      -> PM.PM [(HPQ.Symbol, (Maybe (HPQ.AggrOp, [HPQ.OrderExpr]), HPQ.PrimExpr))] HPQ.PrimExpr
extractAggregateFields = PM.extractAttr "result"

-- { Boilerplate instances

instance Functor (Aggregator a) where
  fmap f (Aggregator g) = Aggregator (fmap f g)

instance Applicative (Aggregator a) where
  pure = Aggregator . pure
  Aggregator f <*> Aggregator x = Aggregator (f <*> x)

instance P.Profunctor Aggregator where
  dimap f g (Aggregator q) = Aggregator (P.dimap f g q)

instance PP.ProductProfunctor Aggregator where
  empty = PP.defaultEmpty
  (***!) = PP.defaultProfunctorProduct

instance PP.SumProfunctor Aggregator where
  Aggregator x1 +++! Aggregator x2 = Aggregator (x1 PP.+++! x2)

-- }
