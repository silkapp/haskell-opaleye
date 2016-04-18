module Opaleye.Internal.Order where

import qualified Opaleye.Column as C
import qualified Opaleye.Internal.Column as IC
import qualified Opaleye.Internal.Tag as T
import qualified Opaleye.Internal.PrimQuery as PQ

import qualified Opaleye.Internal.HaskellDB.PrimQuery as HPQ
import qualified Data.Functor.Contravariant as C
import qualified Data.Profunctor as P
import qualified Data.Monoid as M
import qualified Data.Semigroup as S

data SingleOrder a = SingleOrder HPQ.OrderOp (a -> HPQ.PrimExpr)

instance C.Contravariant SingleOrder where
  contramap f (SingleOrder op g) = SingleOrder op (P.lmap f g)

{-|
An `Order` represents an expression to order on and a sort
direction. Multiple `Order`s can be composed with
`Data.Monoid.mappend`.  If two rows are equal according to the first
`Order`, the second is used, and so on.
-}
newtype Order a = Order [SingleOrder a]

instance C.Contravariant Order where
  contramap f (Order xs) = Order (fmap (C.contramap f) xs)

instance S.Semigroup (Order a) where
  Order o <> Order o' = Order (o S.<> o')

instance M.Monoid (Order a) where
  mempty = Order M.mempty
  mappend = (S.<>)

order :: HPQ.OrderOp -> (a -> C.Column b) -> Order a
order op f = C.contramap f (Order [SingleOrder op IC.unColumn])

orderByU :: Order a -> (a, PQ.PrimQuery, T.Tag) -> (a, PQ.PrimQuery, T.Tag)
orderByU os (columns, primQ, t) = (columns, primQ', t)
  where primQ' = PQ.Order orderExprs primQ
        Order sos = os
        orderExprs = map (\(SingleOrder op f)
                          -> HPQ.OrderExpr op (f columns)) sos

orderExprs :: a -> Order a -> [HPQ.OrderExpr]
orderExprs x (Order os) = map (orderExpr x) os

orderExpr :: a -> SingleOrder a -> HPQ.OrderExpr
orderExpr x (SingleOrder op mkE) = HPQ.OrderExpr op (mkE x)

limit' :: Int -> (a, PQ.PrimQuery, T.Tag) -> (a, PQ.PrimQuery, T.Tag)
limit' n (x, q, t) = (x, PQ.Limit (PQ.LimitOp n) q, t)

offset' :: Int -> (a, PQ.PrimQuery, T.Tag) -> (a, PQ.PrimQuery, T.Tag)
offset' n (x, q, t) = (x, PQ.Limit (PQ.OffsetOp n) q, t)
