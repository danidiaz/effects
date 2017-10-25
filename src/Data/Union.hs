{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE ConstraintKinds, DataKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE MagicHash #-}

-- Only for MemberU below, when emulating Monad Transformers
{-# LANGUAGE FunctionalDependencies, UndecidableInstances #-}

{-|
Module      : Data.Union
Description : Open unions (type-indexed co-products) for extensible effects.
Copyright   : Allele Dev 2015
License     : BSD-3
Maintainer  : allele.dev@gmail.com
Stability   : experimental
Portability : POSIX

All operations are constant-time, and there is no Typeable constraint

This is a variation of OpenUnion5.hs, which relies on overlapping
instances instead of closed type families. Closed type families
have their problems: overlapping instances can resolve even
for unground types, but closed type families are subject to a
strict apartness condition.

This implementation is very similar to OpenUnion1.hs, but without
the annoying Typeable constraint. We sort of emulate it:

Our list r of open union components is a small Universe.
Therefore, we can use the Typeable-like evidence in that
universe.

The data constructors of Union are not exported.
-}

module Data.Union (
  Union,
  decompose,
  weaken,
  inj,
  prj,
  type(:<),
  type(:<:),
  MemberU2,
  Apply(..),
  apply',
  apply2,
  apply2'
) where

import Data.Functor.Classes (Eq1(..), eq1, Ord1(..), compare1, Show1(..), showsPrec1)
import Data.Maybe (fromMaybe)
import Data.Proxy
import Data.Union.Templates
import Unsafe.Coerce(unsafeCoerce)
import GHC.Exts (Constraint)
import GHC.Prim (Proxy#, proxy#)
import GHC.TypeLits

infixr 5 :<

-- Strong Sum (Existential with the evidence) is an open union
-- t is can be a GADT and hence not necessarily a Functor.
-- Int is the index of t in the list r; that is, the index of t in the
-- universe r.
data Union (r :: [ * -> * ]) (v :: *) where
  Union :: {-# UNPACK #-} !Int -> t v -> Union r v

{-# INLINE prj' #-}
{-# INLINE inj' #-}
inj' :: Int -> t v -> Union r v
inj' = Union

prj' :: Int -> Union r v -> Maybe (t v)
prj' n (Union n' x) | n == n'   = Just (unsafeCoerce x)
                    | otherwise = Nothing

newtype P (t :: * -> *) (r :: [* -> *]) = P { unP :: Int }

infixr 5 :<:
-- | Find a list of members 'm' in an open union 'r'.
type family m :<: r :: Constraint where
  (t ': c) :<: r = (t :< r, c :<: r)
  '[] :<: r = ()

{-
-- Optimized specialized instance
instance (t :< '[t]) where
  {-# INLINE inj #-}
  {-# INLINE prj #-}
  inj x           = Union 0 x
  prj (Union _ x) = Just (unsafeCoerce x)
-}

-- | Inject a functor into a type-aligned union.
inj :: forall e r v. e :< r => e v -> Union r v
inj = inj' (unP (elemNo :: P e r))
{-# INLINE inj #-}

-- | Maybe project a functor out of a type-aligned union.
prj :: forall e r v. e :< r => Union r v -> Maybe (e v)
prj = prj' (unP (elemNo :: P e r))
{-# INLINE prj #-}


decompose :: Union (t ': r) v -> Either (Union r v) (t v)
decompose (Union 0 v) = Right $ unsafeCoerce v
decompose (Union n v) = Left  $ Union (n-1) v
{-# INLINE [2] decompose #-}


-- | Specialized version of 'decompose'.
decompose0 :: Union '[t] v -> Either (Union '[] v) (t v)
decompose0 (Union _ v) = Right $ unsafeCoerce v
-- No other case is possible
{-# RULES "decompose/singleton"  decompose = decompose0 #-}
{-# INLINE decompose0 #-}

weaken :: Union r w -> Union (any ': r) w
weaken (Union n v) = Union (n+1) v

-- Find an index of an element in an `r'.
-- The element must exist, so this is essentially a compile-time computation.
class KnownNat (ElemIndex t r) => ((t :: * -> *) :< (r :: [* -> *])) where
  elemNo :: P t r

type family ElemIndex (t :: * -> *) (ts :: [* -> *]) :: Nat where
  ElemIndex t (t ': _) = 0
  ElemIndex t (_ ': ts) = 1 + ElemIndex t ts

instance KnownNat (ElemIndex t r) => (t :< r) where
  elemNo = P (fromIntegral (natVal' (proxy# :: Proxy# (ElemIndex t r))))

-- | Helper to apply a function to a functor of the nth type in a type list.
class Apply (c :: (* -> *) -> Constraint) (fs :: [* -> *]) where
  apply :: proxy c -> (forall g . c g => g a -> b) -> Union fs a -> b

apply' :: Apply c fs => proxy c -> (forall g . c g => (forall x. g x -> Union fs x) -> g a -> b) -> Union fs a -> b
apply' proxy f u@(Union n _) = apply proxy (\ r -> f (Union n) r) u

apply2 :: Apply c fs => proxy c -> (forall g . c g => g a -> g b -> d) -> Union fs a -> Union fs b -> Maybe d
apply2 proxy f u@(Union n1 _) (Union n2 r2)
  | n1 == n2  = Just (apply proxy (\ r1 -> f r1 (unsafeCoerce r2)) u)
  | otherwise = Nothing

apply2' :: Apply c fs => proxy c -> (forall g . c g => (forall x. g x -> Union fs x) -> g a -> g b -> d) -> Union fs a -> Union fs b -> Maybe d
apply2' proxy f u@(Union n1 _) (Union n2 r2)
  | n1 == n2  = Just (apply' proxy (\ reinj r1 -> f reinj r1 (unsafeCoerce r2)) u)
  | otherwise = Nothing

pure (mkApplyInstance <$> [1..150])


type family EQU (a :: * -> *) (b :: * -> *) :: Bool where
  EQU a a = 'True
  EQU a b = 'False

-- This class is used for emulating monad transformers
class (t :< r) => MemberU2 (tag :: (* -> *) -> * -> *) (t :: * -> *) r | tag r -> t
instance (KnownNat (ElemIndex t1 (t2 ': r)), t1 :< r, MemberU' (EQU t1 t2) tag t1 (t2 ': r)) => MemberU2 tag t1 (t2 ': r)

class (t :< r) =>
      MemberU' (f::Bool) (tag :: (* -> *) -> * -> *) (t :: * -> *) r | tag r -> t

instance MemberU' 'True tag (tag e) (tag e ': r)
instance (t :< (t' ': r), MemberU2 tag t r) =>
           MemberU' 'False tag t (t' ': r)

instance Apply Foldable fs => Foldable (Union fs) where
  foldMap f u = apply (Proxy :: Proxy Foldable) (foldMap f) u

instance Apply Functor fs => Functor (Union fs) where
  fmap f u = apply' (Proxy :: Proxy Functor) (\ reinj -> reinj . fmap f) u

instance (Apply Foldable fs, Apply Functor fs, Apply Traversable fs) => Traversable (Union fs) where
  traverse f u = apply' (Proxy :: Proxy Traversable) (\ reinj -> fmap reinj . traverse f) u


instance Apply Eq1 fs => Eq1 (Union fs) where
  liftEq eq u1 u2 = fromMaybe False (apply2 (Proxy :: Proxy Eq1) (liftEq eq) u1 u2)

instance (Apply Eq1 fs, Eq a) => Eq (Union fs a) where
  (==) = eq1


instance (Apply Eq1 fs, Apply Ord1 fs) => Ord1 (Union fs) where
  liftCompare compareA u1@(Union n1 _) u2@(Union n2 _) = fromMaybe (compare n1 n2) (apply2 (Proxy :: Proxy Ord1) (liftCompare compareA) u1 u2)

instance (Apply Eq1 fs, Apply Ord1 fs, Ord a) => Ord (Union fs a) where
  compare = compare1


instance Apply Show1 fs => Show1 (Union fs) where
  liftShowsPrec sp sl d u = apply (Proxy :: Proxy Show1) (liftShowsPrec sp sl d) u

instance (Apply Show1 fs, Show a) => Show (Union fs a) where
  showsPrec = showsPrec1
