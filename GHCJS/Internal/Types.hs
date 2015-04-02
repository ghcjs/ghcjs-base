{-# LANGUAGE EmptyDataDecls, TypeFamilies, DataKinds, KindSignatures, PolyKinds #-}

module GHCJS.Internal.Types where

data MutabilityType s = Mutable
                      | Immutable
                      | STMutable s

data IsItMutable = IsImmutable
                 | IsMutable

type family Mutability (a :: MutabilityType s) :: IsItMutable where
  Mutability Immutable     = IsImmutable
  Mutability Mutable       = IsMutable
  Mutability (STMutable s) = IsMutable


