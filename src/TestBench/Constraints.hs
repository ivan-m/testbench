{-# LANGUAGE CPP, ConstraintKinds, FlexibleInstances, MultiParamTypeClasses,
             UndecidableInstances #-}

#if __GLASGOW_HASKELL__ >= 800
{-# LANGUAGE UndecidableSuperClasses #-}
#endif

{- |
   Module      : TestBench.Constraints
   Description : Constraint definitions
   Copyright   : (c) Ivan Lazar Miljenovic
   License     : MIT
   Maintainer  : Ivan.Miljenovic@gmail.com

 -}
module TestBench.Constraints where

--------------------------------------------------------------------------------

-- | An alias for readability.
type SameAs a = (~) a

-- | The union of two @(* -> 'Constraint')@ values.
--
--   Whilst @type EqNum a = ('Eq' a, 'Num' a)@ is a valid
--   specification of a 'Constraint' when using the @ConstraintKinds@
--   extension, it cannot be used with 'compareFuncConstraint' as type
--   aliases cannot be partially applied.
--
--   As such, you can use @type EqNum = CUnion Eq Num@ instead.
class (c1 a, c2 a) => CUnion c1 c2 a
instance (c1 a, c2 a) => CUnion c1 c2 a
