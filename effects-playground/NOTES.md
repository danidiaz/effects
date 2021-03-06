    data Union (r :: [ (* -> *) -> (* -> *) ]) (f :: * -> *) (v :: *) where

    Λ :k Eff
    Eff :: [(* -> *) -> * -> *] -> * -> *

    Λ :k Effect (Writer ())
    Effect (Writer ()) :: Constraint

    Is "Effectful" something like MonadEff?

    Λ :k Effectful
    Effectful :: ([(* -> *) -> * -> *] -> * -> *) -> Constraint

    -- | An effectful computation that returns 'b' and sends a list of 'effects'.
    data Eff effects b
      -- | Done with the value of type `b`.
      = Return b
      -- | Send an union of 'effects' and 'eff a' to handle, and a queues of effects to apply from 'a' to 'b'.
      | forall a. E (Union effects (Eff effects) a) (Queue (Eff effects) a b)

    -- | A queue of effects to apply from 'a' to 'b'.
    type Queue m = BinaryTree (Arrow m)


      --   First-order effects (ones not using the @m@ parameter) have relatively simple definitions, more or less just pushing the distributive law through the continuation. Higher-order effects (like @Reader@’s @Local@ constructor) must additionally apply the handler to their scoped actions.
      handleState :: (Functor c, Functor m, Functor n)
                  => c ()
                  -> (forall x . c (m x) -> n (c x))
                  -> Request effect m a
                  -> Request effect n (c a)

From the [paper](http://okmij.org/ftp/Haskell/extensible/more.pdf):

    "The remaining part of the request signature f x tells the type x of the
    reply, to be fed into the continuation. Different requests have their
    own reply types, hence x is existentially quantified"

The Union type in the library has more type parameters than the Union type from the paper...

Higher-order effects:

- Reader's "local"
- Exception's "catch"
- Fresh's "reset"
- any others?


from the [freer-simple](http://hackage.haskell.org/package/freer-simple-1.1.0.0/docs/Control-Monad-Freer.html) docs:

	As mentioned in the documentation for Eff, it’s rare to actually specify a
	concrete list of effects for an Eff computation, since that has two significant
	downsides:

	It couples the computation to that specific list of effects, so it cannot be
	used in functions that perform a strict superset of effects.  It forces the
	effects to be handled in a particular order, which can make handler code
	brittle when the list of effects is changed.  Fortunately, these restrictions
	are easily avoided by using effect constraints, such as Member or Members,
	which decouple a computation from a particular concrete list of effects.


