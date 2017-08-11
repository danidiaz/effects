{-# LANGUAGE TemplateHaskell #-}
module Data.Union.Templates
( mkApply1Instance
, mkApply1Instances
) where

import Control.Monad
import Language.Haskell.TH
import Unsafe.Coerce (unsafeCoerce)

mkApply1Instances :: Int -> Q [Dec]
mkApply1Instances n = concat <$> traverse mkApply1Instance [2..n]

mkApply1Instance :: Int -> Q [Dec]
mkApply1Instance paramN = do
  [c, f, n, n1, n2, r, r1, r2, a, u, u1, u2, proxy] <- traverse newName ["c", "f", "n", "n1", "n2", "r", "r1", "r2", "a", "u", "u1", "u2", "proxy"]
  params <- replicateM paramN (newName "f")
  pure
    [ InstanceD Nothing (AppT (VarT c) . VarT <$> params) (AppT (AppT (ConT (mkName "Apply1")) (VarT c)) (foldr (AppT . AppT PromotedConsT . VarT) PromotedNilT params))
      [ FunD apply1'
        [ Clause
          [ WildP, VarP f, ConP union [ LitP (IntegerL 0), VarP r ] ]
          (NormalB (AppE (AppE (VarE f) (AppE (ConE union) (LitE (IntegerL 0)))) (SigE (AppE (VarE 'unsafeCoerce) (VarE r)) (AppT (VarT (head params)) (VarT a)))))
          []
        , Clause
          [ VarP proxy, VarP f, AsP u (ConP union [ VarP n, VarP r ]) ]
          (NormalB (AppE (AppE (AppE (VarE apply1') (VarE proxy)) (LamE [WildP] (AppE (VarE f) (AppE (ConE union) (VarE n))))) (AppE (AppE (VarE asStrongerUnionTypeOf) (AppE (AppE (ConE union) (AppE (VarE 'pred) (VarE n))) (VarE r))) (VarE u))))
          []
        ]
      , FunD apply1_2'
        [ Clause
          [ WildP, VarP f, ConP union [ LitP (IntegerL 0), VarP r1 ], ConP union [ LitP (IntegerL 0), VarP r2 ] ]
          (NormalB (AppE (ConE 'Just) (AppE (AppE (AppE (VarE f) (AppE (ConE union) (LitE (IntegerL 0)))) (SigE (AppE (VarE 'unsafeCoerce) (VarE r1)) (AppT (VarT (head params)) (VarT a)))) (AppE (VarE 'unsafeCoerce) (VarE r2)))))
          []
        , Clause
          [ VarP proxy, VarP f, AsP u1 (ConP union [ VarP n1, VarP r1 ]), AsP u2 (ConP union [ VarP n2, VarP r2 ]) ]
          (GuardedB
            [ (NormalG (AppE (AppE (VarE '(==)) (VarE n1)) (VarE n2)), (AppE (AppE (AppE (AppE (VarE apply1_2') (VarE proxy)) (LamE [WildP] (AppE (VarE f) (AppE (ConE union) (VarE n1))))) (AppE (AppE (VarE asStrongerUnionTypeOf) (AppE (AppE (ConE union) (AppE (VarE 'pred) (VarE n1))) (VarE r1))) (VarE u1))) (AppE (AppE (VarE asStrongerUnionTypeOf) (AppE (AppE (ConE union) (AppE (VarE 'pred) (VarE n2))) (VarE r2))) (VarE u2))))
            , (NormalG (VarE 'otherwise), ConE 'Nothing)
            ])
          []
        ]
      ]
    ]
  where union = mkName "Union"
        apply1' = mkName "apply1'"
        apply1_2' = mkName "apply1_2'"
        asStrongerUnionTypeOf = mkName "asStrongerUnionTypeOf"
