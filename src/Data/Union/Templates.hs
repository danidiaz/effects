{-# LANGUAGE TemplateHaskell #-}
module Data.Union.Templates
( mkElemIndexTypeFamily
, mkApplyInstance
) where

import Language.Haskell.TH
import Unsafe.Coerce (unsafeCoerce)

mkElemIndexTypeFamily :: Integer -> Dec
mkElemIndexTypeFamily paramN =
  ClosedTypeFamilyD (TypeFamilyHead elemIndex [KindedTV t functorK, KindedTV ts (AppT ListT functorK)] (KindSig (ConT nat)) Nothing) (mkEquation <$> [0..pred paramN])
  where [elemIndex, t, ts, nat] = mkName <$> ["ElemIndex", "t", "ts", "Nat"]
        functorK = AppT (AppT ArrowT StarT) StarT
        mkT = VarT . mkName . ('t' :) . show
        mkEquation i = TySynEqn [ mkT i, typeListT WildCardT (mkT <$> [0..i]) ] (LitT (NumTyLit i))

mkApplyInstance :: Integer -> Dec
mkApplyInstance paramN =
  InstanceD Nothing (AppT constraint <$> typeParams) (AppT (AppT (ConT applyC) constraint) (typeListT PromotedNilT typeParams))
    [ FunD apply (zipWith mkClause [0..] typeParams)
    , PragmaD (InlineP apply Inlinable FunLike AllPhases)
    ]
  where typeParams = VarT . mkName . ('f' :) . show <$> [0..pred paramN]
        [applyC, apply, f, r, union] = mkName <$> ["Apply", "apply", "f", "r", "Union"]
        [constraint, a] = VarT . mkName <$> ["constraint", "a"]
        mkClause i nthType = Clause
          [ WildP, VarP f, ConP union [ LitP (IntegerL i), VarP r ] ]
          (NormalB (AppE (VarE f) (SigE (AppE (VarE 'unsafeCoerce) (VarE r)) (AppT nthType a))))
          []

typeListT :: Type -> [Type] -> Type
typeListT = foldr (AppT . AppT PromotedConsT)
