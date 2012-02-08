{-# LANGUAGE DeriveGeneric, KindSignatures, TemplateHaskell, 
    QuasiQuotes, FlexibleInstances, TypeOperators, TypeSynonymInstances,
    MultiParamTypeClasses, FunctionalDependencies, OverlappingInstances,
    ScopedTypeVariables, EmptyDataDecls, DefaultSignatures, ViewPatterns,
    UndecidableInstances, FlexibleContexts, StandaloneDeriving, IncoherentInstances #-}
module TemplateReflect where
import Language.Haskell.TH
import Data.Word
import GHC.Generics
import Data.DList (DList, toList)
import Data.Monoid (mappend)
import Control.Applicative

data Simple = Simple Int
    deriving(Generic)
    
instance ToExp Simple     
    
class TemplateReflect a where
    to_decl :: a -> Dec
    
class ToExp a where
    to_exp :: a -> Exp
    default to_exp :: (Generic a, GToExp (Rep a)) => a -> Exp
    to_exp a = g_to_exp (from a)
    
class GToExp f where
    g_to_exp :: f a -> Exp
    
instance GToExp U1 where
    g_to_exp U1 = ConE $ mkName "GHC.Unit.()"
    
instance (Constructor c, ConsToExp a) => GToExp (C1 c a) where
    g_to_exp = consToExp (conName $ (undefined :: t c a p)) . unM1

instance (GToExp a) => GToExp (M1 i c a) where
    g_to_exp = g_to_exp . unM1
    
instance (ToExp a) => GToExp (K1 i a) where
    g_to_exp = to_exp . unK1
    
instance (GToExp a, GToExp b) => GToExp (a :+: b) where
    g_to_exp (L1 x) = g_to_exp x
    g_to_exp (R1 x) = g_to_exp x

instance (GToExp a, GToExp b) => GToExp (a :*: b) where
    g_to_exp =  foldl1 AppE . toList . gConArgToLit 
 
class GConArgToLit f where
    gConArgToLit :: f a -> DList Exp 
    
instance (GConArgToLit a, GConArgToLit b) => GConArgToLit (a :*: b) where
    gConArgToLit (a :*: b) = gConArgToLit a `mappend` gConArgToLit b
    
instance (GToExp a) => GConArgToLit a where
    gConArgToLit a = pure (g_to_exp a)
   
--------------------------------------------------------------------------------

class ConsToExp    f where consToExp  ::           String -> f a -> Exp

instance GToExp f => ConsToExp f where
    consToExp name p = foldl AppE (ConE (mkName name)) . toList $ gConArgToLit p

    
instance (ToExp a) => ToExp [a] where
    to_exp x = ListE $ map to_exp x

instance ToExp Char where
     to_exp x = LitE $ CharL x
    
instance ToExp String where
    to_exp x = LitE $ StringL x

instance ToExp Integer where
    to_exp x = LitE $ IntegerL x

instance ToExp Rational where
     to_exp x = LitE $ RationalL x

instance ToExp Int where
    to_exp x = LitE $ IntPrimL $ fromIntegral x

instance ToExp Word32 where
    to_exp x = LitE $ WordPrimL $ fromIntegral x

instance ToExp Float where
     to_exp x = LitE $ FloatPrimL $ toRational x

instance ToExp Double where
    to_exp x = LitE $ DoublePrimL $ toRational x
    
class IsRecord (f :: * -> *) b | f -> b

data True
data False

instance (IsRecord f b) => IsRecord (f :*: g) b
instance IsRecord (M1 S NoSelector f) False
instance (IsRecord f b) => IsRecord (M1 S c f) b
instance IsRecord (K1 i c) True
instance IsRecord U1 False
    
instance (ToExp a, ToExp b) => ToExp (a, b) where
    to_exp (x, y) = TupE [to_exp x, to_exp y]

instance (ToExp a, ToExp b, ToExp c) => ToExp (a, b, c) where
    to_exp (x, y, z) = TupE [to_exp x, to_exp y, to_exp z]
    
instance ToExp Name where
    to_exp n = to_exp $ show n

--deriving instance Generic Dec
--deriving instance Generic Clause
--deriving instance Generic Pat
--deriving instance Generic Body
--deriving instance Generic FunDep
--deriving instance Generic Foreign
--deriving instance Generic Pragma
--deriving instance Generic FamFlavour
--deriving instance Generic TyVarBndr
--deriving instance Generic Kind
--deriving instance Generic Con

--deriving instance Generic Pred 
--deriving instance Generic Type



--instance ToExp Dec

--mk_template_reflect name = do
--    (TyConI dec) <- reify name
--    return $ [| $(listE . dec) |] 