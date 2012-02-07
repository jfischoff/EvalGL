{-# LANGUAGE TemplateHaskell #-}
module CQuasiTH where
import Language.Haskell.TH
import Control.Applicative

data Simple = Simple 
    {
        x :: Int,
        y :: Float
    }
    
$(do
     print <$> reify ''Simple
     return [])

get_field_names_and_types name = do
    (TyConI (DataD _ _ _ [RecC _ fields] _)) <- reify name
    return $ ListE $ map (\(n, _, (ConT y)) -> TupE [LitE . StringL $ nameBase n, LitE . StringL $ nameBase y]) fields
    
    

    
