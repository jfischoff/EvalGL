{-# LANGUAGE TemplateHaskell #-}
module ToCTypeTemplate where
    
import StructCreation
import Language.Haskell.TH
import Language.Haskell.TH.Syntax

mk_simple_c_type_record :: Name -> Q [Dec]
mk_simple_c_type_record name = do
    info <- reify name
    let members = get_members info
    return $ [mk_simple_c_type_record' name members]

mk_simple_c_type_record' :: Name -> [VarStrictType] -> Dec
mk_simple_c_type_record' name members = result where
    result = InstanceD [] (AppT (ConT $ mkName "ToCType") (ConT name)) [
            FunD (mkName "to_c_type") [
                Clause [] (NormalB $ AppE (VarE $ mkName "const") $ mk_to_c_exp name members) []]]
                
 
mk_to_c_exp :: Name -> [VarStrictType] -> Exp
mk_to_c_exp name members = AppE (AppE (VarE $ mkName "Conversion.mk_record_ctype") 
                   (LitE (StringL $ show name)))
                   (ListE $ map make_member_arg members)
     
make_member_arg :: VarStrictType -> Exp
make_member_arg (name, _, typ) = InfixE (Just (LitE (StringL $ show name)))
                                (VarE $ mkName "Conversion.<::>")
                                (Just (SigE (VarE $ mkName "undefined") typ))
                                
get_members (TyConI dec) = get_members_dec dec
                                
get_members_dec (DataD _ _ _ [con] _) = get_members_con con 
get_members_dec x = error ("whoops " ++ show x)

get_members_con (RecC _ members) = members 



