{-# LANGUAGE QuasiQuotes, TemplateHaskell #-}
module NonRecursiveCTypeTH where
import CType
import Language.Haskell.TH
import Language.Haskell.TH.Syntax
import Data.List
import Control.Applicative
import Control.Arrow
import ToCUtils

class ToTH a where
    to_th :: a -> Dec
    
--mk_c_type_instance name = do
--    to_c_type_exp <- mk_c_type' name

--some thoughts if it is fixed arrays I need to handle things differently in the sense
--that some of the tyvars need to be NaturalNumbers
--and some need to be ToCType 
--how does it know what they are?
--existential types?
--no I can use the context of the data type to help me


mk_c_named_members :: Name -> Q [Dec]
mk_c_named_members name = do 
    (TyConI dec) <- reify name 
    (:[]) <$> mk_c_type_instance mk_c_type_named_typ [] [] dec

mk_c_type_instance' :: [(String, [String])] -> Name -> Q [Dec]
mk_c_type_instance' classes' name = do 
    (TyConI dec) <- reify name 
    let classes = map (first mkName) classes'
        cxts = map mk_cxt classes
        typs = map fst  classes
    (:[]) <$> mk_c_type_instance mk_c_type_type typs cxts dec
    
mk_cxt (var_n, class_names) = map (mk_pred var_n) class_names

mk_pred var_n class_name = ClassP (mkName class_name) [(VarT var_n)]

--mk_c_type_instance :: [Cxt] -> Dec -> Q Dec
mk_c_type_instance f typs cxts d@(DataD _ name ty_vars cons _) = do
    let instance_cxt = concat cxts
        header       = appT (conT $ mkName "ToCType") (foldl' appT (conT name) (map varT typs))
        
    instanceD (cxt $ map return instance_cxt) header [mk_c_type_fun f typs d]
    
get_cxt_types cxts = concatMap (map (\(ClassP _ (x:[])) -> (get_type_name x))) cxts

get_type_name (VarT a) = a
get_type_name (ConT a) = a


mk_c_type_fun f typs d = do
    let c = clause [] (normalB $ appE (varE $ mkName "const") $ mk_c_type f typs d) [] 
    funD (mkName "to_c_type") [c]


--mk_c_type :: [Type] -> Dec -> Q Exp
mk_c_type f typs (DataD [] name ty_vars cons _) = 
    [| TStruct $(stringE $ show name) 
        [   TEnum $(stringE enum_name) $(listE $ map stringE enum_options),
            TUnion $(stringE $ show name) $(listE $ map (mk_c_type_con f typs) cons)]|] where
    enum_name    = enum_name_from_name $ show name
    enum_options = map (name_to_enum_option (show name) . show) con_names
    con_names    = map get_con_name cons
    
get_con_name (NormalC x _) = x
get_con_name (RecC x _) = x 
    
    

--mk_c_type_con :: [Types] -> Con -> Q Exp
mk_c_type_con f typs (NormalC name strict_types) = 
    [| TStruct $(stringE $ show name) $(listE $ zipWith (mk_strict_type_member f typs) [0..] strict_types) |]
    
mk_c_type_con f typs (RecC name var_strict_types) = 
    [| TStruct $(stringE $ show name) $(listE $ map (mk_var_strict_type_member f typs) var_strict_types) |]

--mk_strict_type_member :: Int -> StrictType -> Q Exp
mk_strict_type_member f typs index (_, typ) = [| TMember $(stringE name) $(f typs typ) |] where
    name = "x_" ++ (show index)

--mk_var_strict_type_member :: VarStrictType -> Q Exp
mk_var_strict_type_member f typs (name, _, typ) = [| TMember $(stringE $ show name) $(f typs typ) |] 


mk_c_type_type typs x = [| to_c_type (undefined :: $(return $ convert_type typs x)) |]

mk_c_type_named_typ typs (ConT x) = [| TNamed $(stringE $ show x) |]

convert_type typs (VarT x) = VarT $ head $ filter (\t -> head (show x) == head (show t)) typs
convert_type typs c@(ConT x) = c
convert_type typs (AppT x y) = AppT (convert_type typs x) (convert_type typs y)


--------------------------------------------------------------------------------------------------------------

--I need to make the definitions from the functions
--I need to also make the evalutator

type CommandHandler = (String, Func)
                
make_command_code :: Name -> [CommandHandler] -> Q ((String, [Definition]), (String, [Func]))
make_command_code cmd handlers = do
    (TyConI (DataD [] _ _ cons _)) <- reify name 
    let result        = ((header_name, definitions), (source_name, function_defs))
        header_name   = name ++ ".h"
        source_name   = name ++ ".c"
        command_name  = name
        definitions   = undefined
        function_defs = undefined










