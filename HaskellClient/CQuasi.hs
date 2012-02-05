{-# LANGUAGE QuasiQuotes, TemplateHaskell, StandaloneDeriving #-}
module CQuasi where

import Data.DeriveTH
import Language.C
import Language.C.Quote.C
import Data.Loc
import Language.C.Syntax
import Data.Symbol
import Text.PrettyPrint.Mainland
import Data.Char (isLower, isUpper, toLower)

$(derive makeShow ''SrcLoc)
$(derive makeShow ''Id)
$(derive makeShow ''Storage)
$(derive makeShow ''TypeQual)
$(derive makeShow ''Sign)
$(derive makeShow ''TypeSpec)
$(derive makeShow ''DeclSpec)
$(derive makeShow ''ArraySize)
$(derive makeShow ''Decl)
$(derive makeShow ''Type)
$(derive makeShow ''Designator)
$(derive makeShow ''Designation)
$(derive makeShow ''Initializer)
$(derive makeShow ''Init)
$(derive makeShow ''Typedef)
$(derive makeShow ''InitGroup)
$(derive makeShow ''Field)
$(derive makeShow ''FieldGroup)
$(derive makeShow ''CEnum)
$(derive makeShow ''Attr)
$(derive makeShow ''Param)
$(derive makeShow ''Params)
$(derive makeShow ''Func)
$(derive makeShow ''Definition)
$(derive makeShow ''Stm)
$(derive makeShow ''BlockItem)
$(derive makeShow ''Signed)
$(derive makeShow ''Const)
$(derive makeShow ''ExeConfig)
$(derive makeShow ''Exp)
$(derive makeShow ''BinOp)
$(derive makeShow ''AssignOp)
$(derive makeShow ''UnOp)

class ToCDescription a where
    to_c_desc :: a -> [Definition]
    
data Add  = Add{
        add_x :: Expression,
        add_y :: Expression
    }    

data Multiply = Multiply {
            multiply_x :: Expression,
            multiply_y :: Expression
        }

data Lit = Lit 
    {
        value :: Int
    }
    
data Expression = EAdd Add
                | EMultiply Multiply
                | ELit Lit 
  
mk_id name = Id name noSrcLoc
    
test_cenum = CEnum (Id "test" noSrcLoc) Nothing noSrcLoc

mk_enum x = [cenum| $id:x|]

mk_enum_decl name options = result where
    result = [cedecl| typedef enum { $enums:enum_options } $id:name ;|]
    enum_options = map mk_enum options
                
enum_decl = mk_enum_decl "EExpressionType" [
                                        "EXPRESSION_TYPE_ADD", 
                                        "EXPRESSION_TYPE_MULTIPLY", 
                                        "EXPRESSION_TYPE_LIT"
                                      ]


mk_pointer_type_def name = result where
    result = [cedecl| typedef struct $id:name_t * $id:name_p; |] 
    name_t = name ++ "_t"
    name_p = name ++ "_p"
    
mk_named name = (Tnamed (mk_id name) noSrcLoc)    
    
mk_type ty = Type (DeclSpec [] [] ty noSrcLoc) (DeclRoot noSrcLoc) noSrcLoc

mk_field_group :: String -> TypeSpec -> FieldGroup
mk_field_group name type_spec = result where
    result  = [csdecl| $ty:ty_type $id:name; |]
    ty_type = mk_type type_spec

mk_record_struct other_defs name name_and_types = result where
    result = [cunit| $edecls:other_defs typedef struct $id:name_t { $sdecls:members } $id:name; |]
    members = map (uncurry mk_field_group) name_and_types
    name_t = name ++ "_t"
    
to_under_score = undefined

mk_struct_decl other_defs name constructor_names = result where
    result = [cunit| $edecls:other_defs typedef struct $id:name_t { 
        $sdecl:type_enum_field $sdecl:union_field } $id:name; |]
    --result = undefined
    name_t = name ++ "_t"
    
    type_enum_name = "E" ++ name ++ "Type"
    type_enum_type = mk_named type_enum_name
    type_enum_field = mk_field_group "type" type_enum_type
    
    child_type_names = map tail constructor_names
    members = map (\x -> mk_field_group (to_under_score x) (mk_named x)) child_type_names
    union_field = FieldGroup (DeclSpec [] [] (Tunion Nothing (Just members) [] noSrcLoc) noSrcLoc) [] noSrcLoc
    --need to manually make a anonomous union field
            
struct_decl = mk_struct_decl [] "Expression" [] 
                    
instance ToCDescription Expression where
    to_c_desc x = enum_decl:struct_decl
                  
