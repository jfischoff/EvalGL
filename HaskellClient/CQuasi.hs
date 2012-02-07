{-# LANGUAGE QuasiQuotes, DeriveDataTypeable, TemplateHaskell, StandaloneDeriving, NoMonomorphismRestriction #-}
module CQuasi where

import Data.DeriveTH
import Language.C
import Language.C.Quote.C
import Data.Loc
import Language.C.Syntax
import Data.Symbol
import Text.PrettyPrint.Mainland
import Data.Char (isLower, isUpper, toLower, toUpper)
import Data.List
import Control.Arrow
import Control.Monad.State
import Control.Applicative hiding (Const)
import Data.Maybe
import Data.Tuple.Select
import Debug.Trace
import Debug.Trace.Helpers
import Data.Data
import Data.Typeable
import Data.List.Split
import CQuasiTH

$(derive makeShow ''SrcLoc)
$(derive makeShow ''Id)
$(derive makeFrom ''Id)

$(derive makeShow ''Storage)
$(derive makeIs   ''Storage)
$(derive makeFrom ''Storage)

$(derive makeShow ''TypeQual)
$(derive makeIs   ''TypeQual)
$(derive makeFrom ''TypeQual)

$(derive makeShow ''Sign)
$(derive makeIs   ''Sign)
$(derive makeFrom ''Sign)

$(derive makeShow ''TypeSpec)
$(derive makeIs   ''TypeSpec)
$(derive makeFrom ''TypeSpec)

$(derive makeShow ''DeclSpec)
$(derive makeIs   ''DeclSpec)
$(derive makeFrom ''DeclSpec)

$(derive makeShow ''ArraySize)
$(derive makeIs   ''ArraySize)
$(derive makeFrom ''ArraySize)

$(derive makeShow ''Decl)
$(derive makeIs   ''Decl)
$(derive makeFrom ''Decl)

$(derive makeShow ''Type)
$(derive makeIs   ''Type)
$(derive makeFrom ''Type)

$(derive makeShow ''Designator)
$(derive makeIs   ''Designator)
$(derive makeFrom ''Designator)

$(derive makeShow ''Designation)
$(derive makeIs   ''Designation)
$(derive makeFrom ''Designation)

$(derive makeShow ''Initializer)
$(derive makeIs   ''Initializer)
$(derive makeFrom ''Initializer)

$(derive makeShow ''Init)
$(derive makeIs   ''Init)
$(derive makeFrom ''Init)

$(derive makeShow ''Typedef)
$(derive makeIs   ''Typedef)
$(derive makeFrom ''Typedef)

$(derive makeShow ''InitGroup)
$(derive makeIs   ''InitGroup)
$(derive makeFrom ''InitGroup)

$(derive makeShow ''Field)
$(derive makeIs   ''Field)
$(derive makeFrom ''Field)

$(derive makeShow ''FieldGroup)
$(derive makeIs   ''FieldGroup)
$(derive makeFrom ''FieldGroup)

$(derive makeShow ''CEnum)
$(derive makeIs   ''CEnum)
$(derive makeFrom ''CEnum)

$(derive makeShow ''Attr)
$(derive makeIs   ''Attr)
$(derive makeFrom ''Attr)

$(derive makeShow ''Param)
$(derive makeIs   ''Param)
$(derive makeFrom ''Param)

$(derive makeShow ''Params)
$(derive makeIs   ''Params)
$(derive makeFrom ''Params)

$(derive makeShow ''Func)
$(derive makeIs   ''Func)
$(derive makeFrom ''Func)

$(derive makeShow ''Definition)
$(derive makeIs   ''Definition)
$(derive makeFrom ''Definition)

$(derive makeShow ''Stm)
$(derive makeIs   ''Stm)
$(derive makeFrom ''Stm)

$(derive makeShow ''BlockItem)
$(derive makeIs   ''BlockItem)
$(derive makeFrom ''BlockItem)

$(derive makeShow ''Signed)
$(derive makeIs   ''Signed)
$(derive makeFrom ''Signed)

$(derive makeShow ''Const)
$(derive makeIs   ''Const)
-- $(derive makeFrom ''Const)

$(derive makeShow ''ExeConfig)
$(derive makeIs   ''ExeConfig)
$(derive makeFrom ''ExeConfig)

$(derive makeShow ''Exp)
$(derive makeIs   ''Exp)
$(derive makeFrom ''Exp)

$(derive makeShow ''BinOp)
$(derive makeIs   ''BinOp)
$(derive makeFrom ''BinOp)

$(derive makeShow ''AssignOp)
$(derive makeIs   ''AssignOp)
$(derive makeFrom ''AssignOp)

$(derive makeShow ''UnOp)
$(derive makeIs   ''UnOp)
$(derive makeFrom ''UnOp)

--I need to make sure that primitives are handled properly
--I need a list of types that are primitive

type GLchar  = Char
type GLfloat = Float
type GLint   = Int

type ToCState = State ([String], [Definition], [String])

primitives = ["GLchar", "GLfloat", "GLint"]

class ToCDescription a where
    to_c_desc :: a -> ToCState ()
    
data Add  = Add{
        add_x :: Expression,
        add_y :: Expression
    }  
    deriving(Show, Eq, Data, Typeable)  

data Multiply = Multiply {
            multiply_x :: Expression,
            multiply_y :: Expression
        }
        deriving(Show, Eq, Data, Typeable)

data Lit = Lit 
    {
        value :: GLchar
    }
    deriving(Show, Eq, Data, Typeable)
    
data Expression = EAdd Add
                | EMultiply Multiply
                | ELit Lit 
                deriving(Show, Eq, Data, Typeable)
                
  
mk_id name = Id name noSrcLoc
    
test_cenum = CEnum (Id "test" noSrcLoc) Nothing noSrcLoc

mk_enum x = [cenum| $id:x|]

mk_enum_decl other_defs name options = result where
    result = [cunit| $edecls:other_defs typedef enum { $enums:enum_options } $id:name ;|]
    enum_options = map mk_enum options
      
to_pointer_name name = name ++ "_p"

mk_pointer_type_def others name = result where
    result = [cunit| $edecls:others typedef struct $id:name_t * $id:name_p; |] 
    name_t = name ++ "_t"
    name_p = to_pointer_name name
    
mk_named name = (Tnamed (mk_id name) noSrcLoc)   

mk_named' name = mk_type $ mk_named name
    
mk_type ty = Type (DeclSpec [] [] ty noSrcLoc) (DeclRoot noSrcLoc) noSrcLoc

--mk_field_group :: String -> Type -> FieldGroup
mk_field_group name type_spec = result where
    result  = [csdecl| $ty:ty_type $id:name; |]
    ty_type = type_spec

mk_record_struct other_defs name name_and_types = result where
    result = [cunit| $edecls:other_defs typedef struct $id:name_t { $sdecls:members } $id:name; |]
    members = map (uncurry mk_field_group) name_and_types
    name_t = name ++ "_t"
  
--ugly  
consume :: String -> (String, [String]) -> (String, [String])    
consume [] (current, old)       = ([], (reverse current):old) 
consume (x:[]) (current, old)   = if isUpper x then ([], [x]:(reverse current):old) else ([], (reverse $ x:current):old)
consume (x:y:xs) (current, old) = if isUpper x then consume xs (y:(toLower x):[], (reverse $ current):old) else consume (y:xs) (x:current, old)  
    
to_under_score :: String -> String    
to_under_score camel_case = concat $ intersperse "_" $ reverse $ snd $ consume (tail camel_case) ([toLower $ head camel_case], [])

mk_struct_decl other_defs name constructor_names = result where
    result = [cunit| $edecls:other_defs typedef struct $id:name_t { 
        $sdecl:type_enum_field $sdecl:union_field } $id:name; |]

    name_t = name ++ "_t"
    
    type_enum_name = "E" ++ name ++ "Type"
    type_enum_type = mk_named' type_enum_name
    type_enum_field = mk_field_group "type" type_enum_type
    
    child_type_names = map tail constructor_names
    members = map (\x -> mk_field_group (to_under_score x) (mk_named' x)) child_type_names
    union_field = FieldGroup (DeclSpec [] [] (Tunion Nothing (Just members) [] noSrcLoc) noSrcLoc) [] noSrcLoc

contains x xs = any (x==) xs
 
mk_pointer_type_def' name = do
    (primitives, other, typedefs) <- get
    if not . contains name $ typedefs
        then put(primitives, mk_pointer_type_def other name, name:typedefs)
        else return ()
        
put_1 x = do
    (p, _, y) <- get
    put(p, x, y)
 
mk_struct_decl' name constructor_names = do
    other <- gets sel2
    put_1(mk_struct_decl other name constructor_names)
    
mk_enum_decl' name options = do
    other <- gets sel2
    put_1(mk_enum_decl other name options)    
    
mk_record_struct' name name_and_types = do
    other <- gets sel2
    put_1(mk_record_struct other name name_and_types)
                
struct_decl = mk_struct_decl [] "Expression" ["EAdd", "EMultiply", "ELit"] 

enum_name_from_data_type data_type_name = "E" ++ data_type_name ++ "Type"

capitalize = map toUpper

constructor_name_to_enum_option (_:enum_name) (_:cst_name) = result where
    result = caps_enum_name ++ "_" ++ caps_cst_name
    caps_enum_name = capitalize $ to_under_score enum_name
    caps_cst_name = capitalize $ to_under_score cst_name

mk_adt data_type_name constructor_names = do
    let enum_name    = enum_name_from_data_type data_type_name
        enum_options = map (constructor_name_to_enum_option enum_name) constructor_names 
    mk_enum_decl' enum_name enum_options
    mk_struct_decl' data_type_name constructor_names
    
is_struct_or_union (DecDef (TypedefGroup (DeclSpec [] [] (Tstruct _ _ _ _) _) _ _ _) _) = True
is_struct_or_union (DecDef (TypedefGroup (DeclSpec [] [] (Tunion _ _ _ _) _) _ _ _) _) = True
is_struct_or_union _ = False

get_struct_or_union_name :: Definition -> String
get_struct_or_union_name (DecDef (TypedefGroup (DeclSpec [] [] (Tstruct (Just (Id name _ )) _ _ _) _) _ _ _) _) = name
get_struct_or_union_name (DecDef (TypedefGroup (DeclSpec [] [] (Tunion (Just (Id name _ )) _ _ _) _) _ _ _) _) = name

get_types = do
    (primitives, xs, _) <- get
    let struct_and_unions = filter is_struct_or_union xs
        struct_or_union_names = map get_struct_or_union_name struct_and_unions
    return (struct_or_union_names ++ primitives)
    
--the the type is not defined
--then create a typedef pointer
--and return the pointer name
--else just do nothing and return the name    
ensure_type_definition :: String -> ToCState String 
ensure_type_definition type_name = do
    types <- get_types
    if contains type_name types 
        then return type_name
        else do mk_pointer_type_def' type_name
                return $ to_pointer_name type_name
        
create_name_type type_name = do
    new_type_name <- ensure_type_definition type_name
    return $ mk_named' new_type_name
  
create_int_type type_name = if type_name == "GLint"
    then return $ Just [cty|int|]
    else return Nothing
  
create_float_type type_name = if type_name == "GLfloat"
    then return $ Just [cty|float|]
    else return Nothing

create_char_type type_name = if type_name == "GLchar"
    then return $ Just [cty|char|]
    else return Nothing

primitive_creates = [create_int_type, create_float_type, create_char_type] 
 
try_one_of (x:xs) type_name = do
    result <- x type_name
    if isJust result
        then return $ fromJust result
        else try_one_of xs type_name
try_one_of [] _ = error "bad primitive!"
 
create_primitive_type type_name = try_one_of primitive_creates type_name

is_primitive_name name = contains name primitives
        
create_type type_name = do
    if is_primitive_name type_name 
        then create_primitive_type type_name
        else create_name_type type_name
   
--I should be able to get the constructor names from the data.data class

run_c x = sel2 $ execState (to_c_desc x) (primitives, [], [])



instance ToCDescription Add where
    to_c_desc x = mk_record_to_c_desc "Add" $(get_field_names_and_types ''Add)
   
instance ToCDescription Multiply where
    to_c_desc x = mk_record_to_c_desc "Multiply" $(get_field_names_and_types ''Multiply)
            
instance ToCDescription Lit where
    to_c_desc x = mk_record_to_c_desc "Lit" $(get_field_names_and_types ''Lit)
                    
instance ToCDescription Expression where
    to_c_desc x = mk_ast_c_desc x
             
--things to do
--lets assume generalizing this is easy
--the next question is how to use this to get out the size
--of everything so I can make the CValue
--so one change is all alignment decisions happen here

mk_ast_c_desc :: (Data a, Typeable a) => a -> ToCState ()
mk_ast_c_desc x = mk_adt data_type_name constructor_names where
    data_type_name = tyconUQname $ dataTypeName $ dataTypeOf x
    constructor_names = map showConstr $ dataTypeConstrs $ dataTypeOf x
    
mk_record_to_c_desc name record_names_and_types = do
    types <- mapM create_type $ map snd record_names_and_types
    let record_name_and_type' = zip (map (head . reverse . splitOn "_" . fst) record_names_and_types) types
            
    mk_record_struct' name record_name_and_type'
    
    
traceItNote note x = trace (note ++ (show x)) x

fromDeclSpec_TypeSpec = sel3 . fromDeclSpec 


fromDecDef_InitGroup = sel1 . fromDecDef

fromInitGroup_DeclSpec = sel1 . fromInitGroup

from_def_decl = fromInitGroup_DeclSpec . fromDecDef_InitGroup

expression_as_c = sequence [
        to_c_desc (undefined :: Add),
        to_c_desc (undefined :: Multiply), 
        to_c_desc (undefined :: Lit),
        to_c_desc (undefined :: Expression)
    ]
    

defs = sel2 $ execState (expression_as_c) (primitives, [], [])

--get_def_sizes xs = map (size_of_def (get_types_pairs xs)) xs

--I am unsure of how to handle typedefs
--first there is the question of deep queries
--I think type classes would be useful here
--is_ptr
--is_struct


--another option is to pattern match on everything

--size_of :: [(Id, InitGroup)] -> InitGroup -> Int
--size_of types ()



--I need a way to collect all the named types and make a table
get_types_pairs :: [Definition] -> [(Id, Definition)]
get_types_pairs defs = concatMap get_types_pairs' defs

get_types_pairs' :: Definition -> [(Id, Definition)]
get_types_pairs' def | isDecDef def = get_types_from_init_group def $ sel1 $ fromDecDef def
get_types_pairs' def | otherwise = []

get_types_from_init_group :: Definition -> InitGroup -> [(Id, Definition)]
get_types_from_init_group x (InitGroup decl _ _ _)    = get_types_from_decl x decl
get_types_from_init_group x (TypedefGroup decl _ typedefs _) = get_types_from_decl x decl ++ 
    zip (map (sel1 . fromTypedef ) typedefs) (cycle [x])

get_types_from_decl x decl = result where
    result = get_named_type_from_type_spec x type_spec
    type_spec = sel3 $ fromDeclSpec decl
    
get_named_type_from_type_spec x (Tstruct (Just i) _ _ _) = [(i, x)]
get_named_type_from_type_spec x (Tunion (Just i) _ _ _) = [(i, x)]
get_named_type_from_type_spec x y = []

fromIntConst (IntConst x y z w) = (x,y,z,w)
{-
get_field_sizes types x =  $ size_of types $ sel1 $ fromFieldGroup x

round_up_to amount x =  (((2 * amount) - (x `mod` amount)) `mod` amount) + x

field_sizes :: [(Id, Definition)] -> FieldGroup -> [MemberSize]
field_sizes types x = map (get_field_sizes types) $ maybe [] id $ sel2 x

array_size = fromIntegral . sel3 . fromIntConst . sel1 . fromConst . sel2 . fromArraySize . sel2 . fromArray . sel2 . fromType 

size_of_type types x | isPtr      . sel2 $ fromType x = Leaf 4
size_of_type types x | isArray    . sel2 $ fromType x = Leaf $ (array_size x) * ((\(MemberNode x y) -> y) $ size_of types $ sel1 $ fromType x)
size_of_type types x | isDeclRoot . sel2 $ fromType x = size_of types $ sel1 $ fromType x



size_of :: [(Id, Definition)] -> DeclSpec -> MemberSize
size_of types x | isTtypeofType  $ fromDeclSpec_TypeSpec x = size_of_type types $ sel1 $ fromTtypeofType $ fromDeclSpec_TypeSpec x
size_of types x | isTchar        $ fromDeclSpec_TypeSpec x = Leaf 1
size_of types x | isTshort       $ fromDeclSpec_TypeSpec x = Leaf 2
size_of types x | isTint         $ fromDeclSpec_TypeSpec x = Leaf 4
size_of types x | isTlong        $ fromDeclSpec_TypeSpec x = Leaf 4
size_of types x | isTlong_long   $ fromDeclSpec_TypeSpec x = Leaf 8
size_of types x | isTfloat       $ fromDeclSpec_TypeSpec x = Leaf 4
size_of types x | isTdouble      $ fromDeclSpec_TypeSpec x = Leaf 8
size_of types x | isTlong_double $ fromDeclSpec_TypeSpec x = error "no such thing ... at least for my purposes"
size_of types x | isTstruct      $ fromDeclSpec_TypeSpec x = MemberSizeNode $ field_sizes types $ fromTstruct $ fromDeclSpec_TypeSpec x
size_of types x | isTunion       $ fromDeclSpec_TypeSpec x = MemberSizeNode $ field_sizes types $ fromTunion  $ fromDeclSpec_TypeSpec x
size_of types x | isTenum        $ fromDeclSpec_TypeSpec x = Leaf 4
size_of types x | isTnamed       $ fromDeclSpec_TypeSpec x = 
    size_of_def types . fromJust $ lookup (fst . fromTnamed $ fromDeclSpec_TypeSpec x) types 


size_of_def types x = size_of_init_group types $ sel1 $ fromDecDef x

size_of_init_group types (InitGroup x _ _ _) = size_of types x
size_of_init_group types (TypedefGroup x _ typedef _) | isPtr . sel2 . fromTypedef $ head typedef = Leaf 4
size_of_init_group types (TypedefGroup x _ typedef _) | otherwise = size_of types x

-}


            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
            
                  
