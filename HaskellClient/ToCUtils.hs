module ToCUtils where
import Language.C hiding (Const)
import Data.Loc
import Language.C.Syntax hiding (Const)
import Language.C.Quote.C
import Data.Symbol
import Data.Char (toUpper, toLower, isUpper, isNumber)
import Data.List
    
function_name_to_struct_name (x:y:xs) = (toUpper x):(toUpper y):xs

fn_to_sn = function_name_to_struct_name

--ugly  
consume :: String -> (String, [String]) -> (String, [String])    
consume [] (current, old)       = ([], (reverse current):old) 
consume (x:[]) (current, old)   = if (isUpper x || isNumber x) then ([], [x]:(reverse current):old) else ([], (reverse $ x:current):old)
consume (x:y:xs) (current, old) = if (isUpper x || isNumber x) then consume xs (toLower y:(toLower x):[], (reverse $ current):old) else consume (y:xs) (x:current, old)  


to_under_score :: String -> String    
to_under_score camel_case = result where
    (x:concatted) = concat $ intersperse "_" $ reverse $ snd $ consume (camel_case) ([], [])
    result = if x == '_'
                then concatted
                else x:concatted

enum_name_from_name data_type_name = "E" ++ data_type_name ++ "Type"

mk_id name = Id name noSrcLoc

mk_type ty = Type (DeclSpec [] [] ty noSrcLoc) (DeclRoot noSrcLoc) noSrcLoc

mk_named name = (Tnamed (mk_id name) noSrcLoc)

mk_named' name = mk_type $ mk_named name

capitalize = map toUpper

name_to_enum_option (_:enum_name) name = result where
    result = caps_enum_name ++ "_" ++ caps_name
    caps_enum_name = capitalize $ to_under_score enum_name
    caps_name = capitalize $ to_under_score name