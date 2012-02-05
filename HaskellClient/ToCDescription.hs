module ToCDescription where
import Language.C.Syntax.AST
import Language.C.Data.Ident
import Language.C

type Decl = CDeclaration ()



class ToCDescription a where
    to_c_desc :: a -> [Decl]
    
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
    
mk_options options = map (\x -> (internalIdent $ x, Nothing)) options
                    
mk_enum :: String -> [String] -> Decl
mk_enum name options = CDecl [CStorageSpec $ CTypedef (), 
                CTypeSpec $
                    CEnumType (
                        CEnum Nothing (
                            Just $ mk_options options
                        ) [] ()
                ) ()
            ] [(Just $ CDeclr (Just $ internalIdent name) [] Nothing [] (), 
                Nothing, Nothing)] ()
                
enum_decl = mk_enum "EExpressionType" ["EXPRESSION_TYPE_ADD", "EXPRESSION_TYPE_MULTIPLY", "EXPRESSION_TYPE_LIT"]

mk_struct_pointer_typedef old new = CDecl 
    [
        CStorageSpec 
        (
            CTypedef 
            (
                ()
            )
        ),
        CTypeSpec 
        (
            CSUType 
            (
                CStruct CStructTag (Just old) Nothing [] ()
            ) ()
        )        
    ] 
    [
        (
            Just 
            (
                CDeclr (Just new) [] Nothing [] ()
            ),
            Nothing,
            Nothing
        )
    ] () 
    
mk_struct_pointer_typedef old new = [c| typedef struct $(old) $(new);]

struct_decl = mk_adt "Expression" 
                [
                    ("Add", False),
                    ("Multiply", False),
                    ("Lit", False)
                ]
                
mk_adt = undefined
                    
instance ToCDescription Expression where
    to_c_desc x = [
                     enum_decl,
                     struct_decl
                  ]
                  
                  
                  
                  
                  


Right (
        CTranslUnit 
        [
            CDeclExt (CDecl [CStorageSpec (CTypedef (NodeInfo ("CTest.h": line 3) (("CTest.h": line 3),7) (Name {nameId = 7}))),CTypeSpec (CSUType (CStruct CStructTag (Just "Add_t") (Just [CDecl [CTypeSpec (CTypeDef "Expression_p" (NodeInfo ("CTest.h": line 4) (("CTest.h": line 4),12) (Name {nameId = 11})))] [(Just (CDeclr (Just "x") [] Nothing [] (NodeInfo ("CTest.h": line 4) (("CTest.h": line 4),1) (Name {nameId = 12}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 4) (("CTest.h": line 4),1) (Name {nameId = 13})),CDecl [CTypeSpec (CTypeDef "Expression_p" (NodeInfo ("CTest.h": line 5) (("CTest.h": line 5),12) (Name {nameId = 16})))] [(Just (CDeclr (Just "y") [] Nothing [] (NodeInfo ("CTest.h": line 5) (("CTest.h": line 5),1) (Name {nameId = 17}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 5) (("CTest.h": line 5),1) (Name {nameId = 18}))]) [] (NodeInfo ("CTest.h": line 3) (("CTest.h": line 6),1) (Name {nameId = 20}))) (NodeInfo ("CTest.h": line 3) (("CTest.h": line 6),1) (Name {nameId = 21})))] [(Just (CDeclr (Just "Add") [] Nothing [] (NodeInfo ("CTest.h": line 6) (("CTest.h": line 6),3) (Name {nameId = 22}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 3) (("CTest.h": line 6),1) (Name {nameId = 23}))),CDeclExt (CDecl [CStorageSpec (CTypedef (NodeInfo ("CTest.h": line 8) (("CTest.h": line 8),7) (Name {nameId = 24}))),CTypeSpec (CSUType (CStruct CStructTag (Just "Multiply_t") (Just [CDecl [CTypeSpec (CTypeDef "Expression_p" (NodeInfo ("CTest.h": line 9) (("CTest.h": line 9),12) (Name {nameId = 28})))] [(Just (CDeclr (Just "x") [] Nothing [] (NodeInfo ("CTest.h": line 9) (("CTest.h": line 9),1) (Name {nameId = 29}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 9) (("CTest.h": line 9),1) (Name {nameId = 30})),CDecl [CTypeSpec (CTypeDef "Expression_p" (NodeInfo ("CTest.h": line 10) (("CTest.h": line 10),12) (Name {nameId = 33})))] [(Just (CDeclr (Just "y") [] Nothing [] (NodeInfo ("CTest.h": line 10) (("CTest.h": line 10),1) (Name {nameId = 34}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 10) (("CTest.h": line 10),1) (Name {nameId = 35}))]) [] (NodeInfo ("CTest.h": line 8) (("CTest.h": line 11),1) (Name {nameId = 37}))) (NodeInfo ("CTest.h": line 8) (("CTest.h": line 11),1) (Name {nameId = 38})))] [(Just (CDeclr (Just "Multiply") [] Nothing [] (NodeInfo ("CTest.h": line 11) (("CTest.h": line 11),8) (Name {nameId = 39}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 8) (("CTest.h": line 11),1) (Name {nameId = 40}))),CDeclExt (CDecl [CStorageSpec (CTypedef (NodeInfo ("CTest.h": line 13) (("CTest.h": line 13),7) (Name {nameId = 41}))),CTypeSpec (CSUType (CStruct CStructTag (Just "Lit_t") (Just [CDecl [CTypeSpec (CIntType (NodeInfo ("CTest.h": line 14) (("CTest.h": line 14),3) (Name {nameId = 44})))] [(Just (CDeclr (Just "value") [] Nothing [] (NodeInfo ("CTest.h": line 14) (("CTest.h": line 14),5) (Name {nameId = 45}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 14) (("CTest.h": line 14),5) (Name {nameId = 46}))]) [] (NodeInfo ("CTest.h": line 13) (("CTest.h": line 15),1) (Name {nameId = 48}))) (NodeInfo ("CTest.h": line 13) (("CTest.h": line 15),1) (Name {nameId = 49})))] [(Just (CDeclr (Just "Lit") [] Nothing [] (NodeInfo ("CTest.h": line 15) (("CTest.h": line 15),3) (Name {nameId = 50}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 13) (("CTest.h": line 15),1) (Name {nameId = 51}))),CDeclExt (CDecl [CStorageSpec (CTypedef (NodeInfo ("CTest.h": line 17) (("CTest.h": line 17),7) (Name {nameId = 52}))),CTypeSpec (CEnumType (CEnum Nothing (Just [("EXPRESSION_TYPE_ADD",Nothing),("EXPRESSION_TYPE_MULTIPLY",Nothing),("EXPRESSION_TYPE_LIT",Nothing)]) [] (NodeInfo ("CTest.h": line 17) (("CTest.h": line 21),1) (Name {nameId = 57}))) (NodeInfo ("CTest.h": line 17) (("CTest.h": line 21),1) (Name {nameId = 58})))] [(Just (CDeclr (Just "EExpressionType") [] Nothing [] (NodeInfo ("CTest.h": line 21) (("CTest.h": line 21),15) (Name {nameId = 59}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 17) (("CTest.h": line 21),1) (Name {nameId = 60}))),CDeclExt (CDecl [CStorageSpec (CTypedef (NodeInfo ("CTest.h": line 24) (("CTest.h": line 24),7) (Name {nameId = 61}))),CTypeSpec (CSUType (CStruct CStructTag (Just "Expression_t") (Just [CDecl [CTypeSpec (CTypeDef "EExpressionType" (NodeInfo ("CTest.h": line 25) (("CTest.h": line 25),15) (Name {nameId = 65})))] [(Just (CDeclr (Just "type") [] Nothing [] (NodeInfo ("CTest.h": line 25) (("CTest.h": line 25),4) (Name {nameId = 66}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 25) (("CTest.h": line 25),4) (Name {nameId = 67})),CDecl [CTypeSpec (CSUType (CStruct CUnionTag Nothing (Just [CDecl [CTypeSpec (CTypeDef "Add" (NodeInfo ("CTest.h": line 28) (("CTest.h": line 28),3) (Name {nameId = 70})))] [(Just (CDeclr (Just "add") [] Nothing [] (NodeInfo ("CTest.h": line 28) (("CTest.h": line 28),3) (Name {nameId = 71}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 28) (("CTest.h": line 28),3) (Name {nameId = 72})),CDecl [CTypeSpec (CTypeDef "Multiply" (NodeInfo ("CTest.h": line 29) (("CTest.h": line 29),8) (Name {nameId = 75})))] [(Just (CDeclr (Just "multiply") [] Nothing [] (NodeInfo ("CTest.h": line 29) (("CTest.h": line 29),8) (Name {nameId = 76}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 29) (("CTest.h": line 29),8) (Name {nameId = 77})),CDecl [CTypeSpec (CTypeDef "Lit" (NodeInfo ("CTest.h": line 30) (("CTest.h": line 30),3) (Name {nameId = 80})))] [(Just (CDeclr (Just "lit") [] Nothing [] (NodeInfo ("CTest.h": line 30) (("CTest.h": line 30),3) (Name {nameId = 81}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 30) (("CTest.h": line 30),3) (Name {nameId = 82}))]) [] (NodeInfo ("CTest.h": line 27) (("CTest.h": line 31),1) (Name {nameId = 83}))) (NodeInfo ("CTest.h": line 27) (("CTest.h": line 31),1) (Name {nameId = 84})))] [] (NodeInfo ("CTest.h": line 27) (("CTest.h": line 31),1) (Name {nameId = 85}))]) [] (NodeInfo ("CTest.h": line 24) (("CTest.h": line 32),1) (Name {nameId = 87}))) (NodeInfo ("CTest.h": line 24) (("CTest.h": line 32),1) (Name {nameId = 88})))] [(Just (CDeclr (Just "Expression") [] Nothing [] (NodeInfo ("CTest.h": line 32) (("CTest.h": line 32),10) (Name {nameId = 89}))),Nothing,Nothing)] (NodeInfo ("CTest.h": line 24) (("CTest.h": line 32),1) (Name {nameId = 90})))] (NodeInfo ("CTest.h": line 1) (("CTest.h": line 32),1) (Name {nameId = 91})))










                  
                  
                  
                  
                  
                  
                  
                  
                  
                  