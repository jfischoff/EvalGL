{-# LANGUAGE TemplateHaskell, QuasiQuotes#-}
module Evaluator where
import Language.C hiding (Const)
import Data.Loc
import Language.C.Syntax hiding (Const)
import Language.C.Quote.C
import Data.Symbol
import Data.Char (toUpper, toLower, isUpper, isNumber)
import Data.List 
import ToCUtils

--TODO make this work for ResourceCommand
--the function calling should be uniform
--The I need to make the tests
--I can use quickcheck for testing
--and the ffi 

data FunctionStatement = FunctionStatement
    {
        function_statement_name :: String,
        command_name            :: String,
        parent_command_name     :: String 
    }



evaluator_def name env_name command_type_name = result where
    result = [cedecl| void evaluate( $params:params ); |]
    params = [param_0, param_1]
    param_0  = [cparam| $ty:env_id *env |]
    param_1  = [cparam| $ty:typ_id *command |]
    typ_id = mk_named' command_type_name
    env_id = mk_named' env_name

evaluator env_name command_type_name functions = result where
    result = [cfun| void evaluate( $params:params ){
        switch(command->cmd.type) 
        $stm:switch_block
     }|]
    params = [param_0, param_1]
    param_0  = [cparam| $ty:env_id *env |]
    param_1  = [cparam| $ty:typ_id *command |]
    typ_id = mk_named' command_type_name
    env_id = mk_named' env_name
    case_statements = concatMap (dispatch_function_case command_type_name) functions
    switch_block = Block case_statements noSrcLoc


dispatch_function_case command_name function = result where
    result           = [BlockStm cas, BlockStm $ Break noSrcLoc]
    cas              = Case (Var enum_name noSrcLoc) (Exp (Just $ case_exp) noSrcLoc) noSrcLoc
    struct_name      = function_name_to_struct_name $ function_statement_name function
    enum_name        = function_name_to_enum_name command_name struct_name
    func_id          = mk_id $ function_statement_name function
    env_arg          = Var (mk_id "env") noSrcLoc
    input_arg        = Member (PtrMember (Var (mk_id "command") noSrcLoc) (mk_id struct_name) noSrcLoc) (mk_id "input") noSrcLoc
    output_arg       = Member (PtrMember (Var (mk_id "command") noSrcLoc) (mk_id struct_name) noSrcLoc) (mk_id "output") noSrcLoc
    func_call        = FnCall (Var func_id noSrcLoc) [env_arg, input_arg, output_arg] noSrcLoc
    case_exp         = func_call

function_name_to_enum_name cmd_name fn_name = result where
    enum_name = enum_name_from_name cmd_name
    result    = mk_id $ name_to_enum_option enum_name fn_name

