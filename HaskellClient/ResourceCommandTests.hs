module ResourceCommandTests where
import Evaluator
import Test.Framework (defaultMain, testGroup, defaultMainWithArgs)
import Test.Framework.Providers.HUnit
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck
import Test.HUnit
import Text.Parsec
import Debug.Trace.Helpers
import Debug.Trace    
import Text.PrettyPrint.Mainland

main = defaultMainWithArgs tests ["-a 100"]

tests = [
        testGroup "EvaluatorTest" [
            testCase "test_evaluator_def_0" test_evaluator_def_0,
            testCase "test_dispatch_function_case_0" test_dispatch_function_case_0
        ]
    ]
    

    
test_evaluator_def_0 = actual @?= expected where
    actual   = show $ ppr $ evaluator_def "ResourceEnvironment" "ResourceCommand"
    expected = "void evaluate(ResourceEnvironment* env, ResourceCommand* command);" 
    
put_function = FunctionStatement
    {
        function_statement_name = "put",
        command_name            = "PutCommand",
        parent_command_name     = "ResourceCommand"
    }
    
    
get_function = FunctionStatement
    {
        function_statement_name = "get",
        command_name            = "GetCommand",
        parent_command_name     = "ResourceCommand"
    }

create_function = FunctionStatement
    {
        function_statement_name = "Create",
        command_name            = "CreateCommand",
        parent_command_name     = "ResourceCommand"
    }
    
delete_function = FunctionStatement
    {
        function_statement_name = "Delete",
        command_name            = "DeleteCommand",
        parent_command_name     = "ResourceCommand"
    }
    
test_dispatch_function_case_0 = actual @?= expected where
    actual   = show $ ppr $ dispatch_function_case "ResourceCommand" put_function
    expected = "{\n    \n  case RESOURCE_COMMAND_TYPE_PUT:\n    put(env, command->put.input, command->put.output);\n    break;\n}"

test_evaluator_0 = actual @?= expected where
    actual   = show $ ppr $ evaluator "env" "ResourceCommand" []
    
    
    
    
    
    
    
    
    
    
    
    
    