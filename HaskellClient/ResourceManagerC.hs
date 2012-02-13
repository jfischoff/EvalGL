{-# LANGUAGE DeriveGeneric, KindSignatures, TemplateHaskell, 
   QuasiQuotes, FlexibleInstances, TypeOperators, TypeSynonymInstances,
   MultiParamTypeClasses, FunctionalDependencies, OverlappingInstances,
   ScopedTypeVariables, EmptyDataDecls, DefaultSignatures, ViewPatterns,
   UndecidableInstances, FlexibleContexts, StandaloneDeriving, IncoherentInstances,
   DeriveDataTypeable #-}
module ResourceManagerC where
import ResourceManager
import qualified Data.ByteString as BS
import CType
import NonRecursiveCTypeTH
import TypeLevel.NaturalNumber
import Language.C 
import Data.Loc
import Language.C.Syntax 
import ResourceManagerQuasi
import Data.Symbol
import Command
import Evaluator

instance ToCType ResourceEnv where
    to_c_type = const $ TStruct "ResourceEnv" [
            TMember "resources" $ TPointer   $ TNamed "Resource",
            TMember "count"     $ TPrimitive CTPInt,
            TMember "capacity"  $ TPrimitive CTPInt,
            TMember "in_use"    $ TPointer   $ TPrimitive CTPInt,
            TMember "evalutor_env"  $ TPointer $ TPrimitive CTPvoid,
            TMember "evalutor_eval" $ TFuncPointer [TPointer $ TPrimitive CTPvoid, 
                TPointer $ TPrimitive CTPvoid]]
        
resource_c = TStruct "Resource" [
        TMember "id"    $ TPrimitive CTPInt,
        TMember "bytes" $ TNamed "ByteString"
    ]

instance ToCType BS.ByteString where
    to_c_type = const $ TStruct "ByteString" [
            TMember "bytes" $ TVariable $ TPrimitive CTPchar
        ]
        

$(mk_c_type_instance' [("a", ["ToCType"]), ("b", ["ToCType"])] ''Either)

$(mk_c_type_instance' [("a", ["ToCType"]), ("b", ["ToCType"]), ("c", ["ToCType"])] ''Command)

$(mk_c_type_instance' [] ''IdMissing)

$(mk_c_type_instance' [] ''CreateInput )
$(mk_c_type_instance' [] ''CreateOutput )
$(mk_c_type_instance' [] ''CreateError )

$(mk_c_type_instance' [] ''DeleteInput )
$(mk_c_type_instance' [] ''DeleteOutput )

$(mk_c_type_instance' [] ''PutInput )
$(mk_c_type_instance' [] ''PutOutput )

$(mk_c_type_instance' [] ''GetInput )
$(mk_c_type_instance' [] ''GetOutput )

$(mk_c_named_members ''ResourceCommand)

--I need to make the evaluator 
--first I need to generalize the evalutor code I already have to include an environment
--and take in handlers

--I need to make the 
--environment and the handlers

copy_bytestring = [cfun| void copy_bytestring(const ByteString* input, ByteString* output) {
                        assert(input->size == output->size);
                        memcpy(output->bytes, input->bytes, input->size);
                }|];

handle_create = [cfun| void create(ResourceEnv* env, CreateInput* input, CreateOutput* output) {
                    assert(env->capacity > env->count);
                    ByteString byte_string = {malloc(input->size), input->size};
                    Resource resource = {input->id, byte_string};
                    
                    for(int i = 0; i < env->capacity; i++) {
                        if(env->in_use[i] == 0) {
                            env->resources[i] = resource;
                            env->in_use[i] = 1;
                        }
                    }
                    
                    env->count++;
                }|]



handle_delete = [cfun| void delete(ResourceEnv* env, const DeleteInput* input, DeleteOutput* output) {
                    assert(env->capacity > env->count);
                    
                    for(int i = 0; i < env->capacity; i++) {
                        if(env->in_use[i] == 1) {
                            if(env->resources[i].id == input->id) {
                                free(env->resources.bytes.bytes);
                                env->in_use[i] = 0;                                
                            }
                        }
                    }
                    
                    env->count--;

                }|]

handle_get   = [cfun| void get(ResourceEnv* env, const GetInput* input, GetOutput* output) {
                    assert(env->in_use[input->id]);
                    
                    for(int i = 0; i < env->capacity; i++) {
                        if(env->resources[i].id == input->id) {
                            output->bytestring = &input->bytes;
                            return;
                        }
                    }
                }|]
                
handle_put    = [cfun| void get(ResourceEnv* env, const PutInput* input, PutOutput* output) {
                    assert(env->in_use[input->id]);

                    for(int i = 0; i < env->capacity; i++) {
                        if(env->resources[i].id == input->id) {
                            copy_bytestring(input->bytes, output->bytestring);
                            return;
                        }
                    }
                }|]
            

    
--get the evalutor creating code
--then actually write the code out
--and compile it 
--make that a main function
--then test it by running it from haskell
--test, test, and release online 
                
            
--get the inputs 
{-
fixup_offset = [cfun| void fixup_offset(ResourceEnv* env, int i, RunInput* input) {
                    int offset    = input->offset[i];
                    char* command = input->command;
                    int id = *(int*)&command[offset]; 
                    ((int*)(&command[offset]))[0] = env->resources[id].bytes.bytes;
            } |]

--call the eval            
handle_run = [cfun| void run(ResourceEnv* env, const RunInput* input, RunOutput* output) {
                    for(int i = 0; i < input->fixup_count; i++) {
                        fixup_offset(env, i, input);
                    }
                    
                    env->evaluator_eval(env->evaluator-env, input->command);
                }
            |]
-}
            
            























