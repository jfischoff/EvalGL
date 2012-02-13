{-# LANGUAGE DeriveGeneric, KindSignatures, TemplateHaskell, 
   QuasiQuotes, FlexibleInstances, TypeOperators, TypeSynonymInstances,
   MultiParamTypeClasses, FunctionalDependencies, OverlappingInstances,
   ScopedTypeVariables, EmptyDataDecls, DefaultSignatures, ViewPatterns,
   UndecidableInstances, FlexibleContexts, StandaloneDeriving, IncoherentInstances,
   DeriveDataTypeable #-}
module ResourceManager where
import qualified Data.ByteString as BS
import CType
import NonRecursiveCTypeTH
import TypeLevel.NaturalNumber
import Command
    
type Id   = Int
type Size = Int
type Offset = Int

data IdMissing = IdMissing 
    {
        id_missing_id :: Id
    }

data ResourceEnv = ResourceEnv 
    {
        resources :: [(Id, BS.ByteString)]
    }
    deriving(Show, Eq)
  
data CreateInput = CreateInput {
        create_input_id   :: Id,
        create_input_size :: Size
    }
    
data CreateOutput = CreateOutput
data CreateError  = CreateOutCapacity
                  | CreateOutOfMemory
                  
type CreateCommand = Command CreateInput CreateOutput CreateError

data DeleteInput = DeleteInput 
    {
        delete_input_id :: Id
    }
    
data DeleteOutput = DeleteOutput

type DeleteError = IdMissing

type DeleteCommand = Command DeleteInput DeleteOutput DeleteError
    
data GetInput = GetInput
    {
        get_input :: Id     
    }

data GetOutput = GetOutput
    {
        get_output_bytes :: BS.ByteString
    }

type GetError = IdMissing

type GetCommand = Command GetInput GetOutput GetError

data PutInput = PutInput
    {
        put_input_id   :: Id,
        put_input_data :: BS.ByteString
    }

data PutOutput = PutOutput

type PutError = IdMissing

type PutCommand = Command PutInput PutOutput PutError

data RunInput n a = RunInput
    {
        run_input_offset       :: FixedArray n Offset,
        run_input_offset_count :: Int,
        run_input_command      :: a
    }
    deriving(Show, Eq)
    
data RunError a = RunFixupIdNotFound
                | RunFixupOffsetOutofBounds
                | RunCommandError a
                 
type RunCommand n a b c = Command (RunInput n a) b (RunError c)

data ResourceCommand = C CreateCommand
                     | D DeleteCommand
                     | G GetCommand
                     | P PutCommand
                       
--data ResourceCommandOutput n a =  
                       


               

             