{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveGeneric, DeriveDataTypeable, NoMonomorphismRestriction, TemplateHaskell #-}
module Types where
import Data.Word
import qualified Data.ByteString as BS
import Data.Data
import Data.Typeable
import Foreign
import Foreign.C.Types
import Foreign.Storable
import qualified Data.Binary as DB
import qualified Data.Binary.Get as DB
import qualified Data.Binary.Put as DB
import qualified Data.Binary.IEEE754 as DB
import GLPrimitives
import StructCreation
import Conversion
import GHC.Generics
import Test.QuickCheck hiding (Result)
import Test.QuickCheck.Checkers
import Data.DeriveTH

instance ToValue BS.ByteString where
    to_v x = VArray $ map (VPrimitive . PGLubyte . GLubyte) $ BS.unpack x
    
instance ToCType BS.ByteString where
    to_c_type x = mk_record_ctype "ByteString" [
        "bytes" <:::> (TVariable $ TPrimitive TGLubyte)]

data ResourceMapper = ResourceMapper 
    {
        resource_mapper_map   :: GLboolean,
        resource_mapper_ids :: [Id]
    }
    deriving(Show, Eq, Generic)
    
instance ToValue ResourceMapper

instance ToCType ResourceMapper where
    to_c_type x = mk_record_ctype "ResourceMapper" [
        "resource_mapper_map" <::> (undefined :: GLboolean),
        "resource_mapper_ids" <:::> 10 <||> (undefined :: Id)]

instance Arbitrary ResourceMapper where
    arbitrary = do
        x <- arbitrary
        ids <- vectorOf 10 arbitrary
        return $ ResourceMapper x ids 

data ResourceId = NamedResource Id
                | GLResource GLId
                deriving(Show, Eq, Generic)
                
instance ToValue ResourceId
                
$(derive makeArbitrary ''ResourceId)
                
data MemoryLocation = MemoryLocation 
    {
        memory_location_id     :: Id,
        memory_location_offset :: GLuint
    }
    deriving(Show, Eq, Generic)
    
instance ToValue MemoryLocation
    
type Result a b = Either a b
    

data GCommand a b c = GCommand
    {
        cmd :: a,
        result :: Result b c
    }
    deriving(Show, Eq, Generic)
    
--Loop
data LoopInput = LoopInput
    {
        loop_input_command :: Command
    }
    deriving(Show, Eq, Generic)
    
--instance ToValue LoopInput
    
data LoopOutput = LoopOutput
             deriving(Show, Eq, Generic)
data LoopError = LoopError
    deriving(Show, Eq, Generic)
type Loop = GCommand LoopInput LoopOutput LoopError


--AddData
data AddDataInput = AddDataInput
    {
        add_data_id     :: Id,
        add_data_buffer :: BS.ByteString
    }   
    deriving(Show, Eq, Generic)

    
instance ToValue AddDataInput 
    
data AddDataOutput = AddDataOutput
             deriving(Show, Eq, Generic)
instance ToValue AddDataOutput  
            
data AddDataError = AddDataNoRoom
             deriving(Show, Eq, Generic)
instance ToValue AddDataNoRoom 

type AddData = GCommand AddDataInput AddDataOutput AddDataError
--CopyData
data CopyDataInput = CopyDataInput
    {
        copy_data_id      :: Id,
        copy_data_buffer  :: BS.ByteString
    }
             deriving(Show, Eq, Generic)
instance ToValue CopyDataInput
             
data CopyDataOutput = CopyDataOutput
             deriving(Show, Eq, Generic)
instance ToValue CopyDataOutput
             
data CopyDataError  = CopyErrorNoRoom
                    | TooBig
                             deriving(Show, Eq, Generic)
type CopyData = GCommand CopyDataInput CopyDataOutput CopyDataError
--DeleteData
data DeleteDataInput = DeleteDataInput
    {
        delete_data_id :: Id
    }
             deriving(Show, Eq, Generic)
data DeleteDataOutput = DeleteDataOutput
             deriving(Show, Eq, Generic)
data DeleteDataError = DeleteIdNotFound
             deriving(Show, Eq, Generic)
type DeleteData = GCommand DeleteDataInput DeleteDataOutput DeleteDataError
--UpdateData
data UpdateDataInput = UpdateDataInput 
    {
        update_data_id     :: Id,
        update_data_buffer :: BS.ByteString
    }
             deriving(Show, Eq, Generic)
data UpdateDataOutput = UpdateDataOutput
             deriving(Show, Eq, Generic)
data UpdateDataError  = UpdateIdNotFound
             deriving(Show, Eq, Generic)
type UpdateData = GCommand UpdateDataInput UpdateDataOutput UpdateDataError
--Enable
data EnableInput = EnableInput
    {
        enable_state :: GLenum
    }
             deriving(Show, Eq, Generic)
    
data EnableOutput = EnableOutput
   deriving(Show, Eq, Generic)
type EnableError = GLError
type Enable = GCommand EnableInput EnableOutput EnableError
--GenBuffersInput
data GenBuffersInput = GenBuffersInput
    {
        gen_buffers_input_count  :: GLint,
        gen_buffers_input_mapper :: ResourceMapper
    }
    deriving(Show, Eq, Generic)
data GenBuffersOutput = GenBuffersOutput 
    {
        gen_buffers_output_buffers :: [GLuint]
    }
    deriving(Show, Eq, Generic)
type GenBuffersError = GLError
type GenBuffers = GCommand GenBuffersInput GenBuffersOutput GenBuffersError 
--DeleteBuffers 
data DeleteBuffersInput = DeleteBuffersInput
    {
        delete_buffers_input_buffers :: [ResourceId]
    }
             deriving(Show, Eq, Generic)
data DeleteBuffersOutput = DeleteBuffersOutput
             deriving(Show, Eq, Generic)
type DeleteBuffersError = GLError
type DeleteBuffers = GCommand DeleteBuffersInput DeleteBuffersOutput DeleteBuffersError
--BindBuffer
data BindBufferInput = BindBufferInput
    {
        bind_buffer_input_buffer_target :: GLenum,
        bind_buffer_input_id            :: ResourceId
    }
             deriving(Show, Eq, Generic)
data BindBufferOutput = BindBufferOutput
             deriving(Show, Eq, Generic)
type BindBufferError = GLError
type BindBuffer = GCommand BindBufferInput BindBufferOutput BindBufferError
--BufferData
data BufferDataInput = BufferDataInput
    {
        buffer_data_input_buffer_target   :: GLenum,
        buffer_data_input_size            :: GLuint,
        buffer_data_input_memory_location :: MemoryLocation,
        buffer_data_input_usage           :: GLenum
    }
             deriving(Show, Eq, Generic)
data BufferDataOutput = BufferDataOutput
             deriving(Show, Eq, Generic)
type BufferDataError = GLError
type BufferData = GCommand BufferDataInput BufferDataOutput BufferDataError
--EnableVertexAttribArray
data EnableVertexAttribArrayInput = EnableVertexAttribArrayInput
    {
        enable_vertex_attrib_array_input_index :: GLuint
    }
                 deriving(Show, Eq, Generic)
data EnableVertexAttribArrayOutput = EnableVertexAttribArrayOutput
              deriving(Show, Eq, Generic)
type EnableVertexAttribArrayError = GLError
type EnableVertexAttribArray = GCommand EnableVertexAttribArrayInput 
    EnableVertexAttribArrayOutput EnableVertexAttribArrayError 
--VertexAttribPointer
data VertexAttribPointerInput = VertexAttribPointerInput 
    {
        vertex_attrib_pointer_index      :: GLuint,
        vertex_attrib_pointer_size       :: GLint,
        vertex_attrib_pointer_type       :: GLenum,
        vertex_attrib_pointer_normalized :: GLuint,
        vertex_attrib_pointer_stride     :: GLsizei,
        vertex_attrib_pointer_offset     :: GLuint
    }
             deriving(Show, Eq, Generic)
data VertexAttribPointerOutput = VertexAttribPointerOutput
             deriving(Show, Eq, Generic)
type VertexAttribPointerError = GLError
type VertexAttribPoint = GCommand VertexAttribPointerInput VertexAttribPointerOutput
    VertexAttribPointerError
--GenVertexArrayOES
data GenVertexArraysOESInput = GenVertexArraysOESInput 
    {
        gen_vertex_arrays_oes_input_count           :: GLuint,
        gen_vertex_arrays_oes_input_resource_mapper :: ResourceMapper
    }
             deriving(Show, Eq, Generic)
data GenVertexArraysOESOutput = GenVertexArraysOESOutput 
    {
        gen_vertex_array_oes_output_buffers :: [GLuint]
    }
             deriving(Show, Eq, Generic)
type GenVertexArraysOESError = GLError
--BindVertexArrayOES    
data BindVertexArrayOESInput = BindVertexArrayOESInput 
    {
        bind_vertex_array_oes_input :: ResourceId
    }
             deriving(Show, Eq, Generic)
data BindVertexArrayOESOutput = BindVertexArrayOESOutput
             deriving(Show, Eq, Generic)
type BindVertexArrayOESError = GLError
type BindVertexArray = GCommand BindVertexArrayOESInput BindVertexArrayOESOutput
    BindVertexArrayOESError
--CommandList   
data CommandListInput = CommandListInput
    {
        command_list_input_commands :: [Command]
    }
             deriving(Show, Eq, Generic)
data CommandListOutput  = CommandListOutput
             deriving(Show, Eq, Generic)
type CommandListError = GLError
type CommandList = GCommand CommandListInput CommandListOutput CommandListError
--ClearColor
data ClearColorInput = ClearColorInput
    {
        clear_color_input_r :: GLfloat,
        clear_color_input_g :: GLfloat,
        clear_color_input_b :: GLfloat,
        clear_color_input_a :: GLfloat
    }
             deriving(Show, Eq, Generic)
data ClearColorOutput = ClearColorOutput
             deriving(Show, Eq, Generic)
type ClearColorError = GLError
type ClearColor = GCommand ClearColorInput ClearColorOutput ClearColorError
--Clear
data ClearInput = ClearInput
    {
        clear_input_clear_flags :: GLint
    }
     deriving(Show, Eq, Generic)
data ClearOutput = ClearOutput
     deriving(Show, Eq, Generic)
type ClearError = GLError
type Clear = GCommand ClearInput ClearOutput ClearError
--DrawArrays
data DrawArraysInput = DrawArraysInput
    {
        draw_arrays_input_id :: ResourceId
    }
     deriving(Show, Eq, Generic)
data DrawArraysOutput = DrawArraysOutput
     deriving(Show, Eq, Generic)
type DrawArraysError = GLError
type DrawArrays = GCommand DrawArraysInput DrawArraysOutput DrawArraysError
--UseProgram
data UseProgramInput = UseProgramInput 
    {
        use_program_input_id :: ResourceId
    }
     deriving(Show, Eq, Generic)
data UseProgramOutput = UseProgramOutput
     deriving(Show, Eq, Generic)
type UseProgramError = GLError
type UseProgram = GCommand UseProgramInput UseProgramOutput UseProgramError
--UniformMatrix
data UniformMatrixInput = UniformMatrixInput 
    {
        uniform_matrix_input_uniform_type    :: MatrixUniformType,
        uniform_matrix_input_uniform_index   :: GLuint,
        uniform_matrix_input_count           :: GLuint,
        uniform_matrix_input_transpose       :: GLboolean,
        uniform_matrix_input_memory_location :: MemoryLocation 
    }
     deriving(Show, Eq, Generic)
data UniformMatrixOutput = UniformMatrixOutput
     deriving(Show, Eq, Generic)
type UniformMatrixError = GLError
type UniformMatrix = GCommand UniformMatrixInput UniformMatrixOutput UniformMatrixError
--AttachShader
data AttachShaderInput = AttachShaderInput 
    {
        attach_shader_input_program_id :: ResourceId,
        attach_shader_input_shader_id :: ResourceId
    }
     deriving(Show, Eq, Generic)
data AttachShaderOutput = AttachShaderOutput
         deriving(Show, Eq, Generic)
type AttachShaderError = GLError

type AttachShader = GCommand AttachShaderInput AttachShaderOutput AttachShaderError
--BindAttribLocation 
data BindAttribLocationInput = BindAttribLocationInput 
    {
       bind_attrib_location_input_program_id :: ResourceId,
       bind_attrib_location_input_index      :: GLuint,
       bind_attrib_location_input_name       :: String
    }
     deriving(Show, Eq, Generic)
data BindAttribLocationOutput = BindAttribLocationOutput
     deriving(Show, Eq, Generic)
type BindAttribLocationError = GLError
type BindAttribLocation = GCommand BindAttribLocationInput BindAttribLocationOutput 
                                BindAttribLocationError
--CreateProgram
data CreateProgramInput = CreateProgramInput
    {
        create_program_input :: ResourceMapper
    }
     deriving(Show, Eq, Generic)
data CreateProgramOutput = CreateProgramOutput
     deriving(Show, Eq, Generic)
type CreateProgramError = GLError
type CreateProgram = GCommand CreateProgramInput CreateProgramOutput CreateProgramError
--CreateShader
data CreateShaderInput = CreateShaderInput
    {
        create_shader_input_mapper :: ResourceMapper,
        create_shader_input_type   :: GLenum
    }
     deriving(Show, Eq, Generic)
data CreateShaderOutput = CreateShaderOutput
    {
        create_shader_output_id :: GLuint
    }
     deriving(Show, Eq, Generic)
type CreateShaderError = GLError
type CreateShader = GCommand CreateShaderInput CreateShaderOutput CreateShaderError
--ShaderSource  
data ShaderSourceInput = ShaderSourceInput
    {
        shader_source_id               :: ResourceId,
        shader_source_count            :: [GLuint],
        shader_source_source_location  :: MemoryLocation,
        shader_source_lengths          :: [GLint] 
    }
     deriving(Show, Eq, Generic)
             
data ShaderSourceOutput = ShaderSourceOutput
     deriving(Show, Eq, Generic)
type ShaderSourceError = GLError
type ShaderSource = GCommand ShaderSourceInput ShaderSourceOutput ShaderSourceError
--CompileShader    
data CompileShaderInput = CompileShaderInput
    {
        compile_shader_id :: ResourceId
    }
     deriving(Show, Eq, Generic)

data CompileShaderOutput = CompileShaderOutput
     deriving(Show, Eq, Generic)
     
type CompileShaderError = GLError
type CompileShader = GCommand CompileShaderInput CompileShaderOutput CompileShaderError
--LinkProgram
data LinkProgramInput = LinkProgramInput
    {
        link_program_input :: ResourceId
    }
             deriving(Show, Eq, Generic)
data LinkProgramOutput = LinkProgramOutput
             deriving(Show, Eq, Generic)
type LinkProgramError = GLError
type LinkProgram = GCommand LinkProgramInput LinkProgramOutput LinkProgramError
--GetUniformLocation    
data GetUniformLocationInput = GetUniformLocationInput
    {
        get_uniform_location_program_id :: ResourceId,
        get_uniform_location_name       :: String
    }
     deriving(Show, Eq, Generic)
data GetUniformLocationOutput = GetUniformLocationOutput
    {
        get_uniform_location_output_index :: GLuint
    }
     deriving(Show, Eq, Generic)
type GetUniformLocationError = GLError
type GetUniformLocation = GCommand GetUniformLocationInput GetUniformLocationOutput 
                                GetUniformLocationError

data Command = CLoop Loop
             | CAddData AddData
             | CCopyData CopyData
             | CDeleteData DeleteData
             | CUpdateData UpdateData
             | CEnable Enable
             | CGenBuffers GenBuffers
             | CDeleteBuffers DeleteBuffers
             | CBindBuffer BindBuffer
             | CBufferData BufferData
             | CEnableVertexAttribArray EnableVertexAttribArray
             | CVertexAttribPoint VertexAttribPoint
             | CBindVertexArray BindVertexArray
             | CCommandList CommandList
             | CClearColor ClearColor
             | CClear Clear
             | CDrawArrays DrawArrays
             | CUseProgram UseProgram
             | CUniformMatrix UniformMatrix
             | CAttachShader AttachShader
             | CBindAttribLocation BindAttribLocation
             | CCreateProgram CreateProgram
             | CCreateShader CreateShader
             | CShaderSource ShaderSource
             | CCompileShader CompileShader
             | CLinkProgram LinkProgram 
             | CGetUniformLocation GetUniformLocation
             deriving(Show, Eq, Generic)





















