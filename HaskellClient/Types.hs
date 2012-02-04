{-# LANGUAGE GeneralizedNewtypeDeriving, DeriveDataTypeable #-}
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



newtype GLbitfield = GLbitfield Word32 
    deriving(Show, Eq, Num, Storable, DB.Binary)
newtype GLboolean  = GLboolean Bool
    deriving(Show, Eq, Storable)

instance DB.Binary GLboolean where
    put (GLboolean x) = if x then DB.putWord8 1 else DB.putWord8 0    
    get = undefined

newtype GLbyte     = GLbyte Word8
    deriving(Show, Eq, Num, Storable, DB.Binary)
newtype GLchar     = GLchar Char
    deriving(Show, Eq, Storable, DB.Binary)
newtype GLclampf   = GLclampf Float
    deriving(Show, Eq, Num, Storable, Ord, Fractional, Real)
    
instance DB.Binary GLclampf where
    put (GLclampf x) = DB.putFloat32le x
    get = undefined
    
newtype GLenum     = GLenum Word32
    deriving(Show, Eq, Num, Storable, DB.Binary)
newtype GLfloat    = GLfloat Float
    deriving(Show, Eq, Num, Storable, Ord, Fractional, Real)

instance DB.Binary GLfloat where
    put (GLfloat x) = DB.putFloat32le x
    get = undefined  
      
newtype GLint = GLint Int32
    deriving(Show, Eq, Num, Storable)
    
instance DB.Binary GLint where
    put (GLint x) = DB.putWord32le $ fromIntegral x
    get = undefined 
    
newtype GLshort    = GLshort Int16
    deriving(Show, Eq, Num, Storable)

instance DB.Binary GLshort where
    put (GLshort x) = DB.putWord16le $ fromIntegral x
    get = undefined 
    
newtype GLsizei    = GLsizei Word32
    deriving(Show, Eq, Num, Storable, DB.Binary)
newtype GLubyte    = GLubyte Word8
    deriving(Show, Eq, Num, Storable, DB.Binary)
newtype GLuint     = GLuint Word32
    deriving(Show, Eq, Num, Storable, DB.Binary)
newtype GLushort   = GLushort Word16
    deriving(Show, Eq, Num, Storable, DB.Binary)
newtype GLfixed    = GLfixed Word32
        deriving(Show, Eq, Num, Storable, DB.Binary)
newtype GLclampx    = GLclampx Word32
        deriving(Show, Eq, Num, Storable, DB.Binary)

data MatrixUniformType = MATRIX_UNIFORM_2X2
                       | MATRIX_UNIFORM_3X3
                       | MATRIX_UNIFORM_4X4
                       
type GLError = GLenum

type Id  = GLuint
type GLId = GLuint 

data ResourceMapper = ResourceMapper 
    {
        resource_mapper_map   :: GLboolean,
        resource_mapper_ids :: [Id]
    }

data ResourceId = NamedResource Id
                | GLResource GLId
                
data MemoryLocation = MemoryLocation 
    {
        memory_location_id     :: Id,
        memory_location_offset :: Int
    }
    
type Result a b = Either a b

data GCommand a b c = GCommand
    {
        cmd :: a,
        result :: Result b c
    }
    
--Loop
data LoopInput = LoopInput
    {
        loop_input_command :: Command
    }
data LoopOutput 
data LoopError
type Loop = GCommand LoopInput LoopOutput LoopError

--AddData
data AddDataInput = AddDataInput
    {
        add_data_id     :: Id,
        add_data_buffer :: BS.ByteString
    }   
data AddDataOutput
data AddDataError = AddDataNoRoom
type AddData = GCommand AddDataInput AddDataOutput AddDataError
--CopyData
data CopyDataInput = CopyDataInput
    {
        copy_data_id      :: Id,
        copy_data_buffer  :: BS.ByteString
    }
data CopyDataOutput
data CopyDataError  = CopyErrorNoRoom
                    | TooBig
type CopyData = GCommand CopyDataInput CopyDataOutput CopyDataError
--DeleteData
data DeleteDataInput = DeleteDataInput
    {
        delete_data_id :: Id
    }
data DeleteDataOutput
data DeleteDataError = DeleteIdNotFound
type DeleteData = GCommand DeleteDataInput DeleteDataOutput DeleteDataError
--UpdateData
data UpdateDataInput = UpdateDataInput 
    {
        update_data_id     :: Id,
        update_data_buffer :: BS.ByteString
    }
data UpdateDataOutput = UpdateDataOutput
data UpdateDataError  = UpdateIdNotFound
type UpdateData = GCommand UpdateDataInput UpdateDataOutput UpdateDataError
--Enable
data EnableInput = EnableInput
    {
        enable_state :: GLenum
    }
    
data EnableOutput
type EnableError = GLError
type Enable = GCommand EnableInput EnableOutput EnableError
--GenBuffersInput
data GenBuffersInput = GenBuffersInput
    {
        gen_buffers_input_count  :: GLint,
        gen_buffers_input_mapper :: ResourceMapper
    }
data GenBuffersOutput = GenBuffersOutput 
    {
        gen_buffers_output_buffers :: [GLuint]
    }
type GenBuffersError = GLError
type GenBuffers = GCommand GenBuffersInput GenBuffersOutput GenBuffersError 
--DeleteBuffers 
data DeleteBuffersInput = DeleteBuffersInput
    {
        delete_buffers_input_buffers :: [ResourceId]
    }
data DeleteBuffersOutput
type DeleteBuffersError = GLError
type DeleteBuffers = GCommand DeleteBuffersInput DeleteBuffersOutput DeleteBuffersError
--BindBuffer
data BindBufferInput = BindBufferInput
    {
        bind_buffer_input_buffer_target :: GLenum,
        bind_buffer_input_id            :: ResourceId
    }
data BindBufferOutput
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
data BufferDataOutput
type BufferDataError = GLError
type BufferData = GCommand BufferDataInput BufferDataOutput BufferDataError
--EnableVertexAttribArray
data EnableVertexAttribArrayInput = EnableVertexAttribArrayInput
    {
        enable_vertex_attrib_array_input_index :: GLuint
    }
data EnableVertexAttribArrayOutput
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
data VertexAttribPointerOutput
type VertexAttribPointerError = GLError
type VertexAttribPoint = GCommand VertexAttribPointerInput VertexAttribPointerOutput
    VertexAttribPointerError
--GenVertexArrayOES
data GenVertexArraysOESInput = GenVertexArraysOESInput 
    {
        gen_vertex_arrays_oes_input_count           :: GLuint,
        gen_vertex_arrays_oes_input_resource_mapper :: ResourceMapper
    }
data GenVertexArraysOESOutput = GenVertexArraysOESOutput 
    {
        gen_vertex_array_oes_output_buffers :: [GLuint]
    }
type GenVertexArraysOESError = GLError
--BindVertexArrayOES    
data BindVertexArrayOESInput = BindVertexArrayOESInput 
    {
        bind_vertex_array_oes_input :: ResourceId
    }
data BindVertexArrayOESOutput
type BindVertexArrayOESError = GLError
type BindVertexArray = GCommand BindVertexArrayOESInput BindVertexArrayOESOutput
    BindVertexArrayOESError
--CommandList   
data CommandListInput = CommandListInput
    {
        command_list_input_commands :: [Command]
    }
data CommandListOutput 
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
data ClearColorOutput
type ClearColorError = GLError
type ClearColor = GCommand ClearColorInput ClearColorOutput ClearColorError
--Clear
data ClearInput = ClearInput
    {
        clear_input_clear_flags :: GLint
    }
data ClearOutput
type ClearError = GLError
type Clear = GCommand ClearInput ClearOutput ClearError
--DrawArrays
data DrawArraysInput = DrawArraysInput
    {
        draw_arrays_input_id :: ResourceId
    }
data DrawArraysOutput 
type DrawArraysError = GLError
type DrawArrays = GCommand DrawArraysInput DrawArraysOutput DrawArraysError
--UseProgram
data UseProgramInput = UseProgramInput 
    {
        use_program_input_id :: ResourceId
    }
data UseProgramOutput
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
data UniformMatrixOutput
type UniformMatrixError = GLError
type UniformMatrix = GCommand UniformMatrixInput UniformMatrixOutput UniformMatrixError
--AttachShader
data AttachShaderInput = AttachShaderInput 
    {
        attach_shader_input_program_id :: ResourceId,
        attach_shader_input_shader_id :: ResourceId
    }
data AttachShaderOutput
data AttachShaderError = GLError
type AttachShader = GCommand AttachShaderInput AttachShaderOutput AttachShaderError
--BindAttribLocation 
data BindAttribLocationInput = BindAttribLocationInput 
    {
       bind_attrib_location_input_program_id :: ResourceId,
       bind_attrib_location_input_index      :: GLuint,
       bind_attrib_location_input_name       :: String
    }
data BindAttribLocationOutput
type BindAttribLocationError = GLError
type BindAttribLocation = GCommand BindAttribLocationInput BindAttribLocationOutput 
                                BindAttribLocationError
--CreateProgram
data CreateProgramInput = CreateProgramInput
    {
        create_program_input :: ResourceMapper
    }
data CreateProgramOutput
type CreateProgramError = GLError
type CreateProgram = GCommand CreateProgramInput CreateProgramOutput CreateProgramError
--CreateShader
data CreateShaderInput = CreateShaderInput
    {
        create_shader_input_mapper :: ResourceMapper,
        create_shader_input_type   :: GLenum
    }
data CreateShaderOutput = CreateShaderOutput
    {
        create_shader_output_id :: GLuint
    }
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
data ShaderSourceOutput
type ShaderSourceError = GLError
type ShaderSource = GCommand ShaderSourceInput ShaderSourceOutput ShaderSourceError
--CompileShader    
data CompileShaderInput = CompileShaderInput
    {
        compile_shader_id :: ResourceId
    }
data CompileShaderOutput
type CompileShaderError = GLError
type CompileShader = GCommand CompileShaderInput CompileShaderOutput CompileShaderError
--LinkProgram
data LinkProgramInput = LinkProgramInput
    {
        link_program_input :: ResourceId
    }
data LinkProgramOutput
type LinkProgramError = GLError
type LinkProgram = GCommand LinkProgramInput LinkProgramOutput LinkProgramError
--GetUniformLocation    
data GetUniformLocationInput = GetUniformLocationInput
    {
        get_uniform_location_program_id :: ResourceId,
        get_uniform_location_name       :: String
    }
data GetUniformLocationOutput = GetUniformLocationOutput
    {
        get_uniform_location_output_index :: GLuint
    }
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






















