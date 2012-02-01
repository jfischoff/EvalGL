//
//  OpenGLCommands.h
//  BabyMorpher2
//
//  Created by hi5 networks on 1/28/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#ifndef BabyMorpher2_OpenGLCommands_h
#define BabyMorpher2_OpenGLCommands_h

#import <OpenGLES/ES2/gl.h>

//todo
//get rid of the different types
//make functions for creating the types

#define MAX_BUFFER_COUNT 10
#define MAX_RESOURCE_COUNT 10

typedef enum {
    MATRIX_UNIFORM_2X2,
    MATRIX_UNIFORM_3X3,
    MATRIX_UNIFORM_4X4
} MatrixUniformType;

typedef enum {
    COLOR_BUFFER_BIT = (0x1 << 0),
    DEPTH_BUFFER_BIT = (0x1 << 1),
    CLEAR_FLAG_MAX
} ClearFlag;

typedef enum { 
    E_GL_BLEND,
    E_GL_CULL_FACE,
    E_GL_DEPTH_TEST,
    E_GL_DITHER,
    E_GL_POLYGON_OFFSET_FILL,
    E_GL_SAMPLE_ALPHA_TO_COVERAGE,
    E_GL_SAMPLE_COVERAGE,
    E_GL_SCISSOR_TEST,
    E_GL_STENCIL_TEST
} EnableEnums;

typedef enum {
    TRIANGLES
} DrawComponent;


typedef enum { 
    ARRAY_BUFFER,
    ELEMENT_ARRAY_BUFFER
} BufferTarget;

typedef struct ResourceMapper_t {
    GLboolean map_resource;
    const char* names[MAX_RESOURCE_COUNT];   
    int count;
} ResourceMapper;

typedef enum EResourceType_t {
    RESOURCE_NAME,
    RESOURCE_ID
} EResourceType;

typedef struct ResourceId_t {    
    EResourceType type;
    union {
        const char* name;
        GLuint id;
    };
    
} ResourceId;

typedef struct MemoryLocation_t {
    const char* id;
    int offset;
} MemoryLocation;

typedef enum {
    STATIC_DRAW, 
    STREAM_DRAW,
    DYNAMIC_DRAW
} Usage;

typedef enum {
    BUFFER_SIZE,
    BUFFER_USAGE   
} GetBufferValue;

typedef enum {
    VA_BYTE,
    VA_UNSIGNED_BYTE,
    VA_SHORT,
    VA_UNSIGNED_SHORT,
    VA_FIXED,
    VA_FLOAT
} VertexAttributeType;

typedef enum {
    VAP_BYTE, 
    VAP_UNSIGNED_BYTE, 
    VAP_SHORT, 
    VAP_UNSIGNED_SHORT, 
    VAP_INT, 
    VAP_UNSIGNED_INT, 
    VAP_FLOAT
} VAP_EType;

typedef enum {
    VERTEX_SHADER,
    FRAGMENT_SHADER,
    SHADER_TYPE_MAX,
    SHADER_TYPE_INVALID
} EShaderType;

typedef enum {
    RESULT_SUCCESS,
    RESULT_ERROR
} ECommandResult;

typedef struct GenVertexArraysOES {
    struct {
        GLuint count;
        ResourceMapper mapper;        
    } cmd;
    
    struct {
        ECommandResult type;
        struct {
            GLuint buffers[MAX_BUFFER_COUNT];
            int count;  
        } right;
        struct {
            GLenum error;
        } left;
    } result;

} GenVertexArraysOES;

typedef enum {
    VAPE_INDEX_TO_BIG,
    VAPE_SIZE_IS_NOT_BETWEEN_1_AND_4_INCLUSIVE,
    VAPE_INVALID_TYPE,
    VAPE_STRIDE_IS_NEGATIVE,
    VAPE_GL_ERROR, //something else
    VAPE_MAX,
    VAPE_INVALID
} EVertexAttribPointerError;

typedef struct VertexAttribPointer_t {
    struct {
        GLuint  	index;
        GLint  	    size;
        VAP_EType       type;
        GLboolean  	normalized;
        GLsizei  	stride;
        MemoryLocation memory_location;
    } cmd;
    
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error; 
        } left;
    } result;
} VertexAttribPointer;

typedef enum {
    ADD_DATA_ERROR_NO_ROOM,
    ADD_DATA_ERROR_MAX,
    ADD_DATA_INVALID
} EAddDataError;

typedef struct AddData_t {
    struct {
        const char* id;
        int count;
        char* buffer;
    } cmd;
    
    struct {
        ECommandResult type;
        struct {
            
        } right;
        struct {
            EAddDataError error;
        } left;
    } result;
} AddData;

typedef enum {
    DELETE_DATA_ERROR_ID_NOT_FOUND
} EDeleteDataError;

typedef struct DeleteData_t {
    struct {
        const char* id;
    } cmd;
    
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            EDeleteDataError error_type;
        } left;
    } result;
} DeleteData;

typedef enum {
    UPDATE_DATA_ERROR_ID_NOT_FOUND
} EUpdateDataError;

typedef struct UpdateData_t {
    struct {
        const char* id;
        int count;
        char* buffer;
    } cmd;
    
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            EUpdateDataError error_type;
        } left;
    } result;
} UpdateData;


typedef struct Enable_t {
    struct {
        EnableEnums state;        
    } cmd;
    
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} Enable;

typedef struct GenBuffers_t {
    struct {
        int count;
        ResourceMapper mapper;        
    } cmd;
    
    struct {
        ECommandResult type;
        struct {
            GLuint buffers[MAX_BUFFER_COUNT];
            int count; 
        } right;
        struct {
            GLenum error;
        } left;
    } result;
} GenBuffers;

typedef struct DeleteBuffers_t {
    struct {
        ResourceId* buffers;
        int count;        
    } cmd;
    
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} DeleteBuffers;

typedef struct BindBuffer_t {
    struct {
        BufferTarget buffer_target;
        ResourceId id;        
    } cmd;
    
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} BindBuffer;

typedef struct BufferData_t {
    struct {
        BufferTarget buffer_target;
        GLuint size;
        MemoryLocation memory_location;
        Usage usage;        
    } cmd;
    
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} BufferData;

typedef struct EnableVertexAttribArray_t {
    struct {
        GLuint index;        
    } cmd;

    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} EnableVertexAttribArray;

typedef struct BindVertexArrayOES_t {
    struct {
        ResourceId id;        
    } cmd;

    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} BindVertexArrayOES;

typedef struct VertexAttrib_t {
    struct {
        int size;
        GLboolean is_float;
        GLboolean is_vector;
        GLuint index;        
    } cmd;
    
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} VertexAttrib;

typedef struct Command_t* Command_p;

typedef struct CommandList_t {
    struct {
        Command_p commands;
        int count;
    } cmd;
    
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} CommandList;

typedef struct ClearColor_t {
    struct {
        float r;
        float g;
        float b;
        float a;
    } cmd;
    
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} ClearColor;

typedef struct Clear_t {
    struct {
        GLint clear_flags;
    } cmd;
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;   
    } result;
} Clear;

typedef enum EDrawType_t {
    DRAWTYPE_TRIANGLES
} EDrawType;

typedef struct DrawArrays_t {
    struct {
        DrawComponent component_type;
        GLuint start;
        GLuint count;
    } cmd;
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} DrawArrays;

typedef struct Matrix2x2_t {
    
} Matrix2x2;

typedef struct {
    MatrixUniformType type;
    
    union {
        Matrix2x2 matrix_2x2;
    };
} MatrixUniformValue;

typedef struct UseProgram_t {
    struct {
        ResourceId id;
    } cmd;
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} UseProgram;

typedef struct UniformMatrix_t {
    struct {
        MatrixUniformType uniform_type;
        GLuint uniform_index; 
        int count; 
        GLboolean transpose;
        MemoryLocation memory_location;
    } cmd;
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} UniformMatrix;

typedef struct AttachShader_t {
    struct {
        ResourceId program_id;
        ResourceId shader_id;
    } cmd;
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} AttachShader;

typedef struct BindAttribLocation_t {
    struct {
        ResourceId program_id;
        GLuint index;
        const char* name;
    } cmd;
    struct {
        ECommandResult type;
        struct {} right;
        struct {
            GLenum error;
        } left;
    } result;
} BindAttribLocation;

typedef struct CreateProgram_t {
    struct {
        ResourceMapper mapper;
    } cmd;
    struct {
        ECommandResult type;
        struct {
            GLuint id;
        } right;
        struct {
            GLenum error;
        } left;
    } result;
} CreateProgram;

typedef struct CreateShader_t {
    struct {
        ResourceMapper mapper;
        EShaderType type;
    } cmd;
    struct {
        ECommandResult type;
        struct {
            GLuint id;
        } right;
        struct {
            GLenum error;
        } left;
    } result;
} CreateShader;

typedef struct ShaderSource_t {
    struct {
        ResourceId id;
        GLuint count;
        MemoryLocation source_location;
        GLint* length;
    } cmd;
    struct {
        ECommandResult type;
        struct {
        } right;
        struct {
            GLenum error;
        } left;
    } result;
} ShaderSource;

typedef struct CompileShader_t {
    struct {
        ResourceId id;
    } cmd;
    struct {
        ECommandResult type;
        struct {
        } right;
        struct {
            GLenum error;
        } left;
    } result;
} CompileShader;

typedef struct LinkProgram_t {
    struct {
        ResourceId id;    
    } cmd;
    struct {
        ECommandResult type;
        struct {
        } right;
        struct {
            GLenum error;
        } left;
    } result;
} LinkProgram;

typedef struct GetUniformLocation_t {
    struct {
        ResourceId program_id;
        const char* name;
    } cmd;
    struct {
        ECommandResult type;
        struct {
            GLuint index;
        } right;
        struct {
            GLenum error;
        } left;
    } result;
} GetUniformLocation;

typedef enum {
    ADDDATA,
    DELETEDATA,
    UPDATEDATA,
    ENABLE,
    GENBUFFERS,
    DELETEBUFFERS,
    BINDBUFFER,
    BUFFERDATA,
    VERTEX_ATTRIB_POINTER,
    GEN_VERTEX_ARRAYS_OES,
    BIND_VERTEX_ARRAY_OES,
    ENABLE_VERTEX_ATTRIB_ARRAY, 
    COMMAND_LIST,
    CLEAR_COLOR,
    CLEAR,
    DRAW_ARRAYS,
    USE_PROGRAM,
    UNIFORM_MATRIX,
    ATTACH_SHADER,
    BIND_ATTRIBUTE_LOCATION,
    CREATE_PROGRAM,
    CREATE_SHADER,
    SHADER_SOURCE,
    COMPILE_SHADER,
    LINK_PROGRAM,
    GET_UNIFORM_LOCATION
    
} ECommandType;

typedef struct Command_t {    
    ECommandType type;
    
    union {
        AddData    add_data;
        DeleteData delete_data;
        UpdateData update_data;
        Enable enable;
        GenBuffers gen_buffers;
        DeleteBuffers delete_buffers;
        BindBuffer bind_buffer;
        BufferData buffer_data;
        VertexAttribPointer vertex_attrib_pointer;
        GenVertexArraysOES gen_vertex_arrays_oes;
        BindVertexArrayOES bind_vertex_array_oes;
        EnableVertexAttribArray enable_vertex_attribute_array;
        CommandList command_list;
        ClearColor clear_color;
        Clear clear;
        DrawArrays draw_arrays;
        UseProgram use_program;
        UniformMatrix uniform_matrix;
        AttachShader attach_shader;
        BindAttribLocation bind_attrib_location;
        CreateProgram create_program;
        CreateShader create_shader;
        ShaderSource shader_source;
        CompileShader compile_shader;
        LinkProgram link_program;
        GetUniformLocation get_uniform_location;
    };
} Command;

ResourceId mk_resource_id_s(const char* name);
void mk_resource_mapper1(ResourceMapper* mapper, const char* name);

void mk_add_data(Command* cmd, const char*  id, int count, char* buffer);
void mk_delete_data(Command* cmd, const char*  id);
void mk_update_data(Command* cmd, const char*  id, int count, char* buffer);
void mk_enable(Command* cmd, EnableEnums type);
void mk_gen_buffers(Command* cmd, int count, const char** name);
void mk_delete_buffers(Command* cmd, int count, ResourceId* resources);
void mk_bind_buffer(Command* cmd, BufferTarget target, ResourceId id);
void mk_buffer_data(Command* cmd, BufferTarget buffer_target, GLuint size, MemoryLocation memory_location,
                    Usage usage);
void mk_vertex_attrib_pointer(Command* cmd, GLuint index,
                              GLint size,
                              VAP_EType type,
                              GLboolean normalized,
                              GLsizei stride,
                              MemoryLocation memory_location);
void mk_gen_vertex_arrays_oes(Command* cmd, GLuint count, ResourceMapper mapper);
void mk_bind_vertex_arrays_oes(Command* cmd, ResourceId id);
void mk_enable_vertex_attrib_array(Command* cmd, GLuint index);
void mk_clear_color(Command* cmd, float r, float g, float b, float a);
void mk_clear(Command* cmd, ClearFlag clear_flag);
void mk_draw_arrays(Command* cmd, DrawComponent component_type, GLuint start, GLuint count);
void mk_use_program(Command* cmd, ResourceId id);
void mk_uniform_matrix(Command* cmd, MatrixUniformType uniform_type, GLuint uniform_index, int count, 
                       GLboolean transpose, MemoryLocation value);
void mk_command_list(Command* cmd, Command* commands, int count);
void mk_attack_shader(Command* cmd, const char* program, const char* shader);
void mk_bind_attrib_location(Command* cmd, const char* program, GLuint index, const char* name); 
void mk_create_program(Command* cmd, const char* program);
void mk_create_shader(Command* cmd, const char* shader, EShaderType shader_type);
void mk_shader_source(Command* cmd, const char* shader, GLint count, MemoryLocation source_location, GLint* length);
void mk_compile_shader(Command* cmd, const char* shader);
void mk_link_program(Command* cmd, const char* prog);
void mk_get_uniform_location(Command* cmd, const char* prog, const char* name);
MemoryLocation mk_memory_location(const char* id, int offset);

//enum conversion
EShaderType gl_enum_to_shader_type(GLenum type);
GLenum shader_type_to_gl_enum(EShaderType shader_type);

//To Strings
const char* command_type_string(ECommandType type);
const char* enable_type_string(EnableEnums type);
const char* buffer_target_string(BufferTarget type);
const char* usage_string(Usage type);
const char* vap_string(VAP_EType type);
const char* clear_flag_string(ClearFlag flag);
const char* component_type_string(DrawComponent component_type);
const char* uniform_matrix_string(MatrixUniformType matrix_type);
const char* result_string(ECommandResult result);

//SHOW
void show_resource_id(char* buffer, int size, ResourceId resource_id);
void show_resource_mapper(char* buffer, int size, ResourceMapper resource_mapper);
void show_resource_id_array(char* buffer, int size, ResourceId* resource_ids, int count);
void show_memory_location(char* buffer, MemoryLocation memory_location);

void show_command(GLboolean input, GLboolean output, char* buffer, int size, Command* command);
void show_add_data(GLboolean input, GLboolean output, char* buffer, int size, AddData* add_data);
void show_delete_data(GLboolean input, GLboolean output, char* buffer, int size, DeleteData* delete_data);
void show_update_data(GLboolean input, GLboolean output, char* buffer, int size, UpdateData* update_data);
void show_enable(GLboolean input, GLboolean output, char* buffer, int size, Enable* enable);
void show_gen_buffers(GLboolean input, GLboolean output, char* buffer, int size, GenBuffers* gen_buffers);
void show_delete_buffers(GLboolean input, GLboolean output, char* buffer, int size, DeleteBuffers* delete_buffers);
void show_bind_buffers(GLboolean input, GLboolean output, char* buffer, int size, BindBuffer* bind_buffers);
void show_buffer_data(GLboolean input, GLboolean output, char* buffer, int size, BufferData* buffer_data);
void show_vertex_attrib_pointer(GLboolean input, GLboolean output, char* buffer, int size, VertexAttribPointer* vertex_attrib_pointer);
void show_gen_vertex_arrays_oes(GLboolean input, GLboolean output, char* buffer, int size, GenVertexArraysOES* gen_vertex_arrays_oes);
void show_bind_vertex_arrays_oes(GLboolean input, GLboolean output, char* buffer, int size, BindVertexArrayOES* bind_vertex_arrays_oes);
void show_enable_vertex_attrib_array(GLboolean input, GLboolean output, 
                                     char* buffer, int size, EnableVertexAttribArray* enable_vertex_attrib_array);
void show_command_list(GLboolean input, GLboolean output, char* buffer, int size, CommandList* command_list);
void show_clear_color(GLboolean input, GLboolean output, char* buffer, int size, ClearColor* x);
void show_clear(GLboolean input, GLboolean output, char* buffer, int size, Clear* x);
const char*  show_clear_flag(int index, GLboolean value);
void show_draw_arrays(GLboolean input, GLboolean output, char* buffer, int size, DrawArrays* x);
void show_use_program(GLboolean input, GLboolean output, char* buffer, int size, UseProgram* x);
void show_uniform_matrix(GLboolean input, GLboolean output, char* buffer, int size, UniformMatrix* x);
void show_attack_shader(GLboolean input, GLboolean output, char* buffer, int size, AttachShader* x);
void show_bind_attrib_location(GLboolean input, GLboolean output, char* buffer, int size, BindAttribLocation* x); 
void show_create_program(GLboolean input, GLboolean output, char* buffer, int size, CreateProgram* x);
void show_create_shader(GLboolean input, GLboolean output, char* buffer, int size, CreateShader* x);
void show_shader_source(GLboolean input, GLboolean output, char* buffer, int size, ShaderSource* x);
void show_compile_shader(GLboolean input, GLboolean output, char* buffer, int size, CompileShader* x);
void show_link_program(GLboolean input, GLboolean output, char* buffer, int size, LinkProgram* x);
void show_get_uniform_location(GLboolean input, GLboolean output, char* buffer, int size, GetUniformLocation* x);


















 
#endif
