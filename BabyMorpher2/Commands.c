//
//  Commands.c
//  BabyMorpher2
//
//  Created by hi5 networks on 1/30/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#include "Commands.h"
#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <assert.h>

const char* MatrixUniformTypeStrings[] = {
    "MATRIX_UNIFORM_2X2",
    "MATRIX_UNIFORM_3X3",
    "MATRIX_UNIFORM_4X4"
};

const char* CommandTypeStrings[] = 
{
    "ADD_DATA",
    "DELETE_DATA",
    "UPDATE_DATA",
    "ENABLE",
    "GENBUFFERS",
    "DELETEBUFFERS",
    "BINDBUFFER",
    "BUFFERDATA",
    "VERTEX_ATTRIB_POINTER",
    "GEN_VERTEX_ARRAYS_OES",
    "BIND_VERTEX_ARRAY_OES",
    "ENABLE_VERTEX_ATTRIB_ARRAY", 
    "COMMAND_LIST",
    "CLEAR_COLOR",
    "CLEAR",
    "DRAW_ARRAYS",
    "USE_PROGRAM",
    "UNIFORM_MATRIX",
    "ATTACH_SHADER",
    "BIND_ATTRIBUTE_LOCATION",
    "CREATE_PROGRAM",
    "CREATE_SHADER",
    "SHADER_SOURCE",
    "COMPILE_SHADER",
    "LINK_PROGRAM"
};

const char* ClearFlagStrings[] =  {
    "COLOR_BUFFER_BIT",
    "DEPTH_BUFFER_BIT"
};

const char* EnableEnumsStrings[] = { 
    "E_GL_BLEND",
    "E_GL_CULL_FACE",
    "E_GL_DEPTH_TEST",
    "E_GL_DITHER",
    "E_GL_POLYGON_OFFSET_FILL",
    "E_GL_SAMPLE_ALPHA_TO_COVERAGE",
    "E_GL_SAMPLE_COVERAGE",
    "E_GL_SCISSOR_TEST",
    "E_GL_STENCIL_TEST"
};

const char* DrawComponentStrings[] = {
    "TRIANGLES"
};

const char* BufferTargetStrings[] = { 
    "ARRAY_BUFFER",
    "ELEMENT_ARRAY_BUFFER"
};

const char* EResourceTypeStrings[] =  {
    "RESOURCE_NAME",
    "RESOURCE_ID"
};

const char* UsageStrings[] = {
    "STATIC_DRAW", 
    "STREAM_DRAW",
    "DYNAMIC_DRAW"
};

const char* GetBufferValueStrings[] = {
    "BUFFER_SIZE",
    "BUFFER_USAGE"
};

const char* VertexAttributeTypeStrings[] = {
    "VA_BYTE",
    "VA_UNSIGNED_BYTE",
    "VA_SHORT",
    "VA_UNSIGNED_SHORT",
    "VA_FIXED",
    "VA_FLOAT"
};

const char* VAP_ETypeStrings[] = {
    "VAP_BYTE", 
    "VAP_UNSIGNED_BYTE", 
    "VAP_SHORT", 
    "VAP_UNSIGNED_SHORT", 
    "VAP_INT", 
    "VAP_UNSIGNED_INT", 
    "VAP_FLOAT", 
    "VAP_DOUBLE"
};


EShaderType gl_enum_to_shader_type(GLenum type) {
    switch (type) {
        case GL_VERTEX_SHADER:
            return VERTEX_SHADER;

        case GL_FRAGMENT_SHADER:
            return FRAGMENT_SHADER;

        default:
            break;
    }
    
    return SHADER_TYPE_INVALID;
}



GLenum shader_type_to_gl_enum(EShaderType shader_type) {
    switch (shader_type) {
        case VERTEX_SHADER:
            return GL_VERTEX_SHADER;

        case FRAGMENT_SHADER:
            return GL_FRAGMENT_SHADER;
            
        default:
            break;
    }
    
    return GL_INVALID_ENUM;
}

ResourceId mk_resource_id_s(const char* name) {
    ResourceId id;
    id.type = RESOURCE_NAME;
    id.name = name;
    return id;
}

void mk_resource_mapper1(ResourceMapper* mapper, const char* name) {
    mapper->names[0] = name;
    mapper->map_resource = GL_TRUE;
    mapper->count = 1;
}

void mk_add_data(Command* cmd, const char* id, int count, char* buffer) {
    cmd->type                   = ADDDATA;
    cmd->add_data.cmd.buffer    = buffer;
    cmd->add_data.cmd.count     = count;
    cmd->add_data.cmd.id        = id;
}

void mk_delete_data(Command* cmd, const char* id) {
    cmd->type = DELETEDATA;
    cmd->delete_data.cmd.id = id;
}

void mk_update_data(Command* cmd, const char* id, int count, char* buffer) {
    cmd->update_data.cmd.id     = id;
    cmd->update_data.cmd.count  = count;
    cmd->update_data.cmd.buffer = buffer; 
}

void mk_enable(Command* cmd, EnableEnums type) {
    cmd->type         = ENABLE;
    cmd->enable.cmd.state = E_GL_DEPTH_TEST;
}

void mk_gen_buffers(Command* cmd, int count, const char** names) {
    cmd->type                     = GENBUFFERS;
    cmd->gen_buffers.cmd.count        = count;
    for (int i = 0; i < count; i++) {
        cmd->gen_buffers.cmd.mapper.names[i] = names[i];
    }
    cmd->gen_buffers.cmd.mapper.count = count;
    cmd->gen_buffers.cmd.mapper.map_resource = names == 0 ? GL_TRUE : GL_FALSE;
}

void mk_delete_buffers(Command* cmd, int count, ResourceId* resources) {

}

void mk_bind_buffer(Command* cmd, BufferTarget target, ResourceId id) {
    cmd->type                      = BINDBUFFER;
    cmd->bind_buffer.cmd.id            = id;
    cmd->bind_buffer.cmd.buffer_target = target;    
}

void mk_buffer_data(Command* cmd, BufferTarget buffer_target, GLuint size, MemoryLocation memory_location,
                    Usage usage) {
    cmd->type                        = BUFFERDATA;
    cmd->buffer_data.cmd.buffer_target   = buffer_target;
    cmd->buffer_data.cmd.size            = size;
    cmd->buffer_data.cmd.memory_location = memory_location;
    cmd->buffer_data.cmd.usage           = usage;
}

void mk_vertex_attrib_pointer(Command* cmd, GLuint index,
                              GLint size,
                              VAP_EType type,
                              GLboolean normalized,
                              GLsizei stride,
                              MemoryLocation memory_location) {
    cmd->type                                  = VERTEX_ATTRIB_POINTER;
    cmd->vertex_attrib_pointer.cmd.index           = index;
    cmd->vertex_attrib_pointer.cmd.size            = size;
    cmd->vertex_attrib_pointer.cmd.type            = type;
    cmd->vertex_attrib_pointer.cmd.normalized      = normalized;
    cmd->vertex_attrib_pointer.cmd.stride          = stride;
    cmd->vertex_attrib_pointer.cmd.memory_location = memory_location;
    
}

void mk_gen_vertex_arrays_oes(Command* cmd, GLuint count, ResourceMapper mapper) {
    cmd->type                         = GEN_VERTEX_ARRAYS_OES;
    cmd->gen_vertex_arrays_oes.cmd.count  = count;
    cmd->gen_vertex_arrays_oes.cmd.mapper = mapper;
}

void mk_bind_vertex_arrays_oes(Command* cmd, ResourceId id) {
    cmd->type                         = BIND_VERTEX_ARRAY_OES;
    cmd->bind_vertex_array_oes.cmd.id = id;
}

void mk_enable_vertex_attrib_array(Command* cmd, GLuint index) {
    cmd->type                                = ENABLE_VERTEX_ATTRIB_ARRAY;
    cmd->enable_vertex_attribute_array.cmd.index = index;    
}

void mk_clear_color(Command* cmd, float r, float g, float b, float a) {
    cmd->type = CLEAR_COLOR;
    cmd->clear_color.cmd.r = r;
    cmd->clear_color.cmd.g = g;
    cmd->clear_color.cmd.b = b;
    cmd->clear_color.cmd.a = a;
}

void mk_clear(Command* cmd, ClearFlag clear_flag) {
    cmd->type = CLEAR; 
    cmd->clear.cmd.clear_flags = clear_flag;
}

void mk_draw_arrays(Command* cmd, DrawComponent component_type, GLuint start, GLuint count) {
    cmd->type = DRAW_ARRAYS;  
    cmd->draw_arrays.cmd.component_type = component_type;
    cmd->draw_arrays.cmd.start          = start;
    cmd->draw_arrays.cmd.count          = count;
}

void mk_use_program(Command* cmd, ResourceId id) {
    cmd->type = USE_PROGRAM;   
    cmd->use_program.cmd.id   = id;
}

void mk_uniform_matrix(Command* cmd, MatrixUniformType uniform_type, GLuint uniform_index, int count, 
                       GLboolean transpose, MemoryLocation value) {
    cmd->type = UNIFORM_MATRIX;    
    cmd->uniform_matrix.cmd.uniform_type    = uniform_type;
    cmd->uniform_matrix.cmd.uniform_index   = uniform_index;
    cmd->uniform_matrix.cmd.count           = count;
    cmd->uniform_matrix.cmd.transpose       = transpose;
    cmd->uniform_matrix.cmd.memory_location = value;
}

void mk_command_list(Command* cmd, Command* commands, int count) {
    cmd->type                       = COMMAND_LIST;
    cmd->command_list.cmd.count     = count;
    cmd->command_list.cmd.commands  = commands;
}

void mk_attack_shader(Command* cmd, const char* program, const char* shader) {
    cmd->type = ATTACH_SHADER;
    
    ResourceId program_id = mk_resource_id_s(program);
    ResourceId shader_id = mk_resource_id_s(shader);
    
    cmd->attach_shader.cmd.program_id = program_id;
    cmd->attach_shader.cmd.shader_id = shader_id;
}

void mk_bind_attrib_location(Command* cmd, const char* program, GLuint index, const char* name) {
    cmd->type = BIND_ATTRIBUTE_LOCATION;
    
    ResourceId program_id = mk_resource_id_s(program);
    cmd->bind_attrib_location.cmd.program_id = program_id;
    cmd->bind_attrib_location.cmd.index = index;
    cmd->bind_attrib_location.cmd.name = name;
}

void mk_create_program(Command* cmd, const char* program) {
    cmd->type = CREATE_PROGRAM;
    
    ResourceMapper rm;
    mk_resource_mapper1(&rm, program);
    cmd->create_program.cmd.mapper = rm;
}

void mk_create_shader(Command* cmd, const char* shader, EShaderType shader_type) {
    cmd->type = CREATE_SHADER;
    
    ResourceMapper rm; 
    mk_resource_mapper1(&rm, shader);
    cmd->create_shader.cmd.type = shader_type;
    cmd->create_shader.cmd.mapper = rm;
}

void mk_shader_source(Command* cmd, const char* shader, GLint count, MemoryLocation source_location, GLint* length) {
    cmd->type = SHADER_SOURCE;
    
    ResourceId shader_id = mk_resource_id_s(shader);
    cmd->shader_source.cmd.id = shader_id;
    cmd->shader_source.cmd.count = count;
    cmd->shader_source.cmd.source_location = source_location;
    cmd->shader_source.cmd.length = length;
}

void mk_compile_shader(Command* cmd, const char* shader) {
    cmd->type = COMPILE_SHADER;
    
    ResourceId shader_id = mk_resource_id_s(shader);
    cmd->compile_shader.cmd.id = shader_id;
}

void mk_link_program(Command* cmd, const char* prog) {
    cmd->type = LINK_PROGRAM;
    ResourceId program_id = mk_resource_id_s(prog);
    cmd->link_program.cmd.id = program_id;
}

MemoryLocation mk_memory_location(const char* id, int offset) {
    MemoryLocation ml = {id, offset};
    return ml;
}

void show_command(char* buffer, int size, Command* command) {
    char sub_buffer[500];
    memset(sub_buffer, 0, 500);
    
    const char* type_string = command_type_string(command->type);
    
    switch (command->type) {
        case ADDDATA:
            show_add_data(sub_buffer, 500, &command->add_data);
            break;
        case DELETEDATA:
            show_delete_data(sub_buffer, 500, &command->delete_data);
            break;
            
        case UPDATEDATA:
            show_update_data(sub_buffer, 500, &command->update_data);
            break;    
            
        case ENABLE:
            show_enable(sub_buffer, 500, &command->enable);
            break;
            
        case GENBUFFERS:
            show_gen_buffers(sub_buffer, 500, &command->gen_buffers);
            break;
            
        case DELETEBUFFERS:
            show_delete_buffers(sub_buffer, 500, &command->delete_buffers);
            break;
            
        case BINDBUFFER:
            show_bind_buffers(sub_buffer, 500, &command->bind_buffer);
            break;
            
        case BUFFERDATA:
            show_buffer_data(sub_buffer, 500, &command->buffer_data);
            break;
            
        case VERTEX_ATTRIB_POINTER:
            show_vertex_attrib_pointer(sub_buffer, 500, &command->vertex_attrib_pointer);
            break;
            
        case GEN_VERTEX_ARRAYS_OES:
            show_gen_vertex_arrays_oes(sub_buffer, 500, &command->gen_vertex_arrays_oes);
            break;
            
        case BIND_VERTEX_ARRAY_OES:
            show_bind_vertex_arrays_oes(sub_buffer, 500, &command->bind_vertex_array_oes);
            break;
            
        case ENABLE_VERTEX_ATTRIB_ARRAY:
            show_enable_vertex_attrib_array(sub_buffer, 500, &command->enable_vertex_attribute_array);
            break;
            
        case COMMAND_LIST:            
            show_command_list(sub_buffer, 500, &command->command_list);
            break;
            
        case CLEAR_COLOR:
            show_clear_color(sub_buffer, 500, &command->clear_color);
            break;
            
        case CLEAR:
            show_clear(sub_buffer, 500, &command->clear);
            break;
            
        case DRAW_ARRAYS:
            show_draw_arrays(sub_buffer, 500, &command->draw_arrays);
            break;
            
        case USE_PROGRAM:
            show_use_program(sub_buffer, 500, &command->use_program);
            break;
            
        case UNIFORM_MATRIX:
            show_uniform_matrix(sub_buffer, 500, &command->uniform_matrix);
            break;
            
        case ATTACH_SHADER:
            show_attack_shader(sub_buffer, 500, &command->attach_shader);
            break;
            
        case BIND_ATTRIBUTE_LOCATION:
            show_bind_attrib_location(sub_buffer, 500, &command->bind_attrib_location);
            break;
            
        case CREATE_PROGRAM:
            show_create_program(sub_buffer, 500, &command->create_program);
            break;
            
        case CREATE_SHADER:
            show_create_shader(sub_buffer, 500, &command->create_shader);
            break;
            
        case SHADER_SOURCE:
            show_shader_source(sub_buffer, 500, &command->shader_source);
            break;
            
        case COMPILE_SHADER:
            show_compile_shader(sub_buffer, 500, &command->compile_shader);
            break;
            
        case LINK_PROGRAM:
            show_link_program(sub_buffer, 500, &command->link_program);
            break;
            
        default:
            assert(0);
            break;        

    }
    
    sprintf(buffer, "Command type = %s with value %s", type_string, sub_buffer);
}

void show_add_data(char* buffer, int size, AddData* add_data) {
    sprintf(buffer, "id %s, count %d, data %d", add_data->cmd.id, 
            add_data->cmd.count, (int)add_data->cmd.buffer);
}


void show_delete_data(char* buffer, int size, DeleteData* delete_data) {
    sprintf(buffer, "id %s", delete_data->cmd.id);    
}

void show_update_data(char* buffer, int size, UpdateData* x) {
    sprintf(buffer, "id %s", x->cmd.id);        
}

void show_enable(char* buffer, int size, Enable* x) {
    sprintf(buffer, "state %s", enable_type_string(x->cmd.state));    
}
 
void show_gen_buffers(char* buffer, int size, GenBuffers* x) {
    char sub_buffer[50];
    memset(sub_buffer, 0, 50);
    show_resource_mapper(sub_buffer, 50, x->cmd.mapper);
    
    sprintf(buffer, "count %d, resource id %s", x->cmd.count, sub_buffer);
}

void show_delete_buffers(char* buffer, int size, DeleteBuffers* x) {
    char sub_buffer[200];
    memset(sub_buffer, 0, 200);
    show_resource_id_array(sub_buffer, 200, x->cmd.buffers, x->cmd.count);
    
    sprintf(buffer, "count %d, ids %s", x->cmd.count, sub_buffer);
}

void show_bind_buffers(char* buffer, int size, BindBuffer* x) {
    char sub_buffer[50];
    memset(sub_buffer, 0, 50);
    show_resource_id(sub_buffer, 50, x->cmd.id);
    
    sprintf(buffer, "target %s, id %s", buffer_target_string(x->cmd.buffer_target), 
            sub_buffer);
}

void show_buffer_data(char* buffer, int size, BufferData* x) {
    char sub_buffer[50];
    memset(sub_buffer, 0, 50);    
    show_memory_location(sub_buffer, x->cmd.memory_location);
    
    sprintf(buffer, "buffer target %s, size %d, memory location %s, usage %s", 
            buffer_target_string(x->cmd.buffer_target),
            x->cmd.size, sub_buffer, usage_string(x->cmd.usage));
}

void show_vertex_attrib_pointer(char* buffer, int size, VertexAttribPointer* x) {
    char sub_buffer[50];
    memset(sub_buffer, 0, 50);
    show_memory_location(buffer, x->cmd.memory_location);
    
    sprintf(buffer, "index %d, size %d, type %s, normalized %d, stride %d, memory_location %s",
            x->cmd.index, x->cmd.size, vap_string(x->cmd.type), x->cmd.normalized, 
            x->cmd.stride, sub_buffer);
}

void show_gen_vertex_arrays_oes(char* buffer, int size, GenVertexArraysOES* x) {
    char sub_buffer[200];
    memset(sub_buffer, 0, 200);
    show_resource_mapper(sub_buffer, 200, x->cmd.mapper);
    
    sprintf(buffer, "count %d, mapper %s", x->cmd.count, sub_buffer);
}
 
void show_bind_vertex_arrays_oes(char* buffer, int size, BindVertexArrayOES* x) {
    char sub_buffer[50];
    memset(sub_buffer, 0, 50);
    show_resource_id(sub_buffer, 50, x->cmd.id);
    
    sprintf(buffer, "id %s", sub_buffer);
}

void show_enable_vertex_attrib_array(char* buffer, int size, EnableVertexAttribArray* x) {
    sprintf(buffer, "index %d", x->cmd.index);
}

void show_command_list(char* buffer, int size, CommandList* x) {
    sprintf(buffer, "count %d", x->cmd.count);
}

void show_resource_id(char* buffer, int size, ResourceId resource_id) {
    if (resource_id.type == RESOURCE_ID) {
        sprintf(buffer, "id %d", resource_id.id);
    } else {
        sprintf(buffer, "name %s", resource_id.name);
    }
}

void show_resource_mapper(char* buffer, int size, ResourceMapper resource_mapper) {
    char sub_buffer[200];
    memset(sub_buffer, 0, 200);
    
    for (int i = 0; i < resource_mapper.count; i++) {
        strcat(sub_buffer, resource_mapper.names[i]);
    }
    
    sprintf(buffer, "count %d, names %s", resource_mapper.count, sub_buffer);
}

void show_resource_id_array(char* buffer, int size, ResourceId* resource_ids, int count) {
    memset(buffer, 0, size);
    for(int i = 0; i < count; i++) {
        char sub_buffer[20];
        show_resource_id(sub_buffer, 20, resource_ids[i]);
        if(i != 0) {
           strcat(buffer, ", ");
        }
        strcat(buffer, sub_buffer);
    }
}

void show_memory_location(char* buffer, MemoryLocation memory_location) {
    sprintf(buffer, "id %s, offset %d", memory_location.id, memory_location.offset);
}

const char* command_type_string(ECommandType type) {
    return CommandTypeStrings[type];
}

const char* enable_type_string(EnableEnums type) {
    return EnableEnumsStrings[type];
}

const char* buffer_target_string(BufferTarget type) {
    return BufferTargetStrings[type];
}

const char* usage_string(Usage type) {
    return UsageStrings[type];
}

const char* vap_string(VAP_EType type) {
    return VAP_ETypeStrings[type];
}

const char* clear_flag_string(ClearFlag flag) {
    return ClearFlagStrings[flag];
}

const char* component_type_string(DrawComponent component_type) {
    return DrawComponentStrings[component_type];
}

const char* uniform_matrix_string(MatrixUniformType matrix_type) {
    return MatrixUniformTypeStrings[matrix_type];
}

void show_clear_color(char* buffer, int size, ClearColor* x) {
    sprintf(buffer, "r %f, g %f, b %f, a %f", x->cmd.r, x->cmd.g, 
            x->cmd.b, x->cmd.a);
}

void show_clear(char* buffer, int size, Clear* x) {
    int flags = x->cmd.clear_flags;
    
    for (int i = 0; i < CLEAR_FLAG_MAX; i++) {
        if (!flags) {
            break;
        }
        
        const char* name = show_clear_flag(0x1 << i, flags & 0x1);
        if (i != 0) {
            strcat(buffer, " | ");
        }
        
        strcat(buffer, name);
        
        flags >>= 1;        
    }
}

const char* show_clear_flag(int index, GLboolean value) {
    switch (index) {
        case COLOR_BUFFER_BIT:
            return "COLOR_BUFFER_BIT";
            
        case DEPTH_BUFFER_BIT:
            return "DEPTH_BUFFER_BIT";
            
        default:
            break;
    }
    
    return "Invalid clear flag!";
}

void show_draw_arrays(char* buffer, int size, DrawArrays* x) {
    sprintf(buffer, "component_type %s, start %d, count %d", 
            component_type_string(x->cmd.component_type), x->cmd.start, x->cmd.count);
}

void show_use_program(char* buffer, int size, UseProgram* x) {
    show_resource_id(buffer, size, x->cmd.id);
}

void show_uniform_matrix(char* buffer, int size, UniformMatrix* x) {
    char sub_buffer[50];
    memset(sub_buffer, 0, 50);
    show_memory_location(sub_buffer, x->cmd.memory_location);
    
    sprintf(buffer, "uniform_type %s, uniform_index %d, count %d, transpose %d, memory_location %s", 
            uniform_matrix_string(x->cmd.uniform_type), 
            x->cmd.uniform_index,
            x->cmd.count,
            x->cmd.transpose,
            sub_buffer);
}

void show_attack_shader(char* buffer, int size, AttachShader* x) {
    char program_buffer[50]; 
    memset(program_buffer, 0, 50);
    show_resource_id(program_buffer, 50, x->cmd.program_id);
    
    char shader_buffer[50];
    memset(shader_buffer, 0, 50);
    show_resource_id(shader_buffer, 50, x->cmd.shader_id);
    
    sprintf(buffer, "program %s, shader %s", program_buffer, shader_buffer);
}

void show_bind_attrib_location(char* buffer, int size, BindAttribLocation* x) {
    char program_buffer[50];
    memset(program_buffer, 0, 50);
    show_resource_id(program_buffer, 50, x->cmd.program_id);
    
    sprintf(buffer, "program_id %s, index %d, name %s", program_buffer, x->cmd.index, x->cmd.name);
}

void show_create_program(char* buffer, int size, CreateProgram* x) {
    char sub_buffer[50];
    memset(sub_buffer, 0, 50);
    show_resource_mapper(sub_buffer, size, x->cmd.mapper);
    
    sprintf(buffer, "mapper %s", sub_buffer);
    
}

void show_create_shader(char* buffer, int size, CreateShader* x) {
    char sub_buffer[50];
    memset(sub_buffer, 0, 50);
    show_resource_mapper(sub_buffer, size, x->cmd.mapper);
    
    sprintf(buffer, "mapper %s", sub_buffer);
}

void show_shader_source(char* buffer, int size, ShaderSource* x) {
    char sub_buffer[50];
    memset(sub_buffer, 0, 50);
    show_resource_id(sub_buffer, 50, x->cmd.id);
    
    char memory_buffer[50];
    memset(memory_buffer, 0, 50);
    show_memory_location(memory_buffer, x->cmd.source_location);
    
    char lengths_buffer[50];
    memset(lengths_buffer, 0, 50);
    
    if(x->cmd.length != NULL) {    
        for(int i = 0; i < x->cmd.count; i++) {
            char int_buff[10];
            memset(int_buff, 0, 10);
            sprintf(int_buff, "%d", x->cmd.length[i]);
            strcat(lengths_buffer, int_buff);
        }
    }
        
    sprintf(buffer, "id %s, count %d, memory_location %s, length %s", sub_buffer, x->cmd.count, memory_buffer,
            lengths_buffer);
}

void show_compile_shader(char* buffer, int size, CompileShader* x) {
    char sub_buffer[50];
    memset(sub_buffer, 0, 50);
    show_resource_id(sub_buffer, 50, x->cmd.id);
    
    sprintf(buffer, "id %s", sub_buffer);
}

void show_link_program(char* buffer, int size, LinkProgram* x) {
    char sub_buffer[50];
    memset(sub_buffer, 0, 50);
    show_resource_id(sub_buffer, 50, x->cmd.id);
    
    sprintf(buffer, "id %s", sub_buffer);
}












