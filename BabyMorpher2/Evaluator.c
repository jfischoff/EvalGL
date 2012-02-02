//
//  File.c
//  BabyMorpher2
//
//  Created by hi5 networks on 1/30/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#include "Evaluator.h"
#include <OpenGLES/ES2/gl.h>
#include <OpenGLES/ES2/glext.h>
#include <stdlib.h>
#include <stdio.h>
#include <memory.h>
#include <assert.h>

Environment* alloc_environment(int resource_capacity, int resource_mapping_count) {
    Environment* env = malloc(sizeof(Environment));
    
    env->resources = malloc(sizeof(Resource) * resource_capacity);
    env->resource_capacity = resource_capacity;
    env->resource_count = 0;
    
    env->resource_mappings = malloc(sizeof(ResourceMapping) * resource_capacity);
    env->resource_mapping_capacity = resource_capacity;
    env->resource_mapping_count = 0;    
    
    return env;
}

void free_environment(Environment* env) {
    free(env->resources);
    free(env->resource_mappings);
    free(env);
}

void evaluate(Environment* environment, Command* command) {
    log_input(environment, command);
    switch (command->type) {
        case ADDDATA:
            add_resource(environment, mk_resource(command->add_data.cmd.id, 
                                                  command->add_data.cmd.buffer,
                                                  command->add_data.cmd.count));
            break;
            
        case DELETEDATA:
            delete_resource(environment, command->delete_data.cmd.id);
            break;
            
        case UPDATEDATA:
            update_resource(environment, command->update_data.cmd.id, 
                            command->update_data.cmd.buffer, 
                            command->update_data.cmd.count);
            break;    
            
        case ENABLE:
            glEnable(enable_enum_to_gl_enum(command->enable.cmd.state));
            
            command->enable.result.left.error = glGetError();
            command->enable.result.type = command->enable.result.left.error == GL_NO_ERROR ? 
                                                RESULT_SUCCESS : RESULT_ERROR;
 
            break;
            
        case GENBUFFERS:
            glGenBuffers(command->gen_buffers.cmd.count, command->gen_buffers.result.right.buffers);
            
            map_result(environment, command->gen_buffers.result.right.buffers, command->gen_buffers.cmd.count, 
                       command->gen_buffers.cmd.mapper);
            
            command->gen_buffers.result.left.error = glGetError();
            command->gen_buffers.result.type = command->gen_buffers.result.left.error == GL_NO_ERROR ? 
                                RESULT_SUCCESS : RESULT_ERROR;
            
            
            break;
            
        case DELETEBUFFERS:
            assert(0);
            break;
            
        case BINDBUFFER:
        {
            GLuint id = from_resource_id(environment, command->bind_buffer.cmd.id);
            GLenum target = buffer_target_gl_enum(command->bind_buffer.cmd.buffer_target); 
            glBindBuffer(target, id);
            
            command->bind_buffer.result.left.error = glGetError();
            command->bind_buffer.result.type = command->bind_buffer.result.left.error == GL_NO_ERROR ? 
                                                    RESULT_SUCCESS : RESULT_ERROR;
        }
            
            break;
            
        case BUFFERDATA:
        {
            char* data = from_memory_location(environment, command->buffer_data.cmd.memory_location);
            GLenum target = buffer_target_gl_enum(command->buffer_data.cmd.buffer_target);
            GLenum usage  = usage_to_gl_enum(command->buffer_data.cmd.usage);
            glBufferData(target, 
                         command->buffer_data.cmd.size, data, 
                         usage);
            
            command->buffer_data.result.left.error = glGetError();
            command->buffer_data.result.type = command->buffer_data.result.left.error == GL_NO_ERROR ? 
                                                    RESULT_SUCCESS : RESULT_ERROR;

        }   
            break;
            
        case VERTEX_ATTRIB_POINTER:
            glVertexAttribPointer(command->vertex_attrib_pointer.cmd.index, 
                                  command->vertex_attrib_pointer.cmd.size, 
                                  vap_to_gl_enum(command->vertex_attrib_pointer.cmd.type), 
                                  command->vertex_attrib_pointer.cmd.normalized,
                                  command->vertex_attrib_pointer.cmd.stride, 
                                  (void*)command->vertex_attrib_pointer.cmd.offset);

            command->vertex_attrib_pointer.result.left.error = glGetError();
            command->vertex_attrib_pointer.result.type = command->vertex_attrib_pointer.result.left.error == GL_NO_ERROR ? 
                                                                RESULT_SUCCESS : RESULT_ERROR;
            
            
            break;
            
        case GEN_VERTEX_ARRAYS_OES:
            glGenVertexArraysOES(command->gen_vertex_arrays_oes.cmd.count, 
                                 command->gen_vertex_arrays_oes.result.right.buffers);
            
            map_result(environment, command->gen_vertex_arrays_oes.result.right.buffers, 
                       command->gen_vertex_arrays_oes.cmd.count, 
                       command->gen_vertex_arrays_oes.cmd.mapper);
            
            command->gen_vertex_arrays_oes.result.left.error = glGetError();
            command->gen_vertex_arrays_oes.result.type = command->gen_vertex_arrays_oes.result.left.error == GL_NO_ERROR ? 
                                                            RESULT_SUCCESS : RESULT_ERROR;
            
            break;
            
        case BIND_VERTEX_ARRAY_OES:
        {
            GLuint resource_id = from_resource_id(environment, command->bind_vertex_array_oes.cmd.id);
            glBindVertexArrayOES(resource_id);
            
            command->bind_vertex_array_oes.result.left.error = glGetError();
            command->bind_vertex_array_oes.result.type = command->bind_vertex_array_oes.result.left.error == GL_NO_ERROR ? 
                                                                RESULT_SUCCESS : RESULT_ERROR;
        }
            break;
            
        case ENABLE_VERTEX_ATTRIB_ARRAY:
            glEnableVertexAttribArray(command->enable_vertex_attribute_array.cmd.index);
            
            command->enable_vertex_attribute_array.result.left.error = glGetError();
            command->enable_vertex_attribute_array.result.type = command->enable_vertex_attribute_array.result.left.error == GL_NO_ERROR ? 
                                                                RESULT_SUCCESS : RESULT_ERROR;
            
            break;
            
        case COMMAND_LIST:
            for (int i = 0; i < command->command_list.cmd.count; i++) {
                evaluate(environment, &command->command_list.cmd.commands[i]);
            }
            
            break;
            
        case CLEAR_COLOR:
            glClearColor(command->clear_color.cmd.r, 
                         command->clear_color.cmd.g, 
                         command->clear_color.cmd.b, 
                         command->clear_color.cmd.a);
            
            command->clear_color.result.left.error = glGetError();
            command->clear_color.result.type = command->clear_color.result.left.error == GL_NO_ERROR ? 
                                                                    RESULT_SUCCESS : RESULT_ERROR;
            
            break;
            
        case CLEAR:
            glClear(clear_flag_to_gl_enum(command->clear.cmd.clear_flags));
            
            command->clear.result.left.error = glGetError();
            command->clear.result.type = command->clear.result.left.error == GL_NO_ERROR ? 
                                                    RESULT_SUCCESS : RESULT_ERROR;
            
            break;
            
        case DRAW_ARRAYS:
            glDrawArrays(draw_component_to_gl_enum(command->draw_arrays.cmd.component_type), 
                         command->draw_arrays.cmd.start, 
                         command->draw_arrays.cmd.count);
            
            command->draw_arrays.result.left.error = glGetError();
            command->draw_arrays.result.type = command->draw_arrays.result.left.error == GL_NO_ERROR ? 
                                                RESULT_SUCCESS : RESULT_ERROR;
            
            break;
            
        case USE_PROGRAM:
        {
            GLuint program_id = from_resource_id(environment, command->use_program.cmd.id);
            glUseProgram(program_id);
            
            command->use_program.result.left.error = glGetError();
            command->use_program.result.type = command->use_program.result.left.error == GL_NO_ERROR ? 
                                                RESULT_SUCCESS : RESULT_ERROR;
        }   
            break;
            
        case UNIFORM_MATRIX:
            switch (command->uniform_matrix.cmd.uniform_type) {
                case MATRIX_UNIFORM_2X2:
                    glUniformMatrix2fv(command->uniform_matrix.cmd.uniform_index, 
                                       command->uniform_matrix.cmd.count, command->uniform_matrix.cmd.transpose, 
                                       from_memory_location(environment, command->uniform_matrix.cmd.memory_location));
                    break;

                case MATRIX_UNIFORM_3X3:
                {
                    void* data = from_memory_location(environment, command->uniform_matrix.cmd.memory_location);
                    glUniformMatrix3fv(command->uniform_matrix.cmd.uniform_index, 
                                       command->uniform_matrix.cmd.count, command->uniform_matrix.cmd.transpose, 
                                       data);
                }
                    break;
                    
                case MATRIX_UNIFORM_4X4:
                {
                    void* data = from_memory_location(environment, command->uniform_matrix.cmd.memory_location);
                    glUniformMatrix4fv(command->uniform_matrix.cmd.uniform_index, 
                                       command->uniform_matrix.cmd.count, command->uniform_matrix.cmd.transpose, 
                                       data);
                }
                    break;
                    
                default:
                    break;
            }
            
            command->uniform_matrix.result.left.error = glGetError();
            command->uniform_matrix.result.type = command->uniform_matrix.result.left.error == GL_NO_ERROR ? 
                                                    RESULT_SUCCESS : RESULT_ERROR;
            
            assert(command->uniform_matrix.result.type == RESULT_SUCCESS);
            
            break;
            
        case ATTACH_SHADER:
            glAttachShader(from_resource_id(environment, command->attach_shader.cmd.program_id), 
                           from_resource_id(environment, command->attach_shader.cmd.shader_id));
            
            command->attach_shader.result.left.error = glGetError();
            command->attach_shader.result.type = command->attach_shader.result.left.error == GL_NO_ERROR ? 
                                                        RESULT_SUCCESS : RESULT_ERROR;
            
            break;
            
        case BIND_ATTRIBUTE_LOCATION:
            glBindAttribLocation(from_resource_id(environment, command->bind_attrib_location.cmd.program_id),
                                 command->bind_attrib_location.cmd.index, command->bind_attrib_location.cmd.name);
            
            command->bind_attrib_location.result.left.error = glGetError();
            command->bind_attrib_location.result.type = command->bind_attrib_location.result.left.error == GL_NO_ERROR ? 
                                                    RESULT_SUCCESS : RESULT_ERROR;
            
            break;
            
        case CREATE_PROGRAM:
            command->create_program.result.right.id = glCreateProgram();   
            
            map_result(environment, &command->create_program.result.right.id, 
                       command->create_program.cmd.mapper.count, 
                       command->create_program.cmd.mapper);
            
            command->create_program.result.left.error = glGetError();
            command->create_program.result.type = command->create_program.result.left.error == GL_NO_ERROR ? 
                                                            RESULT_SUCCESS : RESULT_ERROR;
            
            break;
            
        case CREATE_SHADER:
            command->create_shader.result.right.id = glCreateShader(shader_type_to_gl_enum(command->create_shader.cmd.type));   
            
            map_result(environment, &command->create_shader.result.right.id, 
                       command->create_shader.cmd.mapper.count, 
                       command->create_shader.cmd.mapper);
            
            command->create_shader.result.left.error = glGetError();
            command->create_shader.result.type = command->create_shader.result.left.error == GL_NO_ERROR ? 
                                                    RESULT_SUCCESS : RESULT_ERROR;

            
            break;
            
        case SHADER_SOURCE:
        {
            const char** sources  = (const char**)from_memory_location(environment, command->shader_source.cmd.source_location);
            glShaderSource(from_resource_id(environment, command->shader_source.cmd.id), 
                           command->shader_source.cmd.count, 
                           sources, 
                           command->shader_source.cmd.length);
            
            command->shader_source.result.left.error = glGetError();
            command->shader_source.result.type = command->shader_source.result.left.error == GL_NO_ERROR ? 
                                                    RESULT_SUCCESS : RESULT_ERROR;
        }
            
            break;
            
        case COMPILE_SHADER:
            glCompileShader(from_resource_id(environment, command->compile_shader.cmd.id));
            
            command->compile_shader.result.left.error = glGetError();
            command->compile_shader.result.type = command->compile_shader.result.left.error == GL_NO_ERROR ? 
                                                    RESULT_SUCCESS : RESULT_ERROR;

            
            break;
            
        case LINK_PROGRAM:
            glLinkProgram(from_resource_id(environment, command->link_program.cmd.id));
            
            command->link_program.result.left.error = glGetError();
            command->link_program.result.type = command->link_program.result.left.error == GL_NO_ERROR ? 
                                                        RESULT_SUCCESS : RESULT_ERROR;
            
            break;
            
        case GET_UNIFORM_LOCATION:
            command->get_uniform_location.result.right.index = glGetUniformLocation(
                                from_resource_id(environment, command->get_uniform_location.cmd.program_id), 
                                 command->get_uniform_location.cmd.name);
            
            command->get_uniform_location.result.left.error = glGetError();
            command->get_uniform_location.result.type = command->get_uniform_location.result.left.error == GL_NO_ERROR ? 
                                                        RESULT_SUCCESS : RESULT_ERROR;
            
            break;
            
        default:
            assert(0);
            break;
            
    }
    log_output(environment, command);
}

void   add_mapping(Environment* environment, Id name, GLuint id) {
    ResourceMapping mapping;
    mapping.id = id;
    mapping.name = name;
    environment->resource_mappings[environment->resource_mapping_count] = mapping;
    environment->resource_mapping_count++;
}

GLuint get_mapping(Environment* environment, Id name) {
    for(int i = 0; i < environment->resource_mapping_count; i++) {
        ResourceMapping mapping = environment->resource_mappings[i];
        
        if(mapping.name == name) {
            return mapping.id;
        }
    }
    
    assert(0);
    return -1;
}

GLuint from_resource_id(Environment* environment, ResourceId resource_id) {
    GLuint id;
    
    switch (resource_id.type) {
        case RESOURCE_ID:
            id = resource_id.id;
            break;
            
        case RESOURCE_NAME:
            id = get_mapping(environment, resource_id.name);
            break;
            
        default:
            break;
    }
    
    return id;
}

void map_result(Environment* environment, GLuint* resources, int count, ResourceMapper resource_mapper) {
    if (resource_mapper.map_resource) {
        for (int i = 0; i < count; i++) {
            Id resource_name = resource_mapper.names[i];
            add_mapping(environment, resource_name, resources[i]);
        }
    }     
}

void* from_memory_location(Environment* environment, MemoryLocation memory_location) {
    for(int i = 0; i < environment->resource_count; i++) {
        Resource resource = environment->resources[i];
        
        if (resource.id == memory_location.id) {
            return resource.buffer + memory_location.offset;
        }
    }
    
    return 0;
}

void add_resource(Environment* env, Resource resource) {
    assert(env->resource_count < env->resource_capacity);
    
    env->resources[env->resource_count] = resource;
    env->resource_count++;
}

void delete_resource(Environment* env, Id id) {
    assert(0);
}

void update_resource(Environment* env, Id id, const char* data, 
                     int count) {
    assert(0);
}

void log_input(Environment* env, Command* command) {
    if (env->cmd_logging) {
        char buffer[512];
        memset(buffer, 0, 512);
        show_command(GL_TRUE, GL_FALSE, buffer, 512, command);
        printf("%s\n", buffer);
    }
    
    if(env->env_logging) {
        char env_buffer[256];
        memset(env_buffer, 0, 256);
        show_environment(env_buffer, 256, env);
        printf("%s\n", env_buffer);
    }

}

void log_output(Environment* env, Command* command) {
    if (env->cmd_logging) {
        char buffer[512];
        memset(buffer, 0, 512);
        show_command(GL_FALSE, GL_TRUE, buffer, 512, command);
        printf("%s\n", buffer);
    }
    
    if (env->env_logging) {        
        char env_buffer[256];
        memset(env_buffer, 0, 256);
        show_environment(env_buffer, 256, env);
        
        printf("%s\n", env_buffer);
    }
}

void show_environment(char* buffer, int size, Environment* env) {
    char sub_buffer[100];
    memset(sub_buffer, 0, 100);
    show_resource_mapping_array(sub_buffer, 100, env->resource_mappings, 
                                env->resource_mapping_count);
    
    sprintf(buffer, "resources count %d, resources capacity %d, resource mapping count %d, resource mapping capacity %d, mappings %s", 
            env->resource_count, env->resource_capacity, 
            env->resource_mapping_count, env->resource_mapping_capacity, sub_buffer);
}

void show_resource_mapping_array(char* buffer, int size, 
                                 ResourceMapping* resource_mappings, int count) {
    for(int i = 0; i < count; i++) {
        
    }
}

void show_resource_mapping(char* buffer, int size, ResourceMapping resource_mapping){
    
}

Resource mk_resource(Id id, char* buffer, int count)
{
    Resource r = {id, buffer, count};
    return r;
}

GLenum GLEnableEnums[] = {
    GL_BLEND,
    GL_CULL_FACE,
    GL_DEPTH_TEST,
    GL_DITHER,
    GL_POLYGON_OFFSET_FILL,
    GL_SAMPLE_ALPHA_TO_COVERAGE,
    GL_SAMPLE_COVERAGE,
    GL_SCISSOR_TEST,
    GL_STENCIL_TEST    
};

GLenum enable_enum_to_gl_enum(EnableEnums state) {
    return GLEnableEnums[state];
}

GLenum GLBufferTarget[] = {
    GL_ARRAY_BUFFER,
    GL_ELEMENT_ARRAY_BUFFER
};

GLenum buffer_target_gl_enum(BufferTarget type) {
    return GLBufferTarget[type];
}

GLenum GLUsages[] = {
    GL_STATIC_DRAW, 
    GL_STREAM_DRAW,
    GL_DYNAMIC_DRAW
};

GLenum usage_to_gl_enum(Usage type) {
    return GLUsages[type];
}

GLenum GLDrawComponents[] = {
    GL_TRIANGLES
};

GLenum draw_component_to_gl_enum(DrawComponent type) {
    return GLDrawComponents[type];
}

GLenum GLVertexAttributeTypes[] = {
    GL_BYTE,
    GL_UNSIGNED_BYTE,
    GL_SHORT,
    GL_UNSIGNED_SHORT,
    GL_FIXED,
    GL_FLOAT
};

GLenum vertex_attribute_types_gl_enum(VertexAttributeType type) {
    return GLVertexAttributeTypes[type];
}

GLenum GLVAP_ETypes[] =  {
    GL_BYTE, 
    GL_UNSIGNED_BYTE, 
    GL_SHORT, 
    GL_UNSIGNED_SHORT, 
    GL_INT, 
    GL_UNSIGNED_INT, 
    GL_FLOAT
};

GLenum vap_to_gl_enum(VAP_EType type) {
    return GLVAP_ETypes[type];
}

GLenum GLEShaderTypes[] = {
    VERTEX_SHADER,
    FRAGMENT_SHADER,
    SHADER_TYPE_MAX,
    SHADER_TYPE_INVALID
};

GLenum shader_types_to_gl_enum(EShaderType type) {
    return GLEShaderTypes[type];
}

GLenum GLClearFlag[] = {
    GL_COLOR_BUFFER_BIT,
    GL_DEPTH_BUFFER_BIT
};

GLuint clear_flag_to_gl_enum(GLenum flags) {
    GLuint result = 0;
    
    for (int i = 0; i < CLEAR_FLAG_MAX; i++) {
        result |= ((flags >> i) & 0x1) ? GLClearFlag[i] : 0;
    }
    
    return result;
}


















