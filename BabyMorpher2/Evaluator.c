//
//  File.c
//  BabyMorpher2
//
//  Created by hi5 networks on 1/30/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#include "Evaluator.h"
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
    log_command(environment, command);
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
            glEnable(command->enable.cmd.state);
            break;
            
        case GENBUFFERS:
            glGenBuffers(command->gen_buffers.cmd.count, command->gen_buffers.result.right.buffers);
            
            map_result(environment, command->gen_buffers.result.right.buffers, command->gen_buffers.cmd.count, 
                       command->gen_buffers.cmd.mapper);
            break;
            
        case DELETEBUFFERS:
            
            break;
            
        case BINDBUFFER:
            glBindBuffer(command->bind_buffer.cmd.buffer_target, 
                         from_resource_id(environment, command->bind_buffer.cmd.id));
            break;
            
        case BUFFERDATA:
            glBufferData(command->buffer_data.cmd.buffer_target, 
                         command->buffer_data.cmd.size, from_memory_location(environment,
                                command->buffer_data.cmd.memory_location), 
                         command->buffer_data.cmd.usage);
            break;
            
        case VERTEX_ATTRIB_POINTER:
            glVertexAttribPointer(command->vertex_attrib_pointer.cmd.index, 
                                  command->vertex_attrib_pointer.cmd.size, 
                                  command->vertex_attrib_pointer.cmd.type, command->vertex_attrib_pointer.cmd.normalized,
                                  command->vertex_attrib_pointer.cmd.stride, 
                                  from_memory_location(environment, command->vertex_attrib_pointer.cmd.memory_location));
            break;
            
        case GEN_VERTEX_ARRAYS_OES:
            glGenVertexArraysOES(command->gen_vertex_arrays_oes.cmd.count, 
                                 command->gen_vertex_arrays_oes.result.right.buffers);
            
            map_result(environment, command->gen_vertex_arrays_oes.result.right.buffers, 
                       command->gen_vertex_arrays_oes.cmd.count, 
                       command->gen_vertex_arrays_oes.cmd.mapper);
            break;
            
        case BIND_VERTEX_ARRAY_OES:
            glBindVertexArrayOES(from_resource_id(environment, command->bind_vertex_array_oes.cmd.id));
            break;
            
        case ENABLE_VERTEX_ATTRIB_ARRAY:
            glEnableVertexAttribArray(command->enable_vertex_attribute_array.cmd.index);
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
            break;
            
        case CLEAR:
            glClear(command->clear.cmd.clear_flags);
            break;
            
        case DRAW_ARRAYS:
            glDrawArrays(command->draw_arrays.cmd.component_type, command->draw_arrays.cmd.start, 
                         command->draw_arrays.cmd.count);
            break;
            
        case USE_PROGRAM:
            glUseProgram(from_resource_id(environment, command->use_program.cmd.id));
            break;
            
        case UNIFORM_MATRIX:
            switch (command->uniform_matrix.cmd.uniform_type) {
                case MATRIX_UNIFORM_2X2:
                    glUniformMatrix2fv(command->uniform_matrix.cmd.uniform_index, 
                                       command->uniform_matrix.cmd.count, command->uniform_matrix.cmd.transpose, 
                                       from_memory_location(environment, command->uniform_matrix.cmd.memory_location));
                    break;

                case MATRIX_UNIFORM_3X3:
                    glUniformMatrix3fv(command->uniform_matrix.cmd.uniform_index, 
                                       command->uniform_matrix.cmd.count, command->uniform_matrix.cmd.transpose, 
                                       from_memory_location(environment, command->uniform_matrix.cmd.memory_location));
                    break;
                    
                case MATRIX_UNIFORM_4X4:
                    glUniformMatrix4fv(command->uniform_matrix.cmd.uniform_index, 
                                       command->uniform_matrix.cmd.count, command->uniform_matrix.cmd.transpose, 
                                       from_memory_location(environment, command->uniform_matrix.cmd.memory_location));
                    break;
                    
                default:
                    break;
            }
            break;
            
        case ATTACH_SHADER:
            glAttachShader(from_resource_id(environment, command->attach_shader.cmd.program_id), 
                           from_resource_id(environment, command->attach_shader.cmd.program_id));
            break;
            
        case BIND_ATTRIBUTE_LOCATION:
            glBindAttribLocation(from_resource_id(environment, command->bind_attrib_location.cmd.program_id),
                                 command->bind_attrib_location.cmd.index, command->bind_attrib_location.cmd.name);
            break;
            
        case CREATE_PROGRAM:
            command->create_program.result.right.id = glCreateProgram();   
            
            map_result(environment, &command->create_program.result.right.id, 
                       command->create_program.cmd.mapper.count, 
                       command->create_program.cmd.mapper);
            break;
            
        case CREATE_SHADER:
            command->create_shader.result.right.id = glCreateShader(command->create_shader.cmd.type);   
            
            map_result(environment, &command->create_shader.result.right.id, 
                       command->create_shader.cmd.mapper.count, 
                       command->create_shader.cmd.mapper);
            break;
            
        case SHADER_SOURCE:
            glShaderSource(from_resource_id(environment, command->shader_source.cmd.id), 
                           command->shader_source.cmd.count, 
                           from_memory_location(environment, command->shader_source.cmd.source_location), 
                           command->shader_source.cmd.length);
            break;
            
        case COMPILE_SHADER:
            glCompileShader(from_resource_id(environment, command->compile_shader.cmd.id));
            break;
            
        case LINK_PROGRAM:
            glLinkProgram(from_resource_id(environment, command->link_program.cmd.id));
            break;
            
        default:
            assert(0);
            break;
            
    }    
}

void   add_mapping(Environment* environment, const char* name, GLuint id) {
    ResourceMapping mapping;
    mapping.id = id;
    mapping.name = name;
    int count = environment->resource_mapping_count;
    environment->resource_mappings[count] = mapping;
}

GLuint get_mapping(Environment* environment, const char* name) {
    for(int i = 0; i < environment->resource_mapping_count; i++) {
        ResourceMapping mapping = environment->resource_mappings[i];
        
        if(mapping.name == name) {
            return mapping.id;
        }
    }
    
    return -1;
}

GLuint from_resource_id(Environment* environment, ResourceId resource_id) {
    GLuint id;
    
    switch (resource_id.type) {
        case RESOURCE_NAME:
            id = resource_id.id;
            break;
            
        case RESOURCE_ID:
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
            const char* resource_name = resource_mapper.names[i];
            add_mapping(environment, resource_name, resources[i]);
        }
    }     
}

void* from_memory_location(Environment* environment, MemoryLocation memory_location) {
    for(int i = 0; i < environment->resource_count; i++) {
        Resource resource = environment->resources[i];
        
        if (resource.id == memory_location.id) {
            return &resource.buffer[memory_location.offset];
        }
    }
    
    return 0;
}

void add_resource(Environment* env, Resource resource) {
    assert(env->resource_count < env->resource_capacity);
    
    env->resources[env->resource_count] = resource;
    env->resource_count++;
}

void delete_resource(Environment* env, const char* id) {
    assert(0);
}

void update_resource(Environment* env, const char* id, const char* data, 
                     int count) {
    assert(0);
}

void log_command(Environment* env, Command* command) {
    if (env->logging) {
        char buffer[512];
        memset(buffer, 0, 512);
        show_command(buffer, 512, command);
        
        char env_buffer[256];
        memset(env_buffer, 0, 256);
        show_environment(env_buffer, 256, env);
        
        printf("%s\n, %s\n", env_buffer, buffer);
        fflush(stdout);
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

Resource mk_resource(const char* id, char* buffer, int count)
{
    Resource r = {id, buffer, count};
    return r;
}




















