//
//  Evaluator.cpp
//  BabyMorpher2
//
//  Created by hi5 networks on 1/29/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#include "_Evaluator.h"
#include <OpenGLES/ES2/gl.h>
#include <OpenGLES/ES2/glext.h>

void Evaluator::initialize(Environment* environment) {
    m_environment = environment;
}

void Evaluator::setup() {
    evaluate_cmds(m_environment->setup_commands);
}

void Evaluator::evaluate_cmds(FixedArray<CmdAndResult> cmds) {
    for (int i = 0; i < cmds.count; i++) {
        evaluate(cmds.values[i]);
    }
}

void Evaluator::evaluate(CmdAndResult& cmd_and_result) {
    Command& cmd = cmd_and_result.cmd;
    Result& result = cmd_and_result.result;
    
    switch (cmd_and_result.cmd.type) {
        case ADDDATA:
            break;
            
        case DELETEDATA:
            break;
            
        case UPDATEDATA:
            break;    
            
        case ENABLE:
            glEnable(cmd.enable.state);
            break;
            
        case GENBUFFERS:
            glGenBuffers(cmd.gen_buffers.count, result.gen_buffers.buffers);
            
            map_result(result.gen_buffers.buffers, cmd.gen_buffers.count, 
                       cmd.gen_buffers.mapper);
            break;
            
        case DELETEBUFFERS:
            
            break;
            
        case BINDBUFFER:
            glBindBuffer(cmd.bind_buffer.buffer_target, 
                         from_resource_id(cmd.bind_buffer.id));
            break;
            
        case BUFFERDATA:
            glBufferData(cmd.buffer_data.buffer_target, 
                         cmd.buffer_data.size, from_memory_location(cmd.buffer_data.memory_location), 
                         cmd.buffer_data.usage);
            break;
            
        case VERTEX_ATTRIB_POINTER:
            glVertexAttribPointer(cmd.vertex_attrib_pointer.index, 
                                  cmd.vertex_attrib_pointer.size, 
                                  cmd.vertex_attrib_pointer.type, cmd.vertex_attrib_pointer.normalized,
                                  cmd.vertex_attrib_pointer.stride, 
                                  from_memory_location(cmd.vertex_attrib_pointer.memory_location));
            break;
            
        case GEN_VERTEX_ARRAYS_OES:
            glGenVertexArraysOES(cmd.gen_vertex_arrays_oes.count, result.gen_vertex_arrays_oes.buffers);
            
            map_result(result.gen_vertex_arrays_oes.buffers, 
                       cmd.gen_vertex_arrays_oes.count, 
                       cmd.gen_vertex_arrays_oes.mapper);
            break;
            
        case BIND_VERTEX_ARRAYS_OES:
            glBindVertexArrayOES(from_resource_id(cmd.bind_vertex_arrays_oes.id));
            break;
            
        case ENABLE_VERTEX_ATTRIB_ARRAY:
            glEnableVertexAttribArray(cmd.enable_vertex_attribute_array.index);
            break;
            
        case BIND_VERTEX_ARRAY_OES:
            glBindVertexArrayOES(from_resource_id(cmd.bind_vertex_arrays_oes.id));
            break;
            
        default:
            break;

    }
}



GLuint Evaluator::get_mapping(const char* name) {
    for(int i = 0; i < m_environment->resource_mappings.count; i++) {
        ResourceMapping mapping = m_environment->resource_mappings.values[i];
        
        if(mapping.name == name) {
            return mapping.id;
        }
    }
    
    return -1;
}

void Evaluator::add_mapping(const char* name, GLuint id) {
    ResourceMapping mapping;
    mapping.id = id;
    mapping.name = name;
    int count = m_environment->resource_mappings.count;
    m_environment->resource_mappings.values[count] = mapping;
}

GLuint Evaluator::from_resource_id(ResourceId resource_id) {
    GLuint id;
    
    switch (resource_id.type) {
        case RESOURCE_NAME:
            id = resource_id.id;
            break;
            
        case RESOURCE_ID:
            id = get_mapping(resource_id.name);
            break;
            
        default:
            break;
    }
    
    return id;
}

void Evaluator::map_result(GLuint* resources, int count, ResourceMapper resource_mapper) {
    if (resource_mapper.map_resource) {
        for (int i = 0; i < count; i++) {
            const char* resource_name = resource_mapper.names[i];
            add_mapping(resource_name, resources[i]);
        }
    } 
}

void Evaluator::appendCommand(EvaluatorCommand cmd) {
    
}

void Evaluator::runCommands(EvaluatorCommand_p cmds, int count) {
    
}

void Evaluator::draw() {
    evaluate_cmds(m_environment->draw_loop_commands);
}

void Evaluator::teardown() {
    evaluate_cmds(m_environment->teardown_commands);    
}

void* Evaluator::from_memory_location(const MemoryLocation& memory_location) {
    for(int i = 0; i < m_environment->resource_count; i++) {
        Resource resource = m_environment->resources[i];
        
        if (resource.id == memory_location.id) {
            return &resource.buffer.values[memory_location.offset];
        }
    }
    
    return 0;
}











