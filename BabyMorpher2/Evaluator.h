//
//  Evaluator.h
//  BabyMorpher2
//
//  Created by hi5 networks on 1/30/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#ifndef BabyMorpher2_Evaluator_h
#define BabyMorpher2_Evaluator_h

#include "Commands.h"


typedef struct Resource_t {
    Id id;
    char* buffer;
    int count;
} Resource;

Resource mk_resource(Id id, char* buffer, int count);

typedef struct ResourceMapping_t {
    GLuint id;
    Id name;
} ResourceMapping;

typedef struct Environment_t {
    Resource* resources;
    int resource_count;  
    int resource_capacity;
    
    ResourceMapping* resource_mappings;
    int resource_mapping_count;
    int resource_mapping_capacity;
    
    GLboolean cmd_logging;
    GLboolean env_logging;
    
} Environment;

void show_environment(char* buffer, int size, Environment* environment);

void show_resource_mapping_array(char* buffer, int size, 
                                 ResourceMapping* resource_mappings, int count);
void show_resource_mapping(char* buffer, int size, ResourceMapping resource_mapping);

Environment* alloc_environment(int resource_capacity, int resource_mapping_count);
void free_environment(Environment* env);

void   evaluate(Environment* environment, Command* cmd_and_result);
void   add_mapping(Environment* environment, Id name, GLuint id);
GLuint get_mapping(Environment* environment, Id name);
GLuint from_resource_id(Environment* environment, ResourceId resource_id);
void   map_result(Environment* environment, GLuint* resources, int count, ResourceMapper resource_mapper);
void*  from_memory_location(Environment* environment, MemoryLocation memory_location);
void   add_resource(Environment* env, Resource resource);   
void delete_resource(Environment* env, Id id);
void update_resource(Environment* env, Id id, const char* data, int count);
void   log_input(Environment* env, Command* command);
void   log_output(Environment* env, Command* command);

GLenum enable_enum_to_gl_enum(EnableEnums state);
GLenum buffer_target_gl_enum(BufferTarget type);
GLenum usage_to_gl_enum(Usage type);
GLenum draw_component_to_gl_enum(DrawComponent type);
GLenum vertex_attribute_types_gl_enum(VertexAttributeType type);
GLenum vap_to_gl_enum(VAP_EType type);
GLenum shader_types_to_gl_enum(EShaderType type);
GLuint clear_flag_to_gl_enum(GLenum flags);




#endif
