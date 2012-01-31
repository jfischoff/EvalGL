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
    const char* id;
    char* buffer;
    int count;
} Resource;

Resource mk_resource(const char* id, char* buffer, int count);

typedef struct ResourceMapping_t {
    GLuint id;
    const char* name;
} ResourceMapping;

typedef struct Environment_t {
    Resource* resources;
    int resource_count;  
    int resource_capacity;
    
    ResourceMapping* resource_mappings;
    int resource_mapping_count;
    int resource_mapping_capacity;
    
    GLboolean logging;
} Environment;

void show_environment(char* buffer, int size, Environment* environment);

void show_resource_mapping_array(char* buffer, int size, 
                                 ResourceMapping* resource_mappings, int count);
void show_resource_mapping(char* buffer, int size, ResourceMapping resource_mapping);

Environment* alloc_environment(int resource_capacity, int resource_mapping_count);
void free_environment(Environment* env);

void   evaluate(Environment* environment, Command* cmd_and_result);
void   add_mapping(Environment* environment, const char* name, GLuint id);
GLuint get_mapping(Environment* environment, const char* name);
GLuint from_resource_id(Environment* environment, ResourceId resource_id);
void   map_result(Environment* environment, GLuint* resources, int count, ResourceMapper resource_mapper);
void*  from_memory_location(Environment* environment, MemoryLocation memory_location);
void   add_resource(Environment* env, Resource resource);   
void delete_resource(Environment* env, const char* id);
void update_resource(Environment* env, const char* id, const char* data, int count);
void   log_command(Environment* env, Command* command);






#endif
