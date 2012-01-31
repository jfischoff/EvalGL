//
//  Evaluator.h
//  BabyMorpher2
//
//  Created by hi5 networks on 1/28/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#ifndef BabyMorpher2__Evaluator_h
#define BabyMorpher2__Evaluator_h

#include "Commands.h"

typedef struct EvaluatorCommand_t* EvaluatorCommand_p;
typedef struct Result_t* Result_p;

typedef void(*Done)();

template <typename T>
struct FixedArray {
    T* values;
    int count;
    int capacity;
};

template <typename T>
FixedArray<T> new_fixed_array(int capacity) {
    FixedArray<T> result = {new T[capacity], 0, capacity};
    return result;
}

typedef FixedArray<char> Buffer;

typedef FixedArray<CmdAndResult> CmdsAndResults;

struct Resource {
    int id;
    Buffer buffer;
};

struct ResourceMapping {
    GLuint id;
    const char* name;
};

struct Environment {
    Resource* resources;
    int resource_count;
    
    CmdsAndResults setup_commands;
    CmdsAndResults teardown_commands;
    CmdsAndResults draw_loop_commands;
    CmdsAndResults update_commands;
    
    FixedArray<ResourceMapping> resource_mappings;
};

class Evaluator{
    Environment* m_environment;
    
public:
    
    void initialize(Environment* environment);
    
    void setup();
    
    void evaluate_cmds(FixedArray<CmdAndResult> cmds);
    
    void evaluate(CmdAndResult& cmd_and_result);
    
    void appendCommand(EvaluatorCommand cmd);
    
    void add_mapping(const char* name, GLuint id);
    GLuint get_mapping(const char* name);
    
    GLuint from_resource_id(ResourceId resource_id);
    
    void map_result(GLuint* resources, int count, ResourceMapper resource_mapper);
    
    void runCommands(EvaluatorCommand_p cmds, int count);
    
    void draw();
    
    void teardown();
    
    void* from_memory_location(const MemoryLocation& memory_location);
};


#endif
