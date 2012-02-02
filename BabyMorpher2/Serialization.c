//
//  Serialization.c
//  BabyMorpher2
//
//  Created by hi5 networks on 2/1/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#include "Serialization.h"
#include <stdlib.h>
#include <memory.h>
#include <assert.h>
#import <OpenGLES/ES2/gl.h>

void deserialize_1(TypeDescription desc, const char* input, int input_count,
                   char* output, int output_count) {
    ParserState state;
    
    state.input      = input;
    state.input_end  = input + input_count;
    state.output     = output;
    state.output_end = output + output_count; 
    
    parse_type(&state, desc);
}

void deserialize_1_union(UnionDescription desc, int index, const char* input, int input_count,
                         char* output, int output_count) {
    ParserState state;
    
    state.input      = input;
    state.input_end  = input + input_count;
    state.output     = output;
    state.output_end = output + output_count; 
    
    state.next_union_index = index;
    
    parse_union(&state, desc);
    
}

void parse(TypeDescription* type_descriptions,  
           int input_count, const char* input, char* output, int output_count) {
    ParserState state;
    
    state.input      = input;
    state.input_end  = input + input_count;
    state.output     = output;
    state.output_end = output + output_count; 
    
    state.fixups.capacity = get_fixup_count(input);
    state.fixups.pointers = malloc(sizeof(void*) * state.fixups.capacity); 
    state.fixups.count    = 0;
    
    state.id_map.capacity  = get_object_count(input);
    state.id_map.addresses = malloc(sizeof(void*) * state.id_map.capacity); 
    state.id_map.count     = 0;
    
    while (state.input < state.input_end) {
        assert(state.output < state.output_end);
        int type_index = *(int*)read_bytes(&state, sizeof(int));
        add_object(&state);
        parse_typed(&state, type_index);        
    }
    
    fixup(&state);
    
    free(state.fixups.pointers);
    free(state.id_map.addresses);    
}

int get_version(const char* buffer) {
    return *(int*)buffer;
}

int get_output_byte_count(const char* buffer) {
    return *(int*)(buffer + 4);
}

int get_object_count(const char* buffer) {
    return *(int*)(buffer + 8);
}

int get_fixup_count(const char* buffer) {
    return *(int*)(buffer + 12);
}

void parse_type(ParserState* state, TypeDescription desc) {
    char* old_output = state->output;
    
    ETypeDesription type = desc.type;
    switch (type) {
        case TYPE_DESCRIPTION_PRIMITIVE:
            parse_primitive(state, desc.primitive_type);
            break;

        case TYPE_DESCRIPTION_STRUCT:
            parse_struct(state, desc.struct_description);
            break;
            
        case TYPE_DESCRIPTION_UNION:
            parse_union(state, desc.union_description);
            break;
            
        case TYPE_DESCRIPTION_ARRAY:
            parse_array(state, desc.array_description);
            break;
            
        case TYPE_DESCRIPTION_TYPED:
            parse_typed(state, desc.type_description_index);
            break;
            
        default:
            break;
    }
    
    //this is to compensate for padding
    int bytes_written = state->output - old_output;
    skip_output(state, desc.size - bytes_written);
}

void parse_typed(ParserState* parse_state, int index) {
    TypeDescription current_desc = parse_state->type_descriptions[index];
    parse_type(parse_state, current_desc);
}

void parse_array(ParserState* parse_state, ArrayDescription array_description) {
    for (int i = 0; i < array_description.count; i++) {
        parse_type(parse_state, *array_description.type);
    }
}

void parse_union(ParserState* state, UnionDescription desc) {
    //get the member info
    Member member = desc.members[state->next_union_index];
    //recurse
    parse_type(state, *member.description);
}

void parse_struct(ParserState* state, StructDescription desc) {
    for (int i = 0; i < desc.count; i++) {
        Member member = desc.members[i];
        parse_type(state, *member.description);
    }
}

void parse_primitive(ParserState* state, EGLPrimitive desc) {
    int copy_size = 0;
    switch (desc) {
        case GL_PRIMITIVE_BOOLEAN:
            copy_size = sizeof(GLboolean);
            break;
            
        case GL_PRIMITIVE_BYTE:
            copy_size = sizeof(GLbyte);
            break;
            
        case GL_PRIMITIVE_UBYTE:
            copy_size = sizeof(GLubyte);
            break;
            
        case GL_PRIMITIVE_CHAR:
            copy_size = sizeof(GLchar);
            break;
            
        case GL_PRIMITIVE_SHORT:
            copy_size = sizeof(GLshort);
            break;
            
        case GL_PRIMITIVE_USHORT:
            copy_size = sizeof(GLushort);
            break;
            
        case GL_PRIMITIVE_INT:
            copy_size = sizeof(GLint);
            break;
            
        case GL_PRIMITIVE_UINT: 
            copy_size = sizeof(GLuint);
            break;
            
        case GL_PRIMITIVE_ENUM:
            copy_size = sizeof(GLenum);
            break;
            
        case GL_PRIMITIVE_FLOAT: 
            copy_size = sizeof(GLfloat);
            break;
            
        case GL_PRIMITIVE_CLAMPF:
            copy_size = sizeof(GLclampf);
            break;
            
        case GL_PRIMITIVE_POINTER:
            copy_size = sizeof(int);
            add_fixup(state);
            break;
        case GL_UNION_INDEX:
            copy_size = sizeof(int);
            set_next_union_index(state);
            break;
            
        default:
            assert(0);
            break;
    }
    copy(&state->input, &state->output, copy_size);
}

void set_next_union_index(ParserState* state) {
    state->next_union_index = *(int*)state->input;
}

void fixup(ParserState* state) {
    //fixups should store indices
    for (int i = 0; i < state->fixups.count; i++) {
        resolve_fixup(state->fixups.pointers[i], state->id_map);
    }
}

void resolve_fixup(void* address, CommandIdAddressMap map) {
    int id = *(int*)address;
    *(int*)address = (int)map.addresses[id];
}

void copy(const char** input, char** output, int count) {
    memcpy(*output, *input, count);
    *input  += count;
    *output += count; 
}

const void* read_bytes(ParserState* state, int count) {
    const void* result = state->input;
    state->input += count;
    return result;
}

void skip_output(ParserState* state, int count) {
    state->output += count;
}

void add_fixup(ParserState* state) {
    assert(state->fixups.count < state->fixups.capacity);
    state->fixups.pointers[state->fixups.count] = (void*)state->output;
    state->fixups.count++;
}

void add_object(ParserState* state) {
    assert(state->id_map.count < state->id_map.capacity);
    state->id_map.addresses[state->id_map.count] = (void*)state->output;
    state->id_map.count++;    
}


Member mk_member(const char* name, TypeDescription_p description) {
    Member result = {name, description};
    return result;
}

Member mk_struct_member(const char* name, TypeDescription_p desc, Member* members, int count, int size) {
    *desc = mk_struct_type_description(members, count, size);
    Member result = {name, desc};
    return result;
}

Member mk_union_member(const char* name, TypeDescription_p desc, Member* members, int count, int size) {
    *desc = mk_union_type_description(members, count, size);
    Member result = {name, desc};
    return result;
}

Member mk_primitive_member(const char* name, TypeDescription_p desc, EGLPrimitive type) {
    *desc = mk_primitive_type_description(type);
    Member result = {name, desc};
    return result;
}

TypeDescription mk_struct_type_description(Member* members, int count, int size) {
    StructDescription struct_result = {members, count};
    TypeDescription result;
    result.type = TYPE_DESCRIPTION_STRUCT;
    result.struct_description = struct_result;
    result.size = size;
    return result;
}

TypeDescription mk_union_type_description(Member* members, int count, int size) {
    UnionDescription union_description = {members, count};
    TypeDescription result;
    result.type              = TYPE_DESCRIPTION_UNION;
    result.union_description = union_description;
    result.size              = size;
    return result;
}

TypeDescription mk_primitive_type_description(EGLPrimitive type) {
    TypeDescription result;
    
    result.type             = TYPE_DESCRIPTION_PRIMITIVE;
    result.primitive_type   = type;
    result.size             = primitive_type(type);
    
    return result;
}

TypeDescription mk_array_type_description(TypeDescription_p type, int count) {
    TypeDescription result;
    
    result.type                     = TYPE_DESCRIPTION_ARRAY;
    result.array_description.type   = type;
    result.array_description.count  = count;
    
    return result;
}

int PrimitiveSizes[] = {
    sizeof(GLboolean),  //GL_PRIMITIVE_BOOLEAN, 
    sizeof(GLbyte),     //GL_PRIMITIVE_BYTE,
    sizeof(GLubyte),    //GL_PRIMITIVE_UBYTE,
    sizeof(GLchar),     //GL_PRIMITIVE_CHAR,
    sizeof(GLshort),    //GL_PRIMITIVE_SHORT,
    sizeof(GLushort),   //GL_PRIMITIVE_USHORT,
    sizeof(GLint),      //GL_PRIMITIVE_INT,
    sizeof(GLuint),     //GL_PRIMITIVE_UINT, 
    sizeof(GLenum),     //GL_PRIMITIVE_ENUM,
    sizeof(GLfloat),    //GL_PRIMITIVE_FLOAT, 
    sizeof(GLclampf),   //GL_PRIMITIVE_CLAMPF,
    sizeof(void*)       //GL_PRIMITIVE_POINTER
};

int primitive_type(EGLPrimitive type) {
    return PrimitiveSizes[type];
}

void serialize(TypeDescription* descs, int desc_count, TypeDescription desc, const char* input, 
               char* output, int count) {
    SerializationState state;
    state.descriptions      = descs;
    state.description_count = count;
    
    state.input             = input;
    state.input_end         = input + count;
    
    state.output            = output;
    state.output_end        = output + count;
    
    state.next_union_index  = -1;
    
    
    serialize_type(&state, desc);
}

void serialize_1(TypeDescription desc, const char* input, 
                 char* output, int count) {
    serialize(NULL, 0, desc, input, output, count);
}

void serialize_type(SerializationState* state, TypeDescription desc) {
    switch (desc.type) {
        case TYPE_DESCRIPTION_PRIMITIVE:
            serialize_primitive(state, desc.primitive_type);
            break;
            
        case TYPE_DESCRIPTION_STRUCT:
            serialize_struct(state, desc.struct_description);
            break;
            
        case TYPE_DESCRIPTION_UNION:
            serialize_union(state, desc.union_description);
            break;
            
        case TYPE_DESCRIPTION_ARRAY:
            serialize_array(state, desc.array_description);
            break;
            
        case TYPE_DESCRIPTION_TYPED:
            serialize_typed(state, desc.type_description_index);
            break;
            
        default:
            break;
    }
}

void serialize_array(SerializationState* state, ArrayDescription  desc) {
    for (int i = 0; i < desc.count; i++) {
        serialize_type(state, *desc.type);
    }
}

void serialize_union(SerializationState* state, UnionDescription desc) {
    serialize_type(state, *desc.members[state->next_union_index].description);
}

void serialize_struct(SerializationState* state, StructDescription desc) {
    for (int i = 0; i < desc.count; i++) {
        serialize_type(state, *desc.members[i].description);
    }
}

void serialize_primitive(SerializationState* state, EGLPrimitive type) {
    int copy_size = 0;
    switch (type) {
        case GL_PRIMITIVE_BOOLEAN:
            copy_size = sizeof(GLboolean);
            break;
            
        case GL_PRIMITIVE_BYTE:
            copy_size = sizeof(GLbyte);
            break;
            
        case GL_PRIMITIVE_UBYTE:
            copy_size = sizeof(GLubyte);
            break;
            
        case GL_PRIMITIVE_CHAR:
            copy_size = sizeof(GLchar);
            break;
            
        case GL_PRIMITIVE_SHORT:
            copy_size = sizeof(GLshort);
            break;
            
        case GL_PRIMITIVE_USHORT:
            copy_size = sizeof(GLushort);
            break;
            
        case GL_PRIMITIVE_INT:
            copy_size = sizeof(GLint);
            break;
            
        case GL_PRIMITIVE_UINT: 
            copy_size = sizeof(GLuint);
            break;
            
        case GL_PRIMITIVE_ENUM:
            copy_size = sizeof(GLenum);
            break;
            
        case GL_PRIMITIVE_FLOAT: 
            copy_size = sizeof(GLfloat);
            break;
            
        case GL_PRIMITIVE_CLAMPF:
            copy_size = sizeof(GLclampf);
            break;
            
        case GL_PRIMITIVE_POINTER:
            copy_size = sizeof(int);
            break;
        case GL_UNION_INDEX:
            copy_size = sizeof(int);
            break;
            
        default:
            assert(0);
            break;
    }
    copy(&state->input, &state->output, copy_size);
}

void serialize_typed(SerializationState* state, int index) { 
    serialize_type(state, state->descriptions[index]);
}


void serialize_1_union(UnionDescription desc, int index, const char* input, 
                       char* output, int count) {
    SerializationState state;
    state.descriptions      = NULL;
    state.description_count = 0;
    
    state.input             = input;
    state.input_end         = input + count;
    
    state.output            = output;
    state.output_end        = output + count;
    
    state.next_union_index  = index;
    
    
    serialize_union(&state, desc);
}
























