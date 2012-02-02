//
//  Serialization.h
//  BabyMorpher2
//
//  Created by hi5 networks on 2/1/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#ifndef BabyMorpher2_Serialization_h
#define BabyMorpher2_Serialization_h

typedef enum {
    GL_PRIMITIVE_BOOLEAN, 
    GL_PRIMITIVE_BYTE,
    GL_PRIMITIVE_UBYTE,
    GL_PRIMITIVE_CHAR,
    GL_PRIMITIVE_SHORT,
    GL_PRIMITIVE_USHORT,
    GL_PRIMITIVE_INT,
    GL_PRIMITIVE_UINT, 
    GL_PRIMITIVE_ENUM,
    GL_PRIMITIVE_FLOAT, 
    GL_PRIMITIVE_CLAMPF,
    GL_PRIMITIVE_POINTER,
    GL_UNION_INDEX
} EGLPrimitive;

typedef enum {
  TYPE_DESCRIPTION_PRIMITIVE,
  TYPE_DESCRIPTION_STRUCT,
  TYPE_DESCRIPTION_UNION,
  TYPE_DESCRIPTION_ARRAY,
  TYPE_DESCRIPTION_TYPED      
} ETypeDesription;

typedef struct TypeDescription_t* TypeDescription_p;

typedef struct Member_t {
    const char* name;
    TypeDescription_p description;
} Member;

Member mk_member(const char* name, TypeDescription_p description);
Member mk_struct_member(const char* name, TypeDescription_p desc, Member* members, int count, int size);
Member mk_union_member(const char* name, TypeDescription_p desc, Member* members, int count, int size);
Member mk_primitive_member(const char* name, TypeDescription_p desc, EGLPrimitive type);

typedef struct UnionDescription_t {
    Member* members;
    int count;
} UnionDescription;

typedef struct StructDescription_t {
    Member* members;
    int count;
} StructDescription;

typedef struct ArrayDescription_t {
    TypeDescription_p type;
    int count;
} ArrayDescription;

typedef struct TypeDescription_t {
    ETypeDesription type;
    union {
        int                type_description_index;
        StructDescription  struct_description;
        UnionDescription   union_description;
        ArrayDescription   array_description;
        EGLPrimitive       primitive_type;
    };
    int size;
} TypeDescription;


TypeDescription mk_struct_type_description(Member* members, int count, int size);
TypeDescription mk_array_type_description(TypeDescription_p type, int count);
TypeDescription mk_union_type_description(Member* members, int count, int size);
TypeDescription mk_primitive_type_description(EGLPrimitive type);

typedef struct CommandIdAddressMap_t {
    void** addresses;
    int count;
    int capacity;
} CommandIdAddressMap;

typedef struct Fixups_t {
    void** pointers;
    int capacity;
    int count;
} Fixups;

typedef struct ParserState_t {
    Fixups fixups;
    CommandIdAddressMap id_map;
    const char* input;
    const char* input_end;
    char* output;
    const char* output_end;
    TypeDescription* type_descriptions;
    int next_union_index;
} ParserState;

int get_output_byte_count(const char* buffer);
int get_object_count(const char* buffer);
int get_fixup_count(const char* buffer);

void deserialize_1(TypeDescription desc, const char* input, int input_count,
                   char* output, int output_count);

void deserialize_1_union(UnionDescription desc, int index, const char* input, int input_count,
                   char* output, int output_count);

void parse(TypeDescription* type_descriptions,  
           int input_count, const char* input, char* output, int output_count);

void parse_typed(ParserState* parse_state, int index);
void parse_type(ParserState* state, TypeDescription desc);
void parse_array(ParserState* parse_state, ArrayDescription array_description);
void parse_union(ParserState* state, UnionDescription desc);
void parse_struct(ParserState* state, StructDescription desc);
void parse_primitive(ParserState* state, EGLPrimitive desc);
void fixup(ParserState* state);
void resolve_fixup(void* address, CommandIdAddressMap map);

void copy(const char** input, char** output, int count);
const void* read_bytes(ParserState* state, int count);

void skip_output(ParserState* state, int count);

void add_fixup(ParserState* state);
void add_object(ParserState* state);

int primitive_type(EGLPrimitive type);
void set_next_union_index(ParserState* state);

int get_version(const char* buffer);

void* load_in_place(char* buffer);

//SERIALIZATION FUNCTIONS

typedef struct SerializationState_t {
    TypeDescription* descriptions;
    int description_count;
    
    const char* input;
    const char const* input_end;
    
    char* output;
    const char const* output_end;
    
    int next_union_index;
} SerializationState;

void serialize(TypeDescription* descs, int desc_count, TypeDescription desc, 
               const char* input, 
               char* output, int count);

void serialize_1(TypeDescription desc, const char* input, 
               char* output, int count);

void serialize_1_union(UnionDescription desc, int index, const char* input, 
                       char* output, int count);

void serialize_type(SerializationState* state, TypeDescription desc);
void serialize_array(SerializationState* state, ArrayDescription  desc);
void serialize_union(SerializationState* state, UnionDescription  desc);
void serialize_struct(SerializationState* state, StructDescription desc);
void serialize_primitive(SerializationState* state, EGLPrimitive type);
void serialize_typed(SerializationState* state, int index);

























#endif
