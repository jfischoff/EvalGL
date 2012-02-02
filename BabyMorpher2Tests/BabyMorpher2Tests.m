//
//  BabyMorpher2Tests.m
//  BabyMorpher2Tests
//
//  Created by hi5 networks on 1/28/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import "BabyMorpher2Tests.h"
#import "Serialization.h"
#import "Commands.h"

@implementation BabyMorpher2Tests

- (void)setUp
{
    [super setUp];
    
    // Set-up code here.
}

- (void)tearDown
{
    // Tear-down code here.
    
    [super tearDown];
}

typedef enum {
    TEST_0 = 8799
} TestEnum;

- (void)testPrimitiveSerialization {
    //GL_PRIMITIVE_BOOLEAN, 
    {
        TypeDescription BoolType          = mk_primitive_type_description(GL_PRIMITIVE_BOOLEAN);
        GLboolean i = true;
        const int size = sizeof(GLboolean);
        char buffer[size];
        serialize_1(BoolType, (char*)&i, buffer, size);
        GLboolean deserialized;
        deserialize_1(BoolType, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"GL_PRIMITIVE_BOOLEAN d %d not equal to i %d", deserialized, i);
    }
    
    //GL_PRIMITIVE_BYTE,
    {
        TypeDescription type          = mk_primitive_type_description(GL_PRIMITIVE_BYTE);
        GLbyte i = 42;
        const int size = sizeof(GLbyte);
        char buffer[size];
        serialize_1(type, (char*)&i, buffer, size);
        GLbyte deserialized;
        deserialize_1(type, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"d %d not equal to i %d", deserialized, i);
    }
    
    //GL_PRIMITIVE_UBYTE,
    {
        TypeDescription type          = mk_primitive_type_description(GL_PRIMITIVE_UBYTE);
        GLubyte i = 42;
        const int size = sizeof(GLubyte);
        char buffer[size];
        serialize_1(type, (char*)&i, buffer, size);
        GLubyte deserialized;
        deserialize_1(type, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"d %d not equal to i %d", deserialized, i);
    }
    
    //GL_PRIMITIVE_CHAR,
    {
        TypeDescription type          = mk_primitive_type_description(GL_PRIMITIVE_CHAR);
        GLchar i = 42;
        const int size = sizeof(GLchar);
        char buffer[size];
        serialize_1(type, (char*)&i, buffer, size);
        GLchar deserialized;
        deserialize_1(type, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"d %d not equal to i %d", deserialized, i);
    }
    
    //GL_PRIMITIVE_SHORT,
    {
        TypeDescription type          = mk_primitive_type_description(GL_PRIMITIVE_SHORT);
        GLshort i = 42;
        const int size = sizeof(GLuint);
        char buffer[size];
        serialize_1(type, (char*)&i, buffer, size);
        GLshort deserialized;
        deserialize_1(type, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"d %d not equal to i %d", deserialized, i);
    }
    
    //GL_PRIMITIVE_USHORT,
    {
        TypeDescription UIntType          = mk_primitive_type_description(GL_PRIMITIVE_USHORT);
        GLushort i = 42;
        const int size = sizeof(GLushort);
        char buffer[size];
        serialize_1(UIntType, (char*)&i, buffer, size);
        GLushort deserialized;
        deserialize_1(UIntType, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"d %d not equal to i %d", deserialized, i);
    }
    
    //GL_PRIMITIVE_INT,
    {
        TypeDescription type          = mk_primitive_type_description(GL_PRIMITIVE_INT);
        GLint i = 42;
        const int size = sizeof(GLint);
        char buffer[size];
        serialize_1(type, (char*)&i, buffer, size);
        GLint deserialized;
        deserialize_1(type, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"d %d not equal to i %d", deserialized, i);
    }
    
    //GL_PRIMITIVE_UINT
    {
        TypeDescription type          = mk_primitive_type_description(GL_PRIMITIVE_UINT);
        GLuint i = 42;
        const int size = sizeof(GLuint);
        char buffer[size];
        serialize_1(type, (char*)&i, buffer, size);
        GLuint deserialized;
        deserialize_1(type, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"d %d not equal to i %d", deserialized, i);
    }
    
    //GL_PRIMITIVE_ENUM,
    {
        TypeDescription type          = mk_primitive_type_description(GL_PRIMITIVE_ENUM);
        GLenum i = 42;
        const int size = sizeof(GLenum);
        char buffer[size];
        serialize_1(type, (char*)&i, buffer, size);
        GLenum deserialized;
        deserialize_1(type, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"GL_PRIMITIVE_ENUM d %d not equal to i %d", deserialized, i);
    }
    
    //GL_PRIMITIVE_FLOAT, 
    {
        TypeDescription type          = mk_primitive_type_description(GL_PRIMITIVE_FLOAT);
        GLfloat i = 42;
        const int size = sizeof(GLfloat);
        char buffer[size];
        serialize_1(type, (char*)&i, buffer, size);
        GLfloat deserialized;
        deserialize_1(type, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"GL_PRIMITIVE_FLOAT d %d not equal to i %d", deserialized, i);
    }
    
    //GL_PRIMITIVE_CLAMPF,
    {
        TypeDescription type          = mk_primitive_type_description(GL_PRIMITIVE_CLAMPF);
        GLclampf i = 42;
        const int size = sizeof(GLclampf);
        char buffer[size];
        serialize_1(type, (char*)&i, buffer, size);
        GLclampf deserialized;
        deserialize_1(type, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"GL_PRIMITIVE_CLAMPF d %d not equal to i %d", deserialized, i);
    }
    
    //GL_PRIMITIVE_POINTER,
    /*{
        TypeDescription type          = mk_primitive_type_description(GL_PRIMITIVE_POINTER);
        int p = 43;
        void* i = &p;
        const int size = sizeof(void*);
        char buffer[size];
        serialize_1(type, (char*)&i, buffer, size);
        void* deserialized;
        deserialize_1(type, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"GL_PRIMITIVE_POINTER d %d not equal to i %d", deserialized, i);
    }*/
    
    //GL_UNION_INDEX
    {
        TypeDescription type          = mk_primitive_type_description(GL_UNION_INDEX);
        TestEnum i = TEST_0;
        const int size = sizeof(TestEnum);
        char buffer[size];
        serialize_1(type, (char*)&i, buffer, size);
        TestEnum deserialized;
        deserialize_1(type, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized == i, @"GL_UNION_INDEX d %d not equal to i %d", deserialized, i);
    }
}

typedef union UnionTest_t {
    float x;
    char y;
} UnionTest;

typedef struct StructTest_t {
    float x;
    char y;
} StructTest;

int array_test[3] = {0,1,3};

- (void)testUnionSerialization {
    TypeDescription fd;
    Member float_member = mk_primitive_member("x", &fd, GL_PRIMITIVE_FLOAT);
    
    TypeDescription id;
    Member int_member   = mk_primitive_member("y", &id,GL_PRIMITIVE_INT);
    
    Member members[] = {float_member, int_member};
    TypeDescription type = mk_union_type_description(members, 2, sizeof(UnionTest));
    
    {
        UnionTest test = {1.0f};
        
        const int size = sizeof(UnionTest);
        char buffer[size];
        
        serialize_1_union(type.union_description, 0, (char*)&test, buffer, size);
        UnionTest deserialized;
        
        deserialize_1_union(type.union_description, 0, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized.x == test.x, @"testUnionSerialization d %f not equal to i %f", deserialized.x, test.x);
    }

    {
        UnionTest test; 
        test.y = 'i';
        
        const int size = sizeof(UnionTest);
        char buffer[size];
        
        serialize_1_union(type.union_description, 1, (char*)&test, buffer, size);
        UnionTest deserialized;
        
        deserialize_1_union(type.union_description, 1, buffer, size, (char*)&deserialized, size);
        STAssertTrue(deserialized.y == test.y, @"testUnionSerialization d %c not equal to i %c", deserialized.y, test.y);
    }

    
}

- (void)testArraySerialization {
    TypeDescription int_type   = mk_primitive_type_description(GL_PRIMITIVE_INT);
    TypeDescription array_type = mk_array_type_description(&int_type, 5);
    
    int test[5] = {1,2,3,4,5};
    
    const int size = sizeof(test);
    char buffer[size];
    
    serialize_1(array_type, (char*)&test, buffer, size);
    int deserialized[5];
    
    deserialize_1(array_type, buffer, size, (char*)&deserialized, size);
    for (int i = 0; i < 5; i++) {
        STAssertTrue(deserialized[i]== test[i], @"testArraySerialization d %d not equal to i %d", deserialized[i], test[i]);
    }
}

- (void)testStructSerialization {
    TypeDescription fd;
    Member float_member = mk_primitive_member("x", &fd, GL_PRIMITIVE_FLOAT);
    
    TypeDescription id;
    Member int_member   = mk_primitive_member("y", &id,GL_PRIMITIVE_INT);
    
    Member members[] = {float_member, int_member};
    TypeDescription type = mk_struct_type_description(members, 2, sizeof(StructTest));
    
    StructTest test = {11.0f, 'h'};
    
    const int size = sizeof(StructTest);
    char buffer[size];
    
    serialize_1(type, (char*)&test, buffer, size);
    StructTest deserialized;
    
    deserialize_1(type, buffer, size, (char*)&deserialized, size);
    STAssertTrue(deserialized.x == test.x, @"testUnionSerialization d %f not equal to i %f", deserialized, test);
    STAssertTrue(deserialized.y == test.y, @"testUnionSerialization d %c not equal to i %c", deserialized, test);
}

- (void)testTypedSerialization {
    
}

- (void)testStructWithUnion {
    
}

- (void)testEnvironment {
    
}

- (void)testFixupEnvironment {
    
}

//after this code 
//write the haskell code for making the file
//there is a platform specific format for reading this stuff
//that  will allow for super fast fixups

- (void)testSerialization
{
    /*
    TypeDescription map_resource_type = mk_primitive_type_description(GL_PRIMITIVE_BOOLEAN);
    TypeDescription UIntType          = mk_primitive_type_description(GL_PRIMITIVE_UINT);
    TypeDescription names_type        = mk_array_type_description(&UIntType, MAX_RESOURCE_COUNT); 
    TypeDescription count_type        = mk_primitive_type_description(GL_PRIMITIVE_INT);
    
    Member resource_mapper_members[] = {
        {"map_resource", &map_resource_type},
        {"names",        &names_type},
        {"count",        &count_type}
    };
    int resource_mapper_sizes = sizeof(GLboolean) + (sizeof(const char*) * MAX_RESOURCE_COUNT) + sizeof(int);
    TypeDescription resource_mapper = mk_struct_type_description(resource_mapper_members, 3, 
                                                                 resource_mapper_sizes);
    
    ResourceMapper resource_mapper_test;
    resource_mapper_test.map_resource = GL_TRUE;
    resource_mapper_test.names[0] = 0;
    resource_mapper_test.names[1] = 1;
    resource_mapper_test.names[2] = 2;
    resource_mapper_test.count    = 3;
    
    char resource_mapper_test_output[sizeof(ResourceMapper)];
    
    serialize_1(resource_mapper, (char*)&resource_mapper_test, resource_mapper_test_output, sizeof(ResourceMapper));
    
    ResourceMapper deserialized;
    
    deserialize_1(resource_mapper, resource_mapper_test_output, sizeof(ResourceMapper), 
                  (char*)&deserialized, sizeof(ResourceMapper));
    
    STAssertTrue(deserialized.map_resource == resource_mapper_test.map_resource, @"map resource not equal");
    
    for(int i = 0; i < MAX_RESOURCE_COUNT; i++) {
            STAssertTrue(deserialized.names[i] == resource_mapper_test.names[i], @"name is not equal");
    }
    
    STAssertTrue(deserialized.count == resource_mapper_test.count, @"map resource not equal");
    */
    
    /*
    TypeDescription type_desc = {TYPE_DESCRIPTION_PRIMITIVE, GL_PRIMITIVE_ENUM};
    
    Member cmd_types[] = {
        
    }
    
    UnionDescription cmd_union_desc = {cmd_types, sizeof(cmd_types)/(Member)}
    
    TypeDescription cmd_union_type_desc = {TYPE_DESCRIPTION_UNION, cmd_union_desc};
    
    Member command_members[] = {
        type_desc,
        cmd_union_type_desc
    }
    
    StructDescription command_struct_desc = {command_members, 2};
    
    TypeDescription command_desc = {TYPE_DESCRIPTION_STRUCT, command_struct_desc};
    
    TypeDescription descriptions[] = {
        commandDescription
    }
    */
}

@end
