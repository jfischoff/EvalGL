//
//  ViewController.m
//  BabyMorpher2
//
//  Created by hi5 networks on 1/28/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import "ViewController.h"

#include "Evaluator.h"

static Environment* _environment;
static Command* _draw_commands;
static Command _draw_command_list;
static int _draw_commands_count;
static GLKMatrix4 _modelViewProjectionMatrix;
static GLKMatrix3 _normalMatrix;
static char* _vertex_shader_sources[1];
static char* _fragment_shader_sources[1];
static Command* _model_view_get_uniform_command;
static Command* _normal_get_uniform_command;


float _rotation;

// Uniform index.
enum
{
    UNIFORM_MODELVIEWPROJECTION_MATRIX,
    UNIFORM_NORMAL_MATRIX,
    NUM_UNIFORMS
};
GLint uniforms[NUM_UNIFORMS];

// Attribute index.
enum
{
    ATTRIB_VERTEX,
    ATTRIB_NORMAL,
    NUM_ATTRIBUTES
};

GLfloat gCubeVertexData[216] = 
{
    // Data layout for each line below is:
    // positionX, positionY, positionZ,     normalX, normalY, normalZ,
    0.5f, -0.5f, -0.5f,        1.0f, 0.0f, 0.0f,
    0.5f, 0.5f, -0.5f,         1.0f, 0.0f, 0.0f,
    0.5f, -0.5f, 0.5f,         1.0f, 0.0f, 0.0f,
    0.5f, -0.5f, 0.5f,         1.0f, 0.0f, 0.0f,
    0.5f, 0.5f, 0.5f,          1.0f, 0.0f, 0.0f,
    0.5f, 0.5f, -0.5f,         1.0f, 0.0f, 0.0f,
    
    0.5f, 0.5f, -0.5f,         0.0f, 1.0f, 0.0f,
    -0.5f, 0.5f, -0.5f,        0.0f, 1.0f, 0.0f,
    0.5f, 0.5f, 0.5f,          0.0f, 1.0f, 0.0f,
    0.5f, 0.5f, 0.5f,          0.0f, 1.0f, 0.0f,
    -0.5f, 0.5f, -0.5f,        0.0f, 1.0f, 0.0f,
    -0.5f, 0.5f, 0.5f,         0.0f, 1.0f, 0.0f,
    
    -0.5f, 0.5f, -0.5f,        -1.0f, 0.0f, 0.0f,
    -0.5f, -0.5f, -0.5f,       -1.0f, 0.0f, 0.0f,
    -0.5f, 0.5f, 0.5f,         -1.0f, 0.0f, 0.0f,
    -0.5f, 0.5f, 0.5f,         -1.0f, 0.0f, 0.0f,
    -0.5f, -0.5f, -0.5f,       -1.0f, 0.0f, 0.0f,
    -0.5f, -0.5f, 0.5f,        -1.0f, 0.0f, 0.0f,
    
    -0.5f, -0.5f, -0.5f,       0.0f, -1.0f, 0.0f,
    0.5f, -0.5f, -0.5f,        0.0f, -1.0f, 0.0f,
    -0.5f, -0.5f, 0.5f,        0.0f, -1.0f, 0.0f,
    -0.5f, -0.5f, 0.5f,        0.0f, -1.0f, 0.0f,
    0.5f, -0.5f, -0.5f,        0.0f, -1.0f, 0.0f,
    0.5f, -0.5f, 0.5f,         0.0f, -1.0f, 0.0f,
    
    0.5f, 0.5f, 0.5f,          0.0f, 0.0f, 1.0f,
    -0.5f, 0.5f, 0.5f,         0.0f, 0.0f, 1.0f,
    0.5f, -0.5f, 0.5f,         0.0f, 0.0f, 1.0f,
    0.5f, -0.5f, 0.5f,         0.0f, 0.0f, 1.0f,
    -0.5f, 0.5f, 0.5f,         0.0f, 0.0f, 1.0f,
    -0.5f, -0.5f, 0.5f,        0.0f, 0.0f, 1.0f,
    
    0.5f, -0.5f, -0.5f,        0.0f, 0.0f, -1.0f,
    -0.5f, -0.5f, -0.5f,       0.0f, 0.0f, -1.0f,
    0.5f, 0.5f, -0.5f,         0.0f, 0.0f, -1.0f,
    0.5f, 0.5f, -0.5f,         0.0f, 0.0f, -1.0f,
    -0.5f, -0.5f, -0.5f,       0.0f, 0.0f, -1.0f,
    -0.5f, 0.5f, -0.5f,        0.0f, 0.0f, -1.0f
};

//static Evaluator _evaluator;
//static Environment _env;

@interface ViewController () {

}
@property (strong, nonatomic) EAGLContext *context;


- (void)setupGL;
- (void)tearDownGL;


@end

@implementation ViewController

@synthesize context = _context;

- (void)viewDidLoad
{
    printf("viewDidLoad\n");
    
    [super viewDidLoad];
    
    self.context = [[EAGLContext alloc] initWithAPI:kEAGLRenderingAPIOpenGLES2];

    if (!self.context) {
        NSLog(@"Failed to create ES context");
    }
    
    GLKView *view = (GLKView *)self.view;
    view.context = self.context;
    view.drawableDepthFormat = GLKViewDrawableDepthFormat24;
    
    printf("before setupGL\n");
    [self setupGL];
}

- (void)viewDidUnload
{    
    [super viewDidUnload];
    
    [self tearDownGL];
    
    if ([EAGLContext currentContext] == self.context) {
        [EAGLContext setCurrentContext:nil];
    }
	self.context = nil;
}

- (void)didReceiveMemoryWarning
{
    [super didReceiveMemoryWarning];
    // Release any cached data, images, etc. that aren't in use.
}

- (BOOL)shouldAutorotateToInterfaceOrientation:(UIInterfaceOrientation)interfaceOrientation
{
    // Return YES for supported orientations
    if ([[UIDevice currentDevice] userInterfaceIdiom] == UIUserInterfaceIdiomPhone) {
        return (interfaceOrientation != UIInterfaceOrientationPortraitUpsideDown);
    } else {
        return YES;
    }
}

- (void)setupGL
{
    printf("setupGL\n");

    [EAGLContext setCurrentContext:self.context];

    _environment = alloc_environment(6, 4);
    _environment->logging = GL_TRUE;
    //_environment->logging = GL_FALSE;
    
    //allocate all the buffers for the environment;
    const int setup_cmd_count = 40;
    Command setup_commands[setup_cmd_count];
    Command* p_setup_commands = setup_commands;   
    
    mk_add_data(p_setup_commands, 0, 216 * sizeof(GLfloat), (char*)gCubeVertexData);
    p_setup_commands++;

    _modelViewProjectionMatrix = GLKMatrix4MakePerspective(GLKMathDegreesToRadians(65.0f), 1.0, 0.1f, 100.0f);
    mk_add_data(p_setup_commands, "projection_matrix", sizeof(GLKMatrix4), (char*)_modelViewProjectionMatrix.m);

    p_setup_commands++;
 
    mk_add_data(p_setup_commands, "normal_matrix",     sizeof(GLKMatrix3), (char*)_normalMatrix.m);
    p_setup_commands++;
    
    p_setup_commands = [self compile_shader:p_setup_commands];
    
    mk_enable(p_setup_commands, E_GL_DEPTH_TEST);
    p_setup_commands++;
    
    const char* name = "vertex_array";
    
    ResourceMapper vertex_array;
    mk_resource_mapper1(&vertex_array, name);
    
    mk_gen_vertex_arrays_oes(p_setup_commands, 1, vertex_array);
    p_setup_commands++;
    
    ResourceId vertex_array_id;
    vertex_array_id.type = RESOURCE_NAME;
    vertex_array_id.name = name;

    mk_bind_vertex_arrays_oes(p_setup_commands, vertex_array_id);    
    p_setup_commands++;
    
    const char* vertex_buffer_name = "vertex_buffer";
    const char** names = &vertex_buffer_name;
    mk_gen_buffers(p_setup_commands, 1, names);
    p_setup_commands++;
    
    
    ResourceId r;
    r.type = RESOURCE_NAME;
    r.name = "vertex_buffer";
    mk_bind_buffer(p_setup_commands, ARRAY_BUFFER, r);
    
    MemoryLocation vertex_data_location;
    vertex_data_location.id = 0;
    vertex_data_location.offset = 0;

    mk_bind_buffer(p_setup_commands, ARRAY_BUFFER, r);
    p_setup_commands++;
    
    mk_buffer_data(p_setup_commands, ARRAY_BUFFER, sizeof(gCubeVertexData), vertex_data_location, STATIC_DRAW);
    p_setup_commands++;

    mk_enable_vertex_attrib_array(p_setup_commands, GLKVertexAttribPosition);
    p_setup_commands++;

    mk_vertex_attrib_pointer(p_setup_commands, GLKVertexAttribPosition, 3, VAP_FLOAT, 0, 24, 
                             vertex_data_location);
    p_setup_commands++;
    
    mk_enable_vertex_attrib_array(p_setup_commands, GLKVertexAttribNormal);
    p_setup_commands++;

    MemoryLocation normals_location;
    normals_location.id     = 0;
    normals_location.offset = 12;
    
    mk_vertex_attrib_pointer(p_setup_commands, GLKVertexAttribNormal, 3, VAP_FLOAT, GL_FALSE, 24, 
                             normals_location);
    p_setup_commands++;
         
    ResourceId empty_resource_id;
    empty_resource_id.id = 0;
    empty_resource_id.type = RESOURCE_ID;
    
    mk_bind_vertex_arrays_oes(p_setup_commands, empty_resource_id);
    p_setup_commands++;
        
    Command setup_command_list;
    int actually_count = ((long long)p_setup_commands - (long long)setup_commands) / sizeof(Command);
    mk_command_list(&setup_command_list, setup_commands, actually_count);
        
    //I think I need to evaluate this thing and get out the results
    evaluate(_environment, &setup_command_list);
    
    //uniforms[UNIFORM_MODELVIEWPROJECTION_MATRIX] = _model_view_get_uniform_command->get_uniform_location.result.right.index;
    //uniforms[UNIFORM_NORMAL_MATRIX]              = _normal_get_uniform_command->get_uniform_location.result.right.index;
    
    //DRAW COMMANDS
    _draw_commands_count = 10;
    
    _draw_commands = malloc(sizeof(Command) * _draw_commands_count); 
    Command* p_draw_commands = _draw_commands; 
    
    mk_clear_color(p_draw_commands, 1.0f, 0.0f, 0.5f, 1.0f);
    p_draw_commands++;
    
    mk_clear(p_draw_commands, COLOR_BUFFER_BIT | DEPTH_BUFFER_BIT);
    p_draw_commands++;
    
    ResourceId program_id = mk_resource_id_s("program");
    
    mk_bind_vertex_arrays_oes(p_draw_commands, vertex_array_id);
    p_draw_commands++;
    
    mk_use_program(p_draw_commands, program_id);
    p_draw_commands++;
            
    //MemoryLocation project_matrix = mk_memory_location("projection_matrix", 0);
    //mk_uniform_matrix(p_draw_commands, MATRIX_UNIFORM_4X4, uniforms[UNIFORM_MODELVIEWPROJECTION_MATRIX], 1, 
    //                  0, project_matrix);
    //p_draw_commands++;
    
    //MemoryLocation normal_matrix = mk_memory_location("normal_matrix", 0);
    //mk_uniform_matrix(p_draw_commands, MATRIX_UNIFORM_3X3, 
     //                 uniforms[UNIFORM_NORMAL_MATRIX], 1, 
     //                 0, normal_matrix);
    //p_draw_commands++;

    mk_draw_arrays(p_draw_commands, TRIANGLES, 0, 36);
    p_draw_commands++;

    int draw_commands = ((long long)p_draw_commands - (long long)_draw_commands) / sizeof(Command);
    mk_command_list(&_draw_command_list, _draw_commands, draw_commands);

}

- (void)tearDownGL
{
    [EAGLContext setCurrentContext:self.context];
    

    
}

#pragma mark - GLKView and GLKViewController delegate methods

- (void)update
{
    float aspect = fabsf(self.view.bounds.size.width / self.view.bounds.size.height);
    GLKMatrix4 projectionMatrix = GLKMatrix4MakePerspective(GLKMathDegreesToRadians(65.0f), aspect, 0.1f, 100.0f);
    
    //self.effect.transform.projectionMatrix = projectionMatrix;
    
    GLKMatrix4 baseModelViewMatrix = GLKMatrix4MakeTranslation(0.0f, 0.0f, -4.0f);
    baseModelViewMatrix = GLKMatrix4Rotate(baseModelViewMatrix, _rotation, 0.0f, 1.0f, 0.0f);
    
    // Compute the model view matrix for the object rendered with GLKit
    GLKMatrix4 modelViewMatrix = GLKMatrix4MakeTranslation(0.0f, 0.0f, -1.5f);
    modelViewMatrix = GLKMatrix4Rotate(modelViewMatrix, _rotation, 1.0f, 1.0f, 1.0f);
    modelViewMatrix = GLKMatrix4Multiply(baseModelViewMatrix, modelViewMatrix);
    
    //self.effect.transform.modelviewMatrix = modelViewMatrix;
    
    // Compute the model view matrix for the object rendered with ES2
    modelViewMatrix = GLKMatrix4MakeTranslation(0.0f, 0.0f, 1.5f);
    modelViewMatrix = GLKMatrix4Rotate(modelViewMatrix, _rotation, 1.0f, 1.0f, 1.0f);
    modelViewMatrix = GLKMatrix4Multiply(baseModelViewMatrix, modelViewMatrix);
    
    _normalMatrix = GLKMatrix3InvertAndTranspose(GLKMatrix4GetMatrix3(modelViewMatrix), NULL);
    
    _modelViewProjectionMatrix = GLKMatrix4Multiply(projectionMatrix, modelViewMatrix);
    
    _rotation += self.timeSinceLastUpdate * 0.5f;
}

- (void)glkView:(GLKView *)view drawInRect:(CGRect)rect
{
    evaluate(_environment, &_draw_command_list);
}

-(Command*)compile_shader:(Command*)p_shader_commands {

    mk_create_program(p_shader_commands, "program");
    p_shader_commands++;
    
    // Create and compile vertex shader.
    NSString* vertShaderPathname = [[NSBundle mainBundle] pathForResource:@"Shader" ofType:@"vsh"];
    //load the file
    //add as a resource

    
    MemoryLocation vertex_shader_source;
    vertex_shader_source.id     = "vertex_shader_source";
    vertex_shader_source.offset = 0;
    
    //I need to copy the source into a buffer that does not go away
    GLchar* source = (GLchar *)[[NSString stringWithContentsOfFile:vertShaderPathname encoding:NSUTF8StringEncoding error:nil] UTF8String];
    if (!source) {
        NSLog(@"Failed to load vertex shader");
        return NO;
    }
    
    _vertex_shader_sources[0] = malloc(strlen(source) + 1);
    strcpy(_vertex_shader_sources[0], source);
    
    mk_add_data(p_shader_commands, "vertex_shader_source", 1, (char*)_vertex_shader_sources);
    p_shader_commands++;
    
    p_shader_commands = [self compile_shader:p_shader_commands shader:"vertex_shader" type:GL_VERTEX_SHADER memory_location:vertex_shader_source];
    
    if (!p_shader_commands) {
        NSLog(@"Failed to compile vertex shader");
        return NO;
    }

    // Create and compile fragment shader.
    NSString* fragShaderPathname = [[NSBundle mainBundle] pathForResource:@"Shader" ofType:@"fsh"];
    MemoryLocation fragment_shader_source;
    fragment_shader_source.id = "fragment_shader_source";
    fragment_shader_source.offset = 0;
    
    GLchar* frag_source = (GLchar *)[[NSString stringWithContentsOfFile:fragShaderPathname encoding:NSUTF8StringEncoding error:nil] UTF8String];
    if (!frag_source) {
        NSLog(@"Failed to load vertex shader");
        return NO;
    }
    
    _fragment_shader_sources[0] = malloc(strlen(frag_source) + 1);
    strcpy(_fragment_shader_sources[0], frag_source);
    
    mk_add_data(p_shader_commands, "fragment_shader_source", 1, (char*)_fragment_shader_sources);
    p_shader_commands++;
    
    p_shader_commands = [self compile_shader:p_shader_commands shader:"fragment_shader" type:GL_FRAGMENT_SHADER memory_location:fragment_shader_source];
    if (!p_shader_commands) {
        NSLog(@"Failed to compile fragment shader");
        return NO;
    }

    mk_attack_shader(p_shader_commands, "program", "vertex_shader");
    p_shader_commands++;

    mk_attack_shader(p_shader_commands, "program", "fragment_shader");
    p_shader_commands++;
    
    mk_bind_attrib_location(p_shader_commands, "program", ATTRIB_VERTEX, "position"); 
    p_shader_commands++;
    
    mk_bind_attrib_location(p_shader_commands, "program", ATTRIB_NORMAL, "normal");
    p_shader_commands++;
    
    // Link program.
    p_shader_commands = [self linkProgram:p_shader_commands prog:"program"];
    if (!p_shader_commands) {
        NSLog(@"Failed to link program");
    
    
        //if (vertShader) {
        //    glDeleteShader(vertShader);
        //    vertShader = 0;
        //}
        //if (fragShader) {
        //    glDeleteShader(fragShader);
        //    fragShader = 0;
        //}
        //if (_program) {
        //    glDeleteProgram(_program);
        //    _program = 0;
        //}
        
        return p_shader_commands;
    }

    // Get uniform locations.
    //uniforms[UNIFORM_MODELVIEWPROJECTION_MATRIX] = glGetUniformLocation(_program, "modelViewProjectionMatrix");
    //mk_get_uniform_location(p_shader_commands, "program", "modelViewProjectionMatrix");
    //_model_view_get_uniform_command = p_shader_commands;
    //p_shader_commands++;
    
    //uniforms[UNIFORM_NORMAL_MATRIX] = glGetUniformLocation(_program, "normalMatrix");
    //mk_get_uniform_location(p_shader_commands, "program", "normalMatrix");
    //_normal_get_uniform_command = p_shader_commands;
    //p_shader_commands++;

    // Release vertex and fragment shaders.
    //if (vertShader) {
    //    glDetachShader(_program, vertShader);
    //    glDeleteShader(vertShader);
    //}
    //if (fragShader) {
    //    glDetachShader(_program, fragShader);
    //    glDeleteShader(fragShader);
    //}

    return p_shader_commands;
}

-(Command*)compile_shader:(Command*)p_commands shader:(const char*)shader type:(GLenum)type memory_location:(MemoryLocation)memory_location
{
    //*shader = glCreateShader(type);
    mk_create_shader(p_commands, shader, gl_enum_to_shader_type(type));
    p_commands++;
    
    //glShaderSource(*shader, 1, &source, NULL);
    mk_shader_source(p_commands,shader, 1, memory_location, NULL);
    p_commands++;
    
    //glCompileShader(*shader);
    mk_compile_shader(p_commands, shader);
    p_commands++;
    
#if defined(DEBUG)
    //GLint logLength;
    //glGetShaderiv(*shader, GL_INFO_LOG_LENGTH, &logLength);
    //if (logLength > 0) {
    //    GLchar *log = (GLchar *)malloc(logLength);
    //    glGetShaderInfoLog(*shader, logLength, &logLength, log);
    //    NSLog(@"Shader compile log:\n%s", log);
    //    free(log);
    //}
#endif
    
    //glGetShaderiv(*shader, GL_COMPILE_STATUS, &status);
    //if (status == 0) {
    //    glDeleteShader(*shader);
    //    return NO;
    //}

    return p_commands;
}



- (Command*)linkProgram:(Command*)p_commands prog:(const char*)prog
{
    mk_link_program(p_commands, prog);
    p_commands++;
    
#if defined(DEBUG)
    //GLint logLength;
    //glGetProgramiv(prog, GL_INFO_LOG_LENGTH, &logLength);
    //if (logLength > 0) {
    //    GLchar *log = (GLchar *)malloc(logLength);
    //    glGetProgramInfoLog(prog, logLength, &logLength, log);
    //    NSLog(@"Program link log:\n%s", log);
    //    free(log);
    //}
#endif
    
    //glGetProgramiv(prog, GL_LINK_STATUS, &status);
    //if (status == 0) {
    //    return NO;
    //}
    
    return p_commands;
}

- (BOOL)validateProgram:(GLuint)prog
{

    GLint logLength, status;
    
    glValidateProgram(prog);
    glGetProgramiv(prog, GL_INFO_LOG_LENGTH, &logLength);
    if (logLength > 0) {
        GLchar *log = (GLchar *)malloc(logLength);
        glGetProgramInfoLog(prog, logLength, &logLength, log);
        NSLog(@"Program validate log:\n%s", log);
        free(log);
    }
    
    glGetProgramiv(prog, GL_VALIDATE_STATUS, &status);
    if (status == 0) {
        return NO;
    }

    return YES;
}

#pragma mark -  OpenGL ES 2 shader compilation


@end
