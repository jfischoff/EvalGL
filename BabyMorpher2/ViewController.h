//
//  ViewController.h
//  BabyMorpher2
//
//  Created by hi5 networks on 1/28/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

#import <UIKit/UIKit.h>
#import <GLKit/GLKit.h>
#import "Commands.h"



@interface ViewController : GLKViewController

- (Command*)compile_shader:(Command*)p_shader_commands;
- (Command*)compile_shader:(Command*)p_commands shader:(const char*)shader type:(GLenum)type memory_location:(MemoryLocation)memory_location;
- (Command*)linkProgram:(Command*)p_commands prog:(const char*)prog;
- (BOOL)validateProgram:(GLuint)prog;

@end
