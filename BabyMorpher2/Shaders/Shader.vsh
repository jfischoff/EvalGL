//
//  Shader.vsh
//  BabyMorpher2
//
//  Created by hi5 networks on 1/28/12.
//  Copyright (c) 2012 __MyCompanyName__. All rights reserved.
//

attribute vec4 position;
attribute vec3 normal;

varying lowp vec4 colorVarying;

//uniform mat4 modelViewProjectionMatrix;
//uniform mat3 normalMatrix;

void main()
{
    //vec3 eyeNormal = normalize(normalMatrix * normal);
    //vec3 lightPosition = vec3(0.0, 0.0, 1.0);
    vec4 diffuseColor = vec4(0.4, 0.4, 1.0, 1.0);
    
    //float nDotVP = max(1.0, dot(eyeNormal, normalize(lightPosition)));
                 
    //colorVarying = diffuseColor * nDotVP;
    
    //gl_Position = (1.0 - (modelViewProjectionMatrix * 0.001)) * position;
    
    colorVarying = diffuseColor;
    gl_Position = position;
}
