#extension GL_ARB_texture_rectangle : enable

uniform   float     Min;
uniform   float     Range;
uniform   float     Cylindric;
uniform   int       Nb;
uniform   int       Bi;
uniform   int       Bellow;
uniform   int       Above;
uniform   int       IsMask;
uniform   int       Sun;
uniform   sampler1D Colormap;
uniform   sampler2DRect Interval;
uniform   sampler2DRect Data;
uniform   sampler2DRect Mask;

varying   vec3          LightDir;
varying   vec3          Normal;

vec4 CylinCheck(vec4 Vr) {

   vec4 vr=Vr;

   /*In case of cylindric projection, we have to flip side on edges*/
   if (Cylindric>-999.0) {
      if ((vr.x-Cylindric)>2.0) {
         vr.x-=4.0;
      } else if ((vr.x-Cylindric)<-2.0) {
         vr.x+=4.0;
      }
   }
   return(vr);
}

void main() {

   gl_ClipVertex  = gl_ModelViewMatrix*CylinCheck(gl_Vertex);
   gl_Position    = gl_ModelViewProjectionMatrix*CylinCheck(gl_Vertex);
   gl_TexCoord[0] = gl_MultiTexCoord0;
   gl_FrontColor  = gl_Color;

LightDir      = normalize(vec3(gl_LightSource[0].position));
//  Normal        = gl_Position.xyz;

Normal = normalize(gl_NormalMatrix * gl_Normal);
}
