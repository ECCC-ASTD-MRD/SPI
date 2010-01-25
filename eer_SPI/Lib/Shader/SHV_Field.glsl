#extension GL_ARB_texture_rectangle : enable

uniform   float     Min;
uniform   float     Range;
uniform   float     Cylindric;
uniform   int       Nb;
uniform   int       Bi;
uniform   sampler1D Colormap;
//uniform   sampler2D Sprite;
uniform   sampler2DRect Interval;

attribute float     Vd;
varying   float     Fd;

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

   Fd=Vd;
   gl_ClipVertex  = gl_ModelViewMatrix*CylinCheck(gl_Vertex);
   gl_Position    = gl_ModelViewProjectionMatrix*CylinCheck(gl_Vertex);
   gl_FrontColor  = gl_Color;
//   gl_PointSize   = 10.0;
}
