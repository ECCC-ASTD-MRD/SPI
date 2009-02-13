#extension GL_ARB_texture_rectangle : enable

uniform   float         Cylindric;
uniform   sampler2DRect Data;
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

   LightDir      = normalize(vec3(gl_LightSource[0].position));
   gl_ClipVertex = gl_ModelViewMatrix*CylinCheck(gl_Vertex);
   gl_Position   = gl_ModelViewProjectionMatrix*CylinCheck(gl_Vertex);
   Normal        = gl_Position;
//   gl_TexCoord[0]= gl_MultiTexCoord0;
}
