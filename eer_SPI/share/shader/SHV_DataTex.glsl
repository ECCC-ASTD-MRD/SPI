uniform   float     Cylindric;
uniform   int       Map;
uniform   vec4      Sc;
uniform   vec4      Bc;
uniform   sampler1D Colormap;
uniform   sampler2D Data;

varying vec4 GAm;
varying vec3 Nr,LDir;

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

   vec4 color;
   vec4 vr;

   Nr=(gl_NormalMatrix*gl_Normal);
   vr=CylinCheck(gl_Vertex);

   gl_ClipVertex  = gl_ModelViewMatrix*vr;

   /*Compute the light's direction*/
   LDir=normalize(vec3(gl_LightSource[0].position-gl_ClipVertex));

   /* The ambient terms have been separated since one of them suffers attenuation */
   GAm=gl_LightModel.ambient*gl_FrontMaterial.ambient;

   gl_Position    = gl_ModelViewProjectionMatrix*vr;
   gl_TexCoord[0] = gl_MultiTexCoord0;
   gl_FrontColor  = gl_Color;
}
