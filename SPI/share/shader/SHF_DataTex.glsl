#extension GL_ARB_texture_rectangle : require

uniform   float     Cylindric;
uniform   int       Map;
uniform   vec4      Sc;
uniform   vec4      Bc;
uniform   sampler1D Colormap;
uniform   sampler2D Data;

varying vec4 GAm;
varying vec3 Nr,LDir;

void main() {

   vec4  inter,frg=vec4(1.0,1.0,1.0,1.0);
   vec4 ambient,diffuse;
   vec3 halfV;
   vec4 color = GAm;
   float NdotL,NdotHV;
   float att,dist;

   /*Get value, nearest or linear*/
   inter=clamp((texture2D(Data,gl_TexCoord[0].st)+Bc)*abs(Sc),0.00,1.0);

   frg=inter;
   if (Sc.r>=0.0) {
      frg=texture1D(Colormap,inter.r);
      if (Map==0) {
         frg=frg.rrrr;
         frg.a=1.0;
      }
   }
   if (Sc.g>=0.0) {
      frg.g=texture1D(Colormap,inter.g).g;
   }
   if (Sc.b>=0.0) {
      frg.b=texture1D(Colormap,inter.b).b;
   }
   if (Sc.a>=0.0) {
      frg.a=texture1D(Colormap,inter.a).a;
   }
   frg.a*=gl_Color.a;

   /*Set illumination*/
   if (length(Nr)==0.0) {

   } else {
      color*=frg;
      /* compute the dot product between normal and normalized lightdir */
      NdotL=max(0.0,dot(Nr,vec3(gl_LightSource[0].position)));
      halfV=normalize(gl_LightSource[0].halfVector.xyz);

      /* The ambient terms have been separated since one of them */
      /* suffers attenuation */
      ambient=frg*gl_LightSource[0].ambient;
      diffuse=frg*gl_LightSource[0].diffuse;

      if (length(gl_LightSource[0].position)<1.0 || NdotL>0.0) {
         if (length(gl_LightSource[0].position)<1.0)
            NdotL*=0.5;
         dist = length(LDir);
         att=1.0/(gl_LightSource[0].constantAttenuation+gl_LightSource[0].linearAttenuation*dist+gl_LightSource[0].quadraticAttenuation*dist*dist);
         color+=att*(diffuse*NdotL+ambient);
         NdotHV= max(dot(Nr,halfV),0.0);
         color +=att*gl_FrontMaterial.specular*gl_LightSource[0].specular*pow(NdotHV,gl_FrontMaterial.shininess);
      }
      frg=color;
   }

   gl_FragColor=frg;
}
