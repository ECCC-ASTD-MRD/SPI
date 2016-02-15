#extension GL_ARB_texture_rectangle : enable

uniform   float         Cylindric;
uniform   sampler2DRect Data;
varying   vec3          LightDir;
varying   vec3          Normal;

vec3 texture2DRectNr(sampler2DRect Tex,vec2 ST) {

   vec4 st;

   st.xy = floor(ST-0.5)+0.5;
   st.zw = st.xy+1.0;

   vec4 tex00 = texture2DRect(Tex,ST);
   vec4 tex11 = texture2DRect(Tex,st.xy);
   vec4 tex21 = texture2DRect(Tex,st.zy);
   vec4 tex12 = texture2DRect(Tex,st.xw);
   vec4 tex22 = texture2DRect(Tex,st.zw);

   // Gradient normal
   return(vec3((tex11-tex21).x,(tex12-tex22).y,tex00.z));
}

void main() {

   vec3  cf;
   vec4  texel;
   float intensity;
   vec3  normal;

   normal = normalize(gl_NormalMatrix * (texture2DRectNr(Data,gl_TexCoord[0].st)));
   normal = normalize(Normal);
   intensity = max(dot(normal,LightDir),0.0);

   cf=intensity*(gl_FrontMaterial.diffuse).rgb+gl_FrontMaterial.ambient.rgb;

   texel = vec4(0.5,0.5,0.5,1.0);

   gl_FragColor = vec4(texel.rgb*cf,texel.a*gl_FrontMaterial.diffuse.a);
}
