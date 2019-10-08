#extension GL_ARB_texture_rectangle : enable

uniform   float     Min;
uniform   float     Range;
uniform   float     Cylindric;
uniform   int       Nb;
uniform   int       Bi;
uniform   int       Bellow;
uniform   int       Above;
uniform   int       IsMask;
uniform   sampler1D Colormap;
uniform   sampler2DRect Interval;
uniform   sampler2DRect Data;
uniform   sampler2DRect Mask;

//varying   vec3          LightDir;
//varying   vec3          Normal;

vec4 texture2DRectBi(sampler2DRect Tex,vec2 ST) {

   vec4 st;

   st.xy = floor(ST-0.5)+0.5;
   st.zw = st.xy+1.0;

   //interpolating factors
   vec2 t = ST-st.xy;

   vec4 tex11 = texture2DRect(Tex,st.xy);
   vec4 tex21 = texture2DRect(Tex,st.zy);
   vec4 tex12 = texture2DRect(Tex,st.xw);
   vec4 tex22 = texture2DRect(Tex,st.zw);

   // bilinear interpolation
   return(mix(mix(tex11,tex21,t.x),mix(tex12,tex22,t.x),t.y));
}

vec4 texture2DRectMin(sampler2DRect Tex,vec2 ST) {

   vec4 st;

   st.xy = floor(ST-0.5)+0.5;
   st.zw = st.xy+1.0;

   vec4 tex11 = texture2DRect(Tex,st.xy);
   vec4 tex21 = texture2DRect(Tex,st.zy);
   vec4 tex12 = texture2DRect(Tex,st.xw);
   vec4 tex22 = texture2DRect(Tex,st.zw);

   // bilinear interpolation
   return(min(min(tex11,tex21),min(tex12,tex22)));
}
void main() {

   float idx;
   int   n;
   vec4  inter,frg,mask;
   float dd,sz,factor;

   mask.a=1.0;
   
   // Get value, nearest or linear
   if (Bi!=0) {
      inter=texture2DRectBi(Data,gl_TexCoord[0].st);
      if (IsMask==1) {
         mask=texture2DRectMin(Mask,gl_TexCoord[0].st);
       }
   } else {
      inter=texture2DRect(Data,gl_TexCoord[0].st);
      if (IsMask==1) {
         mask=texture2DRect(Mask,gl_TexCoord[0].st);
      }
   }

   // Discard if masked
   if (mask.a<0.01) {
      discard;
   }
   
   dd=inter.r;
   idx=-1.0;

   // If we have intervals, figure out which we fit in
   if (Nb>0) {

//TODO: #version 130     sz=float(textureSize(Colormap,0)); 
      sz=256.0; 
      factor=sz/float(Nb+1);
      
      for(n=0;n<Nb;n++) {
         inter=texture2DRect(Interval,vec2(n,0.0));
         if (dd<inter.r) {
            n=Nb;
         } else {
            idx=floor(factor*float(n+1))/(sz-1.0);
         }
      }
      if (dd>inter.r && Above==0)
         discard;
   } else {
      idx=(dd-Min)/Range;
   }

   // Check for colormap limits
   if ((idx<0.0 && Bellow==0) || (idx>1.0 && Above==0)) {
      discard;
   } else {
      frg=vec4(texture1D(Colormap,idx));

      frg.a*=gl_Color.a;
      gl_FragColor=frg;
   }

//   normal = normalize(Normal);
//   intensity = max(dot(normal,LightDir),0.0);

//   cf=intensity;
//   gl_FragColor = vec4(gl_FragColor.rgb*cf,gl_FragColor.a*gl_FrontMaterial.diffuse.a);
}
