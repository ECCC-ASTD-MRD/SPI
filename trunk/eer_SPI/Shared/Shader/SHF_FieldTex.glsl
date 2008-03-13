#extension GL_ARB_texture_rectangle : enable

uniform   float     Min;
uniform   float     Range;
uniform   float     Cylindric;
uniform   int       Nb;
uniform   int       Bi;
uniform   sampler1D Colormap;
uniform   sampler2DRect Interval;
uniform   sampler2DRect Data;

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
   return(lerp(lerp(tex11,tex21,t.x),lerp(tex12,tex22,t.x),t.y));
}

void main() {

   float idx;
   int   n;
   vec4  inter,frg;
   float dd;

   // Get value, nearest or linear
   if (Bi!=0) {
      inter=texture2DRectBi(Data,gl_TexCoord[0].st);
   } else {
      inter=texture2DRect(Data,gl_TexCoord[0].st);
   }

   dd=inter.r;

   // If we have intervals, figure out which we fit in
   if (Nb>0) {
      idx=-1.0;

      for(n=0;n<Nb;n++) {
         inter=texture2DRect(Interval,vec2(n,0.0));
         if (dd<inter.r) {
            n=Nb;
         } else {
            idx=(256.0/float(Nb+1)*float(n+1))/255.0;
         }
      }
   } else {
      if (dd<Min)
         discard;
      idx=(dd-Min)/Range;
   }

   // Check for colormap limits
   if (idx<-0.0001 || idx>1.0) {
      discard;
   } else {
//      frg=vec4(texture1D(Colormap,clamp(idx,0.01,0.99)));
      frg=vec4(texture1D(Colormap,idx));
      frg.a*=gl_Color.a;
      gl_FragColor=frg;
   }
}
