#extension GL_ARB_texture_rectangle : enable

uniform   float     Min;
uniform   float     Range;
uniform   float     Cylindric;
uniform   int       Nb;
uniform   int       Bellow;
uniform   int       Above;
uniform   sampler1D Colormap;
//uniform   sampler2D Sprite;
uniform   sampler2DRect Interval;

//attribute float     Vd;
varying   float     Fd;

void main() {

   int   n;
   float idx;
   vec4  inter,frg;
   float dd;

   dd=Fd;
   idx=-1.0;
   inter.x=0.0;
   
   if (Nb>0) {
      for(n=0;n<Nb;n++) {
         inter=texture2DRect(Interval,vec2(n,0.0));
         if (dd<inter.r) {
            n=Nb;
         } else {
            idx=(256.0/float(Nb+1)*float(n+1))/255.0;
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
//      frg=texture2D(Sprite,gl_TexCoord[2].st).r*texture1D(Colormap,clamp(idx,0.01,0.99));
      frg.a*=gl_Color.a;
      gl_FragColor=frg;
   }
}
