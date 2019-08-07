uniform sampler3D TextureData3D;
uniform float Elev;
uniform float LI;
uniform float LJ;
uniform sampler1D Colormap;
uniform float MinDataDisplay;
uniform vec3 CameraDir;
uniform sampler2DRect Interval;
uniform int Nb;
uniform float Range;
uniform int Bellow;
uniform int Above;
uniform vec2 LimitX;
uniform vec2 LimitY;
uniform vec2 LimitZ;
varying vec3 rayDirection, rayOrigin;

vec4 rayMarching() {
    int maxLoop = 1000; //The amount of loop done on each fragment
    float data = 0.0;
    vec4 color = vec4(0.0);
    float stepLength = 2.0/float(maxLoop);
    vec3 texCoord = rayOrigin;
    float idx = 0.0;
    float sz=256.0;
    float factor=sz/float(Nb+1);
    vec4 addedColor = vec4(0.0);
    vec4 inter = vec4(0.0);
    for(int i=0; i<maxLoop; i++){
        texCoord += stepLength*rayDirection;
        //if the ray is going to take a sample outside of limits, continue
        if(any(lessThan(texCoord,vec3(LimitX.x,LimitY.x,LimitZ.x))))continue;
        if(any(greaterThan(texCoord,vec3(LimitX.y,LimitY.y,LimitZ.y))))continue;
        data = texture3D(TextureData3D,texCoord).r;

        // If we have intervals, figure out which we fit in
        if (Nb>0) {
            idx = -1.0;//reinitialize idx, if data is lower than first inter, the color will not be added
            for(int n=0;n<Nb;n++) {
                inter=texture2DRect(Interval,vec2(n,0.0));
                if (data<inter.r) {
                n=Nb;
                } else {
                idx=floor(factor*float(n+1))/(sz-1.0);
                }
            }
            if (data>inter.r && Above==0)
                continue;
        } else {
            idx=(data-MinDataDisplay)/Range; //if no intervals, find corresponding index [0,1]
        }

        // Check for colormap limits
        if ((idx<0.0 && Bellow==0) || (idx>1.0 && Above==0)) {
            continue;
        }

        addedColor = texture1D(Colormap,idx).rgba; //color to add
        //add color proportionally to alpha, alpha can't go over 1.0
        color.rgb += (addedColor.a+color.a<=1.0)? (addedColor.rgb*addedColor.a):(addedColor.rgb*(1.0 - color.a));
        color.a = (addedColor.a+color.a<=1.0)? (color.a+addedColor.a):1.0;

        if(color.a>=1.0)break; //stop taking samples when alpha is 1.0
    }
    return color;
}

void main() {
    gl_FragColor=rayMarching();
}
