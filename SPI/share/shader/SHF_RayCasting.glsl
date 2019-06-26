uniform sampler3D TextureData3D;
uniform float Elev;
uniform sampler1D Colormap;
uniform float MinDataDisplay;
uniform float MaxDataDisplay;
uniform vec3 CameraDir;
uniform float MaxData;
varying vec3 rayDirection, rayOrigin;

vec4 rayMarching() {
    int maxLoop = 1000; //The amount of loop done on each fragment
    float data = 0.0;
    vec4 color = vec4(0.0);
    float stepLength = 2.0/float(maxLoop);
    vec3 texCoord = rayOrigin;
    float idx = 0.0;
    vec4 addedColor = vec4(0.0);
    for(int i=0; i<maxLoop; i++){
        texCoord += stepLength*rayDirection;
        if(any(lessThan(vec3(1.0),texCoord)))continue;
        data = texture3D(TextureData3D,texCoord).r;
        if(data*MaxData<MinDataDisplay||data*MaxData>MaxDataDisplay)continue;

        idx = (data*MaxData-MinDataDisplay)/(MaxDataDisplay-MinDataDisplay);//data*MaxData is in [MinDataDisplay, MaxDataDisplay] and idx in [0,1]
        addedColor = texture1D(Colormap,idx).rgba;
        color.rgb += (addedColor.a+color.a<=1.0)? (addedColor.rgb*addedColor.a):(addedColor.rgb*(1.0 - color.a));
        color.a = (addedColor.a+color.a<=1.0)? (color.a+addedColor.a):1.0;

        if(color.a>=1.0)break;
    }
    return color;
}

void main() {
    gl_FragColor=rayMarching();
}
