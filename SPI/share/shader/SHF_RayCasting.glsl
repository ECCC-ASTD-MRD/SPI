uniform sampler3D TextureData3D;
uniform float Elev;
uniform sampler1D Colormap;
uniform float MinDataDisplay;
uniform float MaxDataDisplay;
uniform vec3 CameraDir;
varying vec3 rayDirection, rayOrigin;

vec4 rayMarching() {

    int maxLoop = 1000; //The amount of loop done on each fragment
    float data = 0.0;
    vec4 color = vec4(0.0);
    float stepLength = 2.0/float(maxLoop);
    vec3 texCoord = rayOrigin;
    int count=0;
    float idx = 0.0;
    for(int i=0; i<maxLoop; i++){
        texCoord += stepLength*rayDirection;
        if(all(lessThan(vec3(1.0),texCoord)))continue;
        data = texture3D(TextureData3D,texCoord).r;
        if(data*MaxDataDisplay<MinDataDisplay||data*MaxDataDisplay>MaxDataDisplay)continue;
        
        if(data!=0.0)count+=1;
        idx = (data*MaxDataDisplay-MinDataDisplay)/(MaxDataDisplay-MinDataDisplay);
        color.rgba += texture1D(Colormap,idx).rgba;
    }
    color/=float(count);
    return color;

}

void main() {
    gl_FragColor=(rayMarching());
}
