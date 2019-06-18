uniform sampler3D TextureData3D;
uniform float Elev;
uniform sampler1D Colormap;
uniform float MinDataDisplay;
uniform float MaxDataDisplay;
uniform vec3 CameraDir;
uniform int InterNb;
uniform float Inter;
uniform float MaxData;
varying vec3 rayDirection, rayOrigin;

vec4 rayMarching() {
    bool testInter = false;
    float idxInter = 0.0;
    float highest = 0.0;
    int maxLoop = 1000; //The amount of loop done on each fragment
    float data = 0.0;
    vec4 color = vec4(0.0);
    float stepLength = 2.0/float(maxLoop);
    vec3 texCoord = rayOrigin;
    int count=0;
    float idx = 0.0;
    for(int i=0; i<maxLoop; i++){
        texCoord += stepLength*rayDirection;
        if(any(lessThan(vec3(1.0),texCoord)))continue;
        data = texture3D(TextureData3D,texCoord).r;
        if(data*MaxData<MinDataDisplay||data*MaxData>MaxDataDisplay)continue;
        
        count+=1;
        idx =(testInter)?idxInter:(data*MaxData-MinDataDisplay)/(MaxDataDisplay-MinDataDisplay);//data*MaxData is in [MinDataDisplay, MaxDataDisplay] and idx in [0,1]
        if(data*MaxData>highest&&InterNb>1&&data*MaxData>=Inter){
            highest=data*MaxData;
            testInter = true;
            idxInter = idx;
        }
        color.rgba += texture1D(Colormap,idx).rgba;
    }
    color/=float(count);
    return color;

}

void main() {
    gl_FragColor=rayMarching();
}
