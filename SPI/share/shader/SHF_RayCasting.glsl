uniform sampler3D TextureData3D;
uniform float MaxData;
uniform sampler1D Colormap;
uniform float MinDataDisplay;
uniform float MaxDataDisplay;
uniform vec3 CameraDir;
varying vec3 rayDirection, rayOrigin;

vec4 rayMarching() {
    //test
//     float dataHighlight = 0.41;
//     float delta = 0.05;
//     float fact = 1.0;
    //test

    int maxLoop = 1000; //The amount of loop done on each fragment
    float data = 0.0;
    vec4 color = vec4(0.0);
    float stepLength = 2.0/float(maxLoop);
    vec3 texCoord = rayOrigin;
    int count=0;
    float idx = 0.0;
    for(int i=0; i<maxLoop; i++){
        texCoord += stepLength*rayDirection;
        if(all(lessThan(vec3(1.0),texCoord))){/*color.rgba+=vec4(1.0,1.0,0.0,1.0);*/continue;}
        data = texture3D(TextureData3D,texCoord).r;
        if(data*MaxDataDisplay<MinDataDisplay||data*MaxDataDisplay>MaxDataDisplay)continue;
        
        if(data!=0.0)count+=1;
//         fact=((dataHighlight-delta)<data&&data<(dataHighlight+delta))?1.0:0.40;//test
//         color.a +=fact;
        //color.a +=fact*1.0/(/*float(maxLoop)*/MaxData); //divide by MaxData => added color between 0 and 1 & divide by maxLoop => addition of all the loops between 0 and 1
        idx = (data*MaxDataDisplay-MinDataDisplay)/(MaxDataDisplay-MinDataDisplay);//data/*MaxDataDisplay/*MaxData*/
        color.rgba += texture1D(Colormap,idx).rgba;//float(maxLoop);
    }
    color/=float(count);
    //color.a = (all(equal(vec3(0.0),color.rbg)))?0.0:mix(0.25,1.0,color.a);
    //color = (all(equal(vec4(0.0),color)))?vec4(0.0):mix(vec4(0.25),vec4(1.0),color);
    return color;

}

void main() {
    gl_FragColor=(MaxData!=0.0)?rayMarching():rayMarching();
    //gl_FragColor=(rayMarching().a>=0.0)?vec4(1.0,0.0,0.0,0.8):vec4(0.0,1.0,0.0,0.8);
    //gl_FragColor.a=0.8;
}
