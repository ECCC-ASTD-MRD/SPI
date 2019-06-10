uniform sampler3D TextureData3D;
uniform float MaxData;
uniform sampler1D Colormap;
uniform float MinDataDisplay;
uniform float MaxDataDisplay;
uniform vec3 CameraDir;
varying vec3 rayDirection, rayOrigin;


void main() {

    rayOrigin.xy = (gl_Vertex.xy+1.0)/2.0; //[-1,1] to [0,1]
    rayOrigin.z = (gl_Vertex.z-1.0); //[1,2] to [0,1]
    rayDirection = normalize(CameraDir);
    gl_Position = gl_ModelViewProjectionMatrix*gl_Vertex;

}
