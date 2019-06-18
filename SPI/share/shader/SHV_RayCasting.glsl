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


void main() {

    rayOrigin.xy = (gl_Vertex.xy+1.0)/2.0; //[-1,1] to [0,1]
    rayOrigin.z = ((gl_Vertex.z-1.0)*200.0/Elev); //[1,2] to [0,1]
    rayDirection = normalize(CameraDir);
    rayDirection.z*=(200.0/Elev);
    gl_Position = gl_ModelViewProjectionMatrix*gl_Vertex;

}
