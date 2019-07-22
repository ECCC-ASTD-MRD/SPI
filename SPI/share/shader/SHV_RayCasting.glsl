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
varying vec3 rayDirection, rayOrigin;


void main() {

    rayOrigin.x = (gl_Vertex.x+LI)/(2.0*LI); //[-1,1] to [0,1]
    rayOrigin.y = (gl_Vertex.y+LJ)/(2.0*LJ); //[-1,1] to [0,1]
    rayOrigin.z = ((gl_Vertex.z-1.0)*(1.0/2.0)*(200.0/Elev)); //[1,2] to [0,1]
    rayDirection = normalize(CameraDir);
    rayDirection.z*=(200.0/Elev);
    gl_Position = gl_ModelViewProjectionMatrix*gl_Vertex;

}
