global env 

#----- GEOGRAPHY BEGIN
set Params(Cameras)    {}
set Params(Projection) { -type orthographic -scale 1.0 -mask NONE -mapcoast 1 -maplake 1 -mapriver 1 -mappolit 1 -mapadmin 1 \
      -mapcity 0 -maproad  0 -mapplace 0 -maptopo 0 -mapbath 0 -maptext 0 -mapcoord 1 10 2 -minsize 5 }
      
set Params(Viewport) { -crowd 20 -font XFont12 -bg white -bd 1 -colorcoast #000000 -colorlake #0000ff -colorfillcoast beige -colorfilllake LightSkyBlue1 \
      -colorriver #0000ff -colorpolit #ff0000 -coloradmin #ff0000 -colorcity #ffa500 -colorroad #404040 -colorplace #000000 -colorcoord #000000 }  
#----- GEOGRAPHY END

#----- STYLE CONFIGURATION BEGIN
#----- Variable Style Configurations
set Params(GZ) "-colormap REC_Hue 	-color black -font XFont12 -width 1 -rendercontour 1 -mapall True -intervalmode INTERVAL 6 -extrema HL"
set Params(TT) "-colormap REC_Ocean 	-color black -font XFont12 -width 1 -rendercontour 1 -mapall True -intervalmode INTERVAL 5"
set Params(HU) "-colormap REC_Rainbow 	-color black -font XFont12 -width 1 -rendertexture 1 -mapall True"
set Params(UU) "-colormap REC_Nature 	-color grey  -font XFont12 -width 1 -rendervector BARB -mapall False -intervals \{ -150 -100 -80 -50 -30 -20 -15 -10 0 10 15 20 30 50 80 100 150 \}"
set Params(PN) "-colormap REC_Beach 	-color black -font XFont12 -width 1 -rendercontour 1 -mapall True -intervalmode INTERVAL 4"
#----- STYLE CONFIGURATION END

#----- RANGES BEGIN
set Range(Levels) 	{250 500 700 850 1000}
set Range(Vars)   	{GZ HU TT UU}
set Range(Hours)  	{000 024 036 048}
set Range(Runs)   	{00 06 12 18}
set Range(Sources)	{pres diag eta hyb}
#----- RANGES END

#----- LAYERS BEGIN
#----- Layers (On:Model:Run:Hour:Source:Var:Level)
set Layers {
   True:RDPS:<Runs>:<Hours>:<Sources>:<Vars>:<Levels>
   False:RDPS:<Runs>:<Hours>:<Sources>:PN:0.0
}
#----- LAYERS END

#----- DEFAULT VALUES BEGIN
#----- DEFAULT VALUES END