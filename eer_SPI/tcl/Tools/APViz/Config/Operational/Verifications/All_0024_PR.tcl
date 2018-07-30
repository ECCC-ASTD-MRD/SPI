global env 

#----- GEOGRAPHY BEGIN
set Params(Cameras)    {}
set Params(ViewportNb) 1
set Params(Projection) { -type orthographic -scale 1.0 -mask NONE -mapcoast 1 -maplake 1 -mapriver 0 -mappolit 1 -mapadmin 1 -mapcity 0 -maproad 0 -mapplace 0 -maptopo 0 -mapbath 0 -maptext 0 -mapcoord 1 10.0 2 -minsize 5 }
set Params(Viewport) { -crowd 20 -font XFont12 -bg white -bd 1 -colorcoast #000000 -colorlake #0000ff -colorfillcoast beige -colorfilllake LightSkyBlue1 -colorriver #0000ff -colorpolit #000000 -coloradmin #000000 -colorcity #ffa500 -colorroad #404040 -colorplace #000000 -colorcoord #000000 }
#----- GEOGRAPHY END

#----- STYLE CONFIGURATION BEGIN
#----- Variable Style Configurations
set Params(DZ) "-colormap REC_Hue 	-color blue -font XFont12 -width 1 -dash . -rendercontour 1 -mapall False -intervalmode INTERVAL 6"
set Params(TT) "-colormap REC_Ocean 	-color yellow -font XFont12 -width 3 -rendercontour 1 -mapall False -intervals \{ 0 15 \}"
set Params(PN) "-colormap REC_Beach 	-color green -font XFont12 -width 2 -rendercontour 1 -mapall False -intervalmode INTERVAL 4 -rendervalue 20 -extrema HL"
set Params(PR) "-colormap REC_Beach1     -color black -font XFont12 -width 1 -rendercontour 1 -rendertexture 1 -rendervalue 20 -extrema H -mapall False -intervals \{ 1 5 10 15 20 25 30 40 50 75 100 \}"
set Params(PR1) "-colormap REC_Beach    -color black -font XFont12 -width 2 -rendercontour 1 -mapall False -intervals \{ 10 40 \} "
#----- STYLE CONFIGURATION END

#----- RANGES BEGIN
set Range(Levels_TT) 	{250 500 700 850 1000}
set Range(Levels_DZ)    {1000-500 1000-700 1000-850 1000-925 925-850 850-700 850-500 700-500}
set Range(Hours)  	{006 012 018 024}
set Range(Runs)   	{00 06 12 28}
set Range(Sources)	{pres diag eta hyb}
set Range(Accumul)      {6 12 24}
#----- RANGES END

#----- LAYERS BEGIN
#----- Layers (On:Model:Run:Hour:Source:Var:Level:IP3:VP)
set Layers {
   True:RDPS:<Runs>:024:diag:PR:0.0:<Accumul>
   False:RDPS:<Runs>:024:diag:PR:0.0:<Accumul>
   True:RDPS:<Runs>:024:<Sources>:PN:0.0
   True:RDPS:<Runs>:024:<Sources>:TT:<Levels_TT>
   True:RDPS:<Runs>:024:pres:DZ:<Levels_DZ>
}
#----- LAYERS END

#----- DEFAULT VALUES BEGIN
set DefaultValues {
   True:RDPS:00:024:diag:PR:0.0:24
   True:RDPS:00:024:diag:PR:0.0:24
   True:RDPS:00:024:pres:PN:0.0
   True:RDPS:00:024:pres:TT:850
   True:RDPS:00:024:pres:DZ:1000-500
}
#----- DEFAULT VALUES END