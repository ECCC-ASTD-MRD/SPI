global env 

#----- Geography
set Params(Cameras)    {}
set Params(Projection) { -type orthographic -scale 1.0 -mask NONE -mapcoast 1 -maplake 1 -mapriver 1 -mappolit 1 -mapadmin 1 -mapcity 0 -maproad 0 -mapplace 0 -maptopo 0 -mapbath 0 -maptext 0 -mapcoord 1 10.0 2 -minsize 5 }
set Params(Viewport) { -crowd 20 -font XFont12 -bg white -bd 1 -colorcoast #000000 -colorlake #0000ff -colorfillcoast beige -colorfilllake LightSkyBlue1 -colorriver #0000ff -colorpolit #ff0000 -coloradmin #ff0000 -colorcity #ffa500 -colorroad #404040 -colorplace #000000 -colorcoord #000000 }

#----- Variable Style Configurations
set Params(DZ) "-colormap REC_Hue 	-color blue -font XFont12 -width 1 -dash - -rendercontour 1 -mapall False -intervalmode INTERVAL 6"
set Params(TT) "-colormap REC_Ocean 	-color yellow -font XFont12 -width 3 -rendercontour 1 -mapall True -intervals \{ 0 15 \}"
set Params(PN) "-colormap REC_Beach 	-color green -font XFont12 -width 2 -rendercontour 1 -mapall True -intervalmode INTERVAL 4"
set Params(PR) "-colormap REC_Beach     -color black -font XFont12 -width 1 -rendercontour 1 -mapall True -intervalmode INTERVAL 4"

#----- Ranges
set Range(Levels_TT) 	{0 250 500 700 850 1000}
set Range(Levels_DZ)    {1000-500 1000-700 1000-850 1000-925 925-850 850-700 850-500 700-500}
set Range(Vars_Surf)   	{PR PN}
set Range(Hours)  	{006 012 018 024}
set Range(Runs)   	{00 06 12 28}
set Range(Sources)	{pres diag eta hyb}

#----- Layers (On:Model:Var:Level:Hour:Run:Source)
set Layers {
   True:RDPS:<Vars_Surf>:0.0:<Hours>:<Runs>:<Sources>
   True:RDPS:<Vars_Surf>:0.0:<Hours>:<Runs>:<Sources>
   True:RDPS:<Vars_Surf>:0.0:<Hours>:<Runs>:<Sources>
   True:RDPS:TT:<Levels_TT>:<Hours>:<Runs>:<Sources>
   True:RDPS:DZ:<Levels_DZ>:<Hours>:<Runs>:pres
}

#----- Default Values 
set DefaultValues {
   False:RDPS:PR:0.0:000:00:pres
   False:RDPS:PR:0.0:000:00:pres
   False:RDPS:PN:0.0:000:00:pres
   False:RDPS:TT:850:000:00:pres
   True:RDPS:DZ:1000-500:000:00:pres
}
