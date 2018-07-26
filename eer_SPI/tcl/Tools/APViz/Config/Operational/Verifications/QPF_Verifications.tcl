global env 

#----- Geography
set Params(Cameras)    {}
set Params(Projection) { -type orthographic -scale 1.0 -mask NONE -mapcoast 1 -maplake 1 -mapriver 1 -mappolit 1 -mapadmin 1 \
      -mapcity 0 -maproad  0 -mapplace 0 -maptopo 0 -mapbath 0 -maptext 0 -mapcoord 1 10 2 -minsize 5 }
      
set Params(Viewport) { -crowd 20 -font XFont12 -bg white -bd 1 -colorcoast #000000 -colorlake #0000ff -colorfillcoast beige -colorfilllake LightSkyBlue1 \
      -colorriver #0000ff -colorpolit #ff0000 -coloradmin #ff0000 -colorcity #ffa500 -colorroad #404040 -colorplace #000000 -colorcoord #000000 }  

#----- Variable Style Configurations
set Params(PR) "-colormap COB_Seq_MHue_RdPu -color grey  -font XFont12 -width 1 -rendercontour 1 -mapall True -intervals \{ 0 1 5 10 25 40 75 100 150 \}"
set Params(13023) "-size 10 -icon CIRCLE -color black -colormap REC_B&B2 -mapall True -rendertexture 1 -rendercontour 1 -rendervalue 1 -font XFont12 -intervals { 1 5 10 15 20 30 40 50 75 100 125 150 200 }"
set Params(13193) "-size 10 -icon CIRCLE -color black -colormap COB_Seq_MHue_RdPu -mapall True -rendertexture 1 -rendercontour 1 -rendervalue 1 -font XFont12 -intervals { 1 5 10 15 20 30 40 50 75 100 125 150 200 }"

#----- Ranges
set Range(Levels) 	{250 500 700 850 1000}
set Range(Vars)   	{GZ HU TT UU}
set Range(Hours)  	{006 012 024}
set Range(Runs)   	{00 06 12 18 24}
set Range(Sources)	{pres diag eta hyb}
set Range(Obs)		{13023 13193}
set Range(ObsTypes)	{SHEF SYNOP DERISFC}
set Range(Models)	{RDPS GDPS HRDPS}
set Range(Accumul)      {6 12 24}

#----- Layers (On:Model:Run:Hour:Source:Var:Level:IP3)
set Layers {   
   True:RDPS:<Runs>:024:diag:PR:0.0:<Accumul>
   False:SYNOP:<Runs>:-:BURP:<Obs>:SFC:-
}