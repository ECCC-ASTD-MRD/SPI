global env 

#----- Geography
set Params(Cameras)    {}
set Params(Projection) { -type orthographic -scale 1.0 -mask NONE -mapcoast 1 -maplake 1 -mapriver 0 -mappolit 1 -mapadmin 1 \
      -mapcity 0 -maproad  0 -mapplace 0 -maptopo 0 -mapbath 0 -maptext 0 -mapcoord 1 10 2 -minsize 5 }
      
set Params(Viewport) { -crowd 20 -font XFont12 -bg white -bd 1 -colorcoast #000000 -colorlake #0000ff -colorfillcoast beige -colorfilllake LightSkyBlue1 \
      -colorriver #0000ff -colorpolit #000000 -coloradmin #000000 -colorcity #000000 -colorroad #000000 -colorplace #000000 -colorcoord #000000 }  

#----- Variable Style Configurations
set Params(PR) "-colormap REC_Rainbow -color black -value INTEGER 0 -font XFont12 -width 2 -rendercontour 0 -rendertexture 1 -rendervalue 20 -mapall True -intervals \{ 1 5 10 15 20 25 30 40 50 75 100 \} -factor 1e3"
set Params(13023) "-size 10 -icon CIRCLE -color black -colormap REC_Rainbow -mapall True -rendertexture 1 -rendercontour 1 -rendervalue 1 -font XFont12 -intervals \{ 1 5 10 15 20 25 30 40 50 75 100 \}"
set Params(13193) "-size 10 -icon CIRCLE -color black -colormap REC_Rainbow -mapall True -rendertexture 1 -rendercontour 1 -rendervalue 1 -font XFont12 -intervals \{ 1 5 10 15 20 25 30 40 50 75 100 \}"

#----- Ranges
set Range(Hours)  	{006 012 024}
set Range(Runs)   	{00 06 12 18 24}
set Range(Obs)		{13023 13193}
set Range(ObsTypes)	{SHEF SYNOP DERISFC}
set Range(Accumul)      {6 12 24}

#----- Layers (On:Model:Run:Hour:Source:Var:Level:IP3)
set Layers {   
   True:RDPS:<Runs>:024:diag:PR:0.0:<Accumul>
   True:SYNOP:<Runs>:-:BURP:13023:SFC:-
}

#----- Default Values 
set DefaultValues {
   True:RDPS:00:024:diag:PR:0.0:24
   True:SYNOP:00:-:BURP:13023:SFC:-
}
