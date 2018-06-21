global env 

#----- Geography
set Params(Cameras)    {}
set Params(Projection) { -type orthographic -scale 1.0 -mask NONE -mapcoast 1 -maplake 1 -mapriver 1 -mappolit 1 -mapadmin 1 \
      -mapcity 0 -maproad  0 -mapplace 0 -maptopo 0 -mapbath 0 -maptext 0 -mapcoord 1 10 2 -minsize 5 }
      
set Params(Viewport) { -crowd 20 -font XFont12 -bg white -bd 1 -colorcoast #000000 -colorlake #0000ff -colorfillcoast beige -colorfilllake LightSkyBlue1 \
      -colorriver #0000ff -colorpolit #ff0000 -coloradmin #ff0000 -colorcity #ffa500 -colorroad #404040 -colorplace #000000 -colorcoord #000000 }  

#----- Colormap creation
colormap create CM0 -file $env(HOME)/.spi/Colormap/REC_B&B2.rgba
colormap create CM1 -file $env(HOME)/.spi/Colormap/COB_Div_BrBG.rgba
colormap create CM2 -file $env(HOME)/.spi/Colormap/COB_Seq_MHue_GnBu.rgba
colormap create CM3 -file $env(HOME)/.spi/Colormap/COB_Seq_MHue_RdPu.rgba
colormap create CM4 -file $env(HOME)/.spi/Colormap/COB_Seq_MHue_BuPu.rgba

#----- Variable Style Configurations
set Params(PR) "-colormap CM3 -color grey  -font XFont12 -width 1 -rendercontour 1 -mapall True -intervals \{ 0 1 5 10 25 40 75 100 150 \}"

#----- Ranges
set Range(Levels) 	{250 500 700 850 1000}
set Range(Vars)   	{GZ HU TT UU}
set Range(Hours)  	{000 024 036 048}
set Range(Runs)   	{00 06 12 28}
set Range(Sources)	{pres diag eta hyb}
set Range(Obs)		{13023 13193}
set Range(ObsTypes)	{SHEF SYNOP DERISFC}
set Range(Models)	{RDPS GDPS HRDPS}

#----- Layers (On:Model:Var:Level:Hour:Interval:Run:Source)
set Layers {
   True:RDPS:PR:0.0:<Hours>:-:<Runs>:<Sources>
   True:SYNOP:<Obs>:SFC:-:-:<Runs>:BURP
}