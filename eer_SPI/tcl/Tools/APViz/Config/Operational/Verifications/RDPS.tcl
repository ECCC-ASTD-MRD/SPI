global env 
global GDefs

#----- Geography
set Params(Cameras)    {}
set Params(Projection) { -type orthographic -scale 1.0 -mask NONE -mapcoast 1 -maplake 1 -mapriver 1 -mappolit 1 -mapadmin 1 \
      -mapcity 0 -maproad  0 -mapplace 0 -maptopo 0 -mapbath 0 -maptext 0 -mapcoord 1 10 2 -minsize 5 }
      
set Params(Viewport) { -crowd 20 -font XFont12 -bg white -bd 1 -colorcoast #000000 -colorlake #0000ff -colorfillcoast beige -colorfilllake LightSkyBlue1 \
      -colorriver #0000ff -colorpolit #ff0000 -coloradmin #ff0000 -colorcity #ffa500 -colorroad #404040 -colorplace #000000 -colorcoord #000000 }  

#----- Variable Style Configurations
set Params(GZ) "-colormap REC_B&B1 	-color black -font XFont12 -width 2 -rendercontour 1 -mapall False -intervalmode INTERVAL 6"
set Params(TT) "-colormap REC_B&B2 	-color black -font XFont12 -width 2 -rendercontour 1 -mapall True -intervalmode INTERVAL 5"
set Params(HU) "-colormap REC_Rainbow 	-color black -font XFont12 -width 2 -rendertexture 1"
set Params(UU) "-colormap REC_Nature 	-color grey  -font XFont12 -width 1 -rendervector BARB -mapall False -intervals \{ -150 -100 -80 -50 -30 -20 -15 -10 0 10 15 20 30 50 80 100 150 \}"
set Params(PN) "-colormap REC_Hue 	-color black -font XFont12 -width 2 -rendercontour 1 -intervalmode INTERVAL 4"

#----- Ranges
set Range(Levels) 	{250 500 700 850 1000}
set Range(Vars)   	{GZ HU TT UU}
set Range(Vars_Lev0)	{PN PR}
set Range(Hours)  	{000 024 036 048}
set Range(Runs)   	{00 06 12 28}
set Range(Sources)	{pres diag eta hyb}
set Range(Obs)		{13023 13193}
set Range(ObsTypes)	{SHEF SYNOP DERISFC}

#----- Layers (On:Model:Var:Level:Hour:Run:Source)
set Layers {
   True:RDPS:<Vars>:<Levels>:<Hours>:<Runs>:<Sources>
   False:RDPS:PN:0.0:<Hours>:<Runs>:<Sources>
}

#----- Colormap creation
colormap create COB_Div_BrBG -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Div_BrBG.rgba
colormap create COB_Div_PRGn -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Div_PRGn.rgba
colormap create COB_Div_PiYG -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Div_PiYG.rgba
colormap create COB_Div_PuOr -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Div_PuOr.rgba
colormap create COB_Div_RdBu -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Div_RdBu.rgba
colormap create COB_Div_RdGy -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Div_RdGy.rgba
colormap create COB_Div_RdYlBu -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Div_RdYlBu.rgba
colormap create COB_Div_RdYlGn -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Div_RdYlGn.rgba
colormap create COB_Div_Spectral -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Div_Spectral.rgba
colormap create COB_Qua_Paired -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Qua_Paired.rgba
colormap create COB_Qua_Set3 -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Qua_Set3.rgba
colormap create COB_Seq_MHue_BuGn -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_BuGn.rgba
colormap create COB_Seq_MHue_BuPu -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_BuPu.rgba
colormap create COB_Seq_MHue_GnBu -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_GnBu.rgba
colormap create COB_Seq_MHue_OrRd -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_OrRd.rgba
colormap create COB_Seq_MHue_OrRd_NoWhite -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_OrRd_NoWhite.rgba
colormap create COB_Seq_MHue_PuBu -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_PuBu.rgba
colormap create COB_Seq_MHue_PuBuGn -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_PuBuGn.rgba
colormap create COB_Seq_MHue_PuRd -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_PuRd.rgba
colormap create COB_Seq_MHue_RdPu -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_RdPu.rgba
colormap create COB_Seq_MHue_YlGn -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_YlGn.rgba
colormap create COB_Seq_MHue_YlGnBu -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_YlGnBu.rgba
colormap create COB_Seq_MHue_YlOrBr -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_YlOrBr.rgba
colormap create COB_Seq_MHue_YlOrRd -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_MHue_YlOrRd.rgba
colormap create COB_Seq_SHue_Blues -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_SHue_Blues.rgba
colormap create COB_Seq_SHue_Greens -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_SHue_Greens.rgba
colormap create COB_Seq_SHue_Greys -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_SHue_Greys.rgba
colormap create COB_Seq_SHue_Oranges -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_SHue_Oranges.rgba
colormap create COB_Seq_SHue_Purples -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_SHue_Purples.rgba
colormap create COB_Seq_SHue_Reds -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/COB_Seq_SHue_Reds.rgba
colormap create OTH_Bias -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/OTH_Bias.rgba
colormap create OTH_Black2White -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/OTH_Black2White.rgba
colormap create OTH_Clouds -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/OTH_Clouds.rgba
colormap create OTH_GreenDay -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/OTH_GreenDay.rgba
colormap create OTH_Lightning -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/OTH_Lightning.rgba
colormap create OTH_White2Black -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/OTH_White2Black.rgba
colormap create REC_B&B1 -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_B&B1.rgba
colormap create REC_B&B2 -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_B&B2.rgba
colormap create REC_Beach -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Beach.rgba
colormap create REC_Col.std1 -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Col.std1.rgba
colormap create REC_Col.std2 -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Col.std2.rgba
colormap create REC_Default -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Default.rgba
colormap create REC_Ether -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Ether.rgba
colormap create REC_Gray -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Gray.rgba
colormap create REC_GrayCycle -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_GrayCycle.rgba
colormap create REC_Hue -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Hue.rgba
colormap create REC_Nature -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Nature.rgba
colormap create REC_Ocean -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Ocean.rgba
colormap create REC_PBridge -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_PBridge.rgba
colormap create REC_Purple -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Purple.rgba
colormap create REC_R&B1 -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_R&B1.rgba
colormap create REC_R&B2 -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_R&B2.rgba
colormap create REC_Rainbow -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Rainbow.rgba
colormap create REC_Rhubarb -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Rhubarb.rgba
colormap create REC_Slate -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Slate.rgba
colormap create REC_Smog -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Smog.rgba
colormap create REC_Test4 -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Test4.rgba
colormap create REC_Volcano -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Volcano.rgba
colormap create REC_Warp -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/REC_Warp.rgba
colormap create URP_8Colors -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/URP_8Colors.rgba
colormap create URP_Reflectivity -file $GDefs(Dir)/tcl/Tools/APViz/Config/Colormap/URP_Reflectivity.rgba