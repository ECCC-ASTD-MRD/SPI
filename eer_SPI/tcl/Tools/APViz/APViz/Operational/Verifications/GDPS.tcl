global env 

#Geography
set Map(Cameras)    {}
set Map(Projection) Orthographic

#Styles
variable Params

colormap create CM0
colormap create CM1
colormap create CM2
colormap create CM3
colormap create CM4

colormap read CM0 $env(HOME)/.spi/Colormap/REC_B&B2.rgba
colormap read CM1 $env(HOME)/.spi/Colormap/COB_Div_BrBG.rgba
colormap read CM2 $env(HOME)/.spi/Colormap/COB_Seq_MHue_GnBu.rgba
colormap read CM3 $env(HOME)/.spi/Colormap/COB_Seq_MHue_RdPu.rgba
colormap read CM4 $env(HOME)/.spi/Colormap/COB_Seq_MHue_BuPu.rgba

#Variable configurations
set Params(GZ) "-colormap CM0 -color black -font XFont12 -width 2 -rendercontour 1 -mapall True -intervalmode INTERVAL 6"
set Params(TT) "-colormap CM2 -color black -font XFont12 -width 2 -rendercontour 1 -mapall True -intervalmode INTERVAL 5"
set Params(HU) "-colormap CM1 -color black -font XFont12 -width 2 -rendertexture 1 "
set Params(UU) "-colormap CM3 -color grey  -font XFont12 -width 1 -rendervector BARB -mapall False -intervals \{ -150 -100 -80 -50 -30 -20 -15 -10 0 10 15 20 30 50 80 100 150 \}"
set Params(PN) "-colormap CM4 -color black -font XFont12 -width 2 -rendercontour 1 -intervalmode INTERVAL 4"

set Params(Projection) { -type orthographic -scale 1.0 -mask NONE -mapcoast 1 -maplake 1 -mapriver 1 -mappolit 1 -mapadmin 1 \
  -mapcity 0 -maproad  0 -mapplace 0 -maptopo 0 -mapbath 0 -maptext 0 -mapcoord 1 10 2 -minsize 5 }
 
set Params(Viewport) { -crowd 20 -font XFont12 -bg white -bd 1 -colorcoast #000000 -colorlake #0000ff -colorfillcoast beige -colorfilllake LightSkyBlue1 \
  -colorriver #0000ff -colorpolit #ff0000 -coloradmin #ff0000 -colorcity #ffa500 -colorroad #404040 -colorplace #000000 -colorcoord #000000 }  

#Ranges
variable Range
set Range(Levels) 	{250 500 700 850 1000}
set Range(Vars)   	{GZ HU TT UU}
set Range(Vars_Lev0)	{PN PR}
set Range(Hours)  	{000 024 036 048 060 072 084 096 120 144 168 192 216 240}
set Range(Runs)   	{00 06 12 28}
set Range(Sources)	{pres diag eta hyb}

#Layers (On:Model:Var:Level:Hour:Interval:Run:Source)
set Layers {
   True:GDPS:<Vars>:<Levels>:<Hours>:-:<Runs>:<Sources>
   False:GDPS:PN:0.0:<Hours>:-:<Runs>:<Sources>
}