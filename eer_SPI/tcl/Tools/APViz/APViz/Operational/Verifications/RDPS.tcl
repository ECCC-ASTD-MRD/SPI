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

colormap read CM0 $env(HOME)/.spi/Colormap/REC_B&B1.rgba
colormap read CM1 $env(HOME)/.spi/Colormap/COB_Div_BrBG.rgba
colormap read CM2 $env(HOME)/.spi/Colormap/COB_Seq_MHue_GnBu.rgba
colormap read CM3 $env(HOME)/.spi/Colormap/COB_Seq_MHue_RdPu.rgba
colormap read CM4 $env(HOME)/.spi/Colormap/COB_Seq_MHue_BuPu.rgba

for {set i 0} {$i<=4} {incr i} {
  colormap configure CM$i -invertx rgba 1
}

set Params(GZ) "-colormap CM0 -color black -font XFont12 -width 2 -rendercontour 1 -mapall True -intervalmode INTERVAL 6"
set Params(TT) "-colormap CM2 -color black -font XFont12 -width 2 -rendercontour 1 -mapall True -intervalmode INTERVAL 5"
set Params(HU) "-colormap CM1 -color black -font XFont12 -width 2 -rendertexture 1 "
set Params(UU) "-colormap CM3 -color grey -font XFont12 -width 1 -rendervector BARB -mapall False -intervals \{ -150 -100 -80 -50 -30 -20 -15 -10 0 10 15 20 30 50 80 100 150 \}"
set Params(PN) "-colormap CM4 -color black -font XFont12 -width 2 -rendercontour 1 -intervalmode INTERVAL 4"

#Ranges
variable Range
set Range(Levels) 	{1000 850 700 500 250}
set Range(Vars)   	{GZ HU TT UU}
set Range(Vars_Lev0)	{PN PR}
set Range(Hours)  	{000 024 036 048}
set Range(Runs)   	{00 06 12 28}
set Range(Sources)	{pres diag eta hyb}

#Layers (On:Model:Var:Level:Hour:Interval:Run:Source)
set Layers {
   True:RDPS:<Vars>:<Levels>:<Hours>:-:<Runs>:<Sources>
   False:RDPS:PN:0.0:<Hours>:-:<Runs>:<Sources>
}