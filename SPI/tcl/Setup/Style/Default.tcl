
#----- Style Default

catch { colormap create CMAPDefault }
colormap control CMAPDefault -del
colormap control CMAPDefault -list { {0 0 0 127 255} {38 0 0 254 255} {89 0 253 255 255} {153 255 255 0 255} {217 254 0 0 255} {255 127 0 0 255} }
colormap configure CMAPDefault  -RGBAratio 100 100 100 100 -MMratio 0 100  -curve red LINEAR -curve green LINEAR -curve blue LINEAR -curve alpha LINEAR  -invertx red 0 -invertx green 0 -invertx blue 0 -invertx alpha 0  -inverty red 0 -inverty green 0 -inverty blue 0 -inverty alpha 0  -min red 0.0 -min green 0.0 -min blue 0.0 -min alpha 0.0  -max red 255.0 -max green 255.0 -max blue 255.0 -max alpha 0.0  -interp 1

catch { font create FONTDefault }
font configure FONTDefault -family courier -weight bold -size -12 -slant roman -underline 0 -overstrike 0

dataspec create "Default" -factor 1.0 -value AUTO 0 -size 10.0 -width 1 -icon "NONE" -mark "0" -color "#000000" -fill "" -activefill "" -outline "#000000" -activeoutline ""  -transparency "100" -dash ""  -rendercontour 0 -rendervector NONE -rendertexture 1 -rendervolume 0 -renderparticle 0 -rendercoord 0 -rendervalue 0 -renderlabel 0 -rendergrid 0 -min "" -max "" -intervals "" -interlabels "" -intervalmode NONE 0.0  -interpdegree "LINEAR" -extrapdegree "NEUTRAL"  -sample "2" -stipple "" -colormap "CMAPDefault" -font "FONTDefault"  -texsample "16" -texsize "256" -texres "1"  -interpolation "NEAREST" -topography "" -topographyfactor "1.0"  -mask "" -light "0" -labelvar ""  -sizevar "" -mapvar "" -mapall "0"  -extrude "" -extrudefactor "1.0"
