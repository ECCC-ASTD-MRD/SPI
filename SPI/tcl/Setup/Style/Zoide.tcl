
#----- Style Zoide

catch { colormap create CMAPZoide }
colormap control CMAPZoide -del
colormap control CMAPZoide -list { {0 0 6 135 255} {181 0 199 29 255} {255 255 0 0 255} }
colormap configure CMAPZoide  -RGBAratio 100 100 100 100 -MMratio 0 100  -curve red LINEAR -curve green LINEAR -curve blue LINEAR -curve alpha LINEAR  -invertx red 0 -invertx green 0 -invertx blue 0 -invertx alpha 0  -inverty red 0 -inverty green 0 -inverty blue 0 -inverty alpha 0  -min red 0.0 -min green 0.0 -min blue 0.0 -min alpha 0.0  -max red 255.0 -max green 255.0 -max blue 255.0 -max alpha 0.0  -interp 1

catch { font create FONTZoide }
font configure FONTZoide -family courier -weight bold -size -12 -slant roman -underline 0 -overstrike 0

dataspec create "Zoide" -factor 1.0 -value AUTO 0 -size 5.0 -width 1 -unit "kts" -desc "Streamlines" -icon "NONE" -mark "0" -color "#FFFFFF" -fill "" -activefill "" -outline "#FFFFFF" -activeoutline ""  -transparency "68" -dash ""  -rendercontour 0 -rendervector STREAMLINE -rendertexture 1 -rendervolume 0 -renderparticle 0 -rendercoord 0 -rendervalue 0 -renderlabel 0 -rendergrid 0 -min "" -max "" -intervals "" -interlabels "" -intervalmode NONE 0.0  -interpdegree "LINEAR" -extrapdegree "NEUTRAL"  -sample "2" -stipple "" -colormap "CMAPZoide" -font "FONTZoide"  -texsample "16" -texsize "256" -texres "1"  -interpolation "NEAREST" -topography "" -topographyfactor "1.0"  -mask "" -light "0" -labelvar ""  -sizevar "" -mapvar ""  -extrude "" -extrudefactor "1.0"
