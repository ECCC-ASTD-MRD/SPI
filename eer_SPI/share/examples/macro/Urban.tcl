namespace eval Macro::Urban {} {
   variable Param
   variable Data
   variable Error

   set Param(File) ""           ;#RPN file input (Script/DataIn/20091204_094244_radarsat)
   set Param(Lat)  49.285       ;#Latitude de la source
   set Param(Lon)  -123.112     ;#Longitude de la source

   set Param(Info) { "Generation de produits 3D."
                     "Generate 3D urban products." }
   set Param(InfoArgs) { { "Fichier standard" "Latitude" "Longitude" } { "Standard file" "Latitude" "Longitude" } }
}

proc Macro::Urban::Execute { } {
   global env
   variable Param
   variable Data
   variable Error

   #----- Configure the data for display
   if { ![colormap is URBANMAP] } {
      colormap create URBANMAP
      colormap read URBANMAP $env(HOME)/.spi/Colormap/REC_Col.std1.rgba
      colormap configure URBANMAP -RGBAratio 100 100 100 75
   }
   font create URBANFONT -family Arial -size -12 -weight bold

   Macro::Doing "Reading fields"
   fstdfile open STDFILE read $Param(File)
   fstdfield read CV STDFILE -1 "" 12003 -1 -1 "" CV

   set ll0 [fstdfield stats CV -project 0 0]
   set ll1 [fstdfield stats CV -project [expr [fstdfield define CV -NI]-1] [expr [fstdfield define CV -NJ]-1]]
   Macro::Doing "Setting view limits to ($ll0) - ($ll1)"
   ProjCam::CloseUp $Page::Data(Frame) $Page::Data(Frame) $Urban4Panel::Data(VPTOP) [lindex $ll0 0] [lindex $ll0 1] [lindex $ll1 0] [lindex $ll1 1] -2

   projcam copy cam3d $Page::Data(Frame)
   projcam define cam3d -circlefrom 0.0 -65.0 1.0
   projcam configure cam3d -lens [expr [projcam configure cam3d -lens]*1.25]
   $Page::Data(Canvas) itemconfigure $Urban4Panel::Data(VP3D) -camera cam3d

   #----- Center on source
#   Viewport::Rotate $Page::Data(Frame) $Param(Lat) $Param(Lon)

#    foreach fld [fstdfield find STDFILE -1 "" -1 -1 -1 "" ZH] {
#       fstdfield read ZH STDFILE $fld
#       fstdfield configure ZH -color black -colormap URBANMAP -font URBANFONT \
#        -renderparticle 2 -value INTEGER 2 -min 0 -max 250
#
#       Viewport::UnAssign $Page::Data(Frame) $Urban4Panel::Data(VPTOP)
#       Viewport::UnAssign $Page::Data(Frame) $Urban4Panel::Data(VP3D)
#       Viewport::UnAssign $Page::Data(Frame) $Urban4Panel::Data(VPEAST)
#       Viewport::UnAssign $Page::Data(Frame) $Urban4Panel::Data(VPSOUTH)
#
#    }

   foreach fld [fstdfield find STDFILE -1 "" 12003 -1 -1 "" CV] {
      fstdfield read CV STDFILE $fld
      fstdfield read CV3D STDFILE $fld
      fstdfield configure CV3D -desc 3D -color black -colormap URBANMAP -font URBANFONT \
       -intervals { 5e-9 1e-8 } -rendertexture 1 -rendervolume 1 -value INTEGER 2

      fstdfield configure CV -color black -colormap URBANMAP -font URBANFONT \
       -intervals { 5e-9 1e-8 } -rendertexture 1 -rendercontour 2 -value INTEGER 2

      Viewport::Assign $Page::Data(Frame) $Urban4Panel::Data(VPTOP) CV
      Viewport::Assign $Page::Data(Frame) $Urban4Panel::Data(VP3D) CV3D
#      Viewport::Assign $Page::Data(Frame) $Urban4Panel::Data(VPEAST) CV
#      Viewport::Assign $Page::Data(Frame) $Urban4Panel::Data(VPSOUTH) CV
   }
   fstdfile close STDFILE
}

proc Macro::Urban::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   font delete URBANFONT

   fstdfield free ZH
}

proc Macro::Urban::Args { } {
   global argv argc
   variable Param

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Param(File) [lindex $argv 0] }
   if { $argc>1 } { set Param(Lat)  [lindex $argv 1] }
   if { $argc>2 } { set Param(Lon)  [lindex $argv 2] }
}
