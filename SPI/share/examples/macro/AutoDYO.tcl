#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Impression en (x)batch de cartes en utilisant SPI.
# Fichier  : AutoDYO.tcl
# Creation : Fevrier 2008 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Creer une carte/produit au standard RSMC pour les produits automatiques.
#
# Arguments  :
#   <Path>   : Chemin dans lequel recuperer les fichiers trajectoires
#   <Type>   : Type de fichier image (defaut: png)
#
# Remarques :
#   - Lancement: SPI -batch -layout RSMC_Watch -script AutoDYO -args [path] [format] [zoom] [lat] [lon]
#===============================================================================

namespace eval Macro::AutoDYO {} {
   variable Param
   variable Data
   variable Error

   set Param(Info) { "Creer une carte/produit au standard RSMC pour les simulations automatiques."
                     "Create an RSMC standard  map/product for automated simulations." }

   set Param(Path)   ./     ;#Chemin par defaut pour les fichiers
   set Param(Format) png    ;#Type de fichier image a generer
   set Param(Zoom)   1.0    ;#Facteur de zoom
   set Param(Lat)    -999.0 ;#Latitude du point central
   set Param(Lon)    -999.0 ;#Longitude du point central
}

proc Macro::AutoDYO::GetIntervals { File Var IP1 { Nb 4 } } {

   set max 1e-32
   set min 1e32
   foreach fld [fstdfield find $File -1 "" $IP1 -1 -1 "" "$Var"] {
      fstdfield read FLD $File $fld
       set fa  [lindex [fstdfield stats FLD -max] 0]
       set fi  [lindex [fstdfield stats FLD -min] 0]
       set max [expr $max>$fa?$max:$fa]
       set min [expr $min<$fi?$min:$fi]
   }
   fstdfield free FLD

   set base [expr floor(log10($max))]

   for { set n 0 } { $n<$Nb } { incr n } {
      set val [expr pow(10,($base-($Nb-$n)))]
      if { $val>$min } {
         lappend inter [expr pow(10,($base-($Nb-$n)))]
      }
   }
   return $inter
}

proc Macro::AutoDYO::Execute { } {
   global env
   variable Data
   variable Error
   variable Param
   variable Info

   #----- Creer la palette de couleur
   colormap create DYOCMAP
   colormap read   DYOCMAP $env(HOME)/.spi/Colormap/MAX2_DYO.rgba

   #ST set date [string range [lindex [lsort -dictionary -increasing [glob -directory $env(CMCGRIDF)/prog/regeta/ -tails *$Param(Run)_???]] end] 0 7]
   set date 2012072600

   catch {
      font create FONT12  -family Arial -weight bold -size -12
   }

   fstdfile open DYOFILE read $Param(Path)/2012072600_046_diag

   fstdfield read GZ DYOFILE -1 "GZ_INT" 850 46 -1 "" "GZ"
   fstdfield read WF DYOFILE -1 "W_OR"   700 46 -1 "" "WF"

   #---- BAD width
   #fstdfield configure GZ -color black -font FONT12 -rendertexture 0 -rendercontour 4 -intervals { 132 138 144 150 156 }
   fstdfield configure GZ -color black -width 4 -font FONT12 -rendertexture 0 -rendercontour 4 -intervals { 132 138 144 150 156 }

   # ?? - factor
   #fstdfield configure WF -desc "W_OR" -color black -font FONT12 -rendertexture 1 -colormap DYOCMAP -intervals { -2.0 -1.8 -1.6 -1.4 -1.2 -1.0 -0.8 -0.6 -0.4 -0.2 0.0 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0 } -factor 10.0 -value FLOAT 1
   # good - factor
   #fstdfield configure WF -desc "W_OR" -factor 10.0 -color black -font FONT12 -rendertexture 1 -colormap DYOCMAP -intervals { -2.0 -1.8 -1.6 -1.4 -1.2 -1.0 -0.8 -0.6 -0.4 -0.2 0.0 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0 } -value FLOAT 1
   # good - intervals
   #fstdfield configure WF -desc "W_OR" -factor 10.0 -color black -font FONT12 -rendertexture 1 -colormap DYOCMAP -intervals "-2.0 -1.8 -1.6 -1.4 -1.2 -1.0 -0.8 -0.6 -0.4 -0.2 0.0 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0" -value FLOAT 1
   #----- LAST GOOD
   fstdfield configure WF -desc "W_OR" -factor 10.0 -color black -colormap DYOCMAP -font FONT12 -rendertexture 1 -intervals "-2.0 -1.8 -1.6 -1.4 -1.2 -1.0 -0.8 -0.6 -0.4 -0.2 0.0 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0" -value FLOAT 1
   #----- LAST WRONG factor
   #fstdfield configure WF -desc "W_OR" -color black -colormap DYOCMAP -font FONT12 -rendertexture 1 -intervals "-2.0 -1.8 -1.6 -1.4 -1.2 -1.0 -0.8 -0.6 -0.4 -0.2 0.0 0.2 0.4 0.6 0.8 1.0 1.2 1.4 1.6 1.8 2.0" -factor 10.0 -value FLOAT 1

#   fstdfield configure DIFF_FIELD2 -desc "Temperature" -factor 1 -color black -colormap DIFF_MAP -font DIFF_FONT \
#      -rendertexture 1 -intervals "-10 0 10 20" -value FLOAT 2

   #Macro::JetMapper::StreamPlot
   #Macro::JetMapper::HighLowPlot PN

   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) { GZ WF }

   ColorBar::Create $Page::Data(Frame) $Viewport::Data(VP) 1110 310 80 400

   fstdfield configure WF -rendervalue 9 -color red -font FONT12 -value FLOAT 3

#   Page::UpdateCommand $Page::Data(Frame)

   #PrintBox::Image $Page::Data(Frame) $Param(Format) $Param(Path)/WF_[format "%03i" $ip2]
#   PrintBox::Image $Page::Data(Frame) $Param(Format) $Param(Path)/WF_test

   fstdfile close DYOFILE

   Macro::Doing ""
   if { $SPI::Param(Batch) } {
      SPI::Quit
   }
}

proc Macro::AutoDYO::Clean { } {

   colormap free DYOCMAP
   fstdfield free DYOFLD
}

proc Macro::AutoDYO::Args { } {
   global argv argc
   variable Param

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Param(Path)   [lindex $argv 0] }
   if { $argc>1 } { set Param(Format) [lindex $argv 1] }
   if { $argc>2 } { set Param(Zoom)   [lindex $argv 2] }
   if { $argc>3 } { set Param(Lat)    [lindex $argv 3] }
   if { $argc>4 } { set Param(Lon)    [lindex $argv 4] }
}
