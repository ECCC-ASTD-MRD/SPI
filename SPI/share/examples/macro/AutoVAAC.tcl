#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Impression en (x)batch de cartes en utilisant SPI.
# Fichier  : VAAC.tcl
# Creation : Fevrier 2008 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Creer une carte/produit au standard VAAC pour les produits automatiques.
#
# Arguments  :
#   <Path>   : Chemin dans lequel recuperer les fichiers trajectoires
#   <Type>   : Type de fichier image (defaut: png)
#
# Remarques :
#   - Lancement: SPI -batch -layout VAAC_Watch -script VAAC -args [path] [format]
#===============================================================================

namespace eval Macro::AutoVAAC {} {
   variable Param
   variable Data
   variable Error

   set Param(Info) { "Creer une carte/produit au standard VAAC pour les simulations automatiques."
                     "Create an VAAC standard  map/product for automated simulations." }
   set Param(InfoArgs) { { "Répertoire des fichiers" "Format d'image" } { "Data path" "Image format" } }

   set Param(Path)   ./     ;#Chemin par defaut pour les fichiers trajectoires
   set Param(Format) png    ;#Type de fichier image a generer
}

proc Macro::AutoVAAC::Execute { } {
   global env
   variable Data
   variable Error
   variable Param

   #----- Ouvrir le fichier de resultats
   set fields [fstdfile open VAACFILE read $Param(Path)/tape40]

   #----- Recuperer la description de l'experience
   set dSim [SimInfo::Read VAACFILE]

   #----- Renommer le nom du volcan ( sans le suffixe _watch ) pour la nomenclature CLF2.
   regsub -all _watch [dict get $dSim Location] "" name
   regsub -all __ $name _ name
   set name [string toupper $name)

   #----- Centrer sur la source
   Viewport::Rotate $Page::Data(Frame) [dict get $dSim Lat] [dict get $dSim Lon] 3.0

   #----- Creer la palette de couleur
   colormap create VAACMAP
   colormap read   VAACMAP $env(HOME)/.spi/Colormap/REC_Col.std1.rgba

   #----- Creer l'icone
   set pixel [$VAAC_Watch::Data(VP1) -project [dict get $dSim Lat] [dict get $dSim Lon] 0]
   Shape::DrawIcoVAAC $Page::Data(Canvas) $pixel FIX black 5 False

   set pixel [$VAAC_Watch::Data(VP2) -project [dict get $dSim Lat] [dict get $dSim Lon] 0]
   Shape::DrawIcoVAAC $Page::Data(Canvas) $pixel FIX black 5 False

   set pixel [$VAAC_Watch::Data(VP3) -project [dict get $dSim Lat] [dict get $dSim Lon] 0]
   Shape::DrawIcoVAAC $Page::Data(Canvas) $pixel FIX black 5 False

   #----- Champs de concentrations
   foreach ip2 { 6 12 18 24 } {

      Macro::Doing "Processing concentraion for hour $ip2"

      #----- Read and display fields
      fstdfield read VAACFLD2 VAACFILE -1 "        " -1 $ip2 2 " " "AV"
      fstdfield configure VAACFLD2 -rendertexture 1 -rendercontour 2 -intervals "10 100 1000" \
        -color #000000 -colormap VAACMAP -font XFont10 -value EXPONENTIAL 2
      Viewport::Assign $Page::Data(Frame) $VAAC_Watch::Data(VP3) VAACFLD2

      fstdfield read VAACFLD3 VAACFILE -1 "        " -1 $ip2 3 " " "AV"
      fstdfield configure VAACFLD3 -rendertexture 1 -rendercontour 2 -intervals "10 100 1000" \
        -color #000000 -colormap VAACMAP -font XFont10 -value EXPONENTIAL 2
      Viewport::Assign $Page::Data(Frame) $VAAC_Watch::Data(VP2) VAACFLD3

      fstdfield read VAACFLD4 VAACFILE -1 "        " -1 $ip2 4 " " "AV"
      fstdfield configure VAACFLD4 -rendertexture 1 -rendercontour 2 -intervals "10 100 1000" \
        -color #000000 -colormap VAACMAP -font XFont10 -value EXPONENTIAL 2
      Viewport::Assign $Page::Data(Frame) $VAAC_Watch::Data(VP1) VAACFLD4

      Page::Update $Page::Data(Frame)

      #----- Update legend
      set dateo [clock format [dict get $dSim AccSecs] -format "%a %b %d %Y, %H UTC" -timezone :UTC]
      $Page::Data(Canvas) itemconf INFO -text "Volcanic ash concentrations valid on [MetData::FormatDATEV VAACFLD2]\nfor hypothetical release of volcano\n $name ([dict get $dSim Lat] [dict get $dSim Lon]) on $dateo"

      $Page::Data(Canvas) itemconf LGT -fill "#[fstdfield configure VAACFLD4 -val2map 10]"
      $Page::Data(Canvas) itemconf MDT -fill "#[fstdfield configure VAACFLD4 -val2map 100]"
      $Page::Data(Canvas) itemconf HVY -fill "#[fstdfield configure VAACFLD4 -val2map 1000]"

      PrintBox::Image $Page::Data(Frame) $Param(Format) $Param(Path)/${name}_canerm-watch_[clock format [dict get $dSim AccSecs] -format "%H" -timezone :UTC]Z+[format "%02i" $ip2]
   }

   fstdfile close VAACFILE

   Macro::Doing ""
   if { $SPI::Param(Batch) } {
      SPI::Quit
   }
}

proc Macro::AutoVAAC::Clean { } {

   colormap free VAACMAP
   fstdfield free VAACFLD2 VAACFLD3 VAACFLD4
}

proc Macro::AutoVAAC::Args { } {
   global argv argc
   variable Param

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Param(Path)   [lindex $argv 0] }
   if { $argc>1 } { set Param(Format) [lindex $argv 1] }
}
