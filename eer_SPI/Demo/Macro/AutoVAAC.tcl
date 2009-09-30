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

   set Param(Path)   ./     ;#Chemin par defaut pour les fichiers trajectoires
   set Param(Format) png    ;#Type de fichier image a generer
}

proc Macro::AutoVAAC::Execute { } {
   global GDefs
   variable Data
   variable Error
   variable Param
   variable Info

   #----- Ouvrir le fichier de resultats
   set fields [fstdfile open VAACFILE read $Param(Path)/tape40]

   #----- Recuperer la description de l'experience
   Info::Decode ::Macro::AutoVAAC::Info $CANERM::Sim(Info) [Info::Read VAACFILE]

   #----- Renommer le nom du volcan ( sans le suffixe _watch ) pour la nomenclature CLF2.
   regsub -all _watch $Info(Name) "" Info(Name)
   regsub -all __ $Info(Name) _ Info(Name)
   set Info(Name) [string toupper $Info(Name)]

   #----- Centrer sur la source
   Viewport::Rotate $Page::Data(Frame) $Info(Lat) $Info(Lon) 3.0

   #----- Creer la palette de couleur
   colormap create VAACMAP
   colormap read   VAACMAP $GDefs(DirEER)/eer_Map/REC_Col.std1.rgba

   #----- Creer l'icone
   set pixel [$VAAC_Watch::Data(VP1) -project $Info(Lat) $Info(Lon) 0]
   Shape::DrawIcoVAAC $Page::Data(Canvas) $pixel FIX black 5 False

   set pixel [$VAAC_Watch::Data(VP2) -project $Info(Lat) $Info(Lon) 0]
   Shape::DrawIcoVAAC $Page::Data(Canvas) $pixel FIX black 5 False

   set pixel [$VAAC_Watch::Data(VP3) -project $Info(Lat) $Info(Lon) 0]
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
      set dateo [clock format [clock scan "$Info(AccYear)$Info(AccMonth)$Info(AccDay) $Info(AccHour):00" -gmt True] -format "%a %b %d %Y, %H UTC" -gmt true]
      $Page::Data(Canvas) itemconf INFO -text "Volcanic ash concentrations valid on [MetData::FormatDATEV VAACFLD2]\nfor hypothetical release of volcano\n $Info(Name) ($Info(Lat) $Info(Lon)) on $dateo"

      $Page::Data(Canvas) itemconf LGT -fill "#[fstdfield configure VAACFLD4 -val2map 10]"
      $Page::Data(Canvas) itemconf MDT -fill "#[fstdfield configure VAACFLD4 -val2map 100]"
      $Page::Data(Canvas) itemconf HVY -fill "#[fstdfield configure VAACFLD4 -val2map 1000]"

      PrintBox::Image $Page::Data(Frame) $Param(Format) $Param(Path)/$Info(Name)_canerm-watch_$Info(AccHour)Z+[format "%02i" $ip2]
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

#----- Lire les parametres si il y en a
if { $argc>0 } { set Macro::AutoVAAC::Param(Path)   [lindex $argv 0] }
if { $argc>1 } { set Macro::AutoVAAC::Param(Format) [lindex $argv 1] }
