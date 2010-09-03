#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Analyse objective
# Fichier  : OA4Pan.tcl
# Creation : Juillet 2003 - Stéphane Gaudreault - CMC/CMOE
#
# Description:
#   Impression en (x)batch de cartes en utilisant SPI pour l<analyse objective.
#
# Remarques :
#    Utilise le layout ~4Big.tcl
#
# Modification:
#   Feb  2010: Alain Robichaud, pour rebranchement avec GEM-MACH
#   July 2010: J.P. Gauthier, Update to macro and SPI 7.4.0
#===============================================================================

namespace eval Macro::OA4Pan {} {
   variable Param
   variable Data
   variable Error

   set Param(Info) { "Carte d'analyse objective pour GEM-MACH"
                     "GEM_MACH objective analysis map. " }

   set Param(Model) DataIn/modelOA.fst             ;#Model FSTD file
   set Param(Obs)   DataIn/O3.obs                  ;#Observation data file
}

proc Macro::OA4Pan::GetDateString { Field } {

   #----- Obtenir la date reelle a partir de la date d'origine du champs (STAMP)
   set seconds [fstdstamp toseconds [fstdfield define $Field -DATEV]]

   #----- Retour de la date de validitee
   return "[DateStuff::StringDateFromSeconds $seconds 0] / [DateStuff::StringDateFromSeconds $seconds 1]"
}

proc Macro::OA4Pan::Header { Date } {

   #----- Affichage du logo
   image create photo FLAG -file  /users/dor/afsd/003/ozone_test/mapper/environment_canada_logo.gif
   $Page::Data(Canvas) create image 6 7 -image FLAG -anchor nw

   catch { font create FONTTITLE -family arial -weight bold -size 17 -slant roman -underline 0 -overstrike 0 }
   $Page::Data(Canvas) create text 640 60 -fill black -tag EXP -font FONTTITLE  -text "${Date} (EXPERIMENTAL)"
}

proc Macro::OA4Pan::Legend { Frame } {

   #-----  Primitives de dessin
   catch { font create FONTLEGEND -family arial -weight bold -size 14 -slant roman -underline 0 -overstrike 0 }

   $Page::Data(Canvas) create text 146.0 495.0 -text "Prévision de l'ozone en surface (modèle GEM-MACH)\nSurface ozone forecast (GEM-MACH model)" \
      -fill black -font FONTLEGEND -anchor nw

   $Page::Data(Canvas) create text 754.0 495.0 -text "Analyse objective de l'ozone en surface\nSurface ozone objective analysis" \
      -fill black -font FONTLEGEND -anchor nw

   $Page::Data(Canvas) create text 186.0 965.0 -text "Incrément d'analyse (correction au modèle)\nAnalysis increment (model correction)" \
      -fill black -font FONTLEGEND -anchor nw

   $Page::Data(Canvas) create text 794.0 960.0 -text "Observation d'ozone en surface\nSurface ozone Observations\n(Source: U.S. EPA AIRNOW)" \
      -fill black -font FONTLEGEND -anchor nw
}

proc Macro::OA4Pan::Execute { } {
   global env
   variable Param

   ProjCam::Set OA4Pan { 0.0 0.0 1.0 } {0.05923556991113432 0.002897102696598401 1.9982398279237654} {0.9982435749050949 -0.001128795293248666 -0.05923251625486034} 1.189207115002721 87.19999999999997 3.4000000000000004 1 0 0 0 47.740875244140625 -100.423828125

   colormap create CMAP
   colormap read CMAP $env(HOME)/.spi/Colormap/REC_Col.std1.rgba

   fstdfile open 1 read $Param(Model)

   fstdfield read O3FLD  1 -1 "" -1 -1 -1 "" "O3"
   fstdfield read OAFLD  1 -1 "" -1 -1 -1 "" "OA"
   fstdfield read INCFLD 1 -1 "" -1 -1 -1 "" "INCR"

   catch { font create FONTCB -family arial -weight bold -size 10 -slant italic -underline 0 -overstrike 0 }

   fstdfield define O3FLD -NOMVAR _O3
   fstdfield configure O3FLD -color black -intervals "0 10 20 30 40 50 60 70 80 90 100 110 120"  \
	   -rendertexture 1 -colormap CMAP -value INTEGER 0 -font FONTCB -unit ppbv

   fstdfield configure OAFLD -color black -intervals "0 10 20 30 40 50 60 70 80 90 100 110 120" \
	   -rendertexture 1 -colormap CMAP -value INTEGER 0 -font FONTCB -unit ppbv

   fstdfield configure INCFLD -color black -intervals "-50 -40 -30 -20 -10 0 10 20 30 40 50"  \
	   -rendertexture 1 -colormap CMAP -value INTEGER 0 -font FONTCB -unit ppbv

   Viewport::Assign $Page::Data(Frame) $4Big::Data(Viewport001) O3FLD
   Viewport::Assign $Page::Data(Frame) $4Big::Data(Viewport002) OAFLD
   Viewport::Assign $Page::Data(Frame) $4Big::Data(Viewport003) INCFLD

   #----- Observation
   set stamp [fstdstamp todate [fstdfield define O3FLD -DATEV]]
   set desc  [lindex $stamp 0][lindex $stamp 1][lindex $stamp 2][lindex $stamp 3]
   set obs   [observation load $Param(Obs)]
   set obs   [lindex $obs [lsearch $obs *$desc*]]

   observation configure $obs -color black -intervals "0 10 20 30 40 50 60 70 80 90 100 110 120"  \
      -icon CIRCLE -rendertexture 1 -colormap CMAP -value INTEGER 0 -size 6 -font FONTCB -unit ppbv

   Viewport::Assign $Page::Data(Frame) $4Big::Data(Viewport004) $obs

   #----- Affichage
   Macro::OA4Pan::Header [GetDateString OAFLD]
   Macro::OA4Pan::Legend $Page::Data(Frame)

   ProjCam::Select $Page::Data(Frame) $Page::Data(Frame) OA4Pan True

   Page::UpdateCommand $Page::Data(Frame)

   if { $SPI::Param(Batch) } {
      PrintBox::Image $Page::Data(Frame) png OA_4PAN.png
      SPI::Quit
   }
}

proc Macro::OA4Pan::Clean { } {

   Viewport::UnAssign $Page::Data(Frame) $4Big::Data(Viewport001)
   Viewport::UnAssign $Page::Data(Frame) $4Big::Data(Viewport002)
   Viewport::UnAssign $Page::Data(Frame) $4Big::Data(Viewport003)

   font delete FONTCB
   font delete FONTLEGEND
   font delete FONTTITLE

   catch {
      fstdfile close VERIF_REGFILE
      fstdfield free O3FLD OAFLD INCFLD
   }
}

proc Macro::OA4Pan::Args { } {
   global argv argc

   #----- Lire les parametres si il y en a
   if { $argc>0 } { set Macro::OA4Pan::Param(Model) [lindex $argv 0] }
   if { $argc>1 } { set Macro::OA4Pan::Param(Obs)   [lindex $argv 1] }
}

