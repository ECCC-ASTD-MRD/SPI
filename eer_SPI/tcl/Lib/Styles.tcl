#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de fonctions Tcl.
# Fichier  : Styles.tcl
# Creation : Juillet 2014 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Gestion des styles
#
# Fonctions:
#
# Remarques :
#
#===============================================================================

package provide Styles 1.0

catch { SPI::Splash "Loading Package Styles 1.0" }

namespace eval Styles { } {
   variable Lbl
   variable Msg
   variable Data
   
   set Data(Name)  ""
   set Data(Names) {}
   
   #----- Definitions des labels
   set Lbl(Yes)       { "Oui" "Yes" }
   set Lbl(No)        { "Non" "No" }

   #----- Definitions des textes
   set Msg(Del)   { "Voulez-vous vraiment supprimer ce style ?"
                    "Do you really want to delete this style ?" }

   set Msg(Exist) { "Ce style déja, voulez-vous l'écraser ?"
                    "This style already exists do you wish to overwrite it ?" }
   set Msg(Save)  { "Sauvegarder le style courant" "Save the current style" }
                    
}

#----------------------------------------------------------------------------
# Nom      : <Styles::Read>
# Creation : Juillet 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Lit le fichier de definitions de styles
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Styles::Read { } {
   global env
   variable Data

   set Data(Styles) {}

   set paths $env(HOME)/.spi
   if { [info exists env(SPI_TOOL)] } {
      set paths [concat [split $env(SPI_TOOL) :] $paths]
   }
   
   foreach path $paths {
      if { [file exists $path/Style] } {
         source $env(HOME)/.spi/Style
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Styles::Write>
# Creation : Juillet 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Ecrit un style dans un fichier
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Styles::Write { File Spec } {

   puts $File "\n#----- Style $Spec"

   set cmap [dataspec configure $Spec -colormap]
   if { [colormap is $cmap] && [lsearch -exact $cmaps $cmap]==-1 } {
      lappend cmaps $cmap
      puts $File "\ncatch { colormap create $cmap }"
      puts $File "colormap control $cmap -del"
      puts $File "colormap control $cmap -list { [colormap control $cmap -list] }"
      puts $File "colormap configure $cmap  -RGBAratio [colormap configure $cmap -RGBAratio] -MMratio [colormap configure $cmap -MMratio] \
         -curve red [colormap configure $cmap -curve red] -curve green [colormap configure $cmap -curve green] -curve blue [colormap configure $cmap -curve blue] -curve alpha [colormap configure $cmap -curve alpha] \
         -invertx red [colormap configure $cmap -invertx red] -invertx green [colormap configure $cmap -invertx green] -invertx blue [colormap configure $cmap -invertx blue] -invertx alpha [colormap configure $cmap -invertx alpha] \
         -inverty red [colormap configure $cmap -inverty red] -inverty green [colormap configure $cmap -inverty green] -inverty blue [colormap configure $cmap -inverty blue] -inverty alpha [colormap configure $cmap -inverty alpha] \
         -min red [colormap configure $cmap -min red] -min green [colormap configure $cmap -min green] -min blue [colormap configure $cmap -min blue] -min alpha [colormap configure $cmap -min alpha] \
         -max red [colormap configure $cmap -max red] -max green [colormap configure $cmap -max green] -max blue [colormap configure $cmap -max blue] -max alpha [colormap configure $cmap -max alpha] \
         -interp [colormap configure $cmap -interp]"
   }

   set font [dataspec configure $Spec -font]
   if { $font!="" && [lsearch -exact $fonts $font]==-1 } {
      lappend fonts $font
      puts $File "\ncatch { font create $font }"
      puts $File "font configure $font -family [font configure $font -family] -weight [font configure $font -weight] -size [font configure $font -size]\
            -slant [font configure $font -slant] -underline [font configure $font -underline] -overstrike [font configure $font -overstrike]"
   }

   puts $File "\ndataspec create \"$Spec\" -factor [dataspec configure $Spec -factor] -value [dataspec configure $Spec -value]\
      -size [dataspec configure $Spec -size] -width [dataspec configure $Spec -width] -unit \"[dataspec configure $Spec -unit]\"\
      -desc \"[dataspec configure $Spec -desc]\" -icon \"[dataspec configure $Spec -icon]\" -mark \"[dataspec configure $Spec -mark]\"\
      -color \"[dataspec configure $Spec -color]\" -fill \"[dataspec configure $Spec -fill]\" -activefill \"[dataspec configure $Spec -activefill]\"\
      -outline \"[dataspec configure $Spec -outline]\" -activeoutline \"[dataspec configure $Spec -activeoutline]\" \
      -transparency \"[dataspec configure $Spec -transparency]\" -dash \"[dataspec configure $Spec -dash]\" \
      -rendercontour [dataspec configure $Spec -rendercontour] -rendervector [dataspec configure $Spec -rendervector]\
      -rendertexture [dataspec configure $Spec -rendertexture] -rendervolume [dataspec configure $Spec -rendervolume] -renderparticle [dataspec configure $Spec -renderparticle]\
      -rendercoord [dataspec configure $Spec -rendercoord] -rendervalue [dataspec configure $Spec -rendervalue] -renderlabel [dataspec configure $Spec -renderlabel]\
      -rendergrid [dataspec configure $Spec -rendergrid] -min \"[dataspec configure $Spec -min]\" -max \"[dataspec configure $Spec -max]\"\
      -intervals \"[dataspec configure $Spec -intervals]\" -interlabels \"[dataspec configure $Spec -interlabels]\" -intervalmode [dataspec configure $Spec -intervalmode] \
      -interpdegree \"[dataspec configure $Spec -interpdegree]\" -extrapdegree \"[dataspec configure $Spec -extrapdegree]\" \
      -sample \"[dataspec configure $Spec -sample]\" -stipple \"[dataspec configure $Spec -stipple]\" -colormap \"[dataspec configure $Spec -colormap]\" -font \"[dataspec configure $Spec -font]\" \
      -texsample \"[dataspec configure $Spec -texsample]\" -texsize \"[dataspec configure $Spec -texsize]\" -texres \"[dataspec configure $Spec -texres]\" \
      -interpolation \"[dataspec configure $Spec -interpolation]\" -topography \"[dataspec configure $Spec -topography]\" -topographyfactor \"[dataspec configure $Spec -topographyfactor]\" \
      -mask \"[dataspec configure $Spec -mask]\" -light \"[dataspec configure $Spec -light]\" -labelvar \"[dataspec configure $Spec -labelvar]\" \
      -sizevar \"[dataspec configure $Spec -sizevar]\" -mapvar \"[dataspec configure $Spec -mapvar]\" \
      -extrude \"[dataspec configure $Spec -extrude]\" -extrudefactor \"[dataspec configure $Spec -extrudefactor]\""
}

proc Styles::Save { { Style "" } } {
   global env
   variable Data
   variable Lbl
   variable Msg

   #----- Check if it exists
   if { $Style!="" && [set idx [lsearch -exact $Data(Names) $Style]]!=-1 } {
      if { [set nok [Dialog::Default . 200 WARNING $Msg(Exist) "" 1 $Lbl(Yes) $Lbl(No)]] } {
         return
      }
   }
   
   #----- Backup previous
   if { [file exists $env(HOME)/.spi/Style] } {
      file rename -force $env(HOME)/.spi/Style $env(HOME)/.spi/Style.old
   }
   
   #----- Save all styles
   set f [open $env(HOME)/.spi/Style w]
   
   foreach name $Data(Names) {
      Styles::Write $f $name
   }
   
   close $f
}

proc Styles::Delete { Style } {
   variable Data
   variable Lbl
   variable Msg
   
   if { [Dialog::Default . 200 WARNING $Msg(Del) "\n\n\t$Data(Name)" 0 $Lbl(No) $Lbl(Yes)] } {
      if { [set idx [lsearch -exact $Data(Names) $Style]]!=-1 } {
         set Data(Names) [lreplace $Data(Names) $idx $idx]
      }
      
      Styles::Save
   }
}

proc Styles::Widget { Frame Command } {

   frame $Frame.styles
      ComboBox::Create $Frame.styles.name Styles::Data(Name) edit sorted nodouble -1 $Styles::Data(Names) 5 6 $Command
      button $Frame.styles.save   -image PALETTESAVE  -relief flat -bd 1 -overrelief raised \
         -command "Styles::Save \[Dialog::Get . \$Styles::Msg(Save) \$Styles::Data(Name)\]"
      button $Frame.styles.del    -image PALETTEDEL   -relief flat -bd 1 -overrelief raised \
         -command "Styles::Delete $Frame.cam.sel"
      pack $Frame.styles.name -side left -fill x -expand True -padx 2
      pack $Frame.styles.save $Frame.styles.del -side left -padx 2
   
   return $Frame.styles
}
