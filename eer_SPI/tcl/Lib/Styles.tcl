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
#   Styles::Read     { }
#   Styles::Save     { Frame { Style "" } }
#   Styles::Delete   { Frame Style }
#   Styles::Apply    { Style }
#   Styles::Overview { Frame X Y } 
#   Styles::Widget   { Frame { Update False } }
#   Styles::Write    { Channel Spec { Style "" } }
#
# Remarques :
#
#===============================================================================

package provide Styles 1.0

catch { SPI::Splash "Loading Package Styles 1.0" }

namespace eval Styles { } {
   variable Lbl
   variable Msg
   variable Bubble
   variable Data
   
   set Data(Style)    ""               ;# Current style
   set Data(Styles)   {}               ;# List of styles
   set Data(Overview) ""               ;# Current style overview
   
   #----- Definitions des labels
   set Lbl(Yes)       { "Oui" "Yes" }
   set Lbl(No)        { "Non" "No" }
   set Lbl(Save)      { "Sauvegarder le style courant" "Save current style" }
   set Lbl(Del)       { "Supprimer le style courant"   "Delete current style" }

   #----- Definitions des textes
   set Msg(Del)   { "Voulez-vous vraiment supprimer ce style ?"
                    "Do you really want to delete this style ?" }

   set Msg(Exist)    { "Ce style déja, voulez-vous l'écraser ?" "This style already exists do you wish to overwrite it ?" }
   set Msg(Save)     { "Sauvegarder le style courant"           "Save the current style" }
   set Msg(Name)     { "Nom du style:"                           "Style name: " }
   set Msg(NotValid) { "Ce style n'est pas valide"              "This is not a valid style" }
   
   set Bubble(Style) { "Styles définissant les paramêtres de rendue des données" "Styles defining the rendering parameters of the data" }
}

#----------------------------------------------------------------------------
# Nom      : <Styles::Read>
# Creation : Juillet 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Read style definitions
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
   
   #----- Loop on possible paths
   foreach path $paths {
      foreach file [glob -nocomplain -tails -directory $path/Style *.tcl] {

         set style [file rootname $file]
         
         source $path/Style/$file
         
         #----- Check for overview
         if { [file exists $path/Style/$style.png] } {
            set Data(Overview$style) $path/Style/$style.png
         }
         lappend Data(Styles) $style
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <Styles::Save>
# Creation : Aout 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Save a style
#
# Parametres :
#   <Frame>  : Widget's frame
#   <Style>  : Style name
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Styles::Save { Frame { Style "" } } {
   global env
   variable Data
   variable Lbl
   variable Msg

   if { $Style!="" } { 
      
      #----- Check if valid
      if { ![dataspec is $FSTD::Param(Spec)] } {
         Dialog::Error . $Msg(NotValid)
         return
      }
      
      #----- Clear bad char
      set Style [string map { " " _ } $Style]
      
      #----- Check if it exists
      if { [file exist $env(HOME)/.spi/Style/$Style.tcl] } {
         if { [set nok [Dialog::Default . 200 WARNING $Msg(Exist) "" 1 $Lbl(Yes) $Lbl(No)]] } {
            return
         }
      }
      
      #----- Save the style
      set f [open $env(HOME)/.spi/Style/$Style.tcl w]
      
      Styles::Write $f $FSTD::Param(Spec) $Style
      
      close $f
      
      #----- Reload the styles
      Styles::Read
      Styles::Widget $Frame True
   }
}

#----------------------------------------------------------------------------
# Nom      : <Styles::Delete>
# Creation : Aout 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Delete a style
#
# Parametres :
#   <Frame>  : Widget's frame
#   <Style>  : Style name
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Styles::Delete { Frame Style } {
   global env
   variable Data
   variable Lbl
   variable Msg
   
   if { [Dialog::Default . 200 WARNING $Msg(Del) "\n\n\t$Style" 0 $Lbl(No) $Lbl(Yes)] } {
      if { [set idx [lsearch -exact $Data(Styles) $Style]]!=-1 } {
         set Data(Styles) [lreplace $Data(Styles) $idx $idx]
      }
      
      Styles::Widget $Frame True
      
      file delete $env(HOME)/.spi/Style/$Style.tcl
   }
}

#----------------------------------------------------------------------------
# Nom      : <Styles::Apply>
# Creation : Aout 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Apply style
#
# Parametres :
#   <Style>  : Style name
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Styles::Apply { Style } {
   variable Data
   
   set Data(Style) $Style
   
   FSTD::ParamGet $Style
   FSTD::ParamPut 
   FSTD::ParamSet

   SPI::ParamsApply
}

#----------------------------------------------------------------------------
# Nom      : <Styles::Overview>
# Creation : Aout 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Display an overview of the style if it exists
#
# Parametres :
#   <Frame>  : Frame into which to build the widget
#   <X>      : X coordinate of the cursor
#   <Y>      : Y coordinate of the cursor
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Styles::Overview { Frame X Y } {
   global env
   variable Data
   
   if { [set item [$Frame.styles.menu index active]]!="" && $item>2 } {
      set style [$Frame.styles.menu entrycget $item -label]
      
      if { $style!="" && [info exist ::Styles::Data(Overview$style)] } {
         if { $style!=$Data(Overview) } {
            set Data(Overview) $style
            STYLE_OVERVIEW read $Data(Overview$style)
         }
         wm deiconify .style;
         wm geometry .style +[expr [winfo rootx $Frame.styles.menu]+[winfo width $Frame.styles.menu]+10]+$Y
      } else {
         wm withdraw  .style
      }   
   }
}

#----------------------------------------------------------------------------
# Nom      : <Styles::Widget>
# Creation : Aout 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Build style interface wigdet
#
# Parametres :
#   <Frame>  : Frame into which to build the widget
#   <Update> : Simply update style list
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Styles::Widget { Frame { Update False } } {
   global GDefs
   variable Data
   variable Lbl
   variable Bubble

   if { $Update } {
      $Frame.styles.menu delete 3 end
   } else {
      menubutton $Frame.styles -image PALETTE -menu $Frame.styles.menu -bd 1 -relief groove -bd 2 
      menu $Frame.styles.menu -tearoff 0 -bd 1
      $Frame.styles.menu add command -label [lindex $Lbl(Save) $GDefs(Lang)] -command "Styles::Save $Frame \[Dialog::Get . \$Styles::Msg(Save) \$Styles::Msg(Name)\]"
      $Frame.styles.menu add command -label [lindex $Lbl(Del) $GDefs(Lang)]  -command "Styles::Delete $Frame \$Styles::Data(Style)"
      $Frame.styles.menu add separator
      
      toplevel            .style
      wm withdraw         .style
      wm overrideredirect .style true
      wm attributes       .style -type splash -alpha 0.85
      
      image create photo STYLE_OVERVIEW
      label .style.overview -image STYLE_OVERVIEW -width 256 -height 256 -relief ridge -bd 2
      pack .style.overview    
      
      bind $Frame.styles.menu <Motion>  "Styles::Overview $Frame %X %Y"
      bind $Frame.styles.menu <Leave>   "wm withdraw .style"
   }
   
   Bubble::Create $Frame.styles $Bubble(Style)
   
   #----- Insert styles in menu
   set n 0
   foreach style $Data(Styles) {
      $Frame.styles.menu add command -columnbreak [expr $n>19] -label $style -command "Styles::Apply $style"  
      
      if { [incr n]>20 } {
         set n 0
      }
   }

   return $Frame.styles
}

#----------------------------------------------------------------------------
# Nom      : <Styles::Write>
# Creation : Juillet 2014 - J.P. Gauthier - CMC/CMOE
#
# But      : Ecrit un style dans un fichier
#
# Parametres :
#   <Channel>: Channel channel to write to
#   <Spec>   : Dataspec definition
#   <Style>  : Style name, otherwise, use Spec name
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Styles::Write { Channel Spec { Style "" } } {
   
   set fonts {}
   set cmaps {}

   if { $Style=="" } {
      set Style $Spec
   }
   
   puts $Channel "\n#----- Style $Style"
   
   set cmap [dataspec configure $Spec -colormap]
   if { [colormap is $cmap] && [lsearch -exact $cmaps CMAP$Style]==-1 } {
      lappend cmaps CMAP$Style
      puts $Channel "\ncatch { colormap create CMAP$Style }"
      puts $Channel "colormap control CMAP$Style -del"
      puts $Channel "colormap control CMAP$Style -list { [colormap control $cmap -list] }"
      puts $Channel "colormap configure CMAP$Style  -RGBAratio [colormap configure $cmap -RGBAratio] -MMratio [colormap configure $cmap -MMratio] \
         -curve red [colormap configure $cmap -curve red] -curve green [colormap configure $cmap -curve green] -curve blue [colormap configure $cmap -curve blue] -curve alpha [colormap configure $cmap -curve alpha] \
         -invertx red [colormap configure $cmap -invertx red] -invertx green [colormap configure $cmap -invertx green] -invertx blue [colormap configure $cmap -invertx blue] -invertx alpha [colormap configure $cmap -invertx alpha] \
         -inverty red [colormap configure $cmap -inverty red] -inverty green [colormap configure $cmap -inverty green] -inverty blue [colormap configure $cmap -inverty blue] -inverty alpha [colormap configure $cmap -inverty alpha] \
         -min red [colormap configure $cmap -min red] -min green [colormap configure $cmap -min green] -min blue [colormap configure $cmap -min blue] -min alpha [colormap configure $cmap -min alpha] \
         -max red [colormap configure $cmap -max red] -max green [colormap configure $cmap -max green] -max blue [colormap configure $cmap -max blue] -max alpha [colormap configure $cmap -max alpha] \
         -interp [colormap configure $cmap -interp]"
   }

   set font [dataspec configure $Spec -font]
   if { $font!="" && [lsearch -exact $fonts FONT$Style]==-1 } {
      lappend fonts $font
      puts $Channel "\ncatch { font create FONT$Style }"
      puts $Channel "font configure FONT$Style -family [font configure $font -family] -weight [font configure $font -weight] -size [font configure $font -size]\
            -slant [font configure $font -slant] -underline [font configure $font -underline] -overstrike [font configure $font -overstrike]"
   }

   puts $Channel "\ndataspec create \"$Style\" -factor [dataspec configure $Spec -factor] -value [dataspec configure $Spec -value]\
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
      -sample \"[dataspec configure $Spec -sample]\" -stipple \"[dataspec configure $Spec -stipple]\" -colormap \"CMAP$Style\" -font \"FONT$Style\" \
      -texsample \"[dataspec configure $Spec -texsample]\" -texsize \"[dataspec configure $Spec -texsize]\" -texres \"[dataspec configure $Spec -texres]\" \
      -interpolation \"[dataspec configure $Spec -interpolation]\" -topography \"[dataspec configure $Spec -topography]\" -topographyfactor \"[dataspec configure $Spec -topographyfactor]\" \
      -mask \"[dataspec configure $Spec -mask]\" -light \"[dataspec configure $Spec -light]\" -labelvar \"[dataspec configure $Spec -labelvar]\" \
      -sizevar \"[dataspec configure $Spec -sizevar]\" -mapvar \"[dataspec configure $Spec -mapvar]\" -mapall \"[dataspec configure $Spec -mapall]\" \
      -mapabove [dataspec configure $Spec -mapabove] -mapbelow [dataspec configure $Spec -mapbelow] -extrude \"[dataspec configure $Spec -extrude]\" -extrudefactor \"[dataspec configure $Spec -extrudefactor]\""
}

Styles::Read
