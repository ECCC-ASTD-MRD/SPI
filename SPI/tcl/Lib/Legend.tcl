#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de fonctions Tcl relatives aux enregistrements info
# Fichier  : Legend.tcl
# Creation : Aout 2018 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Definitions de diverses fonctionnalites relatives a la creatin de grille.
#
# Fonctions:
#
# Remarques :
#
#===============================================================================

package provide Legend 1.0

catch { SPI::Splash "Loading Package Legend 1.0" }


namespace eval Legend {
   global GDefs
   variable Param
   
   set Param(Pading)    2      ;# Space between text and box border
   set Param(Spacing)   5      ;# Space between box and page border
   set Param(LineSpace) -2     ;# Space between lines
   set Param(LWLEGEND_LL)      0
   set Param(LWLEGEND_UL)      0
   set Param(LWLEGEND_LR)      0
   set Param(LWLEGEND_UR)      0
   
   image create photo LEGENDLOGO -file $GDefs(Dir)/share/image/Symbol/Logo/Flag.gif
}

proc Legend::Add { Frame Object Mode args } {
   variable Param

   set width  [Page::CanvasWidth $Frame]
   set height [Page::CanvasHeight $Frame]
   set bbox   [$Frame.page.canvas bbox LEGEND_${Mode}INFO]
   
   set Param(LW$Mode) 0
   
   if { ![llength $bbox] } {
      #----- this is the first line
      switch $Mode {
         "UL" { set ibox { [expr $Param(LWLEGEND_$Mode)+6+$Param(Spacing)]        [expr 2+$Param(Spacing)]         }; set a nw }
         "UR" { set ibox { [expr $width-6-$Param(Spacing)] [expr 2+$Param(Spacing)]         }; set a ne }
         "LL" { set ibox { [expr $Param(LWLEGEND_$Mode)+6+$Param(Spacing)]        [expr $height-2-$Param(Spacing)] }; set a sw }
         "LR" { set ibox { [expr $width-6-$Param(Spacing)] [expr $height-2-$Param(Spacing)] }; set a se }
      }
      if { $Object=="LOGO" } {
         if { !$Param(LWLEGEND_$Mode) } {
            eval $Frame.page.canvas create image $ibox -image LEGENDLOGO -anchor $a -tag \"LEGEND LEGEND_INFO\"
          }
          set Param(LWLEGEND_$Mode) [expr [image width LEGENDLOGO]+$Param(Spacing)]
     }
   } else {
      #----- More than 1 line
      switch $Mode {
         "UL" { set ibox { [expr $Param(LWLEGEND_$Mode)+[lindex $bbox 0]+3] [expr [lindex $bbox 3]+$Param(LineSpace)] }; set a nw }
         "UR" { set ibox { [expr [lindex $bbox 2]-3] [expr [lindex $bbox 3]+$Param(LineSpace)] }; set a ne }
         "LL" { set ibox { [expr $Param(LWLEGEND_$Mode)+[lindex $bbox 0]+3] [expr [lindex $bbox 1]-$Param(LineSpace)] }; set a sw }
         "LR" { set ibox { [expr [lindex $bbox 2]-3] [expr [lindex $bbox 1]-$Param(LineSpace)] }; set a se }
      }
   }
   
   #----- If this legend frame does not exists yet
   if { [set bbox [$Frame.page.canvas bbox LEGEND_${Mode}]]=="" } {
      $Frame.page.canvas create rectangle 0 0 0 0 -fill white -outline black -transparency 90 -width 1 -tag "LEGEND LEGEND_${Mode}" 
   }
   if { [llength $args] } {
      eval $Frame.page.canvas create $Object $ibox -anchor $a -tag \"LEGEND LEGEND_INFO LEGEND_${Mode}INFO\" $args
   }
   Legend::Fit $Frame LEGEND_${Mode} 
}

proc Legend::Delete { Frame { Tags LEGEND_INFO } } {
   variable Param

   set Param(LWLEGEND_LL)      0
   set Param(LWLEGEND_UL)      0
   set Param(LWLEGEND_LR)      0
   set Param(LWLEGEND_UR)      0
   
   eval $Frame.page.canvas delete $Tags
}

proc Legend::Fit { Frame { Tags { LEGEND_UL LEGEND_UR LEGEND_LR LEGEND_LL } } } {
   variable Param
   
   foreach tag $Tags {
      if { [set bbox [$Frame.page.canvas bbox ${tag}INFO]]!="" } {
         $Frame.page.canvas coords $tag [list [expr [lindex $bbox 0]-$Param(Pading)-$Param(LW$tag)] [expr [lindex $bbox 1]-$Param(Pading)] [expr [lindex $bbox 2]+$Param(Pading)] [expr [lindex $bbox 3]+$Param(Pading)]]
      }
   }
}
