#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Interface pour la gestion des experiences.
# Fichier    : <SPI.tcl>
# Creation   : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# Description: Interface de visualisation et de manipulation des champs et
#              autres types de donnees.
#
# Remarques  :
#
#============================================================================

set GDefs(Dir)     [file normalize [file dirname [info script]]/..]
set GDefs(Version) $env(SPI_VERSION)$env(SPI_STATE)

package require TclSystem
package require Logger

Log::Start SPI $GDefs(Version)

source $GDefs(Dir)/tcl/SPI.txt
source $GDefs(Dir)/tcl/SPI.ctes
source $GDefs(Dir)/tcl/SPI.int

#---------------------------------------------------------------------------
# Nom      : <SPI::Setup>
# Creation : Decembre 2009 - J.P. Gauthier - CMC/CMOE
#
# But      :Initialiser les repertoires de definitions de SPI (~/.spi)
#
# Parametres :
#   <Force>  : Forcer l'execution du setup
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SPI::Setup { { Force False } } {
   global env GDefs

   if { $Force || ![file exists $env(HOME)/.spi] } {

      SPI::Splash "Setting up SPI $GDefs(Version) for the first time"

      file mkdir $env(HOME)/.spi $env(HOME)/.spi/Trace $env(HOME)/.spi/Tmp $env(HOME)/.spi/Layout $env(HOME)/.spi/Colormap $env(HOME)/.spi/Scenario $env(HOME)/.spi/Macro

      #----- Installer les fichiers de definitions
      if { ![file exists $env(HOME)/.spi/SPI] } {
         file copy -force $GDefs(Dir)/tcl/Setup/SPI $env(HOME)/.spi/SPI
      }

      #----- Copy standard stuff
      foreach file { Colormap Scenario } {
         exec cp -r $GDefs(Dir)/tcl/Setup/$file $env(HOME)/.spi
      }

      #----- Copy old users definitions
      if { [file exists $env(HOME)/.eer_ToolDefs] } {
         foreach fileold { eer_FieldCalc eer_FileBoxPath eer_Host eer_ObsModel eer_ProjCam eer_SatDomain } filenew { FieldCalc FileBox HFManager ObsModel ProjCam SatData } {
            catch { file copy -force $env(HOME)/.eer_ToolDefs/$fileold $env(HOME)/.spi/$filenew }
         }
         foreach fileold { eer_Layout eer_Scenario Macro } filenew { Layout Scenario Macro } {
            catch { eval exec cp -r [glob $env(HOME)/.eer_ToolDefs/${fileold}/*] $env(HOME)/.spi/${filenew} }
         }

         catch { file copy $env(HOME)/.eer_ToolDefs/Mapper/Params $env(HOME)/.spi/Mapper }
      }
   }
}

#----- Do setup if not done

SPI::Setup

#----- Lire la liste des definitions communes
catch { source $env(HOME)/.spi/SPI }

#---------------------------------------------------------------------------
# Nom      : <SPI::CommandLine>
# Creation : Decembre 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher la liste des arguments et leur description
#            aux type de ces arguments
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SPI::CommandLine { { Args {} }} {
   global GDefs

   Log::Print ERROR "Wrong arguments $Args, must be:\n
      \[-tclsh ...\]                    : Launch a tcl script through SPI's environment (No Tk)
      \[-soft\]                         : Force software OpenGL mode
      \[-hard\]                         : Force hardware OpenGL mode
      \[-nothreads\]                    : Don't use multithreading
      \[-batch\]                        : Launch in batch mode (No screen rendering)
      \[-setup\]                        : Force initial setup (~/.spi)
      \[-default ... ...\]              : Use the file specified as the default parameter definition
      \[-lang 0|1\]                     : Select language (0 Francais, 1 English)
      \[-model\]                        : Open the model tab upon startup
      \[-nowindow\]                     : Does no open the main window (Use to only launch a tool)
      \[-field ... ...\]                : Open the specified standard files
      \[-traj ... ...\]                 : Open the specified trajectory files
      \[-obs ...\]                      : Open the specified observation files
      \[-metobs ...\]                   : Open the specified BURP or BUFR observation files
      \[-icon ... ...\]                 : Open the specified icon files
      \[-macro ... ...\]                : Run the specified macro script
      \[-args ... ...\]                 : Arguments to be used by the previously specified script
      \[-layout ...\]                   : Use the specified layout
      \[-project ...\]                  : Use the specified spi project file
      \[-tool ... ...\]                 : Start a specific tool
      \[-pane WxH ...\]                 : Dimensions of the secondary panes
      \[-side left|right|top|bottom \]  : Side on wich to put the secondary panes
      \[-geom WxH+X+Y\]                 : Global window size"
}

#----- Parcourir la liste des parametres pre-launch

for { set i 0 } { $i < $argc } { incr i } {
   switch -exact [string trimleft [lindex $argv $i] "-"] {
      "valgrind"  { }
      "soft"      { }
      "hard"      { }
      "setup"     { SPI::Setup True; catch { source $env(HOME)/.spi/SPI } }
      "nothreads" { set SPI::Param(Threads) False }
      "batch"     { set SPI::Param(Batch) True }
      "model"     { set SPI::Param(Exp) True }
      "nowindow"  { set SPI::Param(Window) False }
      "geom"      { set i [Args::ParseDo $argv $argc $i 0 1 "set SPI::Param(Geom)"] }
      "lang"      { set GDefs(Lang) [lindex $argv [incr i]] }
      "default"   { set i [Args::ParseDo $argv $argc $i 1 1 "set SPI::Param(Default)"] }
      "field"     { set i [Args::ParseDo $argv $argc $i 1 0 ""] }
      "traj"      { set i [Args::ParseDo $argv $argc $i 1 0 ""] }
      "obs"       { set i [Args::ParseDo $argv $argc $i 1 0 ""] }
      "metobs"    { set i [Args::ParseDo $argv $argc $i 1 0 ""] }
      "icon"      { set i [Args::ParseDo $argv $argc $i 1 0 ""] }
      "args"      { set i [Args::ParseDo $argv $argc $i 1 0 ""] }
      "script"    { set i [Args::ParseDo $argv $argc $i 1 1 ""] }
      "macro"     { set i [Args::ParseDo $argv $argc $i 1 1 ""] }
      "pane"      { set i [Args::ParseDo $argv $argc $i 0 1 ""] }
      "side"      { set i [Args::ParseDo $argv $argc $i 0 1 ""] }
      "layout"    { set i [Args::ParseDo $argv $argc $i 0 1 ""] }
      "project"   { set i [Args::ParseDo $argv $argc $i 0 1 ""] }
      "tool"      { set i [Args::ParseDo $argv $argc $i 0 1 ""] }

      default     { SPI::CommandLine [lindex $argv $i]; Log::End 1 }
   }
}

SPI::Splash "Sourcing packages"

#----- Try for threads
catch { package require Thread }

#----- Source GL and set batch flag early for threading mechanism
package require OpenGL
glrender -xbatch $SPI::Param(Batch) -usethreads $SPI::Param(Threads)

#----- Fonctions en librairie.
package require Tktable
package require http
package require tdom
package require struct::tree

package require Icons
package require Page
package require Viewport
package require Miniport
package require Graph
package require FSTD
package require Obs
package require Trajectory
package require FieldBox
package require ObsBox
package require TrajBox
package require Dialog
package require FileBox
package require CanvasShape
package require Areas
package require TabFrame
package require InfoFrame
package require FieldCalc
package require Export
package require Animator
package require Info
package require MetData

#----- Liste des outils
foreach tool [lsort [glob $GDefs(Dir)/tcl/Tools/*]] {
   set name [file tail [file rootname $tool]]
   uplevel #0 source $tool/$name.tcl
   lappend SPI::Param(Tools) $name
}
Log::Print INFO "System: Available Tools\n   $SPI::Param(Tools)"

if { !$SPI::Param(Batch) } {

   #----- Liste des layouts
   foreach layout [glob -nocomplain $GDefs(Dir)/tcl/Layouts/*.tcl] {
      lappend SPI::Param(Layouts) [file tail [file rootname $layout]]
   }
   foreach layout [glob -nocomplain $env(HOME)/.spi/Layout/*.tcl] {
      lappend SPI::Param(Layouts) [file tail [file rootname $layout]]
   }
   Log::Print INFO "System: Available Layouts\n   $SPI::Param(Layouts)"

   #----- Fichiers complementaires
   source $GDefs(Dir)/tcl/Models/Model.tcl
}

#-------------------------------------------------------------------------------
# Nom      : <Page::Activate>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer la selection apres un changement de page
#            de l'usager.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <Force>  : Force la reactivation
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Page::Activate { Frame { Force 0 } } {
   global GDefs
   variable Data

   if { $Frame=="" || ($Page::Data(Frame)==$Frame && !$Force) } {
      return
   }

   #----- Configurer le frame
   if { [winfo exists $Page::Data(Frame)] } {
      $Page::Data(Frame) configure -background black
      catch { place forget [winfo toplevel $Page::Data(Frame)].active }
      place forget .active
      Page::Update $Page::Data(Frame)
   }
   $Frame configure -background $GDefs(ColorHighLight)

   #----- Dans le cas de fenetres externes
   set top [winfo toplevel $Frame]
   set own True
   if { $top=="." } {
      place .active -in $Frame -relx 0.0 -rely 1.0 -anchor sw
      raise .active
   } else {
      if { [winfo exists $top.active] } {
         place $top.active -in $Frame -relx 0.0 -rely 1.0 -anchor sw
         raise $top.active
      } else {
         set own False
      }
   }

   if { [info exists ProjCam::Data${Frame}::Cam(Name)] } {
      upvar #0 ProjCam::Data${Frame}::Cam(Name) name
      set ProjCam::Data(Name) $name
   }

   #----- Place previous page at end of list
   if { $Page::Data(Frame)!="" && [set idx [lsearch -exact $Page::Data(Frames) $Page::Data(Frame)]]>-1 } {
      set Page::Data(Frames) [lreplace $Page::Data(Frames) $idx $idx]
      lappend Page::Data(Frames) $Page::Data(Frame)
   }

   #----- Initial acitve page variables
   set Page::Data(Frame)   $Frame
   set Page::Data(Canvas)  $Frame.page.canvas
   set Page::Data(Scale)   $Page::Data(Scale$Frame)
   set Page::Data(Full)    $Page::Data(Full$Frame)
   set Page::Data(Width)   $Page::Data(Width$Frame)
   set Page::Data(Height)  $Page::Data(Height$Frame)

   if { $own } {
      set SPI::Param(Layout)  $SPI::Data(Layout$Frame)

      #----- Update des items
      foreach item $SPI::Data(Items) {
         set SPI::Data(Show$item)  $SPI::Data(Show$item$Frame)
      }
      set SPI::Data(ShowColorBar)  [ColorBar::Active $Frame]
      set SPI::Data(ShowDataBar)   [DataBar::Active $Frame]
   }

   #----- Update des outils
   foreach tool $SPI::Param(Tools) {
      upvar #0 ${tool}::Data(Active) active
      if { $active } {
         eval set proc \[info procs ::${tool}::PageActivate\]
         if { $proc!="" } {
            eval ${tool}::PageActivate $Frame
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Page::UpdateCommand>
# Creation : Octobre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" apres un changement de parametres de la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#    - Cette fonctions est appele par le package Page au besoin.
#
#-------------------------------------------------------------------------------

proc Page::UpdateCommand { Frame } {

   ColorBar::Update $Frame
   DataBar::Update  $Frame
   Graph::Update    $Frame

   #----- Update des Outils
   foreach tool $SPI::Param(Tools) {
      upvar #0 ${tool}::Data(Active) active
      if { $active } {
         eval set proc \[info procs ::${tool}::Update\]
         if { $proc!="" } {
            eval ${tool}::Update $Frame
         }
      }
   }

   #----- Update des Layouts
   if { [info exists SPI::Data(Layout$Frame)] } {
      eval set proc \[info procs ::$SPI::Data(Layout$Frame)::LayoutUpdate\]
      if { $proc!="" &&  $Frame!="" } {
         eval $SPI::Data(Layout$Frame)::LayoutUpdate $Frame
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Page::UpdateItems>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectuer le "Refresh" des items de la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#    - Cette fonctions est appele par le package Page au besoin.
#    - La description de l'icone est une liste selon le format suivant:
#
#        { lat lon elev icone color group tag id }
#
#-------------------------------------------------------------------------------

proc Page::UpdateItems { Frame } {
   global GDefs

   if { ![winfo exists $Frame]} {
      return
   }

   #----- Update des Objects
   SPI::IcoDraw        $Frame
   Graph::UpdateItems  $Frame
   Export::UpdateItems $Frame

   if { [set vp [lindex [Page::Registered $Frame Viewport] 0]]!="" } {

      if { [info exists ::SPI::Data(Layout$Frame)] } {

         if { $SPI::Data(ShowClock$Frame) } {
            CVClock::Update $Frame [lindex [Viewport::Assigned $Frame $vp] 0]
         }

         if { $SPI::Data(ShowScale$Frame) } {
            CVScale::Update $Frame $vp
         }

         if { $SPI::Data(ShowCompass$Frame) } {
            set bear [$vp -bearing]
            CVCompass::Update $Frame $bear [projcam stats $Frame -angle] \
               [expr [projcam stats $Frame -dist]/[projcam configure $Frame -lens]] $Viewport::Map(Speed)
         }

         if { $SPI::Data(ShowGeoLegend$Frame) } {
            set items ""
            foreach item { Coast Lake River Polit Admin City Road Rail Util Canal Coord } {

               if { $Viewport::Map($item) >0 } {
                  lappend items [list $Viewport::Map($item) $Viewport::Resources($item) [lindex $Viewport::Lbl($item) $GDefs(Lang)]]
               }
            }
            CVGeoLegend::Update $Frame $items
         }

         #----- Update des Layouts
         if { $SPI::Data(Layout$Frame)!= "SPI" } {
            eval set proc \[info procs ::$SPI::Data(Layout$Frame)::UpdateItems\]
            if { $proc!="" } {
               eval $SPI::Data(Layout$Frame)::UpdateItems $Frame
            }
         }
      }

      #----- Update des outils
      foreach tool $SPI::Param(Tools) {
         upvar #0 ${tool}::Data(Active) active
         if { $active } {
            eval set proc \[info procs ::${tool}::UpdateItems\]
            if { $proc!="" } {
               eval ${tool}::UpdateItems $Frame
            }
         }
      }
   }
   $Frame.page.canvas raise TOP
}

#----------------------------------------------------------------------------
# Nom      : <SPI::Layout>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Initialise l'affichage de la page
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SPI::Layout { Frame } {
   global   GDefs
   global   env
   variable Title
   variable Data

   wm title [winfo toplevel $Frame] "[lindex $Title(SPI) $GDefs(Lang)] $GDefs(Version) ($env(USER) $GDefs(Host))"

   Page::Size $Frame 0 0
   Viewport::Create $Frame 1 1 1 1 1 1
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::LayoutDelete>
# Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer le layout courant.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::LayoutDelete { } {
   global   GDefs env
   variable Error
   variable Msg
   variable Lbl

   if { $SPI::Param(Layout)!="SPI" } {
      if { [file exists $GDefs(Dir)/tcl/Layouts/${SPI::Param(Layout)}.tcl] } {
         Dialog::Error . $Error(LayoutDel) "\n\n\t$SPI::Param(Layout)"
         return
      }

      if { ![Dialog::Default . 300 WARNING $Msg(LayoutErase) "\n\n\t$SPI::Param(Layout)\n" 1 $Lbl(Yes) $Lbl(No)] } {
         set idx [lsearch -exact $SPI::Param(Layouts) $SPI::Param(Layout)]
         set SPI::Param(Layouts) [lreplace $SPI::Param(Layouts) $idx $idx]
         file delete $env(HOME)/.spi/Layout/${SPI::Param(Layout)}.tcl
      } else {
         return
      }

      ComboBox::Del .bar.layout.sel $SPI::Param(Layout)
      set SPI::Param(Layout) SPI
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::LayoutLoad>
# Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire le "layout" specifie et l'initialiser.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Layout>  : Identificateur du Layout
#  <Lock>    : Bloquer les changements suivant a cette page
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::LayoutLock { Frame } {
   set Page::Data(Lock$Frame) True
}
proc SPI::LayoutUnLock { Frame } {
   set Page::Data(Lock$Frame) False
}

proc SPI::LayoutLoad { Frame Layout } {
   global   GDefs env
   global   env
   variable Title
   variable Lbl
   variable Msg
   variable Data

   #----- Invalid layout
   if { $Layout=="" } {
      return 1
   }

   #----- Current page layout is locked
   if { $Page::Data(Lock$Frame) } {
      set so [Dialog::Default . 300 INFO $Msg(Locked) "" 0 $Lbl(PageOther) $Lbl(PageNew) $Lbl(Cancel)]

      switch $so {
         0 { ;#----- Find first layout not locked
            foreach frame $Page::Data(Frames) {
               if { !$Page::Data(Lock$frame) } {
                  set Frame $frame
                  break
               }
            }
         }
         1 { ;#----- Use a new page
            set Frame [SPI::PageNew False]
         }
         2 { ;#----- Cancel the layout
            return 1
         }
      }
   }

   #----- Can't find an unlocked page
   if { $Page::Data(Lock$Frame) } {
      return 1
   }

   #----- Lire le layout
   if { $Layout=="SPI" } {
   } elseif { [file exists $Layout] && ![file isdirectory $Layout] } {
      uplevel #0 source $Layout
      set Layout [file rootname [file tail $Layout]]
   } elseif { [file exists $Layout.tcl] && ![file isdirectory $Layout.tcl] } {
      uplevel #0 source $Layout.tcl
      set Layout [file rootname [file tail $Layout]]
   } elseif { [file exists $GDefs(Dir)/tcl/Layouts/$Layout.tcl] } {
      uplevel #0 source \$GDefs(Dir)/tcl/Layouts/$Layout.tcl
   } elseif { [file exists $env(HOME)/.spi/Layout/$Layout.tcl] } {
      uplevel #0 source \$env(HOME)/.spi/Layout/$Layout.tcl
   } elseif { [namespace exists ::$Layout] && [llength [info procs ::${Layout}::Layout]] } {
   } else {
      Log::Print ERROR "Invalid Layout"
      return 0
   }

   $Frame.page.canvas config -cursor watch
   . config -cursor watch
   update idletasks

   if { [info exists  SPI::Data(Layout$Frame)] } {
      eval set proc \[info procs ::$SPI::Data(Layout$Frame)::LayoutClear\]
      if { $proc!="" } {
         eval $SPI::Data(Layout$Frame)::LayoutClear $Frame
      }
   }
   $Frame.page.canvas delete all

   Drawing::Clear $Frame

   Graph::Destroy    $Frame
   Viewport::Destroy $Frame
   Miniport::Destroy $Frame

   foreach item $SPI::Data(Items) {
      set SPI::Data(Show$item)  [set SPI::Data(Show$item$Frame) 0]
   }
   set SPI::Param(Layout)           $Layout
   set SPI::Data(Layout$Frame)      $Layout
   set ColorBar::Data(Active$Frame) 0
   set DataBar::Data(Active$Frame)  0
   catch {
      FieldBox::Select
      ObsBox::Select
      TrajBox::Select
   }

   if { $Layout!="SPI" } {
      wm title . "[lindex $Title(SPI) $GDefs(Lang)] $GDefs(Version) ($Layout) ($env(USER) $GDefs(Host))"
   } else {
      .menu.disp.menu entryconfigure end -state normal
   }

   #----- Initialiser la mise en page
   ${Layout}::Layout $Frame
   Page::Activate $Frame True
   Page::ModeSelect $Page::Data(Mode) $Frame

   $Frame.page.canvas config -cursor left_ptr
   . config -cursor left_ptr
   return 1
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::LayoutSave>
# Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder la layout courant
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <Name>   : Layout name
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::LayoutSave { Frame Name } {
   global   GDefs env
   variable Msg
   variable Lbl
   variable Data
   variable Title
   variable Param

   set oops 0

   #----- If this is a valid name
   if { $Name=="" } {
      return
   }
   regsub -all " " $Name "_" Name

   if { [file exists $env(HOME)/.spi/Layout/$Name.tcl] } {
      if { [Dialog::Default . 300 WARNING $Msg(LayoutOver) "\n\n\t$Name\n" 1 $Lbl(Yes) $Lbl(No)] } {
         return
      }
   }

   set Param(Layout) $Name
   set file [open $env(HOME)/.spi/Layout/$Param(Layout).tcl w]

   #----- Creer la commande d'execution du layout
   puts $file "namespace eval $Param(Layout) { }"
   puts $file ""
   puts $file "proc $Param(Layout)::Layout { Frame } {"
   puts $file "   variable Data"
   puts $file ""

   #----- Dimension du canvas
   puts $file "   Page::Size \$Frame $Page::Data(Width$Frame) $Page::Data(Height$Frame) $Page::Param(Intrusion)"
   puts $file ""

   #----- Parametres de la camera
   if { [llength [Page::Registered $Frame Viewport]] } {
      if { ![Dialog::Default . 300 INFO $Msg(CamSave) "" 1 $Lbl(Yes) $Lbl(No)] } {
         ProjCam::Write $Frame $file
      }
   }

   SPI::LayoutSaveItems $Frame $file

   #----- Activation du layout
   puts $file "   Page::UpdateItems \$Frame"
   puts $file "}"
   puts $file ""
   puts $file "proc $Param(Layout)::LayoutUpdate { Frame } {"
   puts $file "   SPI::LayoutUpdate \$Frame"
   puts $file "}"

   close $file

   #----- Ajouter a la liste des layouts usagers
   lappend Param(Layouts) $Param(Layout)
   ComboBox::Add .bar.layout.sel $Param(Layout)

   Dialog::Info . $Msg(LayoutSaved)
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::LayoutSaveItems>
# Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarder les items du layout courant
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <File>   : Fiechier dans lequel sauvegarder le tout
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::LayoutSaveItems { Frame File } {

   #----- Les viewports
   Viewport::Write $Frame $File
   Miniport::Write $Frame $File

   #----- Graphs
   Graph::Write $Frame $File

   #----- Colorbar
   set SPI::Data(ShowColorBar$Frame)   [ColorBar::Active $Frame]
   if { $SPI::Data(ShowColorBar$Frame) } {
      ColorBar::Write $Frame $File
      puts $File "   set SPI::Data(ShowColorBar\$Frame) 1"
   }

   #----- DataBar
   set SPI::Data(ShowDataBar$Frame)   [DataBar::Active $Frame]
   if { $SPI::Data(ShowDataBar$Frame) } {
      DataBar::Write $Frame $File
      puts $File "   set SPI::Data(ShowDataBar\$Frame) 1"
   }

   #----- Les objets statique
   if { $SPI::Data(ShowLOGO$Frame) } {
      set c [$Frame.page.canvas coords LOGO]
      puts $File ""
      puts $File "   set SPI::Data(ShowLOGO) 1"
      puts $File "   SPI::DrawImage \$Frame LOGO [expr int([lindex $c 0])] [expr int([lindex $c 1])] nw 1 0 0"
   }

   if { $SPI::Data(ShowScale$Frame) } {
      puts $File ""
      CVScale::Write $Frame $File
      puts $File "   set SPI::Data(ShowScale\$Frame) 1"
   }

   if { $SPI::Data(ShowCompass$Frame) } {
      puts $File ""
      CVCompass::Write $Frame $File
      puts $File "   set SPI::Data(ShowCompass\$Frame) 1"
   }

   if { $SPI::Data(ShowClock$Frame) } {
      puts $File ""
      CVClock::Write $Frame $File
      puts $File "   set SPI::Data(ShowClock$\Frame) 1"
   }

   if { $SPI::Data(ShowGeoLegend$Frame) } {
      puts $File ""
      CVGeoLegend::Write $Frame $File
      puts $File "   set SPI::Data(ShowGeoLegend\$Frame) 1"
   }

   #----- Les objets relies aux trajectoires
   if { $SPI::Data(ShowTrajGraph$Frame) } {
      puts $File ""
      puts $File "   set SPI::Data(ShowTrajGraph\$Frame) 1"
      set c [$Frame.page.canvas coords TRAJGRAPH]
      set x0 [lindex $c 0]
      set y0 [lindex $c 1]
      set x1 [expr $x0+[lindex [$Frame.page.canvas itemconfigure TRAJGRAPH -width] end]]
      set y1 [expr $y0+[lindex [$Frame.page.canvas itemconfigure TRAJGRAPH -height] end]]
      puts $File "   Trajectory::Graph \$Frame $x0 $y0 $x1 $y1 \$Trajectory::Data(List)"
      puts $File "   Shape::BindMove  \$Frame.page.canvas TRAJGRAPH"
      puts $File "   Shape::BindScale \$Frame.page.canvas TRAJGRAPH \"Trajectory::GraphScale \$Frame white black \\\"\$Trajectory::Data(List)\\\"\""
   }

   if { $SPI::Data(ShowTrajHeight$Frame) } {
      puts $File ""
      puts $File "   set SPI::Data(ShowTrajHeight\$Frame) 1"
      set c [$Frame.page.canvas coords TRAJHEIGHTFRAME]
      puts $File "   Trajectory::Height \$Frame [lindex $c 0] [lindex $c 1] [lindex $c 2] [lindex $c 3] \$Trajectory::Data(List)"
      puts $File "   Shape::BindMove  \$Frame.page.canvas TRAJHEIGHT"
      puts $File "   Shape::BindScale \$Frame.page.canvas TRAJHEIGHT \"Trajectory::HeightScale \$Frame white black \\\"\$Trajectory::Data(List)\\\"\""
   }

   if { $SPI::Data(ShowTrajLegend$Frame) } {
      puts $File ""
      puts $File "   set SPI::Data(ShowTrajLegend\$Frame) 1"
      set c [$Frame.page.canvas coords TRAJLEGENDFRAME]
      puts $File "   Trajectory::Legend \$Frame [lindex $c 0] [lindex $c 1] [lindex $c 2] [lindex $c 3] \$Trajectory::Data(List)"
      puts $File "   Shape::BindMove  \$Frame.page.canvas TRAJLEGEND"
      puts $File "   Shape::BindScale \$Frame.page.canvas TRAJLEGEND \"Trajectory::LegendScale \$Frame white black \\\"\$Trajectory::Data(List)\\\"\""
   }

   #----- Primitives de dessin
   Drawing::Write $Frame $File
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::LayoutUpdate>
# Creation : Mars 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Mise a jour des objets du layout.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::LayoutUpdate { Frame } {
   variable Data

   if { $Data(ShowTrajGraph$Frame) } {
      Trajectory::Graph $Frame -999 -999 -999 -999 $Trajectory::Data(List)
   }
   if { $Data(ShowTrajHeight$Frame) } {
      Trajectory::Height $Frame -999 -999 -999 -999 $Trajectory::Data(List)
   }
   if { $Data(ShowTrajLegend$Frame) } {
      Trajectory::Legend $Frame -999 -999 -999 -999 $Trajectory::Data(List)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::Locate>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulations de la selection sur la projection.
#
# Parametres :
#   <Lat>    : Latitude
#   <Lon>    : Longitude
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::Locate { Lat Lon { Zoom 0 } } {
   variable Data

   if { $Viewport::Map(Type)=="grid" } {
      set ij [projection function $Page::Data(Frame) -gridcoord $Lat $Lon]

      set Viewport::Map(GridI) [lindex $ij 0]
      set Viewport::Map(GridJ) [lindex $ij 1]
   }

   Viewport::GoTo $Page::Data(Frame) $Lat $Lon $Zoom
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::DrawClock>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche l'horloge sur le canvas.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::DrawClock { Frame } {
   variable Data

   set Data(ShowClock$Frame) $Data(ShowClock)

   if { $Data(ShowClock) } {
      CVClock::Create $Frame [expr [winfo width $Frame.page.canvas]-30] 30
      CVClock::Update $Frame [lindex [Viewport::Assigned $Frame $Viewport::Data(VP)] 0]
   } else {
      CVClock::Destroy $Frame
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::DrawBitmap>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche un bitmap sur le canvas.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <Bitmap> : Nom du bitmap
#   <X>      : Coordonnees X
#   <Y>      : Coordonnees Y
#   <Anchor> : Ancrage
#   <Color>  : Couleur
#   <N>      : Facteur de repetition
#   <DX>     : Delta X
#   <DY>     : Delta Y
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::DrawBitmap { Frame Bitmap X Y Anchor Color N DX DY } {
   variable Data
   variable Resources

   set Data(Show$Bitmap$Frame) $Data(Show$Bitmap)

   if { $Data(Show$Bitmap) } {
      for { set i 0 } { $i < $N } { incr i } {
         $Frame.page.canvas create bitmap $X $Y -bitmap $Resources(Ico$Bitmap) -tags "TOP $Bitmap" \
            -foreground $Color -anchor $Anchor
         incr X $DX
         incr Y $DY
      }
      Shape::BindMove $Frame.page.canvas $Bitmap
   } else {
      $Frame.page.canvas delete $Bitmap
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::DrawImage>
# Creation : Octobre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une image sur le canvas.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <Bitmap> : Nom du bitmap
#   <X>      : Coordonnees X
#   <Y>      : Coordonnees Y
#   <Anchor> : Ancrage
#   <N>      : Facteur de repetition
#   <DX>     : Delta X
#   <DY>     : Delta Y
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::DrawImage { Frame Image X Y Anchor N DX DY } {
   variable Data
   variable Resources

   set Data(Show$Image$Frame) $Data(Show$Image)

   if { $Data(Show$Image) } {
      for { set i 0 } { $i < $N } { incr i } {
         $Frame.page.canvas create image $X $Y -image $Image -tags "TOP $Image" -anchor $Anchor
         incr X $DX
         incr Y $DY
      }
      Shape::BindMove $Frame.page.canvas $Image
   } else {
      $Frame.page.canvas delete $Image
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::DrawCompass>
# Creation : Mai 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche le compas.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::DrawCompass { Frame } {
   variable Data

   set Data(ShowCompass$Frame) $Data(ShowCompass)

   if { $Data(ShowCompass) && $Viewport::Data(VP)!="" } {
      CVCompass::Create $Frame 50 50
      CVCompass::Update $Frame [$Viewport::Data(VP) -bearing] [projcam stats $Frame -angle] \
         [expr [projcam stats $Frame -dist]/[projcam configure $Frame -lens]] $Viewport::Map(Speed)
   } else {
      CVCompass::Destroy $Frame
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::DrawGeoLegend>
# Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche l'echelle des distances.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::DrawGeoLegend { Frame } {
   global GDefs
   variable Data

   set Data(ShowGeoLegend$Frame) $Data(ShowGeoLegend)

   if { $Data(ShowGeoLegend) } {

      set items ""
      foreach item { Coast Lake River Polit Admin City Road Rail Util Canal Coord } {

         if { $Viewport::Map($item) >0 } {
            lappend items [list $Viewport::Map($item) $Viewport::Resources($item) [lindex $Viewport::Lbl($item) $GDefs(Lang)]]
         }
      }
      CVGeoLegend::Create $Frame 100 100 $items
   } else {
      CVGeoLegend::Destroy $Frame
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::DrawScale>
# Creation : Janvier 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche l'echelle des distances.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::DrawScale { Frame } {
   variable Data

   set Data(ShowScale$Frame) $Data(ShowScale)

   if { $Data(ShowScale) && $Viewport::Data(VP)!="" } {
      CVScale::Create $Frame 130 [expr [winfo height $Frame.page.canvas]-30] 250
      CVScale::Update $Frame $Viewport::Data(VP)
   } else {
      CVScale::Destroy $Frame
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::DrawTrajGraph>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche le graphique d'elevation des trajectoire sur le canvas.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::DrawTrajGraph { Frame } {
   variable Data
   variable Resources

   set Data(ShowTrajGraph$Frame) $Data(ShowTrajGraph)

   if { $Data(ShowTrajGraph) } {
      Trajectory::Graph $Frame 0 0 300 200 $Trajectory::Data(List)
      Shape::BindMove  $Frame.page.canvas TRAJGRAPH
      Shape::BindScale $Frame.page.canvas TRAJGRAPH "Trajectory::GraphScale $Frame \"\$Trajectory::Data(List)\""
   } else {
      Shape::UnBind $Frame.page.canvas TRAJGRAPH
      $Frame.page.canvas delete TRAJGRAPH
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::DrawTrajHeight>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche la liste d'elevation des trajectoire sur le canvas.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::DrawTrajHeight { Frame } {
   variable Data
   variable Resources

   set Data(ShowTrajHeight$Frame) $Data(ShowTrajHeight)

   if { $Data(ShowTrajHeight) } {
      Trajectory::Height $Frame 0 0 70 350 $Trajectory::Data(List)
      Shape::BindMove  $Frame.page.canvas TRAJHEIGHT
      Shape::BindScale $Frame.page.canvas TRAJHEIGHT "Trajectory::HeightScale $Frame \"\$Trajectory::Data(List)\""
   } else {
      Shape::UnBind $Frame.page.canvas TRAJHEIGHT
      $Frame.page.canvas delete TRAJHEIGHT
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::DrawTrajLegend>
# Creation : Decembre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche la liste d'elevation des trajectoire sur le canvas.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::DrawTrajLegend { Frame } {
   variable Data
   variable Resources

   set Data(ShowTrajLegend$Frame) $Data(ShowTrajLegend)

   if { $Data(ShowTrajLegend) } {
      Trajectory::Legend $Frame 0 0 555 80 $Trajectory::Data(List)
      Shape::BindMove  $Frame.page.canvas TRAJLEGEND
      Shape::BindScale $Frame.page.canvas TRAJLEGEND "Trajectory::LegendScale $Frame \"\$Trajectory::Data(List)\""
   } else {
      Shape::UnBind $Frame.page.canvas TRAJLEGEND
      $Frame.page.canvas delete TRAJLEGEND
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::Execute>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Executer un script.
#
# Parametres :
#   <Script> : Script a executer
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::Execute { Script } {
   global env
   global argv

   if { $Script!="" } {

      #----- Check for regular script
      if { ![file exists $Script] } {
         if { ![file exists $env(HOME)/.spi/Macro/$Script] } {
            if { ![file exists $env(HOME)/.spi/Macro/$Script.tcl] } {
               Log::Print ERROR "Could not find script $Script"
               SPI::Quit 1
            } else {
               set Script $env(HOME)/.spi/Macro/$Script.tcl
            }
         } else {
            set Script $env(HOME)/.spi/Macro/$Script
         }
      }

      #----- Source the script
      namespace inscope :: source $Script

      #----- If Macro then execute it
      set script [file rootname [file tail $Script]]

      if { [namespace exists ::Macro::$script] } {

         Macro::Run $script False
      }
   }
}

#---------------------------------------------------------------------------
# Nom      : <SPI::Clear>
# Creation : Juin 2013 - J.P. Gauthier - CMC/CMOE
#
# But      : Efface toute les donnees.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SPI::Clear { } {
   variable Param

   . config -cursor left_ptr
   update idletasks

   SPI::IcoClear

   #----- Clear data boxes
   FieldBox::Clear False
   TrajBox::Clear  False
   ObsBox::Clear   False
   
   #----- Clear tools
   foreach tool $Param(Tools) {
      eval set proc \[info procs ::${tool}::Clear\]
      if { $proc!="" } {
         eval ${tool}::Clear $Page::Data(Frame)
      }
   }

   . config -cursor left_ptr
}

#---------------------------------------------------------------------------
# Nom      : <SPI::IcoAdd>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajoute la liste d'icones dans les icones definies.
#
# Parametres :
#   <Group>  : Nom du groupe d'icones
#   <Color>  : Couleur de ce groupe
#   <Icos>   : Descriptions des icones (liste)
#
# Retour:
#
# Remarques :
#    -Le format pour une icone est:
#      { "descriptif" lat lon elev }
#
#----------------------------------------------------------------------------

proc SPI::IcoAdd { Frame Group Color Icos } {
   variable Ico

   if { $Group ni $Ico(Groups) } {
      lappend Ico(Groups) $Group
      .menu.disp.menu.icons add checkbutton -label "$Group" -variable SPI::Ico(Set$Group) \
         -command "Page::UpdateItems $Page::Data(Frame)"
      set Ico(Set$Group) 1
   } else {
      if { [winfo exists $Ico(Frame$Group).page.canvas] } {
         $Ico(Frame$Group).page.canvas delete $Group
      }
   }

   set Ico(Frame$Group) $Frame
   set Ico(Def$Group)   $Icos
   set Ico(Col$Group)   $Color

   Page::UpdateItems $Page::Data(Frame)
}

#---------------------------------------------------------------------------
# Nom      : <SPI::IcoClear>
# Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Efface toute les icone de sur la projection.
#
# Parametres :
#   <args>   : Liste des noms de groupe d'icones
#
# Retour:
#
# Remarques :
#    -si args est vide, on efface toutes les icones
#
#----------------------------------------------------------------------------

proc SPI::IcoClear { args } {
   variable Ico

   if { $args=="" } {
      set args $Ico(Groups)
   }

   foreach group $args {
      $Page::Data(Canvas) delete $group
      set Ico(Set$group) 0
   }
}

#---------------------------------------------------------------------------
# Nom      : <SPI::IcoDel>
# Creation : Janvier 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprime le groupe d'icones dans les icones definies.
#
# Parametres :
#   <Group>  : Nom du groupe d'icones
#
# Retour:
#
# Remarques :
#    -Le format pour une icone est:
#      { "descriptif" lat lon elev }
#
#----------------------------------------------------------------------------

proc SPI::IcoDel { Group } {
   variable Ico

   set idx [lsearch $SPI::Ico(Groups) $Group]

   if { $idx != -1 } {
      set SPI::Ico(Groups) [lreplace $SPI::Ico(Groups) $idx $idx]
      .menu.disp.menu.icons delete 5 end
      foreach group $SPI::Ico(Groups) {
         .menu.disp.menu.icons add checkbutton -label "$group" -variable SPI::Ico(Set$group) \
            -command "Page::UpdateItems $Page::Data(Frame)"
      }
      if { [winfo exists $Ico(Frame$Group)] } {
         $Ico(Frame$Group).page.canvas delete $Group
         Page::UpdateItems $Ico(Frame$Group)
      }

      unset Ico(Set$Group)
      unset Ico(Frame$Group)
      unset Ico(Def$Group)
      unset Ico(Col$Group)
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::IcoDraw>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une icone sur le canvas.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::IcoDraw { Frame args } {
   global GDefs
   variable Param
   variable Ico

   if { ![winfo exists $Frame.page.canvas] } {
      return
   }
   eval $Frame.page.canvas delete $Ico(Groups)

   foreach group $Ico(Groups) {

      if { $Ico(Frame$group)=="" } {
         set Ico(Frame$group) $Frame
      }

      if { $Ico(Col$group)!="" } {
         set col $Ico(Col$group)
      } else {
         set col black
      }

      if { $Ico(Set$group) && $Ico(Frame$group)=="$Frame" } {

         set i 0

         foreach def $Ico(Def$group) {

            set id     [lindex $def 0]
            set coords [lrange $def 1 end-1]
            set ico    [lindex $def end]
            set tag    $group$i

            set lat    [lindex $coords 0]
            set lon    [lindex $coords 1]
            set elev   [lindex $coords 2]

            foreach vp [Page::Registered $Frame Viewport] {

              if { [llength $coords]>=4 } {
                  Viewport::DrawArea $Frame $vp $coords "PAGE$vp $group $tag" "" "" $col "" False 2
               }

               #----- Projection des coordonnées
               if { [set xy [$vp -project $lat $lon $elev]]!="" && [lindex $xy 2]>=0 } {

                  set x [lindex $xy 0]
                  set y [lindex $xy 1]

                  #----- Affichage de l'icone
                  if { $Param(IconImage) } {
                     $Frame.page.canvas create image $x $y -image $ico -tags "$group $tag"
                  }

                  if { $Param(IconCircle) } {
                     if { [llength [set cs [$vp -projectcircle $lat $lon $Param(IconCircle)]]] } {
                        eval $Frame.page.canvas create polygon $cs -fill \"\" -outline $col -width 2 -dash . -tags \"$group $tag\"
                     }
                  }

                  #----- Affchage du texte
                  if { $Param(IconId) && $id!=""  } {
                     $Frame.page.canvas create text [expr $x+$Param(IconDX)] [expr $y+$Param(IconDY)] -text "$id" -fill $col \
                        -tags "$group TEXT$group" -anchor $Param(IconAnchor) -font XFontIcon
                     $Frame.page.canvas bind $tag <Enter> ""
                     $Frame.page.canvas bind $tag <Leave> ""
                  } else {
                     $Frame.page.canvas bind $tag <Enter> "Page::CursorInfo $Frame  \[$Frame.page.canvas canvasx %x\] \[$Frame.page.canvas canvasy %y\] \"$id\""
                     $Frame.page.canvas bind $tag <Leave> "Page::CursorInfo $Frame 0 0 \"\""
                  }
               }
            }
            incr i
         }
      }
   }
}

#---------------------------------------------------------------------------
# Nom      : <SPI::IcoOpen>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche une liste d'icones.
#
# Parametres :
#   <Files>  : Fichiers de description d'icones
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SPI::IcoOpen { Files } {
   global GDefs
   global env
   variable Error

   if { $Files == "" } {
      return
   }

   foreach file $Files {

      set f [open $file r]

      gets $f id

      if { [lindex $id 0] != "IconList" } {
         Dialog::Error . $Error(Icon) ":\n\n\t$file"
         close $f
         return
      }

      gets $f group
      gets $f color
      gets $f icon

      regsub -all "\[^a-zA-Z0-9\]"  $group _ group

      #----- Si on n'as pas de couleur, on est en presence d'une image plutot que d'un gif
      if { $group ni [image names] } {
         eval image create photo $group -file $icon
      }
      set icon $group

      #----- Lire la liste d'icones
      set ico ""

      while { ![eof $f] } {

         gets $f line

         if { $line != "" && [string range $line 0 0] != "#" } {
            lappend ico [list [lindex $line 0] [lindex $line 1] [lindex $line 2] [lindex $line 3] $icon]
         }
      }
      close $f

      SPI::IcoAdd $Page::Data(Frame) $group [string trim $color] $ico
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::PageDel>
# Creation : Janvier 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Supprimer la page courante.
#
# Parametres :
#   <All>    : detruire toute les pages
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::PageDel { { Page "" } { All False } } {
   variable Data

   #----- Determiner le frame en cause, celui de la page ou le courant
   if { $Page!="" } {
      if { [set frame [join [lrange [split $Page .] 0 1] .]]!=".mdi" } {
         set frame $frame.mdi
      }
   } else {
      set frame [join [lrange [split $Page::Data(Frame) .] 0 1] .]
      if { $frame==".pan" } {
         set frame .mdi
      } elseif { $frame!=".mdi" } {
         set frame $frame.mdi
      }
   }

   if { $All } {
      for { set i 0 } { $i < [TabFrame::NbFrame $frame] } { incr i } {
         catch { SPI::PageDel $frame.frame$i.frame }
         TabFrame::Delete $frame 1 $i
      }
      set top [winfo toplevel $frame]
      if { $top!="." } {
         TabFrame::Destroy $frame
         destroy $top
      } else {
         TabFrame::Clear $frame
      }
   } else {

      #----- Determiner la page, la page specifie ou la page courante
      set no -1
      if { $Page=="" && [set no [TabFrame::Current $frame]]!=-1} {
         set Page $frame.frame$no.frame
      }

      #----- Supprimer la page et les variales d'etat
      if { $Page!="" } {
         Page::Destroy $Page

         foreach item $Data(Items) {
            unset Data(Show$item$Page)
         }

         if { $no!=-1 } {
            TabFrame::Delete $frame 1 $no
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::PageNew>
# Creation : Janvier 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter un page.
#
# Parametres :
#   New      : Nouvelle fenetre ou nom de la fenetre a creer
#   Label    : Identification de la page
#   Geom     : Geometrie de la fenetre
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::PageNew { New { Label "" } { Geom { 600x600+[winfo rootx .]+[winfo rooty .] } } } {
   global GDefs env
   variable Title
   variable Data

   if { $Label=="" } {
      set Label "Page [incr Data(Page)]"
   }

   #----- If a page name is passed in
   set frame ""
   if { [string index $New 0]=="." } {
      set frame $New
      set New True
   }

   #----- Create a new page
   if { $New } {
      if { $frame=="" } {
         set frame .page[incr Data(Page)]
      }

      if { ![winfo exists $frame] } {
         toplevel     $frame
         wm title     $frame "[lindex $Title(SPI) $GDefs(Lang)] $GDefs(Version) ($env(USER) $GDefs(Host))"
         eval wm geometry $frame $Geom

         label $frame.active -image MOUSE -relief raised -bd 1

         TabFrame::Create $frame.mdi 1 "SPI::PageSetup" 0
         pack $frame.mdi -fill both -expand true

         frame $frame.info -bd 1 -relief raised
            label $frame.info.cooval -textvariable Page::Data(Coord) -bg $GDefs(ColorLight) -relief sunken -bd 1 -width 36
            label $frame.info.altval -textvariable Page::Data(Altitude) -bg $GDefs(ColorLight) -relief sunken -bd 1 -width 5 -anchor e
            label $frame.info.fldval -textvariable Page::Data(Value) -bg $GDefs(ColorLight) -relief sunken -bd 1 -width 10
               pack $frame.info.cooval .info.altval -ipadx 2 -side left -fill y
            pack $frame.info.fldval -side left -fill both -expand true
         pack $frame.info -side bottom -fill x
      }
      if { [winfo exists $frame.mdi] } {
         set page [TabFrame::Add $frame.mdi 1 $Label True].frame
         wm protocol $frame WM_DELETE_WINDOW "SPI::PageDel $page True"
      } else {
         set page $frame.page[incr Data(Page)]
 #        wm protocol $frame WM_DELETE_WINDOW "SPI::PageDel $page False"
      }
   } else {
      set frame [join [lrange [split $Page::Data(Frame) .] 0 1] .]
      if { $frame==".pan" } {
         set frame .mdi
      } elseif { $frame!=".mdi" } {
         if { [winfo exists $frame.mdi] } {
            set frame $frame.mdi
         } else {
            set frame .mdi
         }
      }
      set page [TabFrame::Add $frame 1 $Label True].frame
   }

   set Data(Layout$page)  ""

   foreach item $Data(Items) {
      set Data(Show$item$page) 0
   }

   Page::Create $page -1 -1
   Page::Activate $page

   return $page
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::PageSetup>
# Creation : Octobre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Effecter la selection apres un changement de page des onglets.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::PageSetup { Frame No } {

  Page::Activate $Frame.frame$No.frame

  #----- Forcer le refresh dans les changement d'onglets
  event generate $Frame.frame$No.frame.page.canvas <Configure>
}

#---------------------------------------------------------------------------
# Nom      : <SPI::FileOpen>
# Creation : Avril 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Ouvrir un fichier de donnees.
#
# Parametres :
#    <Box>       : Numero de la boite (0..n,NEW=Creer une nouvelle boite,LAST-Derniere boite)
#    <Type>      : Type d'interface aux donnees
#    <Title>     : Titre de la boite
#    <Extension> : Extension des fichiers
#    <Files>     : Liste des fichiers standards a ouvrir
#    <Command>   : Commande
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SPI::FileOpen { Box Type Title Extension Files } {

   if { $Title == "" } {
      set Title "Undefined"
   }

   switch $Box {
     "NEW" {
         set Box [${Type}::Create . "$Title"]
      }
     "LAST" {
         set Box -1
      }
      default {
         if { ![${Type}::Exist $Box] } {
            set Box [${Type}::Create . "$Title"]
         }
      }
   }
   if { $Files== "" } {
      ${Type}::FileOpen $Box [FileBox::Create . "" Load $Extension]
   } else {
      ${Type}::FileOpen $Box $Files
   }
}

#----------------------------------------------------------------------------
# Nom      : <SPI::ToolMode>
# Creation : Juillet 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Activation du mode de selection d'un outils.
#
# Parametres :
#  <Tool>    : Outils
#  <Mode>    : Mode de manipulation (Zoom, Data, Draw, Cam, Fly, None)
#  <Off>     : Si l'appel provient d'un boutton on/off (CheckButton)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SPI::ToolMode { Tool Mode { Off False } { Square 0 } } {
   variable Param

   if { $Tool=="" } {
      return
   }

   if { $Tool=="SPI" } {
      if { $Off } {
         Page::ModeSelect Zoom
      } else {
         Page::ModeSelect $Mode
      }
   } else {
      #----- Garder l'information sur le dernier outils utilise
      set Param(Tool) $Tool
      set Param(ToolMode) $Mode

      Page::ModeSelect $Mode

      #----- Une fonction d'affichage existe-t-elle pour cet outils
      eval set proc \[info procs ::${Tool}::UpdateItems\]
      if { $proc!="" } {
         eval ${Tool}::UpdateItems $Page::Data(Frame)
      }
   }

   if { $Mode!="Draw" && [winfo exists $Page::Data(Canvas)] } {
      $Page::Data(Canvas) delete VERTEXFOLLOW VERTEXTEMPORARY
   }
   set Page::Data(DrawMode) $Tool
   set Page::Data(ToolMode) $Tool
   set Page::Param(Square)  $Square
}

#---------------------------------------------------------------------------
# Nom      : <SPI::Progress>
# Creation : Mars 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Modifier le progress bar de SPI
#
# Parametres  :
#   <Percent> : Increment de pourcentage
#   <Info>    : Information associee
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SPI::Progress { Percent { Info "" } { Var "" } } {
   variable Data

   if { [string index $Percent 0]=="+" || [string index $Percent 0]=="-" } {
      set Data(Percent) [expr $Data(Percent)+$Percent]
   } else {
      set Data(Percent) $Percent
   }

   if { $Var!="" } {
      set $Var $Info
   } else {
      set Page::Data(Value) $Info
   }
   update idletasks
}

#---------------------------------------------------------------------------
# Nom      : <SPI::Quit>
# Creation : Mai 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Fin de l'application.
#
# Parametres :
#   <Code>   : code de sortie (Defaut: 0 = Ok)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SPI::Quit { { Code 0 } } {
   variable Param
   variable Data

   #----- Check if any process is running unless in batch mode where we exit ASAP
   if { !$SPI::Param(Batch) && [Dialog::WaitDestroy True] } {

      #----- Cleanup des outils
      foreach tool $SPI::Param(Tools) {
         upvar #0 ${tool}::Data(Active) active
         if { $active } {
            eval set proc \[info procs ::${tool}::Close\]
            if { $proc!="" } {
               eval ${tool}::Close
            }
         }
      }

      fstdfield   wipe
      observation wipe
      trajectory  wipe
      ogrlayer    wipe
      gdalband    wipe
      dataspec    wipe
      graphitem   wipe
      graphaxis   wipe

      glrender -shutdown
   }

   Log::End $Code
}

#---------------------------------------------------------------------------
# Nom      : <SPI::Password>
# Creation : Avril 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Extrair le mot de passe d'un usager dans le fichier .password.
#
# Parametres :
#   <User>   : Usager
#
# Retour     : Mot de passe
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SPI::Password { User } {
   global env

   set line ""

   #----- Get the user line (We have to catch cause grep gives an exit 1)
   catch { set line [exec grep eerca99 $env(HOME)/.spi/.password] }

   #----- Extract password (user=password)
   return [lindex [split $line =] end]
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::ObjectAdd>
# Creation : Janvier 2002 - J.P. Gauthier - CMC/CMOE -
#
# But      : Ajouter un viewport dans le canvas.
#
# Parametres :
#    <Type>  : Type d'objects
#    <Sub>   : Sous type d'objects
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::ObjectAdd { Type { Sub "" } } {
   global GDefs
   variable Lbl
   variable Msg
   variable Error

   set new [Dialog::Default . 300 QUESTION $Msg(Page) "" 2 $Lbl(Current) $Lbl(Page) $Lbl(Window)]

   switch $new {
      0 { if { ![winfo exists $Page::Data(Canvas)] } { Dialog::Error . $Error(Page); return } }
      1 { SPI::PageNew False $Type${Sub} }
      2 { SPI::PageNew True $Type${Sub} }
   }

   set obj [lindex [Page::Registered $Page::Data(Frame) ${Type}${Sub}] end]

   if { $obj=="" } {
      eval ${Type}${Sub}::Create $Page::Data(Frame) 0 0 [winfo width $Page::Data(Canvas)] [winfo height $Page::Data(Canvas)] 1 1
   } else {
      eval ${Type}${Sub}::Create $Page::Data(Frame) \[expr \$${Type}::Data(X$obj)+5\] \[expr \$${Type}::Data(Y$obj)+5\] \
          \$${Type}::Data(Width$obj) \$${Type}::Data(Height$obj) 1 0
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::ProjectRead>
# Creation : Fevrier 2006 - J.P. Gauthier - CMC/CMOE -
#
# But      : Lire un fichire de project
#
# Parametres :
#   <File>   : Path complet du fichier de projet
#   <Force>  : Don't ask
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::ProjectRead { File { Force False } } {
   global GDefs
   variable Error
   variable Msg
   variable Lbl

   if { $File=="" } {
      return
   }

   #----- Determiner si le fichier de projet est valide
   if { [file exists $File.spi] } {
      set File $File.spi
   }

   set f [open $File r]
   gets $f head
   close $f

   if { !$Force } {
      if { [Dialog::Default . 300 WARNING $Msg(ProjectRead) "" 1 $Lbl(Yes) $Lbl(No)] } {
         return
      }
   }

   #----- Detruire toute les pages courantes
   SPI::PageDel .mdi True
   foreach win [winfo children .] {
      if { [string equal ".page" [string range $win 0 4]] } {
         SPI::PageDel $win True
      }
   }

   #----- Lire le projet
   set err [catch { source $File } msg]
   if { $err } {
      Dialog::Error . $Error(Project) ":\n\n\t$msg"
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::ProjectSaveLayout>
# Creation : Fevrier 2006 - J.P. Gauthier - CMC/CMOE -
#
# But      : Sauvegarder le layout de page pour un fichier de projet
#
# Parametres :
#   <File>   : Path complet du fichier de projet
#   <Frame>  : Identificateur de page a sauvegarder
#   <Cam>    : Sauvegarder la camera ?
#   <Size>  : Sauvegarder les dimensions ?
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::ProjectSaveLayout { File Frame Cam Size } {

   if { $Size } {
      puts $File "   Page::Size \$Frame $Page::Data(Width$Frame) $Page::Data(Height$Frame)\n"
   }
   SPI::LayoutSaveItems $Frame $File
   if { $Cam } {
      puts $File "   set ProjCam::Data(Params\$Frame) \"[ProjCam::Mem $Frame \$Frame]\""
      puts $File "   ProjCam::Select \$Frame \$Frame \$Frame True"
      eval set list \${ProjCam::Data${Frame}::Cam(LLens)}
      puts $File "   set ProjCam::Data\$\{Frame\}::Cam(LLens) \{ $list \}\n"
   }
   puts $File "   Page::UpdateItems \$Frame"
   puts $File "   Page::ModeSelect \$Page::Data(Mode) \$Frame\n"
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::ProjectSavePerPage>
# Creation : Avril 2010 - J.P. Gauthier - CMC/CMOE -
#
# But      : Sauvegarder les donnees par page pour un fichier de projet
#
# Parametres :
#   <File>   : Path complet du fichier de projet
#   <Page>   : Identificateur de page a sauvegarder
#   <Params> : Sauvegarder les parametres des donnees ?
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::ProjectSavePerPage { File Page Params }  {

   foreach tool $SPI::Param(Tools) {
      eval set active \$${tool}::Data(Active)

      if { $active } {
         eval set proc \[info procs ::${tool}::AsProjectPerPage\]
         if { $proc!="" } {
            eval ${tool}::AsProjectPerPage $File $Page $Params
         }
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <SPI::ProjectSave>
# Creation : Fevrier 2006 - J.P. Gauthier - CMC/CMOE -
#
# But      : Sauvegarder du projet courant
#
# Parametres :
#   <File>   : Path complet du fichier de projet
#   <Window> : Sauvegarde de la situatiopn de fenetres ?
#   <Layout> : Sauvegarder es mises en pages ?
#   <Cam>    : Sauvegarder la camera ?
#   <Data>   : Sauvegarder des donnees ?
#   <Params> : Sauvegarder des parametres ?
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SPI::ProjectSave { File Window Layout Cam Data Params } {
   global env
   global GDefs
   variable Msg
   variable Lbl

   if { $File=="" } {
      return
   }

   set SPI::Project(File) $File

  #----- Check for extension
   if { [file extension $File]!=".spi" } {
      set File ${File}.spi
   }

   set project [file rootname [file tail $File]]

   set f [open $File w]
   puts $f "#===============================================================================
# Project name : $project
# Created by   : $env(USER)
# Creation date: [clock format [clock seconds]]
# SPI version  : $GDefs(Version)
#==============================================================================="

   puts $f "\nset SPI::Param(Project) $project"

   #----- Process Params
   if { $Params } {

      puts $f "\n#----- Data parameters"
      set fonts {}
      set cmaps {}

      #----- Force obs parameters to be defined
      foreach obs [observation all] {
         Obs::ParamInit $obs [observation configure $obs -dataspec]
      }

      foreach spec [dataspec all] {

         set cmap [dataspec configure $spec -colormap]
         if { [colormap is $cmap] && [lsearch -exact $cmaps $cmap]==-1 } {
            lappend cmaps $cmap
            puts $f "\ncatch { colormap create $cmap }"
            puts $f "colormap control $cmap -del"
            puts $f "colormap control $cmap -list { [colormap control $cmap -list] }"
            puts $f "colormap configure $cmap  -RGBAratio [colormap configure $cmap -RGBAratio] -MMratio [colormap configure $cmap -MMratio] \
             -curve red [colormap configure $cmap -curve red] -curve green [colormap configure $cmap -curve green] -curve blue [colormap configure $cmap -curve blue] -curve alpha [colormap configure $cmap -curve alpha] \
             -invertx red [colormap configure $cmap -invertx red] -invertx green [colormap configure $cmap -invertx green] -invertx blue [colormap configure $cmap -invertx blue] -invertx alpha [colormap configure $cmap -invertx alpha] \
             -inverty red [colormap configure $cmap -inverty red] -inverty green [colormap configure $cmap -inverty green] -inverty blue [colormap configure $cmap -inverty blue] -inverty alpha [colormap configure $cmap -inverty alpha] \
             -min red [colormap configure $cmap -min red] -min green [colormap configure $cmap -min green] -min blue [colormap configure $cmap -min blue] -min alpha [colormap configure $cmap -min alpha] \
             -max red [colormap configure $cmap -max red] -max green [colormap configure $cmap -max green] -max blue [colormap configure $cmap -max blue] -max alpha [colormap configure $cmap -max alpha] \
             -interp [colormap configure $cmap -interp]"
         }

         set font [dataspec configure $spec -font]
         if { $font!="" && [lsearch -exact $fonts $font]==-1 } {
            lappend fonts $font
            puts $f "\ncatch { font create $font }"
            puts $f "font configure $font -family [font configure $font -family] -weight [font configure $font -weight] -size [font configure $font -size]\
                  -slant [font configure $font -slant] -underline [font configure $font -underline] -overstrike [font configure $font -overstrike]"
         }

         puts $f "\ndataspec create \"$spec\" -factor [dataspec configure $spec -factor] -value [dataspec configure $spec -value]\
            -size [dataspec configure $spec -size] -width [dataspec configure $spec -width] -unit \"[dataspec configure $spec -unit]\"\
            -desc \"[dataspec configure $spec -desc]\" -icon \"[dataspec configure $spec -icon]\" -mark \"[dataspec configure $spec -mark]\"\
            -color \"[dataspec configure $spec -color]\" -fill \"[dataspec configure $spec -fill]\" -activefill \"[dataspec configure $spec -activefill]\"\
            -outline \"[dataspec configure $spec -outline]\" -activeoutline \"[dataspec configure $spec -activeoutline]\" \
            -transparency \"[dataspec configure $spec -transparency]\" -dash \"[dataspec configure $spec -dash]\" \
            -rendercontour [dataspec configure $spec -rendercontour] -rendervector [dataspec configure $spec -rendervector]\
            -rendertexture [dataspec configure $spec -rendertexture] -rendervolume [dataspec configure $spec -rendervolume] -renderparticle [dataspec configure $spec -renderparticle]\
            -rendercoord [dataspec configure $spec -rendercoord] -rendervalue [dataspec configure $spec -rendervalue] -renderlabel [dataspec configure $spec -renderlabel]\
            -rendergrid [dataspec configure $spec -rendergrid] -min \"[dataspec configure $spec -min]\" -max \"[dataspec configure $spec -max]\"\
            -intervals \"[dataspec configure $spec -intervals]\" -interlabels \"[dataspec configure $spec -interlabels]\" -intervalmode [dataspec configure $spec -intervalmode] \
            -interpdegree \"[dataspec configure $spec -interpdegree]\" -extrapdegree \"[dataspec configure $spec -extrapdegree]\" \
            -sample \"[dataspec configure $spec -sample]\" -stipple \"[dataspec configure $spec -stipple]\" -colormap \"[dataspec configure $spec -colormap]\" -font \"[dataspec configure $spec -font]\" \
            -texsample \"[dataspec configure $spec -texsample]\" -texsize \"[dataspec configure $spec -texsize]\" -texres \"[dataspec configure $spec -texres]\" \
            -interpolation \"[dataspec configure $spec -interpolation]\" -topography \"[dataspec configure $spec -topography]\" -topographyfactor \"[dataspec configure $spec -topographyfactor]\" \
            -mask \"[dataspec configure $spec -mask]\" -light \"[dataspec configure $spec -light]\" -labelvar \"[dataspec configure $spec -labelvar]\" \
            -sizevar \"[dataspec configure $spec -sizevar]\" -mapvar \"[dataspec configure $spec -mapvar]\" \
            -extrude \"[dataspec configure $spec -extrude]\" -extrudefactor \"[dataspec configure $spec -extrudefactor]\""
      }
   }

   if { $Window } {
      #----- Process SPI
      puts $f "\n#----- Window parameters\n"
      puts $f "set SPI::Param(Geom) [wm geom .]"
      puts $f "set SPI::Param(PaneSide) \"$SPI::Param(PaneSide)\""

      set panes {}
      foreach pane [.pan.side panes] {
         lappend panes [winfo width $pane]x[winfo height $pane]
      }
      puts $f "set SPI::Param(Panes) \"$panes\""

      puts $f "\nwm geom . \$SPI::Param(Geom)"
      puts $f "\nSPI::WindowLayout \$SPI::Param(PaneSide) \$SPI::Param(Panes)"

      if { [winfo exists .params] && [winfo ismapped .params] } {
         puts $f "\nSPI::Params"
         puts $f "wm geometry .params [wm geom .params]"
         puts $f "TabFrame::Select .params.tab [TabFrame::Current .params.tab]"
      }

      foreach pane [.pan.side panes] {
         if { $Layout } {
             puts $f "set Frame $pane"
             SPI::ProjectSaveLayout $f $pane $Cam False
          }
          if { $Data } {
             SPI::ProjectSavePerPage $f $pane $Params
          }
      }

      puts $f "\n#----- SPI's frame\n"
      foreach tab [TabFrame::GetTabs .mdi] {
         puts $f "set Frame \[SPI::PageNew False \"[TabFrame::GetLabel .mdi $tab]\"\]\n"
         if { $Layout } {
            SPI::ProjectSaveLayout $f .mdi.frame$tab.frame $Cam True
         }
         if { $Data } {
            SPI::ProjectSavePerPage $f .mdi.frame$tab.frame $Params
         }
      }
      puts $f "TabFrame::Select .mdi [TabFrame::Current .mdi]"

      foreach win [winfo children .] {
         if { [string equal ".page" [string range $win 0 4]] } {
            set no -1
            foreach tab [TabFrame::GetTabs $win.mdi] {
               if { ![incr no] } {
                  puts $f "set Frame \[SPI::PageNew True \"[TabFrame::GetLabel $win.mdi $tab]\" [wm geom $win]\]"
               } else {
                  puts $f "set Frame \[SPI::PageNew False \"[TabFrame::GetLabel $win.mdi $tab]\"\]"
               }
               if { $Layout } {
                  SPI::ProjectSaveLayout $f $win.mdi.frame$tab.frame $Cam True
               }
               if { $Data } {
                  SPI::ProjectSavePerPage $f $win.mdi.frame$tab.frame $Params
               }
            }
         }
      }

      #----- Process Tools
      puts $f "\n#----- Invoke tools\n"
      foreach tool $SPI::Param(Tools) {
         eval set active \$${tool}::Data(Active)

         if { $active } {
            eval set proc \[info procs ::${tool}::AsProject\]
            if { $proc!="" } {
               eval ${tool}::AsProject $f
            }
         }
      }

      if { [winfo exists .anim] } {
         puts $f "set Animator::Param(Geom) [wm geom .anim]"
         puts $f "Animator::Window"
      }

      if { [winfo exists .fieldcalc] } {
         puts $f "set FieldCalc::Param(Geom) [wm geom .fieldcalc]"
         puts $f "FieldCalc::Window"
      }

      if { [winfo exists .model] } {
         puts $f "set Model::Param(Dock) $Model::Param(Dock)"
         catch { puts $f "set Model::Param(Geom) [wm geom .model]" }
         puts $f "Model::Window True"
      }
   }

   #----- Process Data
   if { $Data } {

      puts $f "\n#----- Cleanup previous data\n"
      puts $f "foreach box \[FieldBox::Get\] { FieldBox::Close \$box }"
      puts $f "foreach box \[TrajBox::Get\]  { TrajBox::Close \$box }"
      puts $f "foreach box \[ObsBox::Get\]   { ObsBox::Close \$box }"

      set boxes [FieldBox::Get]
      if { [llength $boxes] } {
         puts $f "\n#----- Load standard files\n"
         foreach box $boxes {
            puts $f "set no \[FieldBox::Create . \"\" [wm geometry .fieldbox$box]\]"

            foreach file [FieldBox::GetFile $box] {
               puts $f "FieldBox::FileOpen \$no $file"
            }
         }
      }

      set boxes [TrajBox::Get]
      if { [llength $boxes] } {
         puts $f "\n#----- Load trajectories\n"
         foreach box $boxes {
            puts $f "set no \[TrajBox::Create . \"\" [wm geometry .trajbox$box]\]"

            foreach file [TrajBox::GetFile $box] {
               puts $f "TrajBox::FileOpen \$no $file"
            }
         }
      }

      set boxes [ObsBox::Get]
      if { [llength $boxes] } {
         puts $f "\n#----- Load observations\n"
         foreach box $boxes {
            puts $f "set no \[ObsBox::Create . \"\" [wm geometry .obsbox$box]\]"

            foreach file [ObsBox::GetFile $box] {
               puts $f "ObsBox::FileOpen \$no $file"
            }
         }
      }
   }

   close $f
}

#----- Demmarage de l'application
SPI::Splash "Initializing interface"
SPI::Init

#----- Initialisations de certaines fonctions et package
Areas::Init
ProjCam::Read

#----- Inclure les parametres usagers
if { [file exists $SPI::Param(Default)] } {
   source $SPI::Param(Default)
}

#----- Parcourir la liste des parametres post-launch
for { set i 0 } { $i < $argc } { incr i } {
   switch -exact [string trimleft [lindex $argv $i] "-"] {
      "valgrind"  { }
      "soft"      { }
      "hard"      { }
      "nothreads" { }
      "setup"     { }
      "batch"     { set SPI::Param(Batch) True }
      "model"     { set SPI::Param(Exp) True }
      "nowindow"  { set SPI::Param(Window) False }
      "geom"      { set i [Args::ParseDo $argv $argc $i 0 1 "set SPI::Param(Geom)"] }
      "lang"      { set GDefs(Lang) [lindex $argv [incr i]] }
      "default"   { set i [Args::ParseDo $argv $argc $i 1 1 ""] }
      "field"     { set i [Args::ParseDo $argv $argc $i 1 0 "SPI::FileOpen NEW FieldBox \"\" \[list \$FileBox::Type(FSTD)\]"] }
      "traj"      { set i [Args::ParseDo $argv $argc $i 1 0 "SPI::FileOpen NEW TrajBox \"\" \[list \$FileBox::Type(TRAJ) \$FileBox::Type(HYSPLIT)\]"] }
      "obs"       { set i [Args::ParseDo $argv $argc $i 1 0 "SPI::FileOpen NEW ObsBox \"\" \[list \$FileBox::Type(OBS)\]"] }
      "metobs"    { set i [Args::ParseDo $argv $argc $i 1 0 "lappend SPI::Param(Tool) NowCaster; NowCaster::Obs::Add"] }
      "icon"      { set i [Args::ParseDo $argv $argc $i 1 0 "set SPI::Param(Icons)"] }
      "args"      { set i [Args::ParseDo $argv $argc $i 1 0 "set SPI::Param(Args)"] }
      "script"    { set i [Args::ParseDo $argv $argc $i 1 1 "set SPI::Param(Script)"] }
      "macro"     { set i [Args::ParseDo $argv $argc $i 1 1 "set SPI::Param(Script)"] }
      "pane"      { set i [Args::ParseDo $argv $argc $i 1 1 "set SPI::Param(Panes)"] }
      "side"      { set i [Args::ParseDo $argv $argc $i 0 1 "set SPI::Param(PaneSide)"] }
      "layout"    { set i [Args::ParseDo $argv $argc $i 0 1 "set SPI::Param(Layout)"] }
      "project"   { set i [Args::ParseDo $argv $argc $i 0 1 "set SPI::Param(Project)"] }
      "tool"      { set i [Args::ParseDo $argv $argc $i 1 1 "set SPI::Param(Tool)"] }
   }
}

SPI::Window
SPI::WindowMenu
SPI::Params
SPI::ContextMenuProj

#----- Creation des pages
set layout $SPI::Param(Layout)
SPI::WindowLayout $SPI::Param(PaneSide) $SPI::Param(Panes)

foreach page $SPI::Param(Pages) {
   set frame [SPI::PageNew False $page]
}
TabFrame::Select .mdi 0

SPI::Splash "Setting up tools"

#----- Boite d'experience
if { $SPI::Param(Exp) } {
   Model::Window True
}

#----- Ouvrir les fichiers icons
SPI::IcoOpen $SPI::Param(Icons)

#----- Inclusion du projet
if { $SPI::Param(Project)!="" } {
   SPI::ProjectRead $SPI::Param(Project) True
} else {

   #----- Selection du Layout pour la premiere page
   SPI::Splash "Setting up initial layout"
   if { ![SPI::LayoutLoad [lindex $Page::Data(Frames) 0] $layout] } {
      SPI::Quit 1
   }
}

#----- Selection d'un outils
foreach tool $SPI::Param(Tool) {
   eval ${tool}::Window
}

#----- Refresh final
SPI::Splash $SPI::Param(Script)
Page::Update

#----- On bypass les arguments regulier de SPI pour ceux du script a etre execute
set argv $SPI::Param(Args)
set argc [llength $SPI::Param(Args)]

#----- Patch pour les $#@%@! de system RedHat en #$%$#^@$ de 64 bit
#      Y faut faire un LD_PRELOAD de la lib GL avant de lancer le tout parce que le LD_LIBRARY_PATH
#      marche pas pis y faut le deloader parce que le 64 bit kapote quand on fait un exec
set env(LD_PRELOAD) ""

#----- Execution du script si necessaire
foreach script $SPI::Param(Script) {
   Log::Print INFO "System: Starting execution of script $script"
   SPI::Splash "Executing $script"
   SPI::Execute $script
   Log::Print INFO "System: Done executing script $script"
}
SPI::Splash
