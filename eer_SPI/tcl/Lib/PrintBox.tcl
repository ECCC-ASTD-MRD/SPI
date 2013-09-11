#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Widget de librairie.
# Fichier  : PrintBox.tcl
# Creation : Juillet 1998 - J.P. Gauthier - CMC/CMOE
#
# Description:
#    Definitions d'une boite de dialogue pour l'impresson de canvas
#
# Fonctions:
#    PrintBox::Create           { Frame Mode args }
#    PrintBox::Destroy          { }
#    PrintBox::Do               { Frame }
#    PrintBox::FilePathDefine   { Path }
#    PrintBox::Image            { Frame Format File { Angle portrait } }
#    PrintBox::Postscript       { Frame Format File { Device ps } }
#    PrintBox::Print            { Frame X Y Width Height { Format "" } }
#    PrintBox::PrintCommand     { Frame }
#    PrintBox::PrintTXT         { File }
#    PrintBox::Save             { Frame X Y Width Height File }
#    PrintBox::SelectOutputType { Type }
#
# Modifications :
#
#===============================================================================

package provide PrintBox 3.2

catch { SPI::Splash "Loading Widget Package PrintBox 3.2" }

package require FileBox
package require ComboBox
package require TabFrame
package require InfoFrame

namespace eval PrintBox {
   global  env GDefs
   variable Param
   variable Print
   variable Lbl
   variable Titre
   variable Txt
   variable Error

   #----- Definitions des parametres de transmission sur site WEB
   set Param(Path)           $env(HOME)                                       ;#Path du fichier de sortie
   set Param(Filename)       "output"                                         ;#Nom du fichier de sortie
   set Param(FullName)       "$Param(Path)/$Param(Filename)"                  ;#Nom complet
   set Param(Size)           "8.5_x_11"                                       ;#Format par defaut
   set Param(Sizes)          { 4_x_4 6_x_6 7_x_7 8.5_x_11 8.5_x_14 17_x_22 }  ;#Liste des formats
   set Param(Angle)          portrait                                         ;#Orientation
   set Param(Color)          color                                            ;#Couleur
   set Param(Margin)         0.20                                             ;#Marge
   set Param(WEBNameList)    ""                                               ;#Liste des noms de sites
   set Param(WEBPathList)    ""                                               ;#Liste des path de sites

   #----- Recuperer les imprimantes
   catch {
      set Param(Printers)    {}
      foreach printer [split [exec lpstat -a] \n] {
         lappend Param(Printers) [lindex $printer 0]
      }
      set Param(Printer) [lindex $Param(Printers) 0]                  ;#Imprimante par defaut
   }

   set Print(WEBSite)        ""                                             ;#Site WEB du transfert
   set Print(Job)            ""                                               ;#Traivail en cours
   set Print(DPI)            [expr [winfo screenwidth .]/([winfo screenmmwidth .]*0.03937008231878280600)] ;#DPI de l'ecran
   set Print(Type)           printer                                        ;#Type de sortie

   set Type(RASTER)  { {Adobe PostScript {*.ps}}
                       {Portable Network Graphics {*.png}}
                       {CompuServe Graphics Interchange {*.gif}}
                       {Portable pixmap {*.ppm}}}

   set Param(Format)    [lindex $Type(RASTER) 1]
   set Print(Device)    png                                            ;#Type de fichier

   #----- Definitions des labels

   set Lbl(Color)      { "Couleur" "Color" }
   set Lbl(Margin)     { "Marge" "Margin" }
   set Lbl(Close)      { "Fermer" "Close" }
   set Lbl(File)       { "Fichier" "File" }
   set Lbl(Format)     { "Format" "Format" }
   set Lbl(Gray)       { "Gris" "Gray" }
   set Lbl(Mono)       { "Mono" "Mono" }
   set Lbl(WEBSite)    { "Site WEB" "WEB Site" }
   set Lbl(JobTitle)   { "Impression en cours" "Printing" }
   set Lbl(Land)       { "Panoramique" "Landscape" }
   set Lbl(Orient)     { "Orientation" "Orientation" }
   set Lbl(Dim)        { "Dimension" "Dimension" }
   set Lbl(Portrait)   { "Portrait" "Portrait" }
   set Lbl(PrintDev)   { "M�thode" "Device" }
   set Lbl(Printer)    { "Imprimante" "Printer" }
   set Lbl(Print)      { "Imprimer" "Print" }
   set Lbl(Save)       { "Sauvegarder" "Save" }
   set Lbl(SendTo)     { "Envoyer vers" "Send to" }
   set Lbl(Version)    "3.1"

   #----- Definitions des titres

   set Titre(PrintBoxPrint) { "Param�tres d'impression" "Printing parameters" }
   set Titre(PrintBoxSave)  { "Param�tres de sauvegarde" "Saving parameters" }

   #----- Definitions des textes

   set Txt(Postscript) { "G�n�ration du postscript pour le canvas" "Generating postscript for canvas" }
   set Txt(Print)      { "Impression" "Printing" }
   set Txt(Image)      { "G�n�ration de l'image" "Generating image file" }

   #----- Definition des erreurs

   set Error(Convert)  { "Il semble y avoir un probl�me avec le fichier postscript ou la conversion de celui-ci"
                         "There seems to be an error within the postscript file or while converting the file" }
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::FontMap>
# Creation : Novembre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer un mapping des polices de caracteres.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::FontMap { } {
   variable Map

   foreach font [font names] {
      set f [lindex [string totitle [font configure $font -family]] 0]
      set w [string totitle [font configure $font -weight]]
      set i [string totitle [font configure $font -slant]]

      if { $i=="Italic" } {
         set Map($font) "-*-$f-$w-$i [expr abs([font configure $font -size])*[tk scaling]]"
      } else {
         set Map($font) "-*-$f-$w [expr abs([font configure $font -size])*[tk scaling]]"
      }
   }
   return Print::Map
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::PrintTXT>
# Creation : Avril 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Imprimer des fichiers textes.
#
# Parametres :
#  <File>    : Fichier a imprimer
#  <Printer> : Imprimante
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::PrintTXT { File } {
   global   GDefs
   variable Print
   variable Param

   if { $PrintBox::Print(Type)=="printer" } {
      if { $Param(Angle) == "portrait" } {
         catch { exec a2ps --columns=1 --rows=1 --font-size=9.0 -R -T3 $File -P $Param(Printer) }
      } else {
         catch { exec a2ps --columns=1 --rows=1 --font-size=9.0 -r -T3  $File -P $Param(Printer) }
      }
   } else {
      if { $Param(Angle) == "portrait" } {
         catch { exec a2ps --columns=1 --rows=1 --font-size=9.0 -R -T3 $File -o - | convert -density $Print(DPI) - $Param(FullName).$Print(Device) }
      } else {
         catch { exec a2ps --columns=1 --rows=1 --font-size=9.0 -r -T3 $File -o - | convert -density $Print(DPI) - $Param(FullName).$Print(Device) }
      }

      if { $Print(WEBSite)!="" } {
         set site [lindex $Param(WEBPathList) [lsearch -exact $Param(WEBNameList) $Print(WEBSite)]]
         InfoFrame::Msg .printbox.job "Transfering to WEB $site"
         exec chmod 644 $Param(FullName).$Print(Device)
         eval exec scp $Param(FullName).$Print(Device) ${site}/[file tail $Param(FullName)].$Print(Device) > /dev/null
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::PrintCommand>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Commande d'impression.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::PrintCommand { Frame } {
   global GDefs
   variable Print
   variable Lbl

   #----- Calcul du pourcentage maximum

   if { $Print(WEBSite)!="" } {
      set max 4
   } else {
      set max 3
   }
   InfoFrame::Set100 .printbox.job $max

   #----- Lancement de l'impression

   PrintBox::Print $Frame 0 0 [Page::CanvasWidth $Frame] [Page::CanvasHeight $Frame]
   PrintBox::Destroy
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::Create>
# Creation : Juin 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Boite de dialogue de la fonction d'impression.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Mode>    : Type d'impression (PRINT ou SAVE)
#  <args>    : Commande d'impression
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::Create { Frame Mode args } {
   global GDefs
   variable Print
   variable Param
   variable Type
   variable Titre
   variable Lbl

   if { [winfo exist .printbox] } {
      destroy .printbox
   }

   toplevel     .printbox
   wm transient .printbox $Frame
   wm geom      .printbox 335x220+[winfo rootx $Frame]+[winfo rooty $Frame]
   wm protocol  .printbox WM_DELETE_WINDOW { PrintBox::Destroy }

   .printbox configure -cursor left_ptr

   set Param(FullName) "$Param(Path)/$Param(Filename)"
   set Print(Job)      ""

   TabFrame::Create .printbox.tab 1 ""
   pack .printbox.tab -side top -fill both -expand true -padx 5 -pady 5

   if { $Mode=="PRINT" } {
      wm title     .printbox "[lindex $Titre(PrintBoxPrint) $GDefs(Lang)] $Lbl(Version)"
      set frame [TabFrame::Add .printbox.tab 1 [lindex $Lbl(Print) $GDefs(Lang)] False]

      frame $frame.sel
         label $frame.sel.lbl -text [lindex $Lbl(Printer) $GDefs(Lang)] -width 11 -anchor w
         ComboBox::Create $frame.sel.sel PrintBox::Param(Printer) \
            noedit unsorted nodouble -1 $Param(Printers) 28 5
         pack $frame.sel.lbl $frame.sel.sel -side left -padx 2
      pack $frame.sel -side top -pady 5

      frame $frame.page
         label $frame.page.lbl -text [lindex $Lbl(Dim) $GDefs(Lang)] -width 11 -anchor w
         ComboBox::Create $frame.page.sel PrintBox::Param(Size) \
            noedit unsorted nodouble -1 $Param(Sizes) 28 5
         pack  $frame.page.lbl $frame.page.sel -side left -padx 2
      pack $frame.page -side top

      frame $frame.ori
         label $frame.ori.lbl -text [lindex $Lbl(Orient) $GDefs(Lang)] -width 11 -anchor w
         ComboBox::Create $frame.ori.sel PrintBox::Param(Angle) \
            noedit unsorted nodouble -1 "portrait landscape" 28 5
         pack  $frame.ori.lbl $frame.ori.sel -side left -padx 2
      pack $frame.ori -side top -pady 5

      frame $frame.color
         label $frame.color.lbl -text [lindex $Lbl(Color) $GDefs(Lang)] -width 11 -anchor w
         ComboBox::Create $frame.color.sel PrintBox::Param(Color) \
            noedit unsorted nodouble -1 "color gray mono" 28 5
         pack  $frame.color.lbl $frame.color.sel -side left -padx 2
      pack $frame.color -side top

      frame $frame.margin
         label $frame.margin.lbl -text [lindex $Lbl(Margin) $GDefs(Lang)] -width 11 -anchor w
         spinbox $frame.margin.sel -textvariable PrintBox::Param(Margin) -width 10 -from 0.0 -to 5.0 -wrap 0 -bd 1 \
            -bg $GDefs(ColorLight) -increment 0.05 -width 28
         pack  $frame.margin.lbl $frame.margin.sel -side left -padx 2
      pack $frame.margin -side top -pady 5
   }

   if { $Mode=="SAVE" } {
      wm title     .printbox "[lindex $Titre(PrintBoxSave) $GDefs(Lang)] $Lbl(Version)"
      set frame [TabFrame::Add .printbox.tab 1 [lindex $Lbl(Save) $GDefs(Lang)] False]

         frame $frame.file
            label $frame.file.lbl -text [lindex $Lbl(File) $GDefs(Lang)] -width 8 -anchor w
            button $frame.file.sel -image OPEN -relief flat -bd 0 -overrelief raised \
               -command { PrintBox::FilePathDefine [FileBox::Create .printbox $PrintBox::Param(Path) Save [linsert $PrintBox::Type(RASTER) 0 $PrintBox::Param(Format)] $PrintBox::Param(Filename)] [FileBox::GetType] }
            entry $frame.file.name -width 32 -bg $GDefs(ColorLight) -textvariable PrintBox::Param(FullName) \
               -bd 1 -justify left
            $frame.file.name xview moveto 1
            pack $frame.file.lbl $frame.file.name $frame.file.sel -side left
         pack $frame.file -side top -pady 5

         frame $frame.format
            label $frame.format.lbl -text [lindex $Lbl(Format) $GDefs(Lang)] -width 8 -anchor w
            ComboBox::Create $frame.format.sel PrintBox::Param(Format) noedit unsorted nodouble -1 $Type(RASTER) 33 5 PrintBox::SetDevice
            pack $frame.format.lbl $frame.format.sel -side left
         pack $frame.format -side top

         frame $frame.web
            label $frame.web.lbl -text [lindex $Lbl(WEBSite) $GDefs(Lang)] -width 8 -anchor w
            ComboBox::Create $frame.web.sel PrintBox::Print(WEBSite) \
               noedit sorted nodouble -1 $Param(WEBNameList) 33 5
            ComboBox::Add $frame.web.sel ""
            pack $frame.web.lbl $frame.web.sel -side left
         pack $frame.web -side top -pady 5

      bind  $frame.file.name <KeyRelease> { PrintBox::FilePathDefine $PrintBox::Param(FullName) }
   }

   frame .printbox.par -relief raised -bd 1
   pack .printbox.par -side top -padx 5 -fill x

   InfoFrame::Create .printbox.job PrintBox::Print(Job) 100 -relief raised -bd 1
   pack .printbox.job  -side top -fill x -pady 5 -padx 5

   #----- Determiner les fonctions extensibles

   set ext     [lindex $args 0]
   set command "PrintBox::PrintCommand"

   if { $ext!="" } {

      if { [info procs ::${ext}::PrintWidget]!="" } {
         eval ${ext}::PrintWidget $Frame
      }

      if { [info procs ::${ext}::PrintCommand]!="" } {
         set command ${ext}::PrintCommand
      }
   }

   frame .printbox.command
      button .printbox.command.ok -text Ok -command ".printbox configure -cursor watch; update idletasks; $command $Frame" -bd 1
      button .printbox.command.cancel -text [lindex $Lbl(Close) $GDefs(Lang)] -command "destroy .printbox" -bd 1
      pack .printbox.command.ok .printbox.command.cancel -side left -fill x -expand true
   pack .printbox.command -side top -fill x -pady 5 -padx 5

   PrintBox::SelectOutputType $Mode
   PrintBox::SetDevice
   TabFrame::Select .printbox.tab 0

   raise .printbox
   grab .printbox
}

proc PrintBox::SetDevice { } {
   variable Print
   variable Param

   set Print(Device) [lindex [split [lindex $Param(Format) end] .] end]
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::Destroy>
# Creation : Fevrier 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Ferme (detruit) la boite de dialogue d'impression.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::Destroy { } {
   variable Print

   catch {
      ComboBox::Destroy .printbox.tab.frame1.sel.sel
      ComboBox::Destroy .printbox.tab.frame1.page.sel
      ComboBox::Destroy .printbox.tab.frame1.ori.sel
      ComboBox::Destroy .printbox.tab.frame2.format.sel
      ComboBox::Destroy .printbox.tab.frame2.web.sel
   }
   set Print(WEBSite)        ""

   destroy .printbox
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::Do>
# Creation : Novembre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Alias pour lancer l'impression pour les versions xbatch.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::Do { Frame } {

   update idletasks
   PrintBox::PrintCommand $Frame
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::FilePathDefine>
# Creation : Mars 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Extrait les partie du path specifie.
#
# Parametres :
#  <Path>    : Path complet
#  <Type>    : Type de fichier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::FilePathDefine { Path { Type "" } } {
   variable Print
   variable Param

   if { $Path == "" } {
      return
   }

   if { $Type!="" } {
      set Param(Format) $Type
      PrintBox::SetDevice
   }

   set Param(Path)     [file dirname $Path]
   set Param(Filename) [file tail $Path]
   set Param(FullName) $Path
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::Image>
# Creation : Janvier 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'impression du canvas en image.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Format>  : Format de l'image
#  <File>    : Nom du fichier
#  <Angle>   : Angle d'impression (portrait,landscape)
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::Image { Frame Format File { Angle portrait } } {

   set PrintBox::Print(Type)     file
   set PrintBox::Print(Device)   $Format
   set PrintBox::Param(FullName) $File
   set PrintBox::Param(Angle)    $Angle

   PrintBox::Do $Frame
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::Postscript>
# Creation : Mai 2005 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'impression du canvas a travers le postscript.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <Device>  : Type de fichier
#  <Format>  : Format de page
#  <File>    : Nom du fichier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::Postscript { Frame File X Y Width Height { Format "" } } {
   variable Param

   PrintBox::FontMap

   if { $Format!="" } {

      #----- Make sure we print everything asked by adding a 1 pixel border
      set X      [expr $X-1]
      set Y      [expr $Y-1]
      set Width  [expr $Width+2]
      set Height [expr $Height+2]

      #----- Define page output with margins
      set width  [expr [lindex [split $Format _] 0]-$Param(Margin)*2.0]
      set height [expr [lindex [split $Format _] 2]-$Param(Margin)*2.0]
      set w2     [expr ($width/2.0)+$Param(Margin)]
      set h2     [expr $height/2.0+$Param(Margin)]
      set ratio  [expr $width/$height]

      if { $Param(Angle)=="portrait" } {
        if { [expr double($Width)/double($Height)]>=$ratio } {
            $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate false -pagex ${w2}i -pagey ${h2}i -pageanchor c \
               -colormode $Param(Color) -pagewidth ${width}i -file $File.ps -fontmap PrintBox::Map
         } else {
            $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate false  -pagex ${w2}i -pagey ${h2}i -pageanchor c \
               -colormode $Param(Color) -pageheight ${height}i -file $File.ps -fontmap PrintBox::Map
         }
      } else {
         if { [expr double($Height)/double($Width)]<=$ratio } {
            $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate true  -pagex ${w2}i -pagey ${h2}i -pageanchor c \
               -colormode $Param(Color) -pagewidth ${height}i -file $File.ps -fontmap PrintBox::Map
         } else {
            $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate true -pagex ${w2}i -pagey ${h2}i -pageanchor c \
               -colormode $Param(Color) -pageheight ${width}i -file $File.ps -fontmap PrintBox::Map
         }
      }
   } else {
      if { $Param(Angle)=="portrait" } {
         $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate false \
            -colormode $Param(Color) -file $File.ps -fontmap PrintBox::Map
      } else {
         $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate true \
            -colormode $Param(Color) -file $File.ps -fontmap PrintBox::Map
      }
   }
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::Print>
# Creation : Juin 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue l'impression du canvas.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <X>       : Coordonnee X du coin superieur gauche
#  <Y>       : Coordonnee Y du coin superieur gauche
#  <Width>   : Largeur
#  <Height>  : Hauteur
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::Print { Frame X Y Width Height { Format "" } } {
   variable Print
   variable Param
   variable Error
   variable Txt
   global GDefs

   if { [file extension $Param(FullName)]==".$Print(Device)" } {
      set Param(FullName) [file rootname $Param(FullName)]
   }

   #----- Remove control widgets
   $Frame.page.canvas itemconfigure NOPRINT -state hidden

   #----- Generer le postscript en verifiant l'aspect du canvas par rapport au format et
   #      a l'orientation de la page selectionnee pour agrandir au maximum

   if { $Print(Type) == "printer" } {

      set tmpfile "/tmp/output_[pid]_[clock seconds]"

      InfoFrame::Incr .printbox.job 1 "[lindex $Txt(Postscript) $GDefs(Lang)] $Frame"
      PrintBox::Postscript $Frame $tmpfile $X $Y $Width $Height $Param(Size)

      InfoFrame::Incr .printbox.job 1 "[lindex $Txt(Print) $GDefs(Lang)] $Param(Printer)"
      if { $Param(Printer) == "wideprnt1" || $Param(Printer) == "wideprnt2" } {
         catch { exec convert -density 200 -colors 2 -dither -monochrome $tmpfile.ps $tmpfile.tiff }
         exec lpr -s -r -P$Param(Printer) $tmpfile.tiff
         file delete $tmpfile.ps
      } else {
         exec lpr -s -r -P$Param(Printer) $tmpfile.ps
      }
   } else {
      if { $Print(Device)!="ps" } {
         InfoFrame::Incr .printbox.job 1 "[lindex $Txt(Image) $GDefs(Lang)] $Frame"
         PrintBox::Save $Frame $X $Y $Width $Height $Param(FullName)
      } else {
         InfoFrame::Incr .printbox.job 1 "[lindex $Txt(Postscript) $GDefs(Lang)] $Frame"
         PrintBox::Postscript $Frame $Param(FullName) $X $Y $Width $Height
      }

      InfoFrame::Incr .printbox.job 1

      #----- Si un transfert WEB est requis

      if { $Print(WEBSite)!="" } {
         set site [lindex $Param(WEBPathList) [lsearch -exact $Param(WEBNameList) $Print(WEBSite)]]
         InfoFrame::Incr .printbox.job 1 "Transfering to WEB $site"
         exec chmod 644 $Param(FullName).$Print(Device)

         #----- Hardcode temporaire pour envoyer sur le nouveau site

         if { $Print(WEBSite)=="WEB_VAAC" } {
            set prefix [clock format [clock seconds] -format "%Y%m%d-%H%MZ" -gmt True]
            set ErrCatch [catch  { exec $GDefs(Dir)/Script/CMOI_webprods.ksh $Param(FullName).$Print(Device) eer/data/vaac/current/${prefix}_[file tail $Param(FullName)].$Print(Device) $GDefs(TransmitUser) $GDefs(TransmitHost) } MsgCatch ]

            if { $ErrCatch != 0 } {
               Log::Print ERROR "Unable to transfert the $Param(FullName).$Print(Device) on meteo web site via $GDefs(TransmitUser)@$GDefs(TransmitHost).\n\n$MsgCatch"
            }

         } else {
            eval exec scp $Param(FullName).$Print(Device) ${site}/[file tail $Param(FullName)].$Print(Device) > /dev/null
         }

         file delete -force $Param(FullName).$Print(Device)
      }
   }

   #----- Remettre la bordure a sa valeur d'origine
   Page::WidgetShow $Frame.page.canvas {} 0 0 True

   InfoFrame::Incr .printbox.job 1
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::Save>
# Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue la sauvegarde directe du framebuffer.
#
# Parametres :
#  <Frame>   : Identificateur de Page
#  <X>       : Coordonnee X du coin superieur gauche
#  <Y>       : Coordonnee Y du coin superieur gauche
#  <Width>   : Largeur
#  <Height>  : Hauteur
#  <File>    : Path du fichier
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::Save { Frame X Y Width Height File } {
   variable Print

   image create photo TMPIMG
   $Frame.page.canvas buffer TMPIMG $X $Y $Width $Height
   TMPIMG write "$File.$Print(Device)" -format $Print(Device)
   image delete TMPIMG
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::SelectOutputType>
# Creation : Juin 1998 - J.P. Gauthier - CMC/CMOE
#
# But      : Effectue les changements necessaire dan la fenetre d'impression
#            selon le type selectionne (Imprimante ou fichier).
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc PrintBox::SelectOutputType { Type } {
   variable Print
   variable Param

   if { $Type == "PRINT" } {
      set Print(Type)      printer
      set Print(Device)    ps
      set Param(Size)    8.5_x_11
      set Print(WEBSite)   ""
   } else {
      set Print(Type)   file
      PrintBox::SetDevice
   }
}