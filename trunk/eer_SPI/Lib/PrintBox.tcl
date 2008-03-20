#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Widget de librairie.
# Fichier  : PrintBox.tk
# Version  : 3.2
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
#    PrintBox::Image            { Frame Format File }
#    PrintBox::Postscript       { Frame Format File { Device ps } }
#    PrintBox::Print            { Frame X Y Width Height }
#    PrintBox::PrintCommand     { Frame }
#    PrintBox::PrintTXT         { File }
#    PrintBox::ReadWEBPath      { }
#    PrintBox::Save             { Frame X Y Width Height File }
#    PrintBox::SelectOutputType { Type }
#
# Modifications :
#
#   Nom         : J.P. Gauthier
#   Date        : Fevrier 1999
#   Description : Generalisation de l'interface
#
#   Nom         : J.P. Gauthier
#   Date        : Juin 1999
#   Description : Utilisation de "convert" plutot que les filtres "pnm..."
#
#   Nom         : J.P. Gauthier
#   Date        : Juin 1999
#   Description : Ajout du parametre de DPI pour les impressions "image"
#
#   Nom         : J.P. Gauthier
#   Date        : Septembre 2000
#   Description : Refonte complete de l'interface
#
#   Nom         : J.P. Gauthier
#   Date        : Juillet 2001
#   Description : Patch pour retirer le "showpage" lors de conversion avec convert
#                 DPI a 75 par defaut
#===============================================================================

package provide PrintBox 3.2

proc IdPrintBox { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Widget Package PrintBox Version 3.2"
   }

   package require FileBox    ; IdFileBox    False
   package require ComboBox   ; IdComboBox   False
   package require FrameDefs  ; IdFrameDefs  False
   package require InfoFrame  ; IdInfoFrame  False
}

namespace eval PrintBox {
   global  env GDefs
   variable Print
   variable Lbl
   variable Titre
   variable Txt
   variable Error

   #----- Definitions des parametres d'impression

   catch {
      set Print(Printer_List)   $GDefs(Printers)                                 ;#Liste des imprimantes
      set Print(Printer)        [lindex $Print(Printer_List) 0]                  ;#Imprimante par defaut
   }

   set Print(Job)            ""                                               ;#Traivail en cours
   set Print(DPI)            [expr [winfo screenwidth .]/([winfo screenmmwidth .]*0.03937008231878280600)] ;#DPI de l'ecran
   set Print(Format)         "8.5_x_11"                                       ;#Format par defaut
   set Print(Format_List)    { 4_x_4 6_x_6 7_x_7 8.5_x_11 8.5_x_14 17_x_22 }  ;#Liste des formats
   set Print(Type)           printer                                        ;#Type de sortie
   set Print(Angle)          portrait                                       ;#Orientation
   set Print(Path)           $env(HOME)                                     ;#Path du fichier de sortie
   set Print(Filename)       "output"                                       ;#Nom du fichier de sortie
   set Print(FullName)       "$Print(Path)/$Print(Filename)"                ;#Nom complet
   set Print(InternalFile)   ""                                             ;#Fichier interne utilise pour generer le ps

   set Type(RASTER)  { {Adobe PostScript {*.ps}}
                       {Portable Network Graphics {*.png}}
                       {Joint Photographic Experts Group {*.jpg}}
                       {CompuServe Graphics Interchange {*.gif}}
                       {Truevision Targa {*.tga}}
                       {Tagged Image File {*.tif}}
                       {Portable Anymap {*.pnm}}
                       {Portable pixmap {*.ppm}}
                       {Portable bitmap {*.pbm}}
                       {XWindows Bitmap {*.xbm}}
                       {XWindows Pixmap {*.xpm}}
                       {Microsoft Windows DIB {*.dib}}
                       {Microsoft Windows bitmap image {*.bmp}}
                       {ZSoft Paintbrush {*.pcx}}
                       {Hierarchical Data Format {*.hdf}}
                       {Hypertext Markup Language {*.html}}
                       {Magick image {*.miff}}
                       {MTV Raytracing {*.mtv}}
                       {Photo CD {*.pcd}}
                       {Portable Document Format {*.pdf}}
                       {Portable Graymap {*.pgm}}
                       {Apple QuickDraw {*.pict}}
                       {Radiance {*.rad}}
                       {Raw RGB {*.rgb}}
                       {Alias Wavefront {*.rla}}
                       {Utah Run Length Encoded {*.rle}}
                       {Irix RGB {*.sgi}}
                       {SUN Rasterfile {*.sun}}
                       {16Bit/Pixel YUV {*.uyvy}}
                       {Flexible Image Transport System {*.fits}}
                       {AVS X image {*.avs}}
                       {Raw Bytes {*.cmyk}}
                       {Joint Bi-level Image experts Group {*.bie}}
                       {Khoros Visualization {*.viff}}
                       {FAX Group3 {*.fax *.g3}}
                       {FlashPix {*.fpx}}
                       {Raw gray {*.gray}}
                       {CCIR 601 4:1:1 {*.yuv}} }

   set Print(DeviceDef) [lindex $Type(RASTER) 1]
   set Print(FileType)  $Print(DeviceDef)
   set Print(Device)    png                                            ;#Type de fichier

   #----- Definitions des parametres de transmission sur site WEB

   set Print(WEBSite)        ""                                             ;#Site WEB du transfert
   set Print(WEBNameList)    ""                                             ;#Liste des noms de sites
   set Print(WEBPathList)    ""                                             ;#Liste des path de sites

   #----- Definitions des labels

   set Lbl(Color)      { "Couleur" "Color" }
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
   set Lbl(PrintDev)   { "Méthode" "Device" }
   set Lbl(Printer)    { "Imprimante" "Printer" }
   set Lbl(Print)      { "Imprimer" "Print" }
   set Lbl(Save)       { "Sauvegarder" "Save" }
   set Lbl(SendTo)     { "Envoyer vers" "Send to" }
   set Lbl(Version)    "3.1"

   #----- Definitions des titres

   set Titre(PrintBoxPrint) { "Paramêtres d'impression" "Printing parameters" }
   set Titre(PrintBoxSave)  { "Paramêtres de sauvegarde" "Saving parameters" }

   #----- Definitions des textes

   set Txt(Postscript) { "Génération du postscript pour le canvas" "Generating postscript for canvas" }
   set Txt(Print)      { "Impression" "Printing" }
   set Txt(Image)      { "Génération de l'image" "Generating image file" }

   #----- Definition des erreurs

   set Error(Convert)  { "Il semble y avoir un problème avec le fichier postscript ou la conversion de celui-ci"
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc PrintBox::PrintTXT { File } {
   global   GDefs
   variable Print

   if { $PrintBox::Print(Type)=="printer" } {
      if { $Print(Angle) == "portrait" } {
         catch { exec a2ps --columns=1 --rows=1 --font-size=9.0 -R -T3 $File | lpr -P$Print(Printer) }
      } else {
         catch { exec a2ps --columns=1 --rows=1 --font-size=9.0 -r -T3  $File | lpr -P$Print(Printer) }
      }
   } else {
      if { $Print(Angle) == "portrait" } {
         catch { exec a2ps --columns=1 --rows=1 --font-size=9.0 -R -T3 $File -o - | convert -density $Print(DPI) - $Print(FullName).$Print(Device) }
      } else {
         catch { exec a2ps --columns=1 --rows=1 --font-size=9.0 -r -T3 $File -o - | convert -density $Print(DPI) - $Print(FullName).$Print(Device) }
      }

      if { $Print(WEBSite)!="" } {
         set site [lindex $Print(WEBPathList) [lsearch -exact $Print(WEBNameList) $Print(WEBSite)]]
         InfoFrame::Msg .printbox.job "Transfering to WEB $site"
         exec chmod 644 $Print(FullName).$Print(Device)
         eval exec scp $Print(FullName).$Print(Device) ${site}/[file tail $Print(FullName)].$Print(Device) > /dev/null
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc PrintBox::Create { Frame Mode args } {
   global GDefs
   variable Print
   variable Type
   variable Titre
   variable Lbl

   if { [winfo exist .printbox] } {
      destroy .printbox
   }

   #----- Initialisations des donnees

   ReadWEBPath

   toplevel     .printbox
   wm transient .printbox $Frame
   wm geom      .printbox 335x195+[winfo rootx $Frame]+[winfo rooty $Frame]
   wm protocol  .printbox WM_DELETE_WINDOW { PrintBox::Destroy }

   .printbox configure -cursor left_ptr

   set Print(FullName) "$Print(Path)/$Print(Filename)"
   set Print(Job)      ""

   TabFrame::Create .printbox.tab 1 ""
   pack .printbox.tab -side top -fill both -expand true -padx 5 -pady 5

   if { $Mode=="PRINT" } {
      wm title     .printbox "[lindex $Titre(PrintBoxPrint) $GDefs(Lang)] $Lbl(Version)"
   set frame [TabFrame::Add .printbox.tab 1 [lindex $Lbl(Print) $GDefs(Lang)] False]

      frame $frame.sel
         label $frame.sel.lbl -text [lindex $Lbl(Printer) $GDefs(Lang)] -width 11 -anchor w
         ComboBox::Create $frame.sel.sel PrintBox::Print(Printer) \
            noedit unsorted nodouble -1 $Print(Printer_List) 28 5
         pack $frame.sel.lbl $frame.sel.sel -side left -padx 2
      pack $frame.sel -side top -pady 5

      frame $frame.page
         label $frame.page.lbl -text [lindex $Lbl(Dim) $GDefs(Lang)] -width 11 -anchor w
         ComboBox::Create $frame.page.sel PrintBox::Print(Format) \
            noedit unsorted nodouble -1 $Print(Format_List) 28 5
         pack  $frame.page.lbl $frame.page.sel -side left -padx 2
      pack $frame.page -side top

      frame $frame.ori
         label $frame.ori.lbl -text [lindex $Lbl(Orient) $GDefs(Lang)] -width 11 -anchor w
         ComboBox::Create $frame.ori.sel PrintBox::Print(Angle) \
            noedit unsorted nodouble -1 "portrait landscape" 28 2
         pack  $frame.ori.lbl $frame.ori.sel -side left -padx 2
      pack $frame.ori -side top -pady 5
   }

   if { $Mode=="SAVE" } {
   wm title     .printbox "[lindex $Titre(PrintBoxSave) $GDefs(Lang)] $Lbl(Version)"
      set frame [TabFrame::Add .printbox.tab 1 [lindex $Lbl(Save) $GDefs(Lang)] False]

         frame $frame.file
            label $frame.file.lbl -text [lindex $Lbl(File) $GDefs(Lang)] -width 8 -anchor w
            button $frame.file.sel -image OPEN -relief flat -bd 0 -overrelief raised \
               -command { PrintBox::FilePathDefine [FileBox::Create .printbox $PrintBox::Print(Path) Save [linsert $PrintBox::Type(RASTER) 0 $PrintBox::Print(FileType)] $PrintBox::Print(Filename)] [FileBox::GetType] }
            entry $frame.file.name -width 32 -bg $GDefs(ColorLight) -textvariable PrintBox::Print(FullName) \
               -bd 1 -justify left
            $frame.file.name xview moveto 1
            pack $frame.file.lbl $frame.file.name $frame.file.sel -side left
         pack $frame.file -side top -pady 5

         frame $frame.format
            label $frame.format.lbl -text [lindex $Lbl(Format) $GDefs(Lang)] -width 8 -anchor w
            ComboBox::Create $frame.format.sel PrintBox::Print(FileType) noedit unsorted nodouble -1 $Type(RASTER) 33 5 PrintBox::SetDevice
            pack $frame.format.lbl $frame.format.sel -side left
         pack $frame.format -side top

         frame $frame.web
            label $frame.web.lbl -text [lindex $Lbl(WEBSite) $GDefs(Lang)] -width 8 -anchor w
            ComboBox::Create $frame.web.sel PrintBox::Print(WEBSite) \
               noedit sorted nodouble -1 $Print(WEBNameList) 33 5
            ComboBox::Add $frame.web.sel ""
            pack $frame.web.lbl $frame.web.sel -side left
         pack $frame.web -side top -pady 5

      bind  $frame.file.name <KeyRelease> { PrintBox::FilePathDefine $PrintBox::Print(FullName) }
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
}

proc PrintBox::SetDevice { } {
   variable Print

   set Print(Device) [lindex [split [lindex $Print(FileType) end] .] end]
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc PrintBox::FilePathDefine { Path { Type "" } } {
   variable Print

   if { $Path == "" } {
      return
   }

   if { $Type!="" } {
      set Print(FileType) $Type
      PrintBox::SetDevice
   }

   set Print(Path)     [file dirname $Path]
   set Print(Filename) [file tail $Path]
   set Print(FullName) $Path
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
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc PrintBox::Image { Frame Format File } {

   set PrintBox::Print(Type)     file
   set PrintBox::Print(Device)   $Format
   set PrintBox::Print(FullName) $File

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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc PrintBox::Postscript { Frame File X Y Width Height { Angle portrait } { Format "" } } {

   if { $Format!="" } {

      #----- Definir le format par rapport a la page (avec marges de 1 pouces)

      set width  [expr [lindex [split $Format _] 0]-0.5]
      set height [expr [lindex [split $Format _] 2]-0.5]
      set ratio  [expr $width/$height]

      if { $Angle=="portrait" } {
         if { [expr double($Width)/double($Height)]>=$ratio } {
            $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate false \
               -colormode color -pagewidth ${width}i -file $File.ps -fontmap PrintBox::Map
         } else {
            $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate false \
               -colormode color -pageheight ${height}i -file $File.ps -fontmap PrintBox::Map
         }
      } else {
         if { [expr double($Height)/double($Width)]<=$ratio } {
            $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate true \
               -colormode color -pagewidth ${height}i -file $File.ps -fontmap PrintBox::Map
         } else {
            $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate true \
               -colormode color -pageheight ${width}i -file $File.ps -fontmap PrintBox::Map
         }
      }
   } else {
      if { $Angle=="portrait" } {
         $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate false \
            -colormode color -file $File.ps -fontmap PrintBox::Map
      } else {
         $Frame.page.canvas postscript -x $X -y $Y -width $Width -height $Height -rotate true \
            -colormode color -file $File.ps -fontmap PrintBox::Map
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc PrintBox::Print { Frame X Y Width Height { Format "" } } {
   variable Print
   variable Error
   variable Txt
   global GDefs

   set Print(InternalFile) "/tmp/output_[pid]_[clock seconds]"

   if { [file extension $Print(FullName)]==".$Print(Device)" } {
      set Print(FullName) [file rootname $Print(FullName)]
   }

   PrintBox::FontMap

   #----- Configurer la bordure du Canvas a 0 pour ne pas que l'image
   #      depasse sur les cotes

   $Frame.page.canvas move NOPRINT -10000 -10000
   update idletasks

   #----- Generer le postscript en verifiant l'aspect du canvas par rapport au format et
   #      a l'orientation de la page selectionnee pour agrandir au maximum

   if { $Print(Type) == "printer" } {

      InfoFrame::Incr .printbox.job 1 "[lindex $Txt(Postscript) $GDefs(Lang)] $Frame"

      PrintBox::Postscript $Frame $Print(InternalFile) $X $Y $Width $Height $Print(Angle) $Print(Format)

      InfoFrame::Incr .printbox.job 1 "[lindex $Txt(Print) $GDefs(Lang)] $Print(Printer)"

      if { $Print(Printer) == "wideprnt1" || $Print(Printer) == "wideprnt2" } {
         catch { exec convert -density 200 -colors 2 -dither -monochrome $Print(InternalFile).ps $Print(InternalFile).tiff }
         exec lpr -s -r -P$Print(Printer) $Print(InternalFile).tiff
         file delete $Print(InternalFile).ps
      } else {
         exec lpr -s -r -P$Print(Printer) $Print(InternalFile).ps
      }
   } else {
      if { $Print(Device)!="ps" } {
         InfoFrame::Incr .printbox.job 1 "[lindex $Txt(Image) $GDefs(Lang)] $Frame"

         PrintBox::Save $Frame $X $Y $Width $Height $Print(InternalFile)

         InfoFrame::Incr .printbox.job 1 "[lindex $Txt(Image) $GDefs(Lang)]"

         if { $Print(Device)=="ppm" } {
            file rename -force $Print(InternalFile).ppm  $Print(FullName).$Print(Device)
         } else {
            exec convert $Print(InternalFile).ppm $Print(FullName).$Print(Device)
            file delete -force $Print(InternalFile).ppm
         }
      } else {

         InfoFrame::Incr .printbox.job 1 "[lindex $Txt(Postscript) $GDefs(Lang)] $Frame"

         PrintBox::Postscript $Frame $Print(InternalFile) $X $Y $Width $Height

         InfoFrame::Incr .printbox.job 1 "[lindex $Txt(Image) $GDefs(Lang)]"

         file rename -force $Print(InternalFile).ps  $Print(FullName).$Print(Device)
      }

      InfoFrame::Incr .printbox.job 1

      #----- Si un transfert WEB est requis

      if { $Print(WEBSite)!="" } {
         set site [lindex $Print(WEBPathList) [lsearch -exact $Print(WEBNameList) $Print(WEBSite)]]
         InfoFrame::Incr .printbox.job 1 "Transfering to WEB $site"
         exec chmod 644 $Print(FullName).$Print(Device)

         #----- Hardcode temporaire pour envoyer sur le nouveau site

         if { $Print(WEBSite)=="WEATHEROFFICE_VAAC" } {
            exec rsh $GDefs(FrontEnd) -l $GDefs(FrontEndUser) -n ". ~/.profile; /software/pub/bin/udo afsiadm webprods -f $Print(FullName).$Print(Device) -s weather -D 0 -p eer/data/vaac/current/[file tail $Print(FullName)].$Print(Device)"
         } else {
            eval exec scp $Print(FullName).$Print(Device) ${site}/[file tail $Print(FullName)].$Print(Device) > /dev/null
         }

         file delete -force $Print(FullName).$Print(Device)
      }
   }

   #----- Remettre la bordure a sa valeur d'origine

   update idletasks
   $Frame.page.canvas move NOPRINT 10000 10000

   InfoFrame::Incr .printbox.job 1
}

#----------------------------------------------------------------------------
# Nom      : <PrintBox::ReadWEBPath>
# Creation : Mai 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Lire la liste des site web utilises.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc PrintBox::ReadWEBPath { } {
   global env
   variable Print

   set Print(WEBNameList)    ""
   set Print(WEBPathList)    ""

   if { ![catch { set file [open $env(HOME)/.eer_ToolDefs/eer_WEBPath r] }]  } {

      while { ![eof $file] } {

         gets $file line
         if { [string index $line 0] != "#" && [string length $line] > 0 } {
            set line [split $line ","]
            lappend Print(WEBNameList) [lindex $line 0]
            lappend Print(WEBPathList) [lindex $line 1]
         }
      }
      close $file
  }
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc PrintBox::Save { Frame X Y Width Height File } {

   image create photo TMPIMG
   $Frame.page.canvas buffer TMPIMG $X $Y $Width $Height
   TMPIMG write "$File.ppm" -format PPM
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
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc PrintBox::SelectOutputType { Type } {
   variable Print

   if { $Type == "PRINT" } {
      set Print(Type)      printer
      set Print(DeviceDef) $Print(FileType)
      set Print(Device)    ps
      set Print(Format)    8.5_x_11
      set Print(WEBSite)   ""
   } else {
      set Print(Type)   file
      set Print(FileType) $Print(DeviceDef)
      PrintBox::SetDevice
   }
}
