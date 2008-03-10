#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Tools" Tk.
# Fichier  : Mapper_DepotWare.tcl
# Version  : 1.0
# Creation : Novembre 2007 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation et de gestion des depots de donnees georeference.
#
# Fonctions:
#
#
# Remarques :
#
# Modification:
#
#   Nom         :
#   Date        :
#   Description :
#
#===============================================================================

namespace eval Mapper::DepotWare {
   variable WMS
   variable Data
   variable Lbl
   variable Msg

   set Data(CachePath) /tmp/$env(USER)/gdalwmscache                ;#Path du cache pour le WMS
   set Data(CacheSize) 1024                                        ;#Dimension maximale du cache WMS

   ::struct::tree TREE

   set Data(Depots) {}
   set Data(Coo)    ""
   set Data(Select) ""
   set Data(Name)   ""
   set Data(Lat0)   -90.0
   set Data(Lat1)   -90.0
   set Data(Lon0)   -180.0
   set Data(Lon1)   -180.0

   set Data(GDALExclude) { .hdr .jgw .txt .met }  ;# Fichier a exclure
   set Data(OGRExclude)  { .dbf .shx .txt }  ;# Fichier a exclure

   set WMS(BlockSize) 512
   set WMS(Layers)  {}

   set WMS(SizeX)        0
   set WMS(SizeY)        0
   set WMS(Version)      ""
   set WMS(Name)         ""
   set WMS(Format)       ""
   set WMS(Title)        ""
   set WMS(BBox)         ""
   set WMS(Styles)       ""
   set WMS(Geographic)   ""
   set WMS(Opaque)       0

   set Lbl(Title)     { "Paramêtres du dépot de données" "Data repository parameters" }
   set Lbl(Types)     {  "DIR - Data directory" "WMS - Web Mapping Service" }
#   set Lbl(Types)     {  "DIR - Data directory" "WMS - Web Mapping Service" "WCS - Web Coverage Service" "WFS - Web Feature Service" }
   set Lbl(Path)      { "Localisation" "Localisation" }
   set Lbl(Type)      { "Type" "Type" }
   set Lbl(Cache)     { "Cache" "Cache" }
   set Lbl(Yes)       { "Oui" "Yes" }
   set Lbl(No)        { "Non" "No" }
   set Lbl(Del)       { "Supprimer" "Delete" }
   set Lbl(Display)   { "Afficher" "Display" }
   set Lbl(Index)     { "Afficher l'index" "Display index" }
   set Lbl(Name)      { "Identification" "Identification" }
   set Lbl(Params)    { "Paramêtres" "Parameters" }
   set Lbl(Save)      { "Sauvegarder" "Save" }
   set Lbl(Cancel)    { "Annuler" "Cancel" }

   set Msg(WMSRequest) { "Problème dans la requète de capacitées (GetCapabilities)" "Problem requesting capabilities (GetCapabilities)" }
   set Msg(Search)     { "Recherche ..." "Searching ..." }
   set Msg(Del)        { "Voulez-vous vraiment supprimer ce dépot de la liste ?" "Do you really want to remove this repository from the list ?" }
}

proc Mapper::DepotWare::CacheClean { } {
   variable Data

   file delete -force $Data(CachePath)
   file mkdir $Data(CachePath)

   set Data(CacheSize) 0
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::Params>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Interface des parametres d'un depot de donnees.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::Params { { Save 1 } } {
   global GDefs
   variable Data
   variable Lbl

   toplevel         .mapperdepot
   wm title         .mapperdepot [lindex $Lbl(Title) $GDefs(Lang)]
   wm resizable     .mapperdepot 1 1
   wm protocol      .mapperdepot WM_DELETE_WINDOW { }
   wm geom          .mapperdepot +[expr [winfo rootx .mapper]+50]+[expr [winfo rooty .mapper]+50]

  catch { set Data(CacheSize) [lindex [exec du -ms $Data(CachePath)] 0] }

   frame .mapperdepot.bg  -relief raised -bd 1
      frame .mapperdepot.bg.type
         label .mapperdepot.bg.type.lbl -anchor w -text [lindex $Lbl(Type) $GDefs(Lang)] -width 15
         ComboBox::Create .mapperdepot.bg.type.sel Mapper::DepotWare::Data(Type) noedit unsorted nodouble -1 $Lbl(Types) 55 14 ""
         pack .mapperdepot.bg.type.lbl -side left
         pack .mapperdepot.bg.type.sel -side left -fill x -expand True
      pack .mapperdepot.bg.type -fill x -expand True  -padx 5

      frame .mapperdepot.bg.name
         label .mapperdepot.bg.name.lbl -anchor w -text [lindex $Lbl(Name) $GDefs(Lang)] -width 15
         entry .mapperdepot.bg.name.ent -width 1 -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::DepotWare::Data(Name)
         pack .mapperdepot.bg.name.lbl -side left
         pack .mapperdepot.bg.name.ent -side left  -fill x -expand True
      pack .mapperdepot.bg.name -fill x -expand True  -padx 5

      frame .mapperdepot.bg.path
         label .mapperdepot.bg.path.lbl -anchor w -text [lindex $Lbl(Path) $GDefs(Lang)] -width 15
         button .mapperdepot.bg.path.open -image OPEN -bd 0 -relief flat -overrelief raised -relief raised \
            -command  { set Mapper::DepotWare::Data(Path) [FileBox::Create . "" LoadPath [concat [list $FileBox::Type(ALL)] $Mapper::Data(GDALFormats) $Mapper::Data(OGRFormats)]] }
         entry .mapperdepot.bg.path.ent -width 1 -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::DepotWare::Data(Path)
         pack .mapperdepot.bg.path.lbl -side left
         pack .mapperdepot.bg.path.ent -side left  -fill x -expand True
         pack .mapperdepot.bg.path.open -side left
      pack .mapperdepot.bg.path -fill x -expand True  -padx 5

      frame .mapperdepot.bg.cache
         label .mapperdepot.bg.cache.lbl -anchor w -text [lindex $Lbl(Cache) $GDefs(Lang)] -width 15
         button .mapperdepot.bg.cache.open -image OPEN -bd 0 -relief flat -overrelief raised -relief raised \
            -command  { set Mapper::DepotWare::Data(CachePath) [FileBox::Create . $Mapper::DepotWare::Data(CachePath) LoadPath {}] }
         entry .mapperdepot.bg.cache.ent -width 1 -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::DepotWare::Data(CachePath)
         entry .mapperdepot.bg.cache.szent -width 6 -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::DepotWare::Data(CacheSize)
         button .mapperdepot.bg.cache.clean -image DELETE -bd 0 -relief flat -overrelief raised -relief raised \
            -command  { Mapper::DepotWare::CacheClean }
         label .mapperdepot.bg.cache.szlbl -anchor w -text Mb -width 2
         pack .mapperdepot.bg.cache.lbl .mapperdepot.bg.cache.szent .mapperdepot.bg.cache.szlbl -side left
         pack .mapperdepot.bg.cache.ent -side left  -fill x -expand True
         pack .mapperdepot.bg.cache.clean .mapperdepot.bg.cache.open -side left
      pack .mapperdepot.bg.cache -fill x -expand True  -padx 5

   pack .mapperdepot.bg -side top -fill both -expand True

   frame .mapperdepot.cmd
      button .mapperdepot.cmd.ok -bd 1 -text [lindex $Lbl(Save) $GDefs(Lang)] -command  { Mapper::DepotWare::Add $Mapper::DepotWare::Data(Name) [lindex $Mapper::DepotWare::Data(Type) 0] $Mapper::DepotWare::Data(Path); destroy .mapperdepot }
      button .mapperdepot.cmd.cancel -bd 1 -text [lindex $Lbl(Cancel) $GDefs(Lang)] -command  { destroy .mapperdepot }
      pack .mapperdepot.cmd.ok .mapperdepot.cmd.cancel -side left  -fill x -expand True
   pack .mapperdepot.cmd -side top -fill x -expand True

   if { $Save } {
      .mapperdepot.cmd.ok configure -state normal
   } else {
      .mapperdepot.cmd.ok configure -state disabled
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::Add>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajout d<un nouveaux depot de donnees.
#
# Parametres :
#   <Name>   : Identification du depot
#   <Type>   : Type de depot (WMS,WCS,WFS,DIR)
#   <Path>   : Path ou UR du depot
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::Add { Name Type Path } {
   global GDefs
   variable Data

   if { ![file exists $GDefs(DirEER)/Mapper] || ![file isdirectory $GDefs(DirEER)/Mapper] } {
      file mkdir $GDefs(DirEER)/Mapper
   }
   if { $Type!="DIR" } {
      set Type URL$Type
   }

   if  { $Name=="" } {
      set Name $Path
   }

   lappend Data(Depots) [list $Name $Type $Path]
   exec echo "set Mapper::DepotWare::Data(CachePath) $Data(CachePath)\nset Mapper::DepotWare::Data(Depots) { $Data(Depots) }" > $GDefs(DirEER)/Mapper/Params

   set idx [TREE insert root end]
   TREE set $idx open False
   TREE set $idx name $Name
   TREE set $idx path $Path
   TREE set $idx type $Type

   Mapper::DepotWare::Tree $Mapper::Data(Tab2).list.canvas
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::Del>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Suppression d'un depot de donnees.
#
# Parametres :
#   <Branch> : Id de la branche a supprimer
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::Del { Branch } {
   global GDefs
   variable Lbl
   variable Msg
   variable Data

   if { $Branch!="" && [TREE depth $Branch]==1 } {
      if { ![file exists $GDefs(DirEER)/Mapper] || ![file isdirectory $GDefs(DirEER)/Mapper] } {
         file mkdir $GDefs(DirEER)/Mapper
      }

      set name [TREE get $Branch name]
      set type [TREE get $Branch type]
      set path [TREE get $Branch path]
      set nodel [Dialog::CreateDefault . 400 [lindex $Lbl(Del) $GDefs(Lang)] \
         "[lindex $Msg(Del) $GDefs(Lang)]\n\n$name\n" \
         warning 0 [lindex $Lbl(Yes) $GDefs(Lang)] [lindex $Lbl(No) $GDefs(Lang)]]

      if { !$nodel } {
         set idx 0
         foreach depot $Data(Depots) {
            if { [lindex $depot 0]==$name && [lindex $depot 1]==$type &&  [lindex $depot 2]==$path } {
               set Data(Depots) [lreplace $Data(Depots) $idx $idx]
               exec echo "set Mapper::DepotWare::Data(Depots) { $Data(Depots) }" > $GDefs(DirEER)/Mapper/Params
            }
            incr idx
         }
         TREE delete $Branch
         $Mapper::Data(Tab2).list.canvas delete TEXTSELECT
         Mapper::DepotWare::Tree $Mapper::Data(Tab2).list.canvas
      }
   }
   set Data(Select) ""
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::Tree>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher l'arbre des depots de donnees.
#
# Parametres :
#  <Canvas>  : Canvas ou afficher l'arbre
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::Tree { Canvas } {

   $Canvas delete MAPPERTREE

   set X 0
   set Y 0
   Mapper::DepotWare::TreeBranch $Canvas root MAPPERTREE X Y

   #----- Tree migth be empty

   catch { $Canvas configure -scrollregion "0 0 [lrange [$Canvas bbox MAPPERTREE] 2 end]" }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TreeBranch>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher une branche de l'arbre de depots.
#
# Parametres :
#  <Canvas>  : Canvas ou afficher l'arbre
#  <Branch>  : Branche a afficher
#  <Tag>     : Tag de l'arbre
#  <X>       : Coordonnee en X
#  <Y>       : Coordonnee en Y
#
# Retour    :
#
# Remarque :
#   -Cette procedure est recursive et est applique a chaque branche de l'arbre
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::TreeBranch { Canvas Branch Tag  X Y } {
   global GDefs
   variable Data

   upvar $X x
   upvar $Y y

   set dy 20
   set dx 10
   set y0 $y

   incr x $dx

   foreach branch [TREE children $Branch]  {

     set y0 [incr y $dy]

      set type [TREE get $branch type]
      switch [string range $type 0 2] {
         "URL" { set name "([string range $type 3 end]) [TREE get $branch name]" }
         "WMS" { set name [TREE get $branch path] }
         default { if { [TREE depth $branch]>1 } { set name [file tail [TREE get $branch path]] } else { set name "($type) [TREE get $branch name]" } }
      }

      if { $type=="GDAL" || $type=="OGR" || $type=="WMS" } {
         if { [Mapper::DepotWare::Check $branch] } {
            $Canvas create text [expr $x+$dx] $y -text $name -anchor w -tags "$Tag $branch TEXT$branch" -font $GDefs(Font)
            $Canvas create line [expr $x-$dx] $y [expr $x+$dx-5] $y -width 1 -fill black -tags "$Tag"
            $Canvas bind TEXT$branch <Double-ButtonRelease-1> "Mapper::DepotWare::Select $Canvas $branch"
         } else {
            set y0 [incr y -$dy]
         }
      } else {
         $Canvas create text [expr $x+$dx] $y -text $name -anchor w -tags "$Tag $branch TEXT$branch" -font $GDefs(Font)
         if { [expr $x-$dx]>5 } {
            $Canvas create line [expr $x-$dx] $y [expr $x-4] $y -width 1 -fill black -tags "$Tag"
         }

         if { [TREE get $branch open] } {
            $Canvas create bitmap $x $y -bitmap @$GDefs(Dir)/Resources/Bitmap/minus.ico -tags "$Tag $branch"
            $Canvas bind $branch <Button-1> "Mapper::DepotWare::Select $Canvas $branch False"
            set x0 $x
            set y0 $y
            set yend [Mapper::DepotWare::TreeBranch $Canvas $branch $Tag x y]

            set x $x0
            $Canvas create line $x $yend $x [expr $y0+5] -width 1 -fill black -tags "$Tag"
         } else {
            $Canvas create bitmap $x $y -bitmap @$GDefs(Dir)/Resources/Bitmap/plus.ico -tags "$Tag $branch"
            $Canvas bind $branch <Button-1> "Mapper::DepotWare::Select $Canvas $branch True"
         }
      }
      $Canvas bind $branch <Button-3> "Mapper::DepotWare::PopUp $Canvas %X %Y $branch"
   }
   return $y0
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::PopUp>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher le menu contectuel des branches de l'arbre.
#
# Parametres :
#  <Canvas>  : Canvas ou afficher l'arbre
#  <X>       : Coordonnee X du menu
#  <Y>       : Coordonnee Y du menu
#  <Branch>  : Branche selectionnee
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::PopUp { Canvas X Y Branch } {
   global GDefs
   variable Lbl
   variable Data

   if { ![winfo exists .depotwaremenu] } {
      menu .depotwaremenu -type normal
      .depotwaremenu add command -label [lindex $Lbl(Display) $GDefs(Lang)] -command ""
      .depotwaremenu add cascade -label [lindex $Lbl(Index) $GDefs(Lang)] -menu .depotwaremenu.idx
      .depotwaremenu add separator
      .depotwaremenu add command -label [lindex $Lbl(Params) $GDefs(Lang)] -command { Mapper::DepotWare::Params 0 }
      .depotwaremenu add command -label [lindex $Lbl(Del) $GDefs(Lang)] -command { Mapper::DepotWare::Del $Mapper::DepotWare::Data(Select) }

       menu .depotwaremenu.idx -type normal
   }

   if { ![llength [$Canvas find withtag TEXTSELECT]] } {
      eval $Canvas create rectangle [$Canvas bbox TEXT$Branch] -fill $GDefs(ColorHighLight) -outline black -width 1 -tags TEXTSELECT
      $Canvas lower TEXTSELECT
   }
   eval $Canvas coords TEXTSELECT [$Canvas bbox TEXT$Branch]
   set Data(Select) $Branch
   set Data(Name) [TREE get $Branch name]
   set Data(Path) [TREE get $Branch path]
   set Data(Type) [TREE get $Branch type]

   .depotwaremenu entryconfigure 0 -state disabled
   .depotwaremenu entryconfigure 1 -state disabled
   .depotwaremenu entryconfigure 3 -state disabled
   .depotwaremenu entryconfigure 4 -state disabled
   .depotwaremenu.idx delete 0 end

   if { [TREE depth $Branch]==1 } {
      .depotwaremenu entryconfigure 3 -state normal
      .depotwaremenu entryconfigure 4 -state normal
   }

   if { $Data(Type)=="DIR" } {
      if { [file exists $Data(Path)/Index/] } {
         foreach file [glob -nocomplain $Data(Path)/Index/*.shp]  {
            .depotwaremenu.idx add command -label [file tail $file] -command "Mapper::ReadLayer $file; Mapper::UpdateData $Page::Data(Frame)"
         }
         .depotwaremenu entryconfigure 1 -state normal
      }
   } elseif { [TREE isleaf $Branch] } {
      .depotwaremenu entryconfigure 0 -state normal -command "Mapper::DepotWare::Select $Canvas $Branch"
   }

   switch $Data(Type) {
      "DIR"    { set Data(Type) [lindex $Lbl(Types) 0] }
      "URLWMS" { set Data(Type) [lindex $Lbl(Types) 1] }
      "URLWCS" { set Data(Type) [lindex $Lbl(Types) 2] }
      "URLWFS" { set Data(Type) [lindex $Lbl(Types) 3] }
   }
   tk_popup .depotwaremenu $X $Y 0
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::Create>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer l'adrdre des depots.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::Create { } {
   global GDefs
   variable Data

   if { [llength [TREE children root]] } {
      return
   }

   if { [file exists $GDefs(DirEER)/Mapper/Params] } {
      source $GDefs(DirEER)/Mapper/Params
   }

   foreach depot $Data(Depots) {
      set idx [TREE insert root end]
      TREE set $idx open False
      TREE set $idx name [lindex $depot 0]
      TREE set $idx type [lindex $depot 1]
      TREE set $idx path [lindex $depot 2]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::Select>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Gerer la selection d'une branche de l'arbre des depots.
#
# Parametres :
#  <Canvas>  : Canvas ou l'arbre est affiche
#  <Branch>  : Branche selectionnee
#  <Open>    : Overture de la branche
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::Select { Canvas Branch { Open "" } } {
   global GDefs
   variable Data
   variable Lbl
   variable Msg
   variable WMS

   if { ![info exist Viewport::Data(Data$Page::Data(Frame))] } {
      return
   }

   if { $Open!="" } {
      TREE set $Branch open $Open
   }

   if { ![llength [$Canvas find withtag TEXTSELECT]] } {
      eval $Canvas create rectangle [$Canvas bbox TEXT$Branch] -fill $GDefs(ColorHighLight) -outline black -width 1 -tags TEXTSELECT
      $Canvas lower TEXTSELECT
   }
   eval $Canvas coords TEXTSELECT [$Canvas bbox TEXT$Branch]
   set Data(Select) $Branch

   if { $Open!="False" && [TREE isleaf $Branch] } {

      set Mapper::Data(Job) [lindex $Msg(Search) $GDefs(Lang)]
      $Canvas configure -cursor watch
      update idletasks;

      set path [TREE get $Branch path]

      switch [TREE get $Branch type] {
         "DIR"  {
            foreach file [glob -nocomplain $path/*] {
               set branch [TREE insert $Branch end]
               if { [file isdirectory $file] } {
                  TREE set $branch open False
                  TREE set $branch name ""
                  TREE set $branch path $file
                  TREE set $branch type DIR
               } elseif { [Mapper::DepotWare::AddGDAL $branch $file] || [Mapper::DepotWare::AddOGR $branch $file] } {
               } else {
                  TREE delete $branch
               }
            }
         }

         "URLWMS"  {
            if { [string first "?" ${path}]==-1 } {
               set req [http::geturl "${path}?&SERVICE=WMS&REQUEST=GetCapabilities"]
            } else {
               set req [http::geturl "${path}&SERVICE=WMS&REQUEST=GetCapabilities"]
            }

            if { [catch { set doc [dom::parse [http::data $req]] } ] } {
               Dialog::CreateErrorListing . [lindex $Msg(WMSRequest) $GDefs(Lang)] [http::data $req] $GDefs(Lang)
               return
            }

            set WMS(Version) 1.1.1
            set getmap [lindex [set [dom::document getElementsByTagName $doc GetMap]] 0]
            foreach node [set [dom::document getElementsByTagName $getmap Format]] {
               set WMS(Format) [dom::node cget  [dom::node children $node] -nodeValue]
               if { $WMS(Format)=="image/png" || $WMS(Format)=="image/jpeg" } {
                  break
               }
            }

            set layer [lindex [set [dom::document getElementsByTagName $doc Layer]] 0]
            foreach layer [Mapper::DepotWare::WMSParseLayer $path $layer] {
               set branch [TREE insert $Branch end]
               Mapper::DepotWare::AddWMS $branch $layer
            }
            dom::destroy $doc
         }

         "WMS"  { set path [Mapper::DepotWare::WMSBuildXMLDef $path]
                  if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $path]==-1 } {
                     Mapper::ReadBand $path "" 3
                   }
                }
         "WCS"  { }
         "WFS"  { }
         "GDAL" { if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $path]==-1 } {
                     Mapper::ReadBand $path
                  }
                }
         "OGR"  { if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $path]==-1 } {
                     Mapper::ReadLayer $path
                  }
                }
      }
   }
   Mapper::UpdateData $Page::Data(Frame)
   Mapper::DepotWare::Tree $Canvas

   SPI::Progress 0 ""
   $Canvas configure -cursor left_ptr
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::AddWMS>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une branche pour une couche WMS.
#
# Parametres :
#  <Branch>  : Branche
#  <Layer>   : Couche
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::AddWMS { Branch Layer } {
   variable WMS

   TREE set $Branch open False
   TREE set $Branch name ""
   TREE set $Branch path $Layer
   TREE set $Branch type WMS
   TREE set $Branch width  -1
   TREE set $Branch height -1

   set bbox  [lindex $WMS($Layer) 3]
   TREE set $Branch 00 [list [lindex $bbox 1] [lindex $bbox 0]]
   TREE set $Branch 01 [list [lindex $bbox 3] [lindex $bbox 0]]
   TREE set $Branch 10 [list [lindex $bbox 1] [lindex $bbox 2]]
   TREE set $Branch 11 [list [lindex $bbox 3] [lindex $bbox 2]]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::AddGDAL>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une branche pour une couche GDAL.
#
# Parametres :
#  <Branch>  : Branche
#  <File>    : Fichier
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::AddGDAL { Branch File } {
   variable Data

   if { [lsearch -exact $Data(GDALExclude) [string tolower [file extension $File]]]!=-1 } {
      return False
   }

   eval set bad [catch { set bands [gdalfile open GDALPARSE read $File] }]

   if { !$bad } {
      if { [llength $bands] } {
         set width  [gdalfile width  GDALPARSE]
         set height [gdalfile height GDALPARSE]

         TREE set $Branch open False
         TREE set $Branch name ""
         TREE set $Branch path $File
         TREE set $Branch type GDAL
         TREE set $Branch width  $width
         TREE set $Branch height $height
         TREE set $Branch 00 [gdalfile project GDALPARSE 1 1]
         TREE set $Branch 01 [gdalfile project GDALPARSE 1 $height]
         TREE set $Branch 10 [gdalfile project GDALPARSE $width 1]
         TREE set $Branch 11 [gdalfile project GDALPARSE $width $height]

         gdalfile close GDALPARSE
         return True
      }
      gdalfile close GDALPARSE
   }
   return False
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::AddOGR>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajouter une branche pour une couche OGR.
#
# Parametres :
#  <Branch>  : Branche
#  <File>    : Fichier
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::AddOGR { Branch File } {
   variable Data

   if { [lsearch -exact $Data(OGRExclude) [string tolower [file extension $File]]]!=-1 } {
      return False
   }

   set bad [catch { set layers [ogrfile open OGRPARSE read $File] }]

   if { !$bad } {
      if { [llength $layers] } {
         TREE set $Branch open False
         TREE set $Branch name ""
         TREE set $Branch path $File
         TREE set $Branch type OGR
         TREE set $Branch width  0
         TREE set $Branch height 0
         TREE set $Branch 00 [list -90 -180.0]
         TREE set $Branch 01 [list 90 -180.0]
         TREE set $Branch 10 [list 90 180.0]
         TREE set $Branch 11 [list -90 180.0]

         ogrfile close OGRPARSE
         return True
      }
      ogrfile close OGRPARSE
   }
   return False
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMSParseLayer>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder du XML pour en extraire l'information des couches.
#
# Parametres :
#  <URL>     : URL du depot
#  <Node>    : Node XML
#  <First>   : Premiere couche ?
#
# Retour    :
#
# Remarque :
#   - Certaines couches cont imbriquees alors cette procedure est recursive
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMSParseLayer { URL Node { First True } } {
   variable WMS

   if { $First } {
      set WMS(Layers) {}
      set WMS(SizeX) 864000
      set WMS(SizeY) 432000
      set WMS(Width) 512
      set WMS(Height) 512
      set WMS(Cache)  1
   }
   set WMS(Style)  {}

   foreach node [set [dom::node configure $Node -childNodes]] {
      switch [dom::node configure $node  -nodeName] {
         Layer                    { set WMS(Opaque) [dom::element getAttribute $node opaque]
                                    Mapper::DepotWare::WMSParseLayer $URL $node False }
         EX_GeographicBoundingBox { Mapper::DepotWare::WMSParseGeographic $node }
         LatLonBoundingBox        { Mapper::DepotWare::WMSParseLatLonBoundingBox $node }
         BoundingBox              { Mapper::DepotWare::WMSParseBoundingBox $node }
         Name                     { set WMS(Name)  [dom::node cget [dom::node children $node] -nodeValue] }
         Title                    { set WMS(Title) [dom::node cget [dom::node children $node] -nodeValue] }
         Dimension                { set WMS(Cache) 0 }
         DataURL                  { }
         Style                    { Mapper::DepotWare::WMSParseStyle $node }
      }
   }

   if { $WMS(Name)!="" } {
      set WMS($WMS(Title)) [list $URL $WMS(Name) $WMS(BBox) $WMS(Geographic) $WMS(SizeX) $WMS(SizeY) $WMS(Format) $WMS(Style) $WMS(Opaque) $WMS(Cache)]
      lappend WMS(Layers) $WMS(Title)
   }
   set WMS(Name) ""
   return $WMS(Layers)
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMSParseStyle>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder du XML pour en extraire l'information des styles.
#
# Parametres :
#  <Node>    : Node XML
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMSParseStyle { Node } {
   variable WMS

   lappend WMS(Style) [dom::node cget [lindex [dom::node children $Node] 0] -nodeValue]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMSParseDimension>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder du XML pour en extraire l'information des dimensions.
#
# Parametres :
#  <Node>    : Node XML
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMSParseDimension { Node } {

}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMSParseGeographic>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder du XML pour en extraire l'information des limites geographiques.
#
# Parametres :
#  <Node>    : Node XML
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMSParseGeographic { Node } {
   variable WMS

   set WMS(Geographic) [list\
      [dom::node cget [dom::node children [set [dom::document getElementsByTagName $Node westBoundLongitude]]] -nodeValue]\
      [dom::node cget [dom::node children [set [dom::document getElementsByTagName $Node northBoundLatitude]]] -nodeValue]\
      [dom::node cget [dom::node children [set [dom::document getElementsByTagName $Node eastBoundLongitude]]] -nodeValue]\
      [dom::node cget [dom::node children [set [dom::document getElementsByTagName $Node southBoundLatitude]]] -nodeValue]]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMSParseBoundingBox>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Decoder du XML pour en extraire l'information des limites de la
#            boite de visibilite.
#
# Parametres :
#  <Node>    : Node XML
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMSParseBoundingBox { Node } {
   variable WMS

   set x0 [dom::element getAttribute $Node minx]
   set y0 [dom::element getAttribute $Node miny]
   set x1 [dom::element getAttribute $Node maxx]
   set y1 [dom::element getAttribute $Node maxy]
   set epsg [dom::element getAttribute $Node SRS]
   set crs  [dom::element getAttribute $Node CRS]

   set WMS(BBox)  [list $x0 $y0 $x1 $y1]

   if { $epsg=="EPSG:4326" || $epsg=="EPSG:4269" || $crs=="CRS:84" } {
      set WMS(SizeX) 864000
      set WMS(SizeY) 432000
   } else {
      set WMS(SizeX) [expr $x1-$x0]
      set WMS(SizeY) [expr $y1-$y0]
   }
}

proc Mapper::DepotWare::WMSParseLatLonBoundingBox { Node } {
   variable WMS

   set WMS(Geographic) [list [dom::element getAttribute $Node minx] [dom::element getAttribute $Node maxy] \
      [dom::element getAttribute $Node maxx] [dom::element getAttribute $Node miny]]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::WMSBuildXMLDef>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Construction du fichier de definition XML necessaire a GDAL pour
#            lire les donnees.
#
# Parametres :
#  <Layer>   : Couche
#
# Retour    :
#
# Remarque :
#
# Modifications  :
#    Nom         : -
#    Date        : -
#    Description : -
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::WMSBuildXMLDef { Layer } {
   variable WMS
   variable Data

   set url    [lindex $WMS($Layer) 0]
   set layer  [lindex $WMS($Layer) 1]
   set geog   [lindex $WMS($Layer) 3]
   set sizex  [lindex $WMS($Layer) 4]
   set sizey  [lindex $WMS($Layer) 5]
   set format [lindex $WMS($Layer) 6]
   set style  [lindex $WMS($Layer) 7]
   set opaque [lindex $WMS($Layer) 8]
   set cache  [lindex $WMS($Layer) 9]

   if { $opaque==0 } {
      set bands 4
      set ttag TRANSPARENT=TRUE
    } else {
      set bands 3
      set ttag TRANSPARENT=FALSE
   }

   if { ![file exists $Data(CachePath)] } {
      file mkdir $Data(CachePath)
   }

   set file $Data(CachePath)/[string map { / "" ? "" " " "" } $url$layer].xml
   if { [string first "?" ${url}]==-1 } {
      set url ${url}?
   } else {
      set url $url
   }

   set xml "<GDAL_WMS>\n   <Service name=\"WMS\">\n      <Version>$WMS(Version)</Version>\n"
   append xml "      <ServerUrl>${url}${ttag}&</ServerUrl>\n      <SRS>EPSG:4326</SRS>\n      <ImageFormat>$format</ImageFormat>\n"
   append xml "      <Layers>$layer</Layers>\n      <Styles></Styles>\n   </Service>\n"

   append xml "   <DataWindow>\n      <UpperLeftX>[lindex $geog 0]</UpperLeftX>\n      <UpperLeftY>[lindex $geog 1]</UpperLeftY>\n      <LowerRightX>[lindex $geog 2]</LowerRightX>\n      <LowerRightY>[lindex $geog 3]</LowerRightY>\n      <SizeX>$sizex</SizeX>\n      <SizeY>$sizey</SizeY>\n   </DataWindow>\n"
   append xml "   <Projection>EPSG:4326</Projection>\n   <BandsCount>$bands</BandsCount>\n   <BlockSizeX>$WMS(BlockSize)</BlockSizeX>\n   <BlockSizeY>$WMS(BlockSize)</BlockSizeY>\n"

   if { $cache && $Data(CachePath)!="" } {
      append xml "<Cache>\n   <Path> $Data(CachePath)</Path>\n   <Depth>2</Depth>\n   </Cache>\n"
   }
   append xml "</GDAL_WMS>"

   set f [open $file w]
   puts $f $xml
   close $f

   return $file
}

#----------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::Reset>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Reinitialise les parametres de recherche.
#
# Parametres   :
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

proc Mapper::DepotWare::Reset { } {
   variable Data

   if { $Mapper::Data(Canvas)!="" } {
      $Mapper::Data(Canvas) delete MAPPERSEARCH
   }

   set Data(Coo)  ""
   set Data(Lat0)   -90.0
   set Data(Lat1)   -90.0
   set Data(Lon0)   -180.0
   set Data(Lon1)   -180.0

   Mapper::DepotWare::Tree $Mapper::Data(Tab2).list.canvas
}

#----------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::Check>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Reinitialise les parametres de recherche.
#
# Parametres   :
#   <Branch>   : Branche a verifier
#
# Retour:
#   <Valid>    : Inclusion (True ou False)
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#----------------------------------------------------------------------------

proc Mapper::DepotWare::Check { Branch } {
   variable Data

   if { ![TREE keyexists $Branch 00] } {
      return 1
   }
   set c00 [TREE get $Branch 00]
   set c01 [TREE get $Branch 01]
   set c10 [TREE get $Branch 10]
   set c11 [TREE get $Branch 11]

   if { $Data(Lat0)==$Data(Lat1) || $Data(Lon0)==$Data(Lon1) || [lindex $c00 0]==0 || [lindex $c00 1]==0} {
      return 1
   }

   if { [Viewport::CheckInside $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) [lindex $c00 0] [lindex $c00 1]] } {
      return 1
   }

   if { [Viewport::CheckInside $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) [lindex $c01 0] [lindex $c01 1]] } {
      return 1
   }

   if { [Viewport::CheckInside $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) [lindex $c10 0] [lindex $c10 1]] } {
      return 1
   }

   if { [Viewport::CheckInside $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) [lindex $c11 0] [lindex $c11 1]] } {
      return 1
   }

   set lat0 [expr [lindex $c00 0]<[lindex $c11 0]?[lindex $c00 0]:[lindex $c11 0]]
   set lon0 [expr [lindex $c00 1]<[lindex $c11 1]?[lindex $c00 1]:[lindex $c11 1]]
   set lat1 [expr [lindex $c00 0]>[lindex $c11 0]?[lindex $c00 0]:[lindex $c11 0]]
   set lon1 [expr [lindex $c00 1]>[lindex $c11 1]?[lindex $c00 1]:[lindex $c11 1]]

   if { [Viewport::CheckInside $lat0 $lon0 $lat1 $lon1 $Data(Lat0) $Data(Lon0)] } {
      return 1
   }
   if { [Viewport::CheckInside $lat0 $lon0 $lat1 $lon1 $Data(Lat0) $Data(Lon1)] } {
      return 1
   }
   if { [Viewport::CheckInside $lat0 $lon0 $lat1 $lon1 $Data(Lat1) $Data(Lon1)] } {
      return 1
   }
   if { [Viewport::CheckInside $lat0 $lon0 $lat1 $lon1 $Data(Lat1) $Data(Lon0)] } {
      return 1
   }
   return 0
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare...>
# Creation : Juin 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Fonctions de manipulations de la selection sur la projection.
#
# Parametres :
#   <Frame>  : Identificateur de Page
#   <VP>     : Identificateur du Viewport
#
# Remarques :
#    - Ces fonctions sont appele par le package Page au besoin.
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------
proc Mapper::DepotWare::DrawInit  { Frame VP } {
   variable Data

   set Data(Color)  red
   set Data(Lat0)   $Viewport::Map(LatCursor)
   set Data(Lon0)   $Viewport::Map(LonCursor)
}

proc Mapper::DepotWare::Draw      { Frame VP } {
   variable Data

   if { $Mapper::Data(Canvas)!="" } {
      $Mapper::Data(Canvas) delete MAPPERSEARCH
   }

   set Mapper::Data(Canvas) $Page::Data(Canvas)
   set Mapper::Data(Frame)  $Frame
   set Mapper::Data(VP)     $VP

   set Data(Lat1)   $Viewport::Map(LatCursor)
   set Data(Lon1)   $Viewport::Map(LonCursor)

   Viewport::DrawRange $Frame $VP $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) MAPPERSEARCH red
}

proc Mapper::DepotWare::DrawDone { Frame VP } {
   variable Data

   if { $Data(Lat0)>$Data(Lat1) } {
      set tmp $Data(Lat1)
      set Data(Lat1) $Data(Lat0)
      set Data(Lat0) $tmp
   }

   if { $Data(Lon0)>$Data(Lon1) } {
      set tmp $Data(Lon1)
      set Data(Lon1) $Data(Lon0)
      set Data(Lon0) $tmp
   }

   if { $Data(Lat0)==$Data(Lat1) || $Data(Lon0)==$Data(Lon1) } {
      set Data(Coo) ""
   } else {
      set Data(Coo) "$Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1)"
   }
   Mapper::DepotWare::Tree $Mapper::Data(Tab2).list.canvas
}

proc Mapper::DepotWare::MoveInit { Frame VP } {
   variable Data

   set Data(LonD) $Viewport::Map(LonCursor)
   set Data(LatD) $Viewport::Map(LatCursor)
}

proc Mapper::DepotWare::Move { Frame VP } {
   variable Data

   #----- Effectuer la translation

   set lat0 [expr $Data(Lat0) + $Viewport::Map(LatCursor) - $Data(LatD)]
   set lat1 [expr $Data(Lat1) + $Viewport::Map(LatCursor) - $Data(LatD)]

   if { $lat0 > -90.0 && $lat0 < 90.0 && $lat1 > -90.0 && $lat1 < 90.0 } {

      set Data(Lat0) $lat0
      set Data(Lat1) $lat1
      eval set Data(Lon0) [Viewport::CheckCoord [expr $Data(Lon0) + $Viewport::Map(LonCursor) - $Data(LonD)]]
      eval set Data(Lon1) [Viewport::CheckCoord [expr $Data(Lon1) + $Viewport::Map(LonCursor) - $Data(LonD)]]
   }

   if { $Mapper::Data(Canvas)!="" } {
      $Mapper::Data(Canvas) delete MAPPERSEARCH
   }

   set Mapper::Data(Canvas) $Page::Data(Canvas)
   set Mapper::Data(Frame)  $Frame
   set Mapper::Data(VP)     $VP

   #----- Reaffecter le point de reference de translation

   set Data(LonD) $Viewport::Map(LonCursor)
   set Data(LatD) $Viewport::Map(LatCursor)

   Viewport::DrawRange $Frame $VP $Data(Lat0) $Data(Lon0) $Data(Lat1) $Data(Lon1) MAPPERSEARCH red
}

proc Mapper::DepotWare::MoveDone { Frame VP } {
   variable Data

   Mapper::DepotWare::DrawDone $Frame $VP
}
