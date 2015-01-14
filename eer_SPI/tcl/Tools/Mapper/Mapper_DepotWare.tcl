#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Tools" Tk.
# Fichier  : Mapper_DepotWare.tcl
# Creation : Novembre 2007 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation et de gestion des depots de donnees georeference.
#
# Remarques :
#
#===============================================================================

source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_DepotWare_DIR.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_DepotWare_WMS.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_DepotWare_WFS.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_DepotWare_WCS.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_DepotWare_TMS.tcl
source $GDefs(Dir)/tcl/Tools/Mapper/Mapper_DepotWare_PGS.tcl

namespace eval Mapper::DepotWare {
   global env
   variable WMS
   variable Data
   variable Lbl
   variable Msg
   variable Error
   variable Bubble

   set Data(CachePath) /tmp/$env(USER)/gdalwmscache                ;#Path du cache
   set Data(CacheSize) 0                                           ;#Dimension du cache
   set Data(CacheMax)  1024                                        ;#Dimension maximale du cache

   ::struct::tree TREE

   set Data(Depots) {}
   set Data(Coo)    ""
   set Data(Select) ""
   set Data(Name)   ""
   set Data(Lat0)   -90.0
   set Data(Lat1)   -90.0
   set Data(Lon0)   -180.0
   set Data(Lon1)   -180.0

   set Lbl(Title)       { "Ajout d'un d�pot de donn�es" "Add data repository" }
   set Lbl(TitleParams) { "Param�tres du d�pot de donn�es" "Data repository parameters" }
   set Lbl(Types)       { "TMS - Tileb Mapping Service" "PGS - PostGIS database" "WMS - Web Mapping Service"  "WFS - Web Feature Service" "WCS - Web Coverage Service" "DIR - Data directory" }
   set Lbl(Path)        { "Localisation" "Localisation" }
   set Lbl(Type)        { "Type" "Type" }
   set Lbl(Cache)       { "Cache" "Cache" }
   set Lbl(Max)         { "Maximum (Mo)" "Maximum (Mb)" }
   set Lbl(Add)         { "Ajouter" "Add" }
   set Lbl(Yes)         { "Oui" "Yes" }
   set Lbl(No)          { "Non" "No" }
   set Lbl(Del)         { "Supprimer" "Delete" }
   set Lbl(Apply)       { "Appliquer" "Apply" }
   set Lbl(Display)     { "Afficher" "Display" }
   set Lbl(Index)       { "Afficher l'index" "Display index" }
   set Lbl(Name)        { "Identification" "Identification" }
   set Lbl(Params)      { "Param�tres" "Parameters" }
   set Lbl(Save)        { "Sauvegarder" "Save" }
   set Lbl(Cancel)      { "Annuler" "Cancel" }
   set Lbl(Desc)        { "Description" "Description" }

   set Msg(Search)     { "Recherche ..." "Searching ..." }
   set Msg(Clean)      { "Nettoyage du cache ..." "Cleaning cache ..." }
   set Msg(Clear)      { "Suppression du cache ..." "Deleting cache ..." }
   set Msg(Del)        { "Voulez-vous vraiment supprimer ce d�pot de la liste ?" "Do you really want to remove this repository from the list ?" }
   set Msg(Cache)      { "Ce r�pertoire sera nettoy� d�s que le volume de donn�es d�passera la limite permise, �tes-vous s�r de vouloir le changer ?"
                         "This directory will be cleaned when it's size passes the limit. Are you sure you want to chenge it ?" }

   set Error(Depot)    { "Aucun nom ou type sp�cifi� pour le d�pot" "No name or type has been specified for the data repository." }

   set Bubble(Clear)   { "Supprimer le cache" "Erase cache" }
   set Bubble(Size)    { "Taille courante du cache" "Current cache size" }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::CacheClear>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Liberer tous l'espace en cache.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::CacheClear { } {
   global GDefs
   variable Data
   variable Msg

   set Mapper::Data(Job) [lindex $Msg(Clear) $GDefs(Lang)]
   update idletasks

   file delete -force $Data(CachePath)
   file mkdir $Data(CachePath)

   set Data(CacheSize) 0
   set Mapper::Data(Job) ""
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::CacheClean>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Liberer l'espace en cache depassant le maximum en ordre d'acces.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::CacheClean { } {
   global GDefs
   variable Data
   variable Msg

   set Mapper::Data(Job) [lindex $Msg(Clean) $GDefs(Lang)]
   update idletasks

   #----- If cache exists
   if { ![catch { set Data(CacheSize) [lindex [exec du -sk $Data(CachePath)] 0] }] } {


      #----- Get all files with associated size sorted by access date
      set files [split [exec find $Data(CachePath) -type f -printf "%A@ %p %k\n"  | sort -r] \n]

      #----- Loop on files and erase until cache is smaller than specified maximum
      foreach file $files {
         file delete -force [lindex $file 1]
         if { [set Data(CacheSize) [expr $Data(CacheSize)-[lindex $file end]]]<$Data(CacheMax) } {
            break
         }
      }
      set Data(CacheSize) [lindex [exec du -h $Data(CachePath)] 0]
   }
   set Mapper::Data(Job) ""
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::CacheSet>
# Creation : Avril 2010 - J.P. Gauthier - CMC/CMOE
#
# But      : Redefinir le path du cache.
#
# Parametres :
#   <Path>   : Path du cache
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::CacheSet { Path } {
   global GDefs
   variable Data
   variable Msg
   variable Lbl

   if { ![Dialog::Default . 400 WARNING $Msg(Cache) "\n\n$Path\n" 0 $Lbl(Yes) $Lbl(No)] } {
      set Data(CachePath) $Path
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::ParamsSave>
# Creation : Decembre 2008 - J.P. Gauthier - CMC/CMOE
#
# But      : Sauvegarde des parametres d'un depot de donnees.
#
# Parametres :
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::ParamsSave { } {
   global env
   variable Data

   if { [file exists $env(HOME)/.spi/Mapper] } {
      file rename -force $env(HOME)/.spi/Mapper $env(HOME)/.spi/Mapper.old
   }
   
   set f [open $env(HOME)/.spi/Mapper w]
   puts $f "#----- Configuration file for the Mapper tool
set Mapper::DepotWare::Data(CachePath) $Data(CachePath)
set Mapper::DepotWare::Data(CacheMax) $Data(CacheMax)
set Mapper::DepotWare::Data(Depots) {"

   foreach depot $Data(Depots) {
      puts $f "   \{ $depot \}"
   }
   puts $f "}"
   
   close $f
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
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::ParamsSelect { } {
   global GDefs
   variable Data
   variable Lbl

   destroy .mapperdepot.params

   labelframe .mapperdepot.params -text [lindex $Lbl(Params) $GDefs(Lang)]
   pack .mapperdepot.params -side top -fill both -expand True -padx 5 -after .mapperdepot.type

   Mapper::DepotWare::[lindex ${Data(Type)} 0]::Params .mapperdepot.params
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
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::Params { { Save 1 } } {
   global GDefs
   variable Data
   variable Lbl
   variable Bubble

   if { [winfo exists .mapperdepotparams] } {
      raise .mapperdepotparams
      return
   }

   toplevel         .mapperdepotparams -relief raised -bd 1
   wm title         .mapperdepotparams [lindex $Lbl(TitleParams) $GDefs(Lang)]
   wm resizable     .mapperdepotparams 1 1
   wm protocol      .mapperdepotparams WM_DELETE_WINDOW { }
   wm geom          .mapperdepotparams +[expr [winfo rootx .mapper]+50]+[expr [winfo rooty .mapper]+50]
   wm transient     .mapperdepotparams .mapper

   catch { set Data(CacheSize) [lindex [exec du -sh $Data(CachePath)] 0] }

   labelframe .mapperdepotparams.cache -text [lindex $Lbl(Cache) $GDefs(Lang)]
      frame .mapperdepotparams.cache.path
         label .mapperdepotparams.cache.path.lbl -anchor w -text [lindex $Lbl(Path) $GDefs(Lang)] -width 15
         button .mapperdepotparams.cache.path.open -image OPEN -bd 1 -relief flat -overrelief raised \
            -command  { Mapper::DepotWare::CacheSet [FileBox::Create . $Mapper::DepotWare::Data(CachePath) LoadPath {}] }
         entry .mapperdepotparams.cache.path.ent -width 30 -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::DepotWare::Data(CachePath)
         pack .mapperdepotparams.cache.path.lbl -side left
         pack .mapperdepotparams.cache.path.ent -side left  -fill x -expand True
         pack .mapperdepotparams.cache.path.open -side left
      frame .mapperdepotparams.cache.sz
         label .mapperdepotparams.cache.sz.lbl -anchor w -text [lindex $Lbl(Max) $GDefs(Lang)] -width 15
         spinbox .mapperdepotparams.cache.sz.inc -bd 1 -bg $GDefs(ColorLight) -increment 10 -from 0 -to 4096 \
            -textvariable Mapper::DepotWare::Data(CacheMax)
         label .mapperdepotparams.cache.sz.cur -anchor w -textvariable Mapper::DepotWare::Data(CacheSize)
         button .mapperdepotparams.cache.sz.clear -image DELETE -bd 1 -relief flat -overrelief raised \
            -command  { Mapper::DepotWare::CacheClear }
         pack .mapperdepotparams.cache.sz.lbl -side left
         pack .mapperdepotparams.cache.sz.inc -side left  -fill x -expand True
         pack .mapperdepotparams.cache.sz.cur -side left -padx 5
         pack .mapperdepotparams.cache.sz.clear -side left
      pack .mapperdepotparams.cache.path .mapperdepotparams.cache.sz -side top -fill x
   pack .mapperdepotparams.cache -side top -fill x -padx 5 -pady 5

   frame .mapperdepotparams.cmd -relief sunken -bd 1
      button .mapperdepotparams.cmd.ok -bd 1 -text [lindex $Lbl(Apply) $GDefs(Lang)] -command  { Mapper::DepotWare::ParamsSave; destroy .mapperdepotparams }
      button .mapperdepotparams.cmd.cancel -bd 1 -text [lindex $Lbl(Cancel) $GDefs(Lang)] -command  { destroy .mapperdepotparams }
      pack .mapperdepotparams.cmd.ok .mapperdepotparams.cmd.cancel -side left  -fill x -expand True
   pack .mapperdepotparams.cmd -side top -fill x -padx 5 -pady 5

   Bubble::Create .mapperdepotparams.cache.sz.cur $Bubble(Size)
   Bubble::Create .mapperdepotparams.cache.sz.clear $Bubble(Clear)
}

proc Mapper::DepotWare::Window { } {
   global GDefs
   variable Data
   variable Lbl

   if { [winfo exists .mapperdepot] } {
      raise .mapperdepot
      return
   }

   toplevel         .mapperdepot -relief raised -bd 1
   wm title         .mapperdepot [lindex $Lbl(Title) $GDefs(Lang)]
   wm resizable     .mapperdepot 1 1
   wm protocol      .mapperdepot WM_DELETE_WINDOW { }
   wm geom          .mapperdepot +[expr [winfo rootx .mapper]+50]+[expr [winfo rooty .mapper]+50]
   wm transient     .mapperdepot .mapper

   set Data(Type) ""
   set Data(Name) ""

   labelframe .mapperdepot.type -text [lindex $Lbl(Desc) $GDefs(Lang)]
      frame .mapperdepot.type.name
         label .mapperdepot.type.name.lbl -anchor w -text [lindex $Lbl(Name) $GDefs(Lang)] -width 15
         entry .mapperdepot.type.name.ent -width 1 -bd 1 -bg $GDefs(ColorLight) -textvariable Mapper::DepotWare::Data(Name)
         pack .mapperdepot.type.name.lbl -side left
         pack .mapperdepot.type.name.ent -side left  -fill x -expand True
      pack .mapperdepot.type.name -fill x -expand True
      frame .mapperdepot.type.type
         label .mapperdepot.type.type.lbl -anchor w -text [lindex $Lbl(Type) $GDefs(Lang)] -width 15
         ComboBox::Create .mapperdepot.type.type.sel Mapper::DepotWare::Data(Type) noedit unsorted nodouble -1 $Lbl(Types) 50 5 \
            { Mapper::DepotWare::ParamsSelect }
         pack .mapperdepot.type.type.lbl -side left
         pack .mapperdepot.type.type.sel -side left -fill x -expand True
      pack .mapperdepot.type.type -fill x -expand True
   pack  .mapperdepot.type -side top -fill x -padx 5 -pady 5

   frame .mapperdepot.cmd -relief sunken -bd 1
      button .mapperdepot.cmd.ok -bd 1 -text [lindex $Lbl(Add) $GDefs(Lang)] -command  { Mapper::DepotWare::Add $Mapper::DepotWare::Data(Name) [lindex $Mapper::DepotWare::Data(Type) 0] }
      button .mapperdepot.cmd.cancel -bd 1 -text [lindex $Lbl(Cancel) $GDefs(Lang)] -command  { destroy .mapperdepot }
      pack .mapperdepot.cmd.ok .mapperdepot.cmd.cancel -side left  -fill x -expand True
   pack .mapperdepot.cmd -side top -fill x -padx 5 -pady 5
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::Add>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Ajout d<un nouveaux depot de donnees.
#
# Parametres :
#   <Name>   : Identification du depot
#   <Type>   : Type de depot (WMS,WCS,WFS,DIR,PGS)
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::Add { Name Type } {
   variable Data
   variable Error

   if { $Name=="" || $Type=="" } {
      Dialog::Error . $Error(Depot)
      return 
   }
   
   set req [Mapper::DepotWare::${Type}::Request]

   set type $Type
   if { $type!="DIR" && $type!="TMS" } {
      set type "URL$type"
   }

   if  { $Name=="" } {
      set Name $req
   }

   lappend Data(Depots) [list $Name $type $req]
   Mapper::DepotWare::ParamsSave

   #----- Force the newly added branch parent to open
   TREE set $Type open True

   #----- Add new node
   set idx [TREE insert $Type end]
   TREE set $idx open False
   TREE set $idx name $Name
   TREE set $idx path $req
   TREE set $idx type $type

   CVTree::Render $Mapper::Data(Tab2).list.canvas Mapper::DepotWare::TREE

   destroy .mapperdepot
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
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::Del { Branch } {
   global GDefs
   variable Lbl
   variable Msg
   variable Data

   if { $Branch!="" && [TREE depth $Branch]==2 } {
      set name [TREE get $Branch name]
      set type [TREE get $Branch type]
      set path [TREE get $Branch path]
      if { ![Dialog::Default . 400 WARNING $Msg(Del) "\n\n$name\n" 0 $Lbl(Yes) $Lbl(No)] } {
         set idx 0
         foreach depot $Data(Depots) {
            if { [lindex $depot 0]==$name && [lindex $depot 1]==$type && [lindex $depot 2]==$path } {
               set Data(Depots) [lreplace $Data(Depots) $idx $idx]
               Mapper::DepotWare::ParamsSave
               break
            }
            incr idx
         }
         TREE delete $Branch

         CVTree::SelectionClear $Mapper::Data(Tab2).list.canvas Mapper::DepotWare::TREE
         CVTree::Render $Mapper::Data(Tab2).list.canvas Mapper::DepotWare::TREE
      }
   }
   set Data(Select) ""
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TreeId>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Creer l'identification de la branche
#
# Parametres :
#  <Tree>    : Arbre
#  <Branch>  : Branche
#
# Retour    :
#  <Id>     : Identification
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::TreeId { Tree Branch Leaf } {

   upvar $Leaf leaf

   set type [$Tree get $Branch type]
   set id ""
   set leaf True

   if { $type=="DIR" || [string range $type 0 2]=="URL" } {
      set leaf False
   }

   if { $type=="GDAL" || $type=="OGR" || $type=="WMS" || $type=="WCS" || $type=="PGS"  } {
      if { ![Mapper::DepotWare::Check $Branch] } {
         return ""
      }
   }

   switch [string range $type 0 2] {
      "ROO"  -
      "URL"  { set id [$Tree get $Branch name] }
      "WMS"  -
      "WCS"  -
      "PGS"  { set id [$Tree get $Branch path] }
      default { if { [$Tree depth $Branch]>2 } { set id [file tail [$Tree get $Branch path]] } else { set id "[$Tree get $Branch name]" } }
   }
   return $id
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::TreeSelect>
# Creation : Novembre 2007 - J.P. Gauthier - CMC/CMOE
#
# But      : Gerer la selection d'une branche de l'arbre des depots.
#
# Parametres :
#  <Tree>    : Arbre
#  <Branch>  : Branche selectionnee
#  <Open>    : Overture de la branche
#
# Retour    :
#
# Remarque :
#
#-------------------------------------------------------------------------------

proc  Mapper::DepotWare::TreeSelect { Tree Branch Open } {
   global GDefs
   variable Data
   variable Lbl
   variable Msg
   variable WMS

   if { ![info exist Viewport::Data(Data$Page::Data(Frame))] } {
      return
   }

   set Data(Select) $Branch

   set path [$Tree get $Branch path]

   switch -glob [set type [$Tree get $Branch type]] {
      "GDAL" { if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $path]==-1 } {
                  Mapper::GDAL::Read $path
               }
               }
      "OGR"  { if { [lsearch -exact $Viewport::Data(Data$Page::Data(Frame)) $path]==-1 } {
                  Mapper::OGR::Read $path
               }
               }
      "URL*"  { }
      default { Mapper::DepotWare::${type}::Select $Tree $Branch $Open }
   }
   
   Mapper::UpdateData $Page::Data(Frame)
   SPI::Progress 0 ""
}

proc  Mapper::DepotWare::TreeParse { Tree Branch Open } {
   global GDefs
   variable Data
   variable Lbl
   variable Msg
   variable WMS

   if { ![info exist Viewport::Data(Data$Page::Data(Frame))] } {
      return
   }

   set Data(Select) $Branch

   if { [$Tree isleaf $Branch] } {

      set Mapper::Data(Job) [lindex $Msg(Search) $GDefs(Lang)]
      set path [$Tree get $Branch path]

      switch -glob [set type [$Tree get $Branch type]] {
         "URL*"  { Mapper::DepotWare::[string range $type 3 end]::Parse $Tree $Branch }
         default { Mapper::DepotWare::${type}::Parse $Tree $Branch }
      }
   }
   Mapper::UpdateData $Page::Data(Frame)
   SPI::Progress 0 ""
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
#      .depotwaremenu add command -label [lindex $Lbl(Params) $GDefs(Lang)] -command { Mapper::DepotWare::Window 0 }
      .depotwaremenu add command -label [lindex $Lbl(Del) $GDefs(Lang)] -command { Mapper::DepotWare::Del $Mapper::DepotWare::Data(Select) }

       menu .depotwaremenu.idx -type normal
   }

   set Data(Select) $Branch
   set Data(Name) [TREE get $Branch name]
   set Data(Path) [TREE get $Branch path]
   set Data(Type) [TREE get $Branch type]

   if { $Data(Type)!="ROOT" } {

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
               .depotwaremenu.idx add command -label [file tail $file] -command "Mapper::OGR::Read $file; Mapper::UpdateData $Page::Data(Frame)"
            }
            .depotwaremenu entryconfigure 1 -state normal
         }
      } elseif { [TREE isleaf $Branch] } {
         .depotwaremenu entryconfigure 0 -state normal -command "Mapper::DepotWare::TreeSelect Mapper::DepotWare::TREE $Branch True"
      }

      set type [lindex $Data(Type) end]
      foreach lbl  $Lbl(Types) {
         if { $type==[lindex $lbl 0] } {
            set Data(Type) $lbl
            break
         }
      }
      tk_popup .depotwaremenu $X $Y 0
   }
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
#-------------------------------------------------------------------------------

proc Mapper::DepotWare::Create { } {
   global env
   variable Data
   variable Lbl

   if { [llength [TREE children root]] } {
      return
   }

   if { [file exists $env(HOME)/.spi/Mapper] } {
      source $env(HOME)/.spi/Mapper
   }

 
   foreach type $Lbl(Types) {
      set type [lindex $type 0]

      #----- Add default data wharehouse if any
      eval set proc \[info procs ::Mapper::DepotWare::${type}::Default\]
      if { $proc!="" } {
         eval ::Mapper::DepotWare::${type}::Default
      }
           
      TREE insert root end $type
      TREE set $type name $type
      TREE set $type type ROOT
      TREE set $type path ""

      if { $type=="TMS" } {
         TREE set $type open True
      } else {
         TREE set $type open False
      }
   }

   foreach depot $Data(Depots) {
      set type [string range [lindex $depot 1] end-2 end]
      set idx  [TREE insert $type end]
      
      TREE set $idx open False
      TREE set $idx name [lindex $depot 0]
      TREE set $idx type [lindex $depot 1]

      #----- Make sure env variables are evaluated
      if { $type=="DIR" } {
         eval set path \"[lindex $depot 2]\"
      } else {
         set path [lindex $depot 2]
      }
      TREE set $idx path $path
   }
#   CVTree::Render $Mapper::Data(Tab2).list.canvas Mapper::DepotWare::TREE \
#      Mapper::DepotWare::TreeId \
#      Mapper::DepotWare::TreeParse \
#      Mapper::DepotWare::TreeSelect \
#      Mapper::DepotWare::PopUp
   CVTree::Create $Mapper::Data(Tab2).list.canvas Mapper::DepotWare::TREE \
      IdCmd Mapper::DepotWare::TreeId \
      ParseCmd Mapper::DepotWare::TreeParse \
      SelectCmd Mapper::DepotWare::TreeSelect \
      PopUpCmd Mapper::DepotWare::PopUp
}

#----------------------------------------------------------------------------
# Nom      : <Mapper::DepotWare::Reset>
# Creation : Juillet 2004 - J.P. Gauthier - CMC/CMOE
#
# But      : Reinitialise les parametres de recherche.
#
# Parametres  :
#   <Clear>   : Clear search params
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Mapper::DepotWare::Reset { { Clear False } } {
   variable Data

   if { $Clear } {
      if { $Mapper::Data(Canvas)!="" } {
         $Mapper::Data(Canvas) delete MAPPERSEARCH
      }

      set Data(Coo)  ""
      set Data(Lat0)   -90.0
      set Data(Lat1)   -90.0
      set Data(Lon0)   -180.0
      set Data(Lon1)   -180.0
   }

   CVTree::Render $Mapper::Data(Tab2).list.canvas Mapper::DepotWare::TREE
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
   CVTree::Render $Mapper::Data(Tab2).list.canvas Mapper::DepotWare::TREE
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
