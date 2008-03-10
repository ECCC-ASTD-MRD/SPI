#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Tools" Tk.
# Fichier  : Mapper_WKT.tcl
# Version  : 1.0
# Creation : Mars 2007 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de manipulation de georeference au format WKT.
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

namespace eval Mapper::WKT {
   variable Data

   set Data(ProjectionList) {}
   set Data(DatumList)      {}
   set Data(CoordRefList)   {}
   set Data(SpheroidList)   {}
   set Data(MeridianList)   {}
   set Data(UnitList)       {}
   set Data(Projections)    {}
   set Data(Datums)         {}
   set Data(CoordRefs)      {}
   set Data(Spheroids)      {}
   set Data(Meridians)      {}
   set Data(Units)          {}
   set Data(Files) { "CoordinateReferenceSystem" "CoordinateOperationMethod" "Datum" "Ellipsoid" "PrimeMeridian" "UnitOfMeasure" }

   foreach file $Data(Files) {
      source  $GDefs(Dir)/Apps/Tools/Mapper/EPSG/$file.tcl
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::WKT::Param>
# Creation : Fevrier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Afficher l'interface de visualisation/modification des parametres WKT
#
# Parametres :
#   <Proj>   : Definition WKT du referentiel.
#
# Retour   :
#  <List>  :  Nouvelle chaine WKT
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc Mapper::WKT::Param { Proj } {
   global GDefs
   variable Data

   #----- Charger les listes des valeur possibles des différents combobox

   if { ![info exists Mapper::WKT::Data(Proj)] } {
      Mapper::WKT::List Data(CoordRefList)   Data(CoordRefs)   1
      Mapper::WKT::List Data(DatumList)      Data(Datums)      1
      Mapper::WKT::List Data(SpheroidList)   Data(Spheroids)   1
      Mapper::WKT::List Data(MeridianList)   Data(Meridians)   1
      Mapper::WKT::List Data(UnitList)       Data(Units)       1
      Mapper::WKT::List Data(ProjectionList) Data(Projections) 1
   }

   #----- Decode la definition

   Mapper::WKT::Init $Proj
   if { ![llength $Data(Proj)] } {
     return
   }

   set Data(Done) 0

   toplevel     .mapperparamsproj -class Dialog
   wm title     .mapperparamsproj "[lindex $Mapper::Lbl(Ref) $GDefs(Lang)]"
   wm transient .mapperparamsproj .mapperparams
   wm protocol  .mapperparams WM_DELETE_WINDOW { set Mapper::WKT::Data(Done) -1 }

   frame .mapperparamsproj.proj -relief raised -bd 1
   pack .mapperparamsproj.proj -side top -fill both

   frame .mapperparamsproj.cmd
      button .mapperparamsproj.cmd.cancel -relief raised -bd 1 -text [lindex $Mapper::Lbl(Cancel) $GDefs(Lang)] -command { set Mapper::WKT::Data(Done) -1 }
      button .mapperparamsproj.cmd.apply -relief raised -bd 1 -text [lindex $Mapper::Lbl(Apply) $GDefs(Lang)] -command { set Mapper::WKT::Data(Done) 1 }
      pack .mapperparamsproj.cmd.cancel .mapperparamsproj.cmd.apply -side left -fill x -expand True
   pack .mapperparamsproj.cmd -side top -fill x

   if { [lindex $Data(Proj) 0] == "PROJCS" } {
      set prefix ".mapperparamsproj.proj.flnom"
      frame ${prefix}epsg
         label ${prefix}epsg.lbl -text "PROJCS  "
         ComboBox::Create ${prefix}epsg.sel Mapper::WKT::Data(ProjRef) edit unsorted nodouble -1 $Data(CoordRefs) 30 14 "Mapper::WKT::UpdateWidget PROJCS"
         label ${prefix}epsg.val -textvariable Mapper::WKT::Data(PROJCS)
         pack ${prefix}epsg.lbl ${prefix}epsg.sel ${prefix}epsg.val -side left -padx 2
      labelframe ${prefix} -labelwidget ${prefix}epsg
      pack ${prefix} -side top -padx 5 -pady 5 -fill x
   } else {
      set prefix .mapperparamsproj.proj
   }

   set prefix "${prefix}.flsyscoord"
   frame ${prefix}epsg
      label ${prefix}epsg.lbl -text "GEOGCS  "
      ComboBox::Create ${prefix}epsg.sel Mapper::WKT::Data(CoordRef) edit unsorted nodouble -1 $Data(CoordRefs) 30 14 "Mapper::WKT::UpdateWidget GEOGCS"
      label ${prefix}epsg.val -textvariable Mapper::WKT::Data(GEOGCS)
      pack ${prefix}epsg.lbl ${prefix}epsg.sel ${prefix}epsg.val -side left -padx 2
   labelframe ${prefix} -labelwidget ${prefix}epsg
   pack ${prefix} -side top -padx 5 -pady 5 -fill x

   set prefix "${prefix}.fldatum"
   frame ${prefix}epsg
      label ${prefix}epsg.lbl -text "DATUM   "
      ComboBox::Create ${prefix}epsg.sel Mapper::WKT::Data(Datum) edit unsorted nodouble -1 $Data(Datums) 30 14 "Mapper::WKT::UpdateWidget DATUM"
      label ${prefix}epsg.val -textvariable Mapper::WKT::Data(DATUM)
      pack ${prefix}epsg.lbl ${prefix}epsg.sel ${prefix}epsg.val -side left -padx 2
   labelframe ${prefix} -labelwidget ${prefix}epsg
   pack ${prefix} -side top -padx 5 -pady 5 -fill x

   set prefix "${prefix}.flspheroid"
   frame ${prefix}epsg
      label ${prefix}epsg.lbl -text "SPHEROID "
      ComboBox::Create ${prefix}epsg.sel Mapper::WKT::Data(Spheroid) edit unsorted nodouble -1 $Data(Spheroids) 30 14 "Mapper::WKT::UpdateWidget SPHEROID"
      label ${prefix}epsg.val -textvariable Mapper::WKT::Data(SPHEROID)
      pack ${prefix}epsg.lbl ${prefix}epsg.sel ${prefix}epsg.val -side left -padx 2
   labelframe ${prefix} -labelwidget ${prefix}epsg
      frame ${prefix}.semimajoraxis
         label ${prefix}.semimajoraxis.lbl -text [lindex $Mapper::Lbl(SpheroidSMA) $GDefs(Lang)] -width 23 -anchor nw
         entry ${prefix}.semimajoraxis.entry -textvariable Mapper::WKT::Data(SpheroidSMA) -bd 1 -bg $GDefs(ColorLight) -width 1
         pack ${prefix}.semimajoraxis.lbl  -side left
         pack ${prefix}.semimajoraxis.entry -side left -fill x -expand True

      frame ${prefix}.inverseflattening
         label ${prefix}.inverseflattening.lbl -text [lindex $Mapper::Lbl(SpheroidIF) $GDefs(Lang)] -width 23 -anchor nw
         entry ${prefix}.inverseflattening.entry -textvariable Mapper::WKT::Data(SpheroidIF) -bd 1 -bg $GDefs(ColorLight) -width 1
         pack ${prefix}.inverseflattening.lbl  -side left
         pack ${prefix}.inverseflattening.entry -side left -fill x -expand True
      pack ${prefix}.semimajoraxis ${prefix}.inverseflattening -side top -padx 5 -pady 2 -fill x
   pack ${prefix} -side top -padx 5 -pady 5 -fill x

   set prefix [string range ${prefix} 0 [expr [string last "." ${prefix}] - 1]]
   if { [llength $Data(DatumTOWGS84)] } {
      frame ${prefix}.towgs84
         label ${prefix}.towgs84.lbl -text [lindex $Mapper::Lbl(TOWGS84) $GDefs(Lang)] -width 23 -anchor nw
         entry ${prefix}.towgs84.entry -textvariable Mapper::WKT::Data(DatumTOWGS84) -bd 1 -bg $GDefs(ColorLight) -width 25
         pack ${prefix}.towgs84.lbl -side left
         pack ${prefix}.towgs84.entry -side left -fill x -expand True
      pack ${prefix}.towgs84 -side top -padx 5 -pady 2 -fill x
   }

   set prefix "${prefix}.flpm"
   frame ${prefix}epsg
      label ${prefix}epsg.lbl -text "PRIMEM   "
      ComboBox::Create ${prefix}epsg.sel Mapper::WKT::Data(Meridian) edit unsorted nodouble -1 $Data(Spheroids) 30 14 "Mapper::WKT::UpdateWidget PRIMEM"
      label ${prefix}epsg.val -textvariable Mapper::WKT::Data(PRIMEM)
      pack ${prefix}epsg.lbl ${prefix}epsg.sel ${prefix}epsg.val -side left -padx 2
   labelframe ${prefix} -labelwidget ${prefix}epsg
      frame ${prefix}.value
         label ${prefix}.value.lbl -text [lindex $Mapper::Lbl(MeridianLon) $GDefs(Lang)] -width 32 -anchor nw
         entry ${prefix}.value.entry -textvariable Mapper::WKT::Data(MeridianLon) -bd 1 -bg $GDefs(ColorLight) -width 1
         pack ${prefix}.value.lbl -side left
         pack ${prefix}.value.entry -side left -fill x -expand True
      pack  ${prefix}.value -side top -padx 5 -pady 2 -fill x
   pack ${prefix} -side top -padx 5 -pady 5 -fill x

   set prefix [string range ${prefix} 0 [expr [string last "." ${prefix}] - 1]]
   set prefix "${prefix}.flunite"

   frame ${prefix}epsg
      label ${prefix}epsg.lbl -text "UNIT     "
      ComboBox::Create ${prefix}epsg.sel Mapper::WKT::Data(Unit) edit unsorted nodouble -1 $Data(Units) 30 14 "Mapper::WKT::UpdateWidget UNIT"
      label ${prefix}epsg.val -textvariable Mapper::WKT::Data(UNIT)
      pack ${prefix}epsg.lbl ${prefix}epsg.sel ${prefix}epsg.val -side left -padx 2
   labelframe ${prefix} -labelwidget ${prefix}epsg
      frame ${prefix}.value
         label ${prefix}.value.lbl -text [lindex $Mapper::Lbl(UnitConvert) $GDefs(Lang)] -width 23 -anchor nw
         entry ${prefix}.value.entry -textvariable Mapper::WKT::Data(UnitConvert) -bd 1 -bg $GDefs(ColorLight) -width 1
         pack ${prefix}.value.lbl -side left
         pack ${prefix}.value.entry -side left -fill x -expand True
      pack ${prefix}.value -side top -padx 5 -pady 2 -fill x
   pack ${prefix} -side top -padx 5 -pady 5  -fill x
   set prefix [string range ${prefix} 0 [expr [string last "." ${prefix}] - 1]]

   if { [llength [set projpos [Mapper::WKT::Find $Data(Proj) AXIS sublist]]] } {
      set prefix "${prefix}.flaxis"
      set Data(Axis1Name)  "[lindex [lindex $Data(Proj) $projpos] 0]"
      set Data(Axis1Value) "[lindex [lindex $Data(Proj) $projpos] 1]"
      set Data(AXIS)      "AXIS"
      lset projpos end [expr [lindex $projpos end] + 2]
      set Data(Axis2Name)  "[lindex [lindex $Data(Proj) $projpos] 0]"
      set Data(Axis2Value) "[lindex [lindex $Data(Proj) $projpos] 1]"

      label ${prefix}epsg -textvariable Mapper::WKT::Data(AXIS)
      labelframe ${prefix} -labelwidget ${prefix}epsg
         frame ${prefix}.axis1
            label ${prefix}.axis1.name -textvariable Mapper::WKT::Data(Axis1Name) -width 10 -anchor nw
            entry ${prefix}.axis1.value -textvariable Mapper::WKT::Data(Axis1Value) -bd 1 -bg $GDefs(ColorLight) -width 1
            pack ${prefix}.axis1.name -side left
            pack ${prefix}.axis1.value -side left -fill x -expand True
         frame ${prefix}.axis2
            label ${prefix}.axis2.name -textvariable Mapper::WKT::Data(Axis2Name) -width 10 -anchor nw
            entry ${prefix}.axis2.value -textvariable Mapper::WKT::Data(Axis2Value) -bd 1 -bg $GDefs(ColorLight) -width 1
            pack ${prefix}.axis2.name -side left
            pack ${prefix}.axis2.value -side left -fill x -expand True
         pack ${prefix}.axis1 ${prefix}.axis2 -side top -padx 5 -pady 2 -fill x
      pack ${prefix} -side top -padx 5 -pady 5 -fill x
      set prefix [string range ${prefix} 0 [expr [string last "." ${prefix}] - 1]]
   }

   if { [lindex $Data(Proj) 0] == "PROJCS" } {
      set prefix [string range ${prefix} 0 [expr [string last "." ${prefix}] - 1]]
   }
   update idletasks

   Mapper::WKT::ParamProj ${prefix}

   #----- Wait for result

   raise .mapperparamsproj
   grab .mapperparamsproj
   tkwait variable Mapper::WKT::Data(Done)

   #----- Send the result

   destroy .mapperparamsproj
   if  { $Data(Done)<0 } {
      return $Proj
   } else {
      return [Mapper::WKT::FromList [Mapper::WKT::BuildList $Data(Proj)]]
   }
}

proc Mapper::WKT::ParamProj { { Frame "" } } {
   global GDefs
   variable Data

   if { $Frame!="" } {
      set Data(Frame) $Frame
   } else {
      set Frame $Data(Frame)
   }

   if { [llength [set projpos [Mapper::WKT::Find $Data(Proj) PROJECTION 0]]] } {
      set prefix "[string range ${Frame} 0 [expr [string last "." ${Frame}] - 1]].flprojection"
      destroy $prefix ${prefix}epsg

      set Data(Projection) [lindex $Data(Proj) $projpos]

      frame ${prefix}epsg
         label ${prefix}epsg.lbl -text "PROJECTION"
         ComboBox::Create ${prefix}epsg.sel Mapper::WKT::Data(Projection) edit unsorted nodouble -1 $Data(Projections) 30 14 "Mapper::WKT::UpdateWidget PROJECTION"
         label ${prefix}epsg.val -textvariable Mapper::WKT::Data(PROJECTION)
         pack ${prefix}epsg.lbl ${prefix}epsg.sel ${prefix}epsg.val -side left -padx 2
      labelframe ${prefix} -labelwidget ${prefix}epsg
      pack ${prefix} -side top -padx 5 -pady 5 -fill x

      #----- Positionnement au prochain élément, après PROJECTION
      set indextitre [Mapper::WKT::Find $Data(Proj) PROJECTION]
      lset indextitre end [expr [lindex $indextitre end] + 2]
      set iparams 0
      set prefix "${prefix}.bidon"

      while { [set curparam [lindex $Data(Proj) $indextitre]] != {} && $curparam != "AUTHORITY" } {
         #----- Si le type de paramètre courant n'est pas égale au précédent, on crée une autre section
         if { $curparam != [lindex $Data(Proj) [lreplace $indextitre end end [expr [lindex $indextitre end] - 2]]] } {
            #----- On redéfinie la variable Data(ProjIntPrefix) pour inclure les prochains widgets dans le labelframe
            set prefix [string range ${prefix} 0 [expr [string last "." ${prefix}] - 1]]
            set prefix "${prefix}.flparams$iparams"
            labelframe ${prefix} -text $curparam
            pack ${prefix} -side top -padx 5 -pady 5 -fill x
         }
         frame ${prefix}.frm1params$iparams
            set sublist [lindex $Data(Proj) [lreplace $indextitre end end [expr [lindex $indextitre end] + 1]]]
            set Data(ProjParams1$iparams) [lindex $sublist 0]
            set Data(ProjParams2$iparams) [lindex $sublist 1]
            label ${prefix}.frm1params$iparams.entry1 -anchor w -text $Data(ProjParams1$iparams) -width 25
            entry ${prefix}.frm1params$iparams.entry2 -textvariable Mapper::WKT::Data(ProjParams2$iparams) -bd 1 -bg $GDefs(ColorLight) -width 1
            pack ${prefix}.frm1params$iparams.entry1 -side left
            pack ${prefix}.frm1params$iparams.entry2 -side left -fill x -expand True
         pack ${prefix}.frm1params$iparams -side top -padx 5 -pady 2 -fill x

            #----- Si une section EPSG existe, on l'affiche
            if { [lindex $sublist 2]=="AUTHORITY" } {
               set Data(ProjParams3$iparams) [lindex $sublist {3 1}]
               set Data(PROJECTION) "(EPGS:Data(ProjParams3$iparams))"
            } else {
               set Data(PROJECTION) ""
            }

         lset indextitre end [expr [lindex $indextitre end] + 2]
         incr iparams
      }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::WKT::Item>
# Creation : Fevrier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Cherche une valeur dans une liste de sous-liste. La valeur recherchée
#            se trouve à la position "Colonne" et on retourne la valeur à "RIndex".
#            Si RIndex n'existe pas, la position dans la liste principale est retounée.
#
# Parametres :
#   <List>   : La liste à rechercher. Généralement une liste de sous-liste.
#   <Value>  : La valeur recherché.
#   <Index>  : La position de la valeur recherchée dans la sous-liste.
#
# Retour   :
#  <List>  :  Sous-liste
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc Mapper::WKT::Item { List Value Index } {

   foreach item $List {
      if { [lindex $item $Index]==$Value } {
         return $item
      }
   }
   return {}
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::WKT::List>
# Creation : Fevrier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Crée une liste d'une seule colonne d'une liste de sous-listes.
#
# Parametres :
#   <List>   : Nom de la variable liste de sous-listes
#   <Result> : Nom de la variable liste de sous-listes
#   <Index>  : Index de la colonne à conserver
#
# Retour   :
#  <List>  : Le nom de la variable liste des éléments créés
#
# Remarques : Utilisé pour les listes des combobox
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc Mapper::WKT::List { List Result Index } {
   variable Data

   upvar $List list
   upvar $Result result

   set result {}
   foreach sublist $list {
      lappend result [lindex $sublist $Index]
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::WKT::BuildList>
# Creation : Fevrier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Remplace les valeurs changées de l'interface
#
# Parametres :
#   <WKTList>   : La liste courante traitée. Cette liste est quelque fois un seul
#                 élément lors des appels récurrents de la fonction
#
# Retour   :
#  <NewWKTList> : La liste WKT avec les nouvelles valeurs de l'interface
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc Mapper::WKT::BuildList { WKTList {Index 0}} {
   variable Data

   set type [lindex $WKTList $Index]

   switch $type {
         "SPHEROID" {
            if { [llength $Data(SpheroidEPSG)] && $Data(SpheroidEPSG)!="" } {
               return "SPHEROID \{\"$Data(Spheroid)\" $Data(SpheroidSMA) $Data(SpheroidIF) AUTHORITY \{\"EPSG\" \"$Data(SpheroidEPSG)\"\}\}"
            } else {
               return "SPHEROID \{\"$Data(Spheroid)\" $Data(SpheroidSMA) $Data(SpheroidIF)\}"
            }
         }

         "PRIMEM" {
            if { [llength $Data(MeridianEPSG)] && $Data(MeridianEPSG)!="" } {
                  return "PRIMEM \{\"$Data(Meridian)\" $Data(MeridianLon) AUTHORITY \{\"EPSG\" \"$Data(MeridianEPSG)\"\}\}"
               } else {
                  return "PRIMEM \{\"$Data(Meridian)\" $Data(MeridianLon)\}"
               }
         }

         "UNIT" {
            if { [llength $Data(UnitEPSG)] && $Data(UnitEPSG)!="" } {
               return "UNIT \{\"$Data(Unit)\" $Data(UnitConvert) AUTHORITY \{\"EPSG\" \"$Data(UnitEPSG)\"\}\}"
            } else {
               return "UNIT \{\"$Data(Unit)\" $Data(UnitConvert)\}"
            }
         }

         "DATUM" {
            set towgs84 ""
            if { [llength $Data(DatumTOWGS84)] && $Data(DatumTOWGS84)!="" } {
               set towgs84 " TOWGS84 \{$Data(DatumTOWGS84)\}"
            }

            if { [llength $Data(DatumEPSG)] && $Data(DatumEPSG)!="" } {
               return "DATUM \{\"$Data(Datum)\" [Mapper::WKT::BuildList $Data(Proj) \
                  [Mapper::WKT::Find $Data(Proj) SPHEROID]]${towgs84} AUTHORITY \{\"EPSG\" \"$Data(DatumEPSG)\"\}\}"
            } else {
               return "DATUM \{\"$Data(Datum)\" [Mapper::WKT::BuildList $Data(Proj) \
                  [Mapper::WKT::Find $Data(Proj) SPHEROID]]${towgs84}\}"
            }
         }

         "GEOGCS" {
            set tmp ""
            # Si la section AXIS existe, on l'ajoute à la chaîne finale
            # Même test pour le no epsg
            if { [llength [Mapper::WKT::Find $Data(Proj) AXIS]] } {
               set tmpaxis " AXIS \{\"$Data(Axis1Name)\" $Data(Axis1Value)\} AXIS \{\"$Data(Axis2Name)\" $Data(Axis2Value)\}"
            } else {
               set tmpaxis ""
            }
            if { [llength $Data(CoordRefEPSG)] && $Data(CoordRefEPSG)!="" } {
               set tmpepsg " AUTHORITY \{\"EPSG\" \"$Data(CoordRefEPSG)\"\}"
            } else {
               set tmpepsg ""
            }

            set Data(UNITn) 0
            return "GEOGCS \{\"$Data(CoordRef)\"\
               [Mapper::WKT::BuildList $Data(Proj) [Mapper::WKT::Find $Data(Proj) DATUM]]\
               [Mapper::WKT::BuildList $Data(Proj) [Mapper::WKT::Find $Data(Proj) PRIMEM]]\
               [Mapper::WKT::BuildList $Data(Proj) [Mapper::WKT::Find $Data(Proj) UNIT]]$tmpaxis$tmpepsg\}"
         }

         "PROJCS" {
            # Si la section PROJECTION existe, on définit la variable "proj" pour assigner les différentes sections
            # et l'ajouter à la chaîne finale
            if { [llength $Data(ProjRefEPSG)] && $Data(ProjRefEPSG)!="" } {
               set epsg " AUTHORITY \{\"EPSG\" \"$Data(ProjRefEPSG)\"\}"
            } else {
               set epsg ""
            }
            set index [Mapper::WKT::Find $Data(Proj) PROJECTION]
            if { [llength $index] } {
               set proj "PROJECTION \{\"$Data(Projection)\"\}"
               set iparams 0
               lset index end [expr [lindex $index end] + 2]
               while { [info exists Mapper::WKT::Data(ProjParams1$iparams)] } {
                  if { [info exists Mapper::WKT::Data(ProjParams3$iparams)] && $Mapper::WKT::Data(ProjParams3$iparams) != "" } {
                     append proj " [lindex $Data(Proj) $index] \{\"$Data(ProjParams1$iparams)\" $Data(ProjParams2$iparams) AUTHORITY \{\"EPSG\" \"$Data(ProjParams3$iparams)\"\}\}"
                  } else {
                     append proj " [lindex $Data(Proj) $index] \{\"$Data(ProjParams1$iparams)\" $Data(ProjParams2$iparams)\}"
                  }
                  lset index end [expr [lindex $index end] + 2]
                  incr iparams
               }
               return "PROJCS \{\"$Data(ProjRef)\" [Mapper::WKT::BuildList [lindex $Data(Proj) [Mapper::WKT::Find $Data(Proj) GEOGCS]]] $proj$epsg\}"
            } else {
               return "PROJCS \{\"$Data(ProjRef)\" [Mapper::WKT::BuildList [lindex $Data(Proj) [Mapper::WKT::Find $Data(Proj) GEOGCS]]]$epsg\}"
            }
         }

         default {
            puts stderr "Mapper::WKT::BuildList: Liste invalide. La liste doit faire partie du standard WKT."
            return ""
         }
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::WKT::ToList>
# Creation : Fevrier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Transformer une variable WKT (Well-known Text) en une liste
#
# Parametres :
#   <WKT>   : Chaine de caractères du standard WKT
#
# Retour   :
#  <list> : Liste formé par le WKT. Chaque partie de string sous la forme:
#           ELEMENT[ "un sous-element",2,3,4 ] sont convertie en liste:
#           { ELEMENT {"un sous-element" 2 3 4}}
#           Donc, en une liste de l'entete et d'une sous-liste des elements.
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc Mapper::WKT::ToList { WKT } {
   return [string map {\[ " \{" \] \} \, " "} $WKT]
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::WKT::FromList>
# Creation : Fevrier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Transformer une liste en une variable de type WKT (Well Known Text)
#
# Parametres :
#   <Liste>   : Liste à transformer en WKT
#
# Retour   :
#  <String> : Chaîne WKT créé à partir de la liste
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc Mapper::WKT::FromList     { Liste } {

   #----- Éliminer les virgules dans les éléments de la liste

   set str [string map {\, ""} $Liste]

   #----- Tranformer la liste en chaîne WKT

   set str [string map { " \{" \[ \} \] " " \,} $str]
   set index1 0
   set index2 0

   #----- Remplacer les virgules contenues par erreurs dans une paire de guillemets par des espaces
   while { [set index1 [string first "\"" $str [expr $index2 + 1]]] != -1 && [set index2 [string first "\"" $str [expr $index1 + 1]]] != -1 } {
      set str [string replace $str $index1 $index2 [string map { \, " " } [string range $str $index1 $index2]]]
   }
   return $str
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::WKT::GetEPSG>
# Creation : Fevrier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Cherche la liste et retourne le premier EPSG trouvé
#
# Parametres :
#   <Liste>   : Rechercher dans la liste "Liste"
#
# Retour   :
#  <EPSG> : Entier EPSG. Retourne -1 si non trouvé.
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc Mapper::WKT::GetEPSG { Liste } {

   if { [set index [lsearch $Liste "AUTHORITY"]]!=-1 } {
      lset index end [expr [lindex $index end]+1]
      lappend index 1
      return [lindex $Liste $index]
   } else {
      return ""
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::WKT::Find>
# Creation : Fevrier 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Cherche la liste, et toutes ses sous-listes
#
# Parametres :
#   <Liste>   : Rechercher dans la liste "Liste"
#   <Element> : Element a trouver l'indice
#   <NextSublist> : Détermine le retour selon le paramètre recu :
#      "sublist" : Envoie l'index de l'élément suivant, qui est généralement une sous-liste
#      "none" : Par défaut. Envoie l'index de l'élément trouvé
#      *autres* : Utilise NextSubList comme un index (avec lindex) dans l'élément suivant
#   <Prefix>  : Prefix d'une autre liste à concatener
#
# Retour   :
#  <Index> : Index nesté (ex: {0 1 0 2} ). Retourne -1 si non trouvé.
#
# Remarques :
#    - Fonction légèrement modifié de http://wiki.tcl.tk/3726
#    - Data(UNITn) sert a "fourre" le systeme puisque il y a 2 UNIT et on dans ce cas
#         on veur le deuxieme recursivement
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc Mapper::WKT::Find {Liste Element {NextSublist "none"} {Prefix {}} } {
   variable Data

    set pos [lsearch $Liste $Element]
    if { $pos!=-1 && ($Element!="UNIT" || [incr Data(UNITn)]>1) } {
      if { $NextSublist == "sublist" } {
         #----- Renvoie de la supposée sous-liste après Element, en ajoutant 1 à la position trouvé de Element
         lset pos end [expr [lindex $pos end] + 1]
         return [concat $Prefix $pos]
      } elseif { $NextSublist == "none" } {
         #----- Renvoie de la position directe de Element
         return [concat $Prefix $pos]
      } else {
         #----- Renvoie de l'élément de la sous-liste à la position spécifié par NextSubList
         lset pos end [expr [lindex $pos end] + 1]
         lappend pos $NextSublist
         return [concat $Prefix $pos]
      }
   }

   set i 0
   foreach iliste $Liste {
      if { ![string equal $iliste [lindex $iliste 0]] } {
         set pos [Mapper::WKT::Find $iliste $Element $NextSublist [concat $Prefix $i]]
         if { [llength $pos] } {
             return $pos
         }
      }
      incr i
   }
   return {}
}

proc Mapper::WKT::Init  { Proj } {
   variable Data

   set Data(PROJCS)     ""
   set Data(GEOGCS)     ""
   set Data(DATUM)      ""
   set Data(SPHEROID)   ""
   set Data(PRIMEM)     ""
   set Data(UNIT)       ""
   set Data(PROJECTION) ""

   #----- Mettre la string WKT en list

   set Data(Proj) [Mapper::WKT::ToList $Proj]

   #----- Assigner les valeurs de sélections des combobox et des entry

   if { [llength [set index [Mapper::WKT::Find $Data(Proj) PROJCS sublist]]] } {
      set Data(ProjRef)     [lindex [lindex $Data(Proj) $index] 0]
      set Data(ProjRefEPSG) [Mapper::WKT::GetEPSG [lindex $Data(Proj) $index]]
      set Data(PROJCS)      (EPSG:$Data(ProjRefEPSG))
   }

   if { [llength [set index [Mapper::WKT::Find $Data(Proj) GEOGCS sublist]]]} {
      set Data(CoordRef)     [lindex [lindex $Data(Proj) $index] 0]
      set Data(CoordRefEPSG) [Mapper::WKT::GetEPSG [lindex $Data(Proj) $index]]
      set Data(GEOGCS)       (EPSG:$Data(CoordRefEPSG))
   }

   if { [llength [set index [Mapper::WKT::Find $Data(Proj) DATUM sublist]]] } {
      set Data(Datum)     [lindex [lindex $Data(Proj) $index] 0]
      set Data(DatumEPSG) [Mapper::WKT::GetEPSG [lindex $Data(Proj) $index]]
      set Data(DATUM)     (EPSG:$Data(DatumEPSG))
      if { [llength [set index [Mapper::WKT::Find $Data(Proj) TOWGS84 sublist]]] } {
         set Data(DatumTOWGS84) [lindex $Data(Proj) $index]
      } else {
         set Data(DatumTOWGS84) {}
      }
   }

   if { [llength [set index [Mapper::WKT::Find $Data(Proj) SPHEROID sublist]]] } {
      set Data(Spheroid)     [lindex [lindex $Data(Proj) $index] 0]
      set Data(SpheroidSMA)  [lindex [lindex $Data(Proj) $index] 1]
      set Data(SpheroidIF)   [lindex [lindex $Data(Proj) $index] 2]
      set Data(SpheroidEPSG) [Mapper::WKT::GetEPSG [lindex $Data(Proj) $index]]
      set Data(SPHEROID)     (EPSG:$Data(SpheroidEPSG))
   }

   if { [llength [set index [Mapper::WKT::Find $Data(Proj) PRIMEM sublist]]] } {
      set Data(Meridian)     [lindex [lindex $Data(Proj) $index] 0]
      set Data(MeridianLon)  [lindex [lindex $Data(Proj) $index] 1]
      set Data(MeridianEPSG) [Mapper::WKT::GetEPSG [lindex $Data(Proj) $index]]
      set Data(PRIMEM)       (EPSG:$Data(MeridianEPSG))
   }

   set Data(UNITn) 0
   if { [llength [set index [Mapper::WKT::Find $Data(Proj) UNIT sublist]]] } {
      set Data(Unit)        [lindex [lindex $Data(Proj) $index] 0]
      set Data(UnitConvert) [lindex [lindex $Data(Proj) $index] 1]
      set Data(UnitEPSG)    [Mapper::WKT::GetEPSG [lindex $Data(Proj) $index]]
      set Data(UNIT)        (EPSG:$Data(UnitEPSG))
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Mapper::WKT::UpdateWidget>
# Creation : Mars 2007 - Maxime Samuel - CMC/CMOE
#
# But      : Mettre à jour certains widgets de l'interface WKT.
#
# Parametres :
#   <Section> : Nom de la section de l'interface (e.g. DATUM, SPHEROID)
#   <EPSG>  : Optionnel. Spécifie un no EPSG pour la section en cours. Par défaut, 0.
#
# Retour   :
#
# Remarques :
#
# Modifications :
#
#    Nom         : -
#    Date        : -
#    Description : -
#
#-------------------------------------------------------------------------------

proc Mapper::WKT::UpdateWidget { Section { EPSG 0 } } {
   variable Data

   if { $EPSG=="" } {
      set EPSG 0
   }

   switch $Section {
      "PROJECTION" {
         if { $EPSG } {
            set sublist [Mapper::WKT::Item $Data(ProjectionList) $EPSG 0]
         } else {
            set sublist [Mapper::WKT::Item $Data(ProjectionList) $Data(Projection) 1]
         }

         if { [llength $sublist] } {
            set Data(Projection)       [lindex $sublist 1]
            set Data(ProjectionParams) [lindex $sublist 2]
            set Data(ProjectionEPSG)   [lindex $sublist 0]
            set Data(PROJECTION)       (EPSG:$Data(ProjectionEPSG))
         } else {
            set Data(PROJECTION)      ""
         }
#         Mapper::WKTParamProj
         return $sublist
      }

      "SPHEROID" {
         if { $EPSG } {
            set sublist [Mapper::WKT::Item $Data(SpheroidList) $EPSG 0]
         } else {
            set sublist [Mapper::WKT::Item $Data(SpheroidList) $Data(Spheroid) 1]
         }

         if { [llength $sublist] } {
            set Data(Spheroid)     [lindex $sublist 1]
            set Data(SpheroidSMA)  [lindex $sublist 2]
            set Data(SpheroidIF)   [lindex $sublist 4]
            set Data(SpheroidEPSG) [lindex $sublist 0]
            set Data(SPHEROID)     (EPSG:$Data(SpheroidEPSG))
         } else {
            set Data(SPHEROID)    ""
         }
         return $sublist
      }

      "PRIMEM" {
         if { $EPSG } {
            set sublist [Mapper::WKT::Item $Data(MeridianList) $EPSG 0]
         } else {
            set sublist [Mapper::WKT::Item $Data(MeridianList) $Data(Meridian) 1]
         }

         if { [llength $sublist] } {
            set Data(Meridian)     [lindex $sublist 1]
            set Data(MeridianLon)  [lindex $sublist 2]
            set Data(MeridianEPSG) [lindex $sublist 0]
            set Data(PRIMEM)       (EPSG:$Data(MeridianEPSG))
         } else {
            set Data(PRIMEM)     ""
         }
         return $sublist
      }

      "UNIT" {
         if { $EPSG } {
            set sublist [Mapper::WKT::Item $Data(UnitList) $EPSG 0]
         } else {
            set sublist [Mapper::WKT::Item $Data(UnitList) $Data(Unit) 1]
         }

         if { [llength $sublist] } {
            if { [lindex $sublist 4] == [list] || [lindex $sublist 5] == [list] } {
                     set subtmplist [Mapper::WKT::Item $Data(UnitList) [lindex $sublist 3] 0]
                     set convert [expr [lindex $subtmplist 4] / [lindex $subtmplist 5]]
                  } else {
                     set convert [expr [lindex $sublist 4] / [lindex $sublist 5]]
                  }
            set Data(Unit)        [lindex $sublist 1]
            set Data(UnitConvert) $convert
            set Data(UnitEPSG)    [lindex $sublist 0]
            set Data(UNIT)        (EPSG:$Data(UnitEPSG))
         } else {
            set Data(UNIT)        ""
         }

         return $sublist
      }

      "DATUM" {
         if { $EPSG } {
            set sublist [Mapper::WKT::Item $Data(DatumList) $EPSG 0]
         } else {
            set sublist [Mapper::WKT::Item $Data(DatumList) $Data(Datum) 1]
         }

         if { [llength $sublist] } {
            set Data(Datum)     [lindex $sublist 1]
            set Data(DatumEPSG) [lindex $sublist 0]
            set Data(DATUM)     (EPSG:$Data(DatumEPSG))
            set subspheroidlist [Mapper::WKT::UpdateWidget SPHEROID [lindex $sublist 3]]
            Mapper::WKT::UpdateWidget PRIMEM  [lindex $sublist 4]
            if { $subspheroidlist != -1 } {
               Mapper::WKT::UpdateWidget UNIT [lindex $subspheroidlist 3]
            }
         } else {
            set Data(DATUM) ""
         }
         return $sublist
      }

      "GEOGCS" {
         if { $EPSG } {
            set sublist [Mapper::WKT::Item $Data(CoordRefList) $EPSG 0]
         } else {
            set sublist [Mapper::WKT::Item $Data(CoordRefList) $Data(CoordRef) 1]
         }

         if { [llength $sublist] } {
            set Data(CoordRef)     [lindex $sublist 1]
            set Data(CoordRefEPSG) [lindex $sublist 0]
            set Data(GEOGCS)       (EPSG:$Data(CoordRefEPSG))
            Mapper::WKT::UpdateWidget DATUM [lindex $sublist 3]
         } else {
            set Data(GEOGCS) ""
         }

         return $sublist
      }

      "PROJCS" {
         # On prend pour acquis que PROJCS ne recevra jamais de no EPSG
         # Puisque le CRS choisi n'est peut-être pas celui de base, on cherche avec le no epsg à la position
         # 4 dans la sous-liste (sublist) tant qu'il y a un chiffre à cette position.
         set sublist [Mapper::WKT::Item $Data(CoordRefList) $Data(ProjRef) 1]
         set epsgtmp [lindex $sublist 0]
         # variable pour éviter une boucle infinie
         set i 0
         while { $epsgtmp != "" || $i >= 30 } {
            set sublisttmp [Mapper::WKT::Item $Data(CoordRefList) $epsgtmp 0]
            set epsgtmp [lindex $sublisttmp 4]
            incr i
         }
         if { [llength $sublist] } {
            set Data(ProjRef)     [lindex $sublist 1]
            set Data(ProjRefEPSG) [lindex $sublist 0]
            set Data(PROJCS)      (EPSG:$Data(ProjRefEPSG))
            Mapper::WKT::UpdateWidget GEOGCS   [lindex $sublisttmp 0]
         } else {
            set Data(ProjRefEPSG) ""
            set Data(PROJCS)     ""
         }
         return $sublist
      }

      default {
         puts stderr "Mapper::WKT::UpdateWidget: Paramètre de section invalide."
      }
   }
}

