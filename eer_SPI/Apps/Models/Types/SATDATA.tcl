#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Interface pour la gestion des experiences.
# Fichier   : SATDATA.tcl
# Creation  : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But       : Description des procedures relatives au module de donnees satellitaires.
#
#===============================================================================

namespace eval SATDATA {
   variable Lvl

   set Lbl(No)        { "Non" "No" }
   set Lbl(Result)    { "Resultats" "Results" }
   set Lbl(Suppress)  { "Supprimer" "Delete" }
   set Lbl(Yes)       { "Oui" "Yes" }

   set Msg(Suppress) { "Supprimer les donnees satellites de" "Delete satellite data from" }
}

#-------------------------------------------------------------------------------
# Nom      : <SATDATA::New>
# Creation : Octobre 1999 - J.P. Gauthier - CMC/CMOE
#
# But      : Creation d'une nouvelle serie de donnees satellitaires.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SATDATA::New { Parent } {
   global GDefs

   SatData::Window
   set ::SatData::Data(ResultFile) $GDefs(DirData)/$Exp::Data(No)_$Exp::Data(Name)/SatData
   set ::SPI::Src(Type) $Exp::Data(Type)
   set ::SPI::Src(No)   ""
   set ::SPI::Src(Name) $Exp::Data(Name)
   set ::SPI::Src(Area) ""
   set ::SPI::Src(Lat)  $Exp::Data(Lat)
   set ::SPI::Src(Lon)  $Exp::Data(Lon)
   set ::SPI::Src(Elev) 0
   set ::SPI::Src(Info) 1
}

#----------------------------------------------------------------------------
# Nom      : <SATDATA::PoolPrint>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Retourne une chaine formattee contenant les parametres.
#
# Parametres :
#   <Line>   : Ligne non modifiee du fichier Canerm pool.
#   <Langue> : Langue dans laquelle afficher les parametres.
#
# Retour:
#
# Remarques :
#
#----------------------------------------------------------------------------

proc SATDATA::PoolPrint { Line Langue } {

   return ""
}

#-------------------------------------------------------------------------------
# Nom      : <SATDATA::PopUp>
# Creation : Aout 2001 - J.P. Gauthier - CMC/CMOE
#
# But      : Afficher le popup contextuel des simulations.
#
# Parametres :
#    <X>        : ...
#    <Y>        : ...
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SATDATA::PopUp { X Y } {
   global GDefs
   variable Lbl

   if { ![winfo exists .satdatapopup] } {
      menu .satdatapopup -tearoff 0 -bd 1 -type normal -activeborderwidth 1
         .satdatapopup add command -label "SATDATA"  -command "" -background yellow -activebackground yellow
         .satdatapopup add command -label [lindex $Lbl(Result) $GDefs(Lang)] \
            -command { SPI::FileOpen NEW FieldBox "$Exp::Data(Name) (SATDATA)" "" [Exp::Path]/SatData }
         .satdatapopup add separator
         .satdatapopup add command -label [lindex $Lbl(Suppress) $GDefs(Lang)] -command "SATDATA::Suppress"
   }

   tk_popup .satdatapopup $X $Y 0
}

#-------------------------------------------------------------------------------
# Nom      : <SATDATA::Suppress>
# Creation : Octobre 2000 - J.P. Gauthier - CMC/CMOE
#
# But      : Effacer les donnees satellitaires de l'experience selectionnee.
#
# Parametres :
#
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc SATDATA::Suppress { } {
   global GDefs
   variable Lbl
   variable Msg

   if { [Dialog::Default . 400 WARNING $Msg(Suppress) "\n\n$Exp::Data(No) $Exp::Data(Name)" 0 $Lbl(No) $Lbl(Yes)] } {
      file delete -force $GDefs(DirData)/$Exp::Data(No)_$Exp::Data(Name)/SatData
      Model::Check 0
   }
}
