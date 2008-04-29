#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet    : Interface de selection d'especes d'isotopes.
# Fichier   : IsoBox.tcl
# Creation  : Avril 2008 - J.P. Gauthier  - CMC/CMOE
#
# Description:
#
#      Cette interface permet de selectionne une espece d'isotope
#
# Remarques :
#   aucune
#
# Modification:
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#===============================================================================

package provide IsoBox 1.0

proc IdIsoBox { Show } {

   if { $Show } {
      puts "(INFO) Loading Standard CMC/CMOE Widget Package IsoBox Version 1.0"
   }
}

namespace eval IsoBox {
   global GDefs
   variable Lbl
   variable Data

   set Lbl(Title)     { "Sélecteur d'isotopes" "Isotope selector" }

   set Lbl(Name)      { "Nom"                                "Name" }
   set Lbl(Intensity) { "Intensité"                          "Intensity" }
   set Lbl(HalfLife)  { "Demi-vie\n(s)"                      "Half-life\n(s)" }
   set Lbl(Dry)       { "Taux de\nlessivage\nsec\n(s -1)"    "Dry\nScavenging\nRate\n(s -1)" }
   set Lbl(Wet)       { "Taux de\nlessivage\nhumide\n(s -1)" "Wet\nScavenging\nRate\n(s -1)" }
   set Lbl(DryDep)    { "Vitesse\nde dépôt\n(m/s)"           "Deposition\nVelocity\n(m/s)" }
   set Lbl(Unit)      { "Unité"                              "Unit" }
   set Lbl(Close)     { "Fermer"                             "Close" }
}

#-------------------------------------------------------------------------------
# Nom      : <IsoBox::Create>
# Creation : Mai 1997 - J.P. Gauthier - CMC/CMOE
#
# But      : Gere la fenetre de selection des isotopes
#
# Parametres :
#
# Remarques :
#   Aucune
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#-------------------------------------------------------------------------------

proc IsoBox::Create { Parent { Command "" } } {
   global   GDefs
   variable Lbl
   variable Data

   $Parent config -cursor X_cursor
   update idletasks

   toplevel .isobox -bg $GDefs(ColorLight)
   wm transient .isobox $Parent
   wm title .isobox "[lindex $Lbl(Title) $GDefs(Lang)]"
   wm resizable .isobox 0 1
   wm protocol .isobox WM_DELETE_WINDOW { }

   #----- Creer le frame des bouttons entetes.
   frame .isobox.hd
      label .isobox.hd.espece    -bd 1 -text "[lindex $Lbl(Name) $GDefs(Lang)]"      -width 8  -height 5 -relief raised
      label .isobox.hd.intensite -bd 1 -text "[lindex $Lbl(Intensity) $GDefs(Lang)]" -width 11 -height 5 -relief raised
      label .isobox.hd.demi      -bd 1 -text "[lindex $Lbl(HalfLife) $GDefs(Lang)]"  -width 11 -height 5 -relief raised
      label .isobox.hd.sec       -bd 1 -text "[lindex $Lbl(Dry) $GDefs(Lang)]"       -width 11 -height 5 -relief raised
      label .isobox.hd.hum       -bd 1 -text "[lindex $Lbl(Wet) $GDefs(Lang)]"       -width 11 -height 5 -relief raised
      label .isobox.hd.depvel    -bd 1 -text "[lindex $Lbl(DryDep) $GDefs(Lang)]"    -width 11 -height 5 -relief raised
      label .isobox.hd.unit      -bd 1 -text "[lindex $Lbl(Unit) $GDefs(Lang)]"      -width 10 -height 5 -relief raised

      pack .isobox.hd.espece .isobox.hd.intensite .isobox.hd.demi .isobox.hd.sec .isobox.hd.hum .isobox.hd.depvel -side left -fill y
      pack .isobox.hd.unit -side left -fill x -fill y -expand true
   pack .isobox.hd -side top -anchor w -fill x

   #----- Creer le frame du bas qui va contenir le listbox.
   frame .isobox.bas
      listbox .isobox.bas.box -relief sunken -bd 1 -exportselection false  -highlightthickness 0 \
         -yscrollcommand ".isobox.bas.scroll set" -height 10 -width 70 -background $GDefs(ColorLight)
      pack .isobox.bas.box -side left -expand true -fill both

      scrollbar .isobox.bas.scroll -command ".isobox.bas.box yview" -bd 1 -width 10  -highlightthickness 0
      pack .isobox.bas.scroll  -side left -fill y
   pack .isobox.bas -side top -anchor w -expand true -fill both

   #----- Afficher le bouton pour fermer la fenetre de selection
   frame .isobox.plusbas
      button .isobox.plusbas.fermer -bd 1 -text [lindex $Lbl(Close) $GDefs(Lang)] -highlightthickness 0\
         -command { if { [llength [.isobox.bas.box curselection]] } {
                       set IsoBox::Data(Result) [.isobox.bas.box get [.isobox.bas.box curselection]]
                    } else {
                       set IsoBox::Data(Result) ""
                    } }
      pack .isobox.plusbas.fermer -side top -fill both -expand true
   pack .isobox.plusbas -side top -fill x

   bind .isobox.bas.box <B1-ButtonRelease> "if { \"$Command\"!=\"\" } { $Command \[%W get \[%W nearest %y\]\] }"

   #----- Inserer les isotopes dans la ScrollBox

   IsoBox::Insert

   #----- Attente de la selection du fichier

   set prevgrab [grab current]
   grab .isobox
   set Data(Result) ""
   tkwait variable IsoBox::Data(Result)

   catch { destroy .isobox }
   $Parent config -cursor left_ptr

   if { $prevgrab!="" } {
      grab $prevgrab
   }
   return $Data(Result)
}

#-------------------------------------------------------------------------------
# Nom      : <IsoBox::Insert>
# Creation : Mai 1997 - J.P. Gauthier - CMC/CMOE
#
# But      : Affiche la liste de tous les isotopes.
#
# Parametres :
#
# Remarques :
#   aucune
#
# Modifications :
#
#   Nom         : -
#   Date        : -
#   Description : -
#
#-------------------------------------------------------------------------------

proc IsoBox::Insert { }  {
   global GDefs

   set f [open $GDefs(Dir)/Data/Specie.src]

   while { [gets $f ligne]>=0 } {
      if {[string index $ligne 0]!= "C" && [string length $ligne]>90} {
         .isobox.bas.box insert end [format "%-9s %-11s %-10s %-11s %-10s %-10s %-5s"  \
            [lindex $ligne 0] [lindex $ligne 10] [lindex $ligne 1] [lindex $ligne 11] [lindex $ligne 12] [lindex $ligne 13] [lindex $ligne 14]]
      }
   }
   close $f
}

proc IsoBox::Get { Iso }  {
   global GDefs

   set ligne {}

   set ligne [exec egrep -i $Iso $GDefs(Dir)/Data/Specie.src]

   if { [llength $ligne] } {
      return [list [lindex $ligne 0] [lindex $ligne 10] [lindex $ligne 1] [lindex $ligne 11] [lindex $ligne 12] [lindex $ligne 13] [lindex $ligne 14]]
   } else {
      return ""
   }
}
