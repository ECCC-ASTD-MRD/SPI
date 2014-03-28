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
# Fonctions:
#    IsoBox::Create { Parent { Command "" } }
#    IsoBox::Insert { }
#
#
#===============================================================================

package provide IsoBox 1.0

catch { SPI::Splash "Loading Widget Package IsoBox 1.0" }

namespace eval IsoBox {
   global env
   variable Lbl
   variable Param

   set Param(File) ""
   catch { set Param(File) $env(EER_DATA)/Nuclide.txt }

   set Lbl(Title)        { "Sélecteur d'isotopes"              "Isotope selector" }
   set Lbl(Symbol)       { "Symbole"                           "Symbol" }
   set Lbl(Name)         { "Nom"                               "Name" }
   set Lbl(AtomicNumber) { "Numéro\natomique\n(Z)"             "Atomic\nNumber\n(Z)" }
   set Lbl(NbNeutrons)   { "Nb. de\nneutrons\n(N)"             "Nb. of\nNeutrons\n(N)" }
   set Lbl(MassNumber)   { "Nb. de\nmasse\n(A=Z+N)"            "Mass\nNumber\n(A=Z+N)" }
   set Lbl(HalfLife)     { "Demi-vie\n(s)"                     "Half-life\n(s)" }
   set Lbl(Dry)          { "Taux de\nlessivage\nsec\n(s-1)"    "Dry\nScavenging\nRate\n(s-1)" }
   set Lbl(Wet)          { "Taux de\nlessivage\nhumide\n(s-1)" "Wet\nScavenging\nRate\n(s-1)" }
   set Lbl(DryDep)       { "Vitesse\nde dépôt\n(m/s)"          "Deposition\nVelocity\n(m/s)" }
   set Lbl(EDRC)         { "Coefficients de débit de dose externe\n\nPanache            Sol  \n(Sv m3 Bq-1 s-1)  (Sv m2 Bq-1 s-1)"  "External Dose Rate Coefficients\n\nCloudshine        Groundshine\n(Sv m3 Bq-1 s-1)  (Sv m2 Bq-1 s-1)" }
   set Lbl(Close)        { "Fermer"                            "Close" }
}

#-------------------------------------------------------------------------------
# Nom      : <IsoBox::Create>
# Creation : Mai 1997 - J.P. Gauthier - CMC/CMOE
#
# But      : Gere la fenetre de selection des isotopes
#
# Parametres  :
#   <Parent>  : Fenetre Parent
#   <Command> : Commande a utiliser pour chaque selection
#
# Remarques :
#   - Si un commande est passe en parametre, elle est execute evac chaque selection avec
#     comme seul argument la ligne selectionnee.
#
# Retour:
#   <sel>   : La fonction retourne la derniere ligne selectionnee
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
      label .isobox.hd.symbol    -bd 1 -text "[lindex $Lbl(Symbol) $GDefs(Lang)]"       -width 8  -height 5 -relief raised
      label .isobox.hd.name      -bd 1 -text "[lindex $Lbl(Name) $GDefs(Lang)]"         -width 13 -height 5 -relief raised
      label .isobox.hd.z         -bd 1 -text "[lindex $Lbl(AtomicNumber) $GDefs(Lang)]" -width 9  -height 5 -relief raised
      label .isobox.hd.n         -bd 1 -text "[lindex $Lbl(NbNeutrons) $GDefs(Lang)]"   -width 9  -height 5 -relief raised
      label .isobox.hd.a         -bd 1 -text "[lindex $Lbl(MassNumber) $GDefs(Lang)]"   -width 9  -height 5 -relief raised
      label .isobox.hd.demi      -bd 1 -text "[lindex $Lbl(HalfLife) $GDefs(Lang)]"     -width 11 -height 5 -relief raised
      label .isobox.hd.sec       -bd 1 -text "[lindex $Lbl(Dry) $GDefs(Lang)]"          -width 11 -height 5 -relief raised
      label .isobox.hd.hum       -bd 1 -text "[lindex $Lbl(Wet) $GDefs(Lang)]"          -width 11 -height 5 -relief raised
      label .isobox.hd.depvel    -bd 1 -text "[lindex $Lbl(DryDep) $GDefs(Lang)]"       -width 11 -height 5 -relief raised
      label .isobox.hd.edrc      -bd 1 -text "[lindex $Lbl(EDRC) $GDefs(Lang)]"         -width 38 -height 5 -relief raised

      pack .isobox.hd.symbol .isobox.hd.name .isobox.hd.z .isobox.hd.n .isobox.hd.a .isobox.hd.demi .isobox.hd.sec .isobox.hd.hum .isobox.hd.depvel .isobox.hd.edrc -side left -fill x -fill y -expand true
   pack .isobox.hd -side top -anchor w -fill x

   #----- Creer le frame du bas qui va contenir le listbox.
   frame .isobox.bas
      listbox .isobox.bas.box -relief sunken -bd 1 -exportselection false  -highlightthickness 0 \
         -yscrollcommand ".isobox.bas.scroll set" -height 25 -width 100 -background $GDefs(ColorLight)
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
# Retour:
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc IsoBox::Insert { } {
   global GDefs
   variable Param

   set f [open $Param(File)]
   set idxname [expr 16 + $GDefs(Lang)]

   while { [gets $f ligne]>=0 } {
      if {[string index $ligne 0]!= "c" && [string length $ligne]>90} {
         .isobox.bas.box insert end [format "%-8s %-13s %5s %8s %9s %12s %11s %11s %10s %15s %17s"  \
            [lindex $ligne 0] [lindex $ligne $idxname] [lindex $ligne 13] [lindex $ligne 15] [lindex $ligne 14] [lindex $ligne 1] \
            [lindex $ligne 10] [lindex $ligne 11] [lindex $ligne 12] [lindex $ligne 8] [lindex $ligne 9]]
      }
   }
   close $f
}
