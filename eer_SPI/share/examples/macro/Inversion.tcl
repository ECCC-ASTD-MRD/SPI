#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Exemples de scripts de productions de cartes.
# Fichier  : Inversion.tcl
# Creation : Fevrier 2001 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Calcul du la temperature d'inversion et du niveaux correspondant.
#
# Arguments  :
#
# Remarques :
#
#===============================================================================

namespace eval Macro::Inversion {} {
   variable Param
   variable Data
   variable Error

   set Param(In)   ""
   set Param(Info) { "Calcul du la temperature d'inversion et du niveaux correspondant"
                     "Caclulate the inversion temperature and the corresponding level" }

   set Data(Page) ""

   set Error(Field)  { "Aucun champs valide trouvé\nvérifié le viewport actif"
                       "Could not find any valid field\nCheck the active viewport" }
}

proc Macro::Inversion::Execute { } {
   global GDefs
   variable Data
   variable Error

   set field [lindex [Viewport::Assigned $Page::Data(Frame) $Viewport::Data(VP)] 0]

   if { ![fstdfield is $field] } {
      Macro::Error $Error(Field)
      return
   }

   #----- Make a copy of the fields

   fstdfield copy Macro::MAX $field
   fstdfield copy Macro::LVL $field
   fstdfield copy Macro::TMP $field

   #----- Read all of the available levels

   fstdfield readcube Macro::TMP

   #----- Loop on the levels to figure out max and level

   set n 0
   vexpr Macro::MAX Macro::MAX<<-999

   foreach level [fstdfield stats Macro::TMP -levels] {
      vexpr ttt Macro::TMP()()($n)
      vexpr Macro::LVL ifelse(ttt>Macro::MAX,$level,Macro::LVL)
      vexpr Macro::MAX ifelse(Macro::TMP()()($n)>Macro::MAX,Macro::TMP()()($n),Macro::MAX)
      incr n
   }

   #----- Keep only over 950 mb

   vexpr Macro::MAX ifelse(Macro::LVL<950,Macro::MAX,0)

   #----- Configure the fields for rendering

   fstdfield define Macro::LVL -NOMVAR LVL
   fstdfield define Macro::MAX -NOMVAR MAX

   #----- Create the page if it does not already exists

   if { ![winfo exists $Data(Page)] } {
      set Data(Page) [SPI::PageNew True "TT Inversion"]
   }

   Viewport::Create $Data(Page) 1 1 1 1 True True VPTEMP

   Page::Activate $Data(Page)
   Page::ModeSelect Zoom $Data(Page)

   #----- Assign the fields to the viewport for visualisation

   Viewport::UnAssign $Data(Page) VPTEMP
   Viewport::Assign  $Data(Page) VPTEMP { Macro::MAX Macro::LVL }
}

proc Macro::Inversion::Clean { } {

   FSTD::UnRegister Macro::MAX
   FSTD::UnRegister Macro::LVL

   fstdfield free Macro::MAX
   fstdfield free Macro::LVL
   fstdfield free Macro::TMP
}




