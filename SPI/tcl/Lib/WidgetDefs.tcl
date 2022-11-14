#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Librairie de "Widget" Tk.
# Fichier  : WidgetDefs.tk
# Creation : Novembre 2000 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Ce package definit divers widgets tres pratiques
#
# Fonctions:
#    EntryVar::Create        { W Var Type Format Command args }
#    EntryVar::IsBinary      { Val }
#    EntryVar::IsDecimal     { Val }
#    EntryVar::IsDot         { Val }
#    EntryVar::IsHexadecimal { Val }
#    EntryVar::IsSign        { Val Pos }
#    EntryVar::IsOctal       { Val }
#    EntryVar::Trace         { Type Var Format args }
#
#    EntryMenu::Create       { W Edit Label Side Width Color AColor Var List Cmd }
#
#    IcoMenu::Create         { Frame Path Icons Values Var Command Def args }
#    IcoMenu::CreateDef      { Frame Path Icons Values Var Command Def args }
#    IcoMenu::Set            { Frame Value }
#
#    Option::Create          { Frame Label Var Edit Width List Cmd args }
#    Option::Disable         { Frame }
#    Option::Enable          { Frame { Edit True } }
#
# Remarques :
#    -Concu a partir de namespace donc utilisable seulement en TCL 8.0 et +
#
#===============================================================================

package provide WidgetDefs 1.0

catch { SPI::Splash "Loading Widget Package WidgetDefs 1.0" }

namespace eval ColorMenu { }
namespace eval EntryVar { }
namespace eval EntryMenu { }
namespace eval IcoMenu { }
namespace eval Option { }
namespace eval UnitOption { }

#------------------------------------------------------------------------------
# Nom      : <EntryMenu::Create>
# Creation : Avril 1998 - J.P. Gauthier - CMC/CMOE -
#
# But     : Creer le widget EntryMenu.
#
# Parametres :
#   <W>      : Path du widget concerne
#   <Edit>   : Entry editable ou non (edit ou noedit)
#   <Label>  : Label du menu.
#   <Side>   : Cote duquel sera le menu (left top right bottom).
#   <Width>  : Largeur de l'entree, Le menu a toujours la largeur du label ou largeur complete (fill).
#   <Color>  : Couleur de l'entry
#   <AColor> : Couleur du menu (doit etre une couleur permettant "light" ex: "blue")
#   <Var>    : Nom de la variable qui sera sujette au modification
#   <List>   : Liste des valeurs a inserer dans le menu
#   <Cmd>    : Liste de commande a executer apres l'assignement. ( "" pour aucune)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc EntryMenu::Create { W Edit Label Side Width Color AColor Var List Cmd } {
   global GDefs

   #---- Creation du widget

   frame $W
      menubutton $W.select -text $Label -relief raised -bd 1 -menu $W.select.values
      entry $W.entry -textvariable $Var -relief sunken -bd 1 -width $Width -background $Color \
         -disabledbackground $GDefs(ColorLight) -disabledforeground black
   pack $W.select -side $Side -fill both -expand true
   pack $W.entry -side $Side -fill both -expand true
   menu $W.select.values -relief sunken -bg Light$AColor -activebackground $AColor\
      -postcommand "$W.select configure -relief sunken -bg $AColor"

   #----- Si en mode non-editable

   if { $Edit == "noedit" } {
      $W.entry config -state disabled
   }

   #----- Insertion des valeurs dans le menu

   set noitem 0
   foreach item $List {

      if { [expr $noitem % 10] == 0 } {
         set columnbreak 1
      } else {
         set columnbreak 0
      }
      $W.select.values add command -label $item -columnbreak $columnbreak \
         -command "set $Var {$item} $Cmd ; $W.select configure -bg gray70"
      incr noitem
   }
   bind $W.select.values <FocusOut> "$W.select configure -bg $GDefs(ColorFrame)"
}

#-------------------------------------------------------------------------------
# Nom      : <Option::Create>
# Creation : Janvier 2003 - J.P. Gauthier  - CMC/CMOE
#
# But      : Permet de creer un widget associe a une liste de valeur.
#
# Parametres  :
#    <Frame>  : Identificateur du frame global.
#    <Label>  : Label descriptif
#    <Var>    : Variable(s) associee(s) { Var ou { Var1 Var2 } }
#    <Edit>   : Edition possible (0,1 ou -1=checkbutton list)
#    <Width>  : Longueur de l'entry (-1 = fill x)
#    <List>   : Liste des valeurs associees
#    <Cmd>    : Commande a executer en post-traitement
#    <args>   : 2ieme liste associee
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Option::Create { Frame Label Var Edit Width List Cmd args } {
   global GDefs
   variable Data

   set Data(Var$Frame)   $Var
   set Data(Cmd$Frame)   $Cmd
   set Data(Check$Frame) [expr $Edit==-1?1:0]
   set Data(Pre$Frame)   ""

   frame $Frame
      if { $Label!="" } {
         label $Frame.l -relief flat -anchor w -text $Label
         pack $Frame.l -side left -fill y
      }
      entry $Frame.e -relief sunken -bd 1 -width $Width -textvariable [lindex $Var 0] -bg $GDefs(ColorLight)
      
      if { [llength $List] } {
         menubutton $Frame.b -relief groove -bd 2 -bitmap @$GDefs(Dir)/share/bitmap/down.xbm -menu $Frame.b.m
         pack $Frame.b -side left -fill y
      }

   if { $Width==-1 } {
      pack $Frame.e -side left -fill both -expand true
   } else {
      pack $Frame.e -side left -fill y
   }

   if { $Edit<=0 } {
      $Frame.e configure -state disabled -disabledbackground $GDefs(ColorLight) -disabledforeground black
   }

   #----- Insertion des valeurs dans le menu
   if { [llength $List] } {
      menu $Frame.b.m -tearoff $Data(Check$Frame)
      Option::Set $Frame $List [lindex $args 0]
   }
   
   if { $Data(Check$Frame) } {
      eval Option::SetList $Frame \$$Var \$$Var 
   }
}

proc Option::Disable { Frame } {
   global GDefs

   $Frame.e configure -state disabled -disabledbackground $GDefs(ColorFrame) -disabledforeground $GDefs(ColorOff)
   catch { $Frame.b configure -state disabled }
}

proc Option::Enable { Frame { Edit True } } {
   global GDefs

   $Frame.e configure -state normal
   catch { $Frame.b configure -state normal }

   if { !$Edit } {
      $Frame.e configure -state disabled -disabledbackground $GDefs(ColorLight) -disabledforeground black
   }
}

proc Option::Clear { Frame List } {
   variable Data

   set $Data(Var$Frame) {}

   foreach item $List {
      set Data(List$Frame$item) 0  
   }
}

#-------------------------------------------------------------------------------
# Nom      : <Option::Set>
# Creation : Decembre 2003 - J.P. Gauthier - CMC/CMOE
#
# But      : Assigner les valeurs des options au widget.
#
# Parametres   :
#    <Frame>   : Identificateur du frame global.
#    <List>    : Liste des valeurs
#    <ListVal> : Liste des valeurs associees
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc Option::SetList { Frame List { ListVal {} } } {
   variable Data

   set $Data(Var$Frame) {}

   foreach item $List val $ListVal {

      if { $val!="" } {
         set Data(List$Frame$item) 1
      }
      
      if { $Data(List$Frame$item) } {
         lappend $Data(Var$Frame) $item
      }
   }
}

proc Option::Set { Frame List { ListVal {} } } {
   variable Data

   $Frame.b.m delete 0 end

   set noitem 0
   
   foreach item $List val $ListVal {

      if { [expr $noitem % 20] == 0 } {
         set columnbreak 1
      } else {
         set columnbreak 0
      }

      if { $Data(Check$Frame) } {
        set Data(List$Frame$item) 0
        $Frame.b.m add checkbutton -label $item -columnbreak $columnbreak -variable Option::Data(List$Frame$item) \
            -command "Option::SetList $Frame \"$List\"; $Data(Cmd$Frame)"
      } else {
         if { [llength $Data(Var$Frame)] > 1 } {
            if { $ListVal!="" } {
               $Frame.b.m add command -label $item -columnbreak $columnbreak \
                  -command "set [lindex $Data(Var$Frame) 0] {$item} ; set [lindex $Data(Var$Frame) 1]  $val ; $Data(Cmd$Frame)"
            } else {
               $Frame.b.m add command -label $item -columnbreak $columnbreak \
                  -command "set [lindex $Data(Var$Frame) 0] {$item}; set [lindex $Data(Var$Frame) 1] {$noitem}; $Data(Cmd$Frame)"
            }
         } else {
            $Frame.b.m add command -label $item -columnbreak $columnbreak \
               -command "set Option::Data(Pre$Frame) \$$Data(Var$Frame); set $Data(Var$Frame) {$item}; $Data(Cmd$Frame)"
         }
      }
      incr noitem
   }
}

#-------------------------------------------------------------------------------
# Nom      : <EntryVar::Create>
# Creation : Janvier 1998 - J.P. Gauthier  - CMC/CMOE
#
# But      : Permet de creer un widget entry n'acceptant qu'un seul type de valeur.
#
# Parametres  :
#    <W>      : Path du widget entry a creer.
#    <Var>    : Nom de la variable a trace
#    <Type>   : Type de variable accepte (string,coordinate,integer,float,hexadecimal,octal,binary)
#    <Format> : Format de chaine
#    <Command>: Commande secondaire a etre executee. (entre "") (aucune -> "")
#    <args>   : Parametres du widget entry (couleur, largeur ...)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc EntryVar::Create { W Var Type Format Command args } {

   #--- Creation du widget entry

   eval { entry $W -textvariable $Var } $args

   #--- Mise en place de la trace sur la variable et l'entry

   bind $W <KeyRelease> "EntryVar::Trace $Type $Var $Format; $Command"
   trace variable $Var w "EntryVar::Trace $Type $Var $Format"
}

#-------------------------------------------------------------------------------
# Nom      : <EntryVar::Trace>
# Creation : Janvier 1998 - J.P. Gauthier  - CMC/CMOE
#
# But      : Effectue la verification du format et le reformatage.
#
# Parametres  :
#    <Type>   : Type de variable accepte (string,coordinate,integer,float,hexadecimal,octal,binary)
#    <Var>    : Nom de la variable a trace
#    <Format> : Format de chaine
#    <args>   : Parametres suplementaire du trace
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc EntryVar::Trace { Type Var Format args } {

   upvar #0 $Var evar
   set evar_tmp ""

   switch $Type {

      "string"  {
         set evar_tmp [string range $evar 0 $Format]
      }

      "coordinate"  {
         set lst [split [string map { "°" " " \' " " \" " "} $evar]]
         if { [string is double $evar] || ([string is integer [lindex $lst 0]] && [string is integer [lindex $lst 1]] && ([llength $lst]<3 || [string is double [lindex $lst 2]])) } {
            set evar_tmp $evar
         } else {
            bell
         }
         set evar_tmp $evar
      }

      "integer" {
         for { set i 0 } { $i < [string length $evar] } {incr i} {
            set num [string range $evar $i $i]
            if { [EntryVar::IsDecimal $num] || [EntryVar::IsSign $num $i] } {
               set evar_tmp $evar_tmp$num
            } else {
               bell
            }
         }
      }

      "float" {
         set nbpt 0
         for { set i 0 } { $i < [string length $evar] } {incr i} {
            set num [string range $evar $i $i]
            if { [EntryVar::IsDecimal $num ] || [EntryVar::IsSign $num $i] } {
               set evar_tmp $evar_tmp$num
            } elseif { [EntryVar::IsDot $num] } {
               incr nbpt 1
               if { $nbpt == 1 } {
                  set evar_tmp $evar_tmp$num
               }
            } else {
               bell
            }
         }
      }

      "binary" {
         for { set i 0 } { $i < [string length $evar] } {incr i} {
            set num [string range $evar $i $i]
            if { [EntryVar::IsBinary $num] } {
               set evar_tmp $evar_tmp$num
            } else {
               bell
            }
         }
      }

      "hexadecimal" {
         set $evar [string toupper $evar]
         for { set i 0 } { $i < [string length $evar] } {incr i} {
            set num [string range $evar $i $i]
            if { [EntryVar::IsHexadecimal $num] } {
               set evar_tmp $evar_tmp$num
            } else {
               bell
            }
         }
      }

      "octal" {
         set $evar [string toupper $evar]
         for { set i 0 } { $i < [string length $evar] } {incr i} {
            set num [string range $evar $i $i]
            if { [EntryVar::IsOctal $num] } {
               set evar_tmp $evar_tmp$num
            } else {
               bell
            }
         }
      }
   }
   catch { set evar $evar_tmp }
}

#-------------------------------------------------------------------------------
# Nom      : <EntryVar::IsBinary>
# Creation : Janvier 1998 - J.P. Gauthier  - CMC/CMOE
#
# But      : Effectue la verification sur un chiffre binaire.
#
# Parametres :
#    <Val>   : Valeur a verifier
#
# retour     :
#    <retour>: Oui (1) ou Non (0)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc EntryVar::IsBinary { Val } {

   if { $Val == 0 || $Val == 1 } {
      return 1
   } else {
      return 0
   }
}

#-------------------------------------------------------------------------------
# Nom      : <EntryVar::IsDecimal>
# Creation : Janvier 1998 - J.P. Gauthier  - CMC/CMOE
#
# But      : Effectue la verification sur un chiffre decimal.
#
# Parametres :
#    <Val>   : Valeur a verifier
#
# retour     :
#    <retour>: Oui (1) ou Non (0)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc EntryVar::IsDecimal { Val } {

   if { $Val == 0 || $Val == 1 || $Val == 2 || $Val == 3 || $Val == 4 || \
        $Val == 5 || $Val == 6 || $Val == 7 || $Val == 8 || $Val == 9 } {
      return 1
   } else {
      return 0
   }
}

#-------------------------------------------------------------------------------
# Nom      : <EntryVar::IsDot>
# Creation : Janvier 1998 - J.P. Gauthier  - CMC/CMOE
#
# But      : Effectue la verification du point.
#
# Parametres :
#    <Val>   : Valeur a verifier
#
# retour     :
#    <retour>: Oui (1) ou Non (0)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc EntryVar::IsDot { Val } {

   if { $Val == "." || $Val == " " } {
      return 1
   } else {
      return 0
   }
}

#-------------------------------------------------------------------------------
# Nom      : <EntryVar::IsHexadecimal>
# Creation : Janvier 1998 - J.P. Gauthier  - CMC/CMOE
#
# But      : Effectue la verification sur un chiffre hexadecimal.
#
# Parametres :
#    <Val>   : Valeur a verifier
#
# retour     :
#    <retour>: Oui (1) ou Non (0)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc EntryVar::IsHexadecimal { Val } {

   if { $Val == 0 || $Val == 1 || $Val == 2 || $Val == 3 || $Val == 4 || \
        $Val == 5 || $Val == 6 || $Val == 7 || $Val == 8 || $Val == 9  \
        $Val == "A" || $Val == "B" || $Val == "C" || $Val == "D" || $Val == "E" \
        $Val == "F" } {
      return 1
   } else {
      return 0
   }
}

#-------------------------------------------------------------------------------
# Nom      : <EntryVar::IsSign>
# Creation : Janvier 1998 - J.P. Gauthier  - CMC/CMOE
#
# But      : Effectue la verification du signe.
#
# Parametres :
#    <Val>   : Valeur a verifier
#    <Pos>   : Position
#
# retour     :
#    <retour>: Oui (1) ou Non (0)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc EntryVar::IsSign { Val Pos } {

   if { ($Val == "-" || $Val == "+") && $Pos == 0 } {
      return 1
   } else {
      return 0
   }
}

#-------------------------------------------------------------------------------
# Nom      : <EntryVar::IsOctal>
# Creation : Janvier 1998 - J.P. Gauthier  - CMC/CMOE
#
# But      : Effectue la verification sur un chiffre octal
#
# Parametres :
#    <Val>   : Valeur a verifier
#
# retour     :
#    <retour>: Oui (1) ou Non (0)
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc EntryVar::IsOctal { Val } {

   if { $Val == 0 || $Val == 1 || $Val == 2 || $Val == 3 || $Val == 4 || \
        $Val == 5 || $Val == 6 || $Val == 7 || $Val == 8 } {
      return 1
   } else {
      return 0
   }
}

#-------------------------------------------------------------------------------
# Nom      : <IcoMenu::Create>
# Creation : Novembre 2000 - J.P. Gauthier  - CMC/CMOE
#
# But      : Creer un menu de selection avec des bitmaps
#
# Parametres  :
#    <Frame>  : Id du menubutton
#    <Path>   : Path des images
#    <Icons>  : Liste des noms d'images (bitmap)
#    <Values> : Liste des valuers associees aux images
#    <Var>    : Variable d'assignation
#    <Command>: Command a executer
#    <Def>    : Index de l'icone initiale a executer
#    <args>   : parametres dur menubutton
#
# retour     :
#
# Remarques :
#
#-------------------------------------------------------------------------------

proc IcoMenu::Create { Frame Path Icons Values Var Command Def args } {
   variable Data

   set Data(Var$Frame) $Var
   eval menubutton $Frame -menu $Frame.menu $args

   if { [lindex $Icons $Def]!="" } {
      eval $Frame configure -bitmap @\$Path/[lindex $Icons $Def]
   }
   menu $Frame.menu

   foreach ico $Icons value $Values {
      set Data($value$Frame) @$Path/$ico
      if { [file isfile $Path/$ico] } {
         $Frame.menu add radiobutton -bitmap @$Path/$ico -value $value -indicatoron false \
            -command "set $Var \"$value\" ; $Frame configure -bitmap \"@$Path/$ico\" ; $Command"
      } else {
         $Frame.menu add radiobutton -value $value -indicatoron false \
            -command "set $Var \"$value\" ; $Frame configure -bitmap \"@$Path/$ico\" ; $Command"
      }
   }
}

proc IcoMenu::CreateDef { Frame Path Icons Values Var Command Def args } {
   global GDefs
   variable Data

   set Data(Var$Frame) $Var
   eval menubutton $Frame -menu $Frame.menu $args

   menu $Frame.menu

   set no 0
   foreach ico $Icons value $Values {
      eval set Data($value$Frame) @$Path/$ico
      eval set Data($no$Frame) @$Path/$ico
      if { [file isfile $Path/$ico] } {
         $Frame.menu add radiobutton -bitmap @$Path/$ico -value $value -indicatoron false \
            -command "set $Var \"$value\" ; $Frame configure -bitmap \"@$Path/$ico\" ; $Command"
      } else {
         $Frame.menu add radiobutton -value $value -indicatoron false \
            -command "set $Var \"$value\" ; $Frame configure -bitmap \"@$Path/$ico\" ; $Command"
      }
      incr no
   }

   catch { eval $Frame configure -bitmap $Data($Def$Frame) }
}

proc IcoMenu::Set { Frame Value } {
   variable Data

   catch { $Frame configure -bitmap $Data($Value$Frame) }
   eval set $Data(Var$Frame) \"$Value\"
}

#================================ DOCUMENTER ================================
# Nom      : <UnitOption::ButtonEventManager>
# Creation : Novembre 2022 - C. Mitron-Brazeau - CMC/CMOE
#
# But      : Widget's unit button event handler
#
# Parametres :
#  <path> : Widget path
#
# Retour :
#
# Remarques :
#
#================================ DOCUMENTER ================================
proc UnitOption::ButtonEventManager { path } {
   variable Data
   variable Textvar

   #----- Shorthands
   set units $Data(${path}Units)
   set currentUnitIdx $Data(${path}CurrentUnitIdx)

   #----- Update Textvars
   set nextUnitIdx [expr ($currentUnitIdx+1)%[llength $units]]
   set Textvar($path.btn) [lindex $units $nextUnitIdx]

   #----- Update currentUnitIdx
   set Data(${path}CurrentUnitIdx) $nextUnitIdx

   UpdateEntry $path
   UpdateMenuItems $path
}

#================================ DOCUMENTER ================================
# Nom      : <UnitOption::Convert>
# Creation : Novembre 2022 - C. Mitron-Brazeau - CMC/CMOE
#
# But      : Convert a value from a unit to another unit
#
# Parametres :
#  <path>        : Widget path
#  <value>       : Value to convert
#  <fromUnitIdx> : Input value unit index
#  <toUnitIdx>   : Output value unit index
#
# Retour :
#  <value> : Converted value
#
# Remarques :
#
#================================ DOCUMENTER ================================
proc UnitOption::Convert { path value fromUnitIdx toUnitIdx } {
   variable Data
   variable Textvar

   #----- Shorthands
   set correlations $Data(${path}Correlations)

   set correlationFrom [lindex $correlations $fromUnitIdx]
   set correlationTo [lindex $correlations $toUnitIdx]

   if { [catch {
      #----- Different operation orders can give differing results due to precision.
      #-----   This way, we maximize the precision of the outcome
      set value1 [FormatNumber [expr 1.0*$correlationTo/$correlationFrom*$value]]
      set value2 [FormatNumber [expr 1.0*$correlationTo*$value/$correlationFrom]]
      set value $value1
      if { [string length $value1] > [string length $value2] } {
         set value $value2
      }
   } fid] } {
      #----- Not doing anything. The procedure will return the faulty input value.
   }

   return $value
}

#================================ DOCUMENTER ================================
# Nom      : <UnitOption::Create>
# Creation : Novembre 2022 - C. Mitron-Brazeau - CMC/CMOE
#
# But      : Create the widget
#
# Parametres :
#  <path>           : Widget path
#  <text>           : Text of the widget's label
#  <units>          : Units of the widget - ex: { h min s }
#  <correlations>   : Correlations between units - ex: { 1 60 3600 }
#  <textvar>        : Textvar of the widget
#  <textvarUnitIdx> : Textvar's unit index - ex: 0 = h, 1 = min, 2 = s
#  <values>         : Predefined values of the dropdown list
#  <valuesUnitIdx>  : Values' unit index - ex: 0 = h, 1 = min, 2 = s
#
# Retour :
#  <frame> : Widget's main frame path - same as path
#
# Remarques :
#
#================================ DOCUMENTER ================================
proc UnitOption::Create { path text units correlations textvar textvarUnitIdx {values {}} {valuesUnitIdx 0} } {
   global GDefs
   variable Data
   variable Textvar

   #----- Find max unit length
   set unitMaxLength 1
   foreach unitLength [lmap u $units {string length $u}] {
      set unitMaxLength [expr max($unitMaxLength,$unitLength)]
   }

   #----- Variable Data
   set Data(${path}text) $text
   #-----   Add (...) to units
   set units [lmap u $units {format {(%s)} $u}]
   set Data(${path}Units) $units
   set Data(${path}Correlations) $correlations
   set Data(${path}Textvar) $textvar
   set Data(${path}TextvarUnitIdx) $textvarUnitIdx
   set Data(${path}Values) $values
   set Data(${path}ValuesUnitIdx) $valuesUnitIdx
   set currentUnitIdx 0
   set Data(${path}CurrentUnitIdx) $currentUnitIdx

   #----- Variable Textvar
   set Textvar($path.btn) [lindex $units $currentUnitIdx]
   #-----   [set $textvar] is a double dereference that works, such as ${$textvar}
   set Textvar($path.ent) [UpdateEntry $path]

   #----- UI
   set frm [frame $path]

      label $path.lbl -text $text
      grid $path.lbl -row 0 -column 0 -sticky nsw

      button $path.btn -textvariable UnitOption::Textvar($path.btn) -command [list UnitOption::ButtonEventManager $path] -width $unitMaxLength -height 1 -bd 0
      grid $path.btn -row 0 -column 1 -sticky nse -padx { 0 1 }

      if { [llength $values] } {
         menubutton $path.mbtn -menu $path.mbtn.menu -relief groove -bd 2 -bitmap @$GDefs(Dir)/share/bitmap/down.xbm
            menu $path.mbtn.menu -tearoff 0
            UpdateMenuItems $path
         grid $path.mbtn -row 0 -column 2 -sticky nse
      }

      entry $path.ent -textvariable UnitOption::Textvar($path.ent) -relief sunken -bd 1 -bg $GDefs(ColorLight)
      bind $path.ent <KeyRelease> [list UnitOption::EntryKeyReleaseEventManager $path]
      grid $path.ent -row 0 -column 3 -sticky nswe

      grid columnconfigure $path 3 -weight 1

   #----- Trace
   SetTextvarTrace $path

   return $frm
}

proc UnitOption::EntryKeyReleaseEventManager { path } {
   variable Textvar

   UnsetTextvarTrace $path
   SetTextvar $path $Textvar($path.ent)
   SetTextvarTrace $path
}

#================================ DOCUMENTER ================================
# Nom      : <UnitOption::FormatNumber>
# Creation : Novembre 2022 - C. Mitron-Brazeau - CMC/CMOE
#
# But      : Format a value as a number
#
# Parametres :
#  <value> : Value to format
#
# Retour :
#  <value> : Formatted value
#
# Remarques :
#
#================================ DOCUMENTER ================================
proc UnitOption::FormatNumber { value } {
   #----- Remove trailing zeros
   set value [expr $value*1]

   #----- Remove decimals if 0
   if { [expr $value - floor($value)] == 0 } {
      set value [expr int($value)]
   }

   return $value
}

#================================ DOCUMENTER ================================
# Nom      : <UnitOption::FormatNumberLabel>
# Creation : Novembre 2022 - C. Mitron-Brazeau - CMC/CMOE
#
# But      : Format a value as a number for display as a label
#
# Parametres :
#  <value> : Value to format
#
# Retour :
#  <value> : Formatted value
#
# Remarques :
#
#================================ DOCUMENTER ================================
proc UnitOption::FormatNumberLabel { value } {
   #----- Force 4 decimals
   set value [format "%.4f" $value]

   return [FormatNumber $value]
}

#================================ DOCUMENTER ================================
# Nom      : <UnitOption::SetTextvar>
# Creation : Novembre 2022 - C. Mitron-Brazeau - CMC/CMOE
#
# But      : Set the widget's textvar value
#
# Parametres :
#  <path>  : Widget path
#  <value> : Value to set textvar
#
# Retour :
#
# Remarques :
#
#================================ DOCUMENTER ================================
proc UnitOption::SetTextvar { path value } {
   variable Data

   #----- Shorthands
   set textvar $Data(${path}Textvar)
   set textvarUnitIdx $Data(${path}TextvarUnitIdx)
   set currentUnitIdx $Data(${path}CurrentUnitIdx)

   set $textvar [Convert $path $value $currentUnitIdx $textvarUnitIdx]
   puts "SetTextvar : $textvar = [set $textvar]"
}

proc UnitOption::SetTextvarTrace { path } {
   variable Data

   #----- Shorthands
   set textvar $Data(${path}Textvar)

   trace add variable $textvar write [list UnitOption::TextvarTraceEventManager $path]
}

#================================ DOCUMENTER ================================
# Nom      : <UnitOption::TextvarTraceEventManager>
# Creation : Novembre 2022 - C. Mitron-Brazeau - CMC/CMOE
#
# But      : Widget's textvar trace event manager
#
# Parametres :
#  <path> : Widget path
#  <args> : Arguments passed to trace's command
#
# Retour :
#
# Remarques :
#
#================================ DOCUMENTER ================================
proc UnitOption::TextvarTraceEventManager { path args } {
   UpdateEntry $path
}

proc UnitOption::UnsetTextvarTrace { path } {
   variable Data

   #----- Shorthands
   set textvar $Data(${path}Textvar)

   trace remove variable $textvar write [list UnitOption::TextvarTraceEventManager $path]
}

#================================ DOCUMENTER ================================
# Nom      : <UnitOption::UpdateEntry>
# Creation : Novembre 2022 - C. Mitron-Brazeau - CMC/CMOE
#
# But      : Update widget's entry value
#
# Parametres :
#  <path> : Widget path
#
# Retour :
#
# Remarques :
#
#================================ DOCUMENTER ================================
proc UnitOption::UpdateEntry { path } {
   variable Data
   variable Textvar

   #----- Shorthands
   set textvar $Data(${path}Textvar)
   set textvarUnitIdx $Data(${path}TextvarUnitIdx)
   set currentUnitIdx $Data(${path}CurrentUnitIdx)

   set Textvar($path.ent) [Convert $path [set $textvar] $textvarUnitIdx $currentUnitIdx]
}

#================================ DOCUMENTER ================================
# Nom      : <UnitOption::UpdateMenuItems>
# Creation : Novembre 2022 - C. Mitron-Brazeau - CMC/CMOE
#
# But      : Update widget's (dropdown) menu items
#
# Parametres :
#  <path> : Widget path
#
# Retour :
#
# Remarques :
#
#================================ DOCUMENTER ================================
proc UnitOption::UpdateMenuItems { path } {
   variable Data
   variable Textvar

   #----- Early stopping
   if { [winfo exists $path] } {
      return
   }

   #----- Shorthands
   set values $Data(${path}Values)
   set valuesUnitIdx $Data(${path}ValuesUnitIdx)
   set currentUnitIdx $Data(${path}CurrentUnitIdx)

   #----- Delete items
   $path.mbtn.menu delete 0 end

   #----- Add items
   set adjustedValues [lmap v $values {Convert $path $v $valuesUnitIdx $currentUnitIdx}]
   foreach value $adjustedValues {
      $path.mbtn.menu add command -label [FormatNumberLabel $value] -command [list UnitOption::SetTextvar $path $value]
   }
}
