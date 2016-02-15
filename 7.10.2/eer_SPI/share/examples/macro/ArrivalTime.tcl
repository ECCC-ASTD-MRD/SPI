namespace eval Macro::ArrivalTime { } {
   variable Param
   variable Data
   variable Error

   set Data(Something)  "Some data value"

   set Error(Field)  { "Aucun champs valide trouvé\nvérifié le viewport actif"
                       "Could not find any valid field\nCheck the active viewport" }

   set Param(Info)      { "Calcul du temps d'arrivée d'un panache" "Calculates time of  arrival of a plume" }
}

proc Macro::ArrivalTime::Execute { } {
   variable Data
   variable Error

   set field [lindex [Viewport::Assigned $Page::Data(Frame) $Viewport::Data(VP)] 0]

   if { ![fstdfield is $field] } {
      Macro::Error $Error(Field)
      return
   }

   set tags [fstdfield stats $field -tag]
   set box  [lindex $tags 2]

   #----- Make a copy of the fields

   set t0     [fstdstamp toseconds [fstdfield define $field -DATEV]]
   set ip1    [fstdfield define $field -IP1]
   set ip3    [fstdfield define $field -IP3]
   set etiket [fstdfield define $field -ETIKET]
   set var    [fstdfield define $field -NOMVAR]

   fstdfield copy Macro::ARRIVALTIME $field
   fstdfield stats Macro::ARRIVALTIME -nodata 0
   fstdfield clear Macro::ARRIVALTIME
   fstdfield define Macro::ARRIVALTIME -NOMVAR TIME

   FSTD::Register Macro::ARRIVALTIME

   Macro::Doing "Processing fields"
   foreach field [FieldBox::GetContent $box] {

      set fid     [lindex $field 0]
      set idx     [lindex $field 1]
      set tvar    [lindex $field 2]
      set tip1    [lindex $field 4]
      set tip3    [lindex $field 6]
      set tetiket [string trim [lindex $field 7]]

      if { $var==$tvar && $ip1==$tip1 && $ip3==$tip3 && $etiket==$tetiket } {
         fstdfield read Macro::TMP $fid $idx

         set t [expr double([fstdstamp toseconds [fstdfield define Macro::TMP -DATEV]]-$t0)]

         vexpr Macro::ARRIVALTIME ifelse(Macro::ARRIVALTIME==0.0 && Macro::TMP>0.0,$t,Macro::ARRIVALTIME)
      }
   }
   set dt [vexpr dt smin(Macro::ARRIVALTIME)]
   vexpr Macro::ARRIVALTIME ifelse(Macro::ARRIVALTIME!=0.0,Macro::ARRIVALTIME-$dt,Macro::ARRIVALTIME)
   vexpr (Float32)Macro::ARRIVALTIME ifelse(Macro::ARRIVALTIME==0.0,-1,Macro::ARRIVALTIME)

   fstdfield configure Macro::ARRIVALTIME -rendertexture 1 -rendercontour 0 -mapall False -value INTEGER 0 \
      -factor [expr 1.0/3600.0] -renderlabel 0 -font XFont12 -intervals { 0 1 6 12 24 36 48 54 60 72 } \
       -color black -interpdegree NEAREST

#   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP)
   Viewport::Assign $Page::Data(Frame) $Viewport::Data(VP) Macro::ARRIVALTIME
   Macro::Doing ""
}

proc Macro::ArrivalTime::Clean { } {
   variable Data

   Viewport::UnAssign $Page::Data(Frame) $Viewport::Data(VP) Macro::ARRIVALTIME
   fstdfield free Macro::ARRIVALTIME
}










