#===============================================================================
# Environnement Canada
# Centre Meteorologique Canadian
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet   : Outils de SPI
# Fichier  : Mapper_Geo.tcl
# Creation : Octobre 2009 - J.P.Gauthier - CMC/CMOE
#
# Description:
#    Fontions de geo-cododage et geo-codage inverse utilisant divers services web (Geocoder,Google).
#
# Remarques :
#
#===============================================================================

namespace eval Mapper::Geo { } {
   global GDefs
   variable Accuracy
   variable Data
   variable Param
   variable Error
   variable Lbl
   variable Msg
   variable Bubble

   set Param(API)       Geocoder
   set Param(Locate)    True
   set Param(Zoom)      128
   set Param(Key)       ""

   set Data(Address)  ""
   set Data(Street)   ""
   set Data(City)     ""
   set Data(Province) ""
   set Data(Country)  ""
   set Data(Postal)   ""
   set Data(Lat)      0.0
   set Data(Lon)      0.0

   set Lbl(Locate)      { "Centrer sur la localisation" "Zoom to localisation" }

   set Msg(Request)     { "Requète de geo-localisation en cours ..." "Sending geocoding request ..." }

   set Error(Request)   { "Problème dans la requète de geo-localisation." "Problem requesting geocoding information." }
   set Error(NoneFound) { "Aucune information disponible sur cette localisation." "No information available for this localisation." }

   set Bubble(Code)     { "Recherche la localisation d'un endroit ou d'une adresse" "Look for location of a place or specific adress" }
   set Bubble(ICode)    { "Recherce de l'endroit ou l'adresse a une localisation" "Look for place or specific adress at a location" }
   set Bubble(API)      { "Sélection du service à utiliser" "Select service to use" }

   array set Accuracy {
      0  "Unknown accuracy"
      1  "Country level accuracy"
      2  "Region (state, province, prefecture, etc.) level accuracy"
      3  "Sub-region (county, municipality, etc.) level accuracy"
      4  "Town (city, village) level accuracy"
      5  "Post code (zip code) level accuracy"
      6  "Street level accuracy"
      7  "Intersection level accuracy"
      8  "Address level accuracy"
      9  "Premise (building name, property name, shopping center, etc.) level accuracy"
   }

   image create photo GEOGOOGLE -file $GDefs(Dir)/Resources/Image/Icon/Google.gif
   image create photo GEOCODER  -file $GDefs(Dir)/Resources/Image/Icon/Geocoder.gif

#   set str [http::config -useragent]
#   http::config -useragent "EC/CMC/CMOE SPI $GDefs(Version) (through $str)"
}

#----------------------------------------------------------------------------
# Nom      : <Mapper::Geo::Widget>
# Creation : Octobre 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Interface de geo-localisation.
#
# Parametres  :
#   <Frame>   : Widget parent
#
# Retour:
#   <Frame>  : Widget enfant
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Mapper::Geo::Widget { Frame } {
   global GDefs
   variable Lbl
   variable Bubble

   frame $Frame.geo -relief raised -bd 1
      checkbutton $Frame.geo.icode -image GEOCODER -relief sunken -bd 1 -overrelief raised -offrelief flat -indicatoron False -selectcolor $GDefs(ColorLight) \
         -variable Page::Data(ToolMode) -onvalue Mapper::Geo -offvalue SPI -command { SPI::ToolMode $Page::Data(ToolMode) Data True } -width 28 -anchor w
      menubutton $Frame.geo.icode.api -image OPTIONS -relief flat -bd 0 -menu $Frame.geo.icode.api.menu
      place $Frame.geo.icode.api -relx 1.0 -rely 0.0 -anchor ne -relheight 1.0
      menu $Frame.geo.icode.api.menu
      $Frame.geo.icode.api.menu add radiobutton -variable Mapper::Geo::Param(API) -image GEOCODER -label Geocoder.ca -compound left \
         -value Geocoder -command "$Frame.geo.icode configure -image GEOCODER"
      $Frame.geo.icode.api.menu add radiobutton -variable Mapper::Geo::Param(API) -image GEOGOOGLE -label Google -compound left\
         -value Google -command "$Frame.geo.icode configure -image GEOGOOGLE"
      $Frame.geo.icode.api.menu add separator
      $Frame.geo.icode.api.menu add checkbutton -variable Mapper::Geo::Param(Locate) -label [lindex $Lbl(Locate) $GDefs(Lang)] \
         -onvalue True -offvalue False

      entry $Frame.geo.address -textvariable Mapper::Geo::Data(Address) -relief sunken -bd 1 -bg $GDefs(ColorLight)
      pack $Frame.geo.icode -side left -padx 2
      pack $Frame.geo.address -side left -fill both -expand True
   pack $Frame.geo -side left -fill x -expand true

   bind $Frame.geo.address <Return> { Mapper::Geo::Code $Mapper::Geo::Data(Address) $Mapper::Geo::Param(API) }
   bind $Frame.geo.address <Any-KeyRelease> { if { $Mapper::Geo::Data(Address)=="" } { SPI::IcoDel GEOCODE } }

   Bubble::Create $Frame.geo.icode     $Bubble(ICode)
   Bubble::Create $Frame.geo.icode.api $Bubble(API)
   Bubble::Create $Frame.geo.address   $Bubble(Code)

   return $Frame.geo
}

#----------------------------------------------------------------------------
# Nom      : <Mapper::Geo::Code>
# Creation : Octobre 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Geo-coder une localisation. Trouver les coordonnees d'un endroit
#            ou addresse.
#
# Parametres  :
#   <Request> : ndroit ou adresse
#   <API>     : Service a utiliser (Geocoder ou Google)
#
# Retour:
#   <Coord>   : Coordonnees latlon
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Mapper::Geo::ToUnicode { String } {
   return [string map "À %C0 Á %C1 Â %C2 Ã %C3 Ä %C4 Å %C5 Æ %C6 Ç %C7 È %C8 É %C9 Ê %CA Ë %CB Ì %CC Í %CD Î %CE Ï %CF Ð %D0 Ñ %D1 \
     Ò %D2 Ó %D3 Ô %D4 Õ %D5 Ö %D6 × %D7 Ø %D8 Ù %D9 Ú %DA Û %DB Ü %DC Ý %DD Þ %DE ß %DF à %E0 á %E1 â %E2 ã %E3 ä %E4 å %E5 æ %E6 \
     ç %E7 è %E8 é %E9 ê %EA ë %EB ì %EC í %ED î %EE ï %EF ð %F0 ñ %F1 ò %F2 ó %F3 ô %F4 õ %F5 ö %F6 ÷ %F7 ø %F8 ù %F9 ú %FA û %FB \
     ü %FC ý %FD þ %FE ÿ %FF" $String]
}

proc Mapper::Geo::FromUnicode { String } {
   return [string map "%C0 À %C1 Á %C2 Â %C3 Ã %C4 Ä %C5 Å %C6 Æ %C7 Ç %C8 È %C9 É %CA Ê %CB Ë %CC Ì %CD Í %CE Î %CF Ï %D0 Ð %D1 Ñ \
     %D2 Ò %D3 Ó %D4 Ô %D5 Õ %D6 Ö %D7 × %D8 Ø %D9 Ù %DA Ú %DB Û %DC Ü %DD Ý %DE Þ %DF ß %E0 à %E1 á %E2 â %E3 ã %E4 ä %E5 å %E6 æ \
     %E7 ç %E8 è %E9 é %EA ê %EB ë %EC ì %ED í %EE î %EF ï %F0 ð %F1 ñ %F2 ò %F3 ó %F4 ô %F5 õ %F6 ö %F7 ÷ %F8 ø %F9 ù %FA ú %FB û \
     %FC ü %FD ý %FE þ %FF ÿ" $String]
}

proc Mapper::Geo::Code { Request { API Geocoder } } {
   global GDefs
   variable Msg
   variable Error
   variable Accuracy
   variable Param

   #----- Cleanup location info
   set Data(Lat) 0.0
   set Data(Lon) 0.0

   Dialog::Wait . $Msg(Request)

   switch $API {
      "Google" {
         #----- Send request through Google
         set req [http::geturl "http://maps.googleapis.com/maps/api/geocode/xml?address=[join ${Request} +]&sensor=false"]

         if { [catch { set doc [dom parse [http::data $req]] } msg ] } {
            Dialog::ErrorListing . $Error(Request) "$msg\n[http::data $req]"
            return
         }
         #----- Extract info from XML
         if { [set root [$doc documentElement]]!="" } {

            set Data(Address) [[[$root getElementsByTagName formatted_address] firstChild] nodeValue]

            set node [lindex  [$root getElementsByTagName location] 0]
            set Data(Lat)    [[[$node getElementsByTagName lat] firstChild] nodeValue]
            set Data(Lon)    [[[$node getElementsByTagName lng] firstChild] nodeValue]
         }
         $doc delete
         http::cleanup $req
      }
      "Geocoder" {
         #----- Send request through Geocoder.ca
         set req [http::geturl "http://geocoder.ca/?locate=[join ${Request} %20]&geoit=XML"]
         if { [catch { set doc [dom parse [http::data $req]] } msg ] } {
            Dialog::ErrorListing . $Msg(Request) "$msg\n[http::data $req]"
            return
         }
         #----- Extract info from XML
         if { [set root [$doc documentElement]]!="" } {
            catch {
                set Data(Lat)     [[[$root getElementsByTagName latt] firstChild] nodeValue]
                set Data(Lon)     [[[$root getElementsByTagName longt] firstChild] nodeValue]
            }
            set Data(Address) ${Request}
         }
         $doc delete
         http::cleanup $req
      }
   }
   Dialog::WaitDestroy

   if { $Data(Lat)==0.0 && $Data(Lon)==0.0 } {
      Dialog::Error . $Error(NoneFound)
   } elseif { $Param(Locate) } {
      SPI::IcoAdd $Page::Data(Frame) GEOCODE "" [list [list $Data(Address) $Data(Lat) $Data(Lon) 0 ICO_THERE]]
      SPI::Locate $Data(Lat) $Data(Lon) $Param(Zoom)
   }
   return [list $Data(Lat) $Data(Lon)]
}

#----------------------------------------------------------------------------
# Nom      : <Mapper::Geo::InverseCode>
# Creation : Octobre 2009 - J.P. Gauthier - CMC/CMOE
#
# But      : Geo-codage inverse pour une localisation. Trouver l'endroit ou
#            l'address d'une coordonnee.
#
# Parametres  :
#   <Lat>     : Latitude
#   <Lon>     : Longitude
#   <API>     : Service a utiliser (Geocoder ou Google)
#
# Retour:
#   <Place>   : Endroit ou addresse
#
# Remarques :
#
#----------------------------------------------------------------------------

proc Mapper::Geo::InverseCode { Lat Lon { API Geocoder } } {
   global GDefs
   variable Msg
   variable Error
   variable Data
   variable Param

   #----- Cleanup location info
   set Data(Address)  ""
   set Data(Street)   ""
   set Data(City)     ""
   set Data(Province) ""
   set Data(Country)  ""
   set Data(Postal)   ""

   Dialog::Wait . $Msg(Request)

   switch $API {
      "Google" {
         #----- Send request through Google
         set req [http::geturl "http://maps.googleapis.com/maps/api/geocode/xml?latlng=$Lat,$Lon&sensor=false"]
         if { [catch { set doc [dom parse [http::data $req]] } msg ] } {
            Dialog::ErrorListing . $Msg(Request) "$msg\n[http::data $req]"
            return
         }

         #----- Extract info from XML
         if { [set root [$doc documentElement]]!="" } {
            set Data(Address) [[[lindex [$root getElementsByTagName formatted_address] 0] firstChild] nodeValue]
         }
         $doc delete
         http::cleanup $req
      }
      "Geocoder" {
         #----- Send request through Geocoder.ca
         set req [http::geturl "http://geocoder.ca/?latt=$Lat&longt=$Lon&geoit=XML&reverse=1"]
         if { [catch { set doc [dom parse [http::data $req]] } msg ] } {
            Dialog::ErrorListing . $Msg(Request) "$msg\n[http::data $req]"
            return
         }

         #----- Extract info from XML
         if { [set root [$doc documentElement]]!="" } {
            catch { set Data(Street)  "[[[$root getElementsByTagName stnumber] firstChild] nodeValue] [[[$root getElementsByTagName staddress] firstChild] nodeValue]" }
            catch { set Data(City)     [[[$root getElementsByTagName city] firstChild] nodeValue] }
            catch { set Data(Province) [[[$root getElementsByTagName prov] firstChild] nodeValue] }
            catch { set Data(Country)  [[[$root getElementsByTagName AdministrativeAreaName] firstChild] nodeValue] }
            catch { set Data(Postal)   [[[$root getElementsByTagName postal] firstChild] nodeValue] }

            set Data(Address) [string trim "$Data(Street) $Data(City) $Data(Province) $Data(Postal)"]
         }

         $doc delete
         http::cleanup $req
      }
   }
   Dialog::WaitDestroy

   if { $Data(Address)=="" } {
      Dialog::Error . $Error(NoneFound)
   } elseif { $Param(Locate) } {
      set Data(Lat) $Lat
      set Data(Lon) $Lon
      SPI::IcoAdd $Page::Data(Frame) GEOCODE "" [list [list $Data(Address) $Data(Lat) $Data(Lon) 0 ICO_THERE]]
   }
   return $Data(Address)
}

proc Mapper::Geo::DrawInit  { Frame VP } {
}

proc Mapper::Geo::Draw      { Frame VP } {
}

proc Mapper::Geo::DrawDone { Frame VP } {
   variable Data
   variable Param

   Mapper::Geo::InverseCode $Viewport::Map(LatCursor) $Viewport::Map(LonCursor) $Param(API)
}

proc Mapper::Geo::MoveInit { Frame VP } {
}

proc Mapper::Geo::Move { Frame VP } {
}

proc Mapper::Geo::MoveDone { Frame VP } {
}

