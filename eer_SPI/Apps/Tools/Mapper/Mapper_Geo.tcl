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

   set Param(API)      Geocoder
   set Param(Locate)   True

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

   set str [http::config -useragent]
   http::config -useragent "EC/CMC/CMOE SPI $GDefs(Version) (through $str)"
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
      $Frame.geo.icode.api.menu add radiobutton -indicatoron False -variable Mapper::Geo::Param(API) -image GEOCODER -label Geocoder -compound left \
         -value Geocoder -command "$Frame.geo.icode configure -image GEOCODER"
      $Frame.geo.icode.api.menu add radiobutton -indicatoron False -variable Mapper::Geo::Param(API) -image GEOGOOGLE -label Google -compound left\
         -value Google -command "$Frame.geo.icode configure -image GEOGOOGLE"
      $Frame.geo.icode.api.menu add separator
      $Frame.geo.icode.api.menu add checkbutton -indicatoron True -variable Mapper::Geo::Param(Locate) -label [lindex $Lbl(Locate) $GDefs(Lang)] \
         -onvalue True -offvalue False

      entry $Frame.geo.address -textvariable Mapper::Geo::Data(Address) -relief sunken -bd 1 -bg $GDefs(ColorLight)
      pack $Frame.geo.icode -side left -padx 2
      pack $Frame.geo.address -side left -fill both -expand True
   pack $Frame.geo -side left -fill x -expand true

   bind $Frame.geo.address <Return> { Mapper::Geo::Code $Mapper::Geo::Data(Address) $Mapper::Geo::Param(API) }

   Bubble::Create $Frame.geo.icode     [lindex $Bubble(ICode) $GDefs(Lang)]
   Bubble::Create $Frame.geo.icode.api [lindex $Bubble(API) $GDefs(Lang)]
   Bubble::Create $Frame.geo.address   [lindex $Bubble(Code) $GDefs(Lang)]

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

proc Mapper::Geo::Code { Request { API Geocoder } } {
   global GDefs
   variable Msg
   variable Error
   variable Accuracy
   variable Param

   #----- Cleanup location info
   set Data(Lat) 0.0
   set Data(Lon) 0.0

   Dialog::CreateWait . [lindex $Msg(Request) $GDefs(Lang)] 600

   switch $API {
      "Google" {
         #----- Send request through Google
         set req [http::geturl "http://maps.google.com/maps/geo?q=[join ${Request} +]&output=xml&oe=utf8&sensor=false"]
         if { [catch { set doc [dom parse [http::data $req]] } msg ] } {
            Dialog::CreateErrorListing . "[lindex $Error(Request) $GDefs(Lang)]\n\n$msg" [http::data $req] $GDefs(Lang)
            return
         }

         #----- Extract info from XML
         if { [set root [$doc documentElement]]!="" } {
            set node [lindex  [$root getElementsByTagName Placemark] 0]
            set Data(Address) [[[$node getElementsByTagName address] firstChild] nodeValue]
            set coords        [split [[[$node getElementsByTagName coordinates] firstChild] nodeValue] ,]
            set Data(Lat)     [lindex $coords 1]
            set Data(Lon)     [lindex $coords 0]
         } else {
            Dialog::CreateError. [lindex $Error(NoneFound) $GDefs(Lang)]  $GDefs(Lang)
         }
         $doc delete
         http::cleanup $req
      }
      "Geocoder" {
         #----- Send request through Geocoder.ca
         set req [http::geturl "http://geocoder.ca/?locate=[join ${Request} %20]&geoit=XML"]
         if { [catch { set doc [dom parse [http::data $req]] } msg ] } {
            Dialog::CreateErrorListing . "[lindex $Msg(Request) $GDefs(Lang)]\n\n$msg" [http::data $req] $GDefs(Lang)
            return
         }

         #----- Extract info from XML
         if { [set root [$doc documentElement]]!="" } {
            catch { set Data(Lat)     [[[$root getElementsByTagName latt] firstChild] nodeValue] }
            catch { set Data(Lon)     [[[$root getElementsByTagName longt] firstChild] nodeValue] }
            set Data(Address) ${Request}
         } else {
            Dialog::CreateError. [lindex $Error(NoneFound) $GDefs(Lang)]  $GDefs(Lang)
         }
         $doc delete
         http::cleanup $req
      }
   }
   Dialog::DestroyWait

   if { $Data(Lat)!=0.0 && $Data(Lon)!=0.0 && $Param(Locate) } {
      SPI::Locate $Data(Lat) $Data(Lon) 128
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

   #----- Cleanup location info
   set Data(Address)  ""
   set Data(Street)   ""
   set Data(City)     ""
   set Data(Province) ""
   set Data(Country)  ""
   set Data(Postal)   ""

   Dialog::CreateWait . [lindex $Msg(Request) $GDefs(Lang)] 600

   switch $API {
      "Google" {
         #----- Send request through Google
         set req [http::geturl "http://maps.google.com/maps/geo?q=$Lat,$Lon&output=xml&oe=utf8&sensor=false"]
         if { [catch { set doc [dom parse [http::data $req]] } msg ] } {
            Dialog::CreateErrorListing . "[lindex $Msg(Request) $GDefs(Lang)]\n\n$msg" [http::data $req] $GDefs(Lang)
            return
         }

         #----- Extract info from XML
         if { [set root [$doc documentElement]]!="" } {
            set node [lindex [$root getElementsByTagName Placemark] 0]
            catch { set Data(Street)   [[[$root getElementsByTagName ThoroughfareName] firstChild] nodeValue] }
            catch { set Data(City)     [[[$root getElementsByTagName LocalityName] firstChild] nodeValue] }
            catch { set Data(Province) [[[$root getElementsByTagName AdministrativeAreaName] firstChild] nodeValue] }
            catch { set Data(Country)  [[[$root getElementsByTagName CountryNameCode] firstChild] nodeValue] }
            catch { set Data(Postal)   [[[$root getElementsByTagName PostalCodeNumber] firstChild] nodeValue] }

            set Data(Address) [[[$node getElementsByTagName address] firstChild] nodeValue]
         } else {
            Dialog::CreateError. [lindex $Error(NoneFound) $GDefs(Lang)]  $GDefs(Lang)
         }
         $doc delete
         http::cleanup $req
      }
      "Geocoder" {
         #----- Send request through Geocoder.ca
         set req [http::geturl "http://geocoder.ca/?latt=$Lat&longt=$Lon&geoit=XML&reverse=1"]
         if { [catch { set doc [dom parse [http::data $req]] } msg ] } {
            Dialog::CreateErrorListing . "[lindex $Msg(Request) $GDefs(Lang)]\n\n$msg" [http::data $req] $GDefs(Lang)
            return
         }

         #----- Extract info from XML
         if { [set root [$doc documentElement]]!="" } {
            catch { set Data(Street)  "[[[$root getElementsByTagName stnumber] firstChild] nodeValue] [[[$root getElementsByTagName staddress] firstChild] nodeValue]" }
            catch { set Data(City)     [[[$root getElementsByTagName city] firstChild] nodeValue] }
            catch { set Data(Province) [[[$root getElementsByTagName prov] firstChild] nodeValue] }
            catch { set Data(Country)  [[[$root getElementsByTagName AdministrativeAreaName] firstChild] nodeValue] }
            catch { set Data(Postal)   [[[$root getElementsByTagName postal] firstChild] nodeValue] }

            set Data(Address) "$Data(Street) $Data(City) $Data(Province) $Data(Postal)"
         } else {
            Dialog::CreateError. [lindex $Error(NoneFound) $GDefs(Lang)]  $GDefs(Lang)
         }

         $doc delete
         http::cleanup $req
      }
   }
   Dialog::DestroyWait

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

