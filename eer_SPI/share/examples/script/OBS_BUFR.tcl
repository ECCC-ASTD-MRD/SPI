#!/bin/sh
# the next line restarts using tclsh \
exec $SPI_PATH/tclsh "$0" "$@"
#============================================================================
# Environnement Canada
# Centre Meteorologique Canadien
# 2100 Trans-Canadienne
# Dorval, Quebec
#
# Projet     : Exemple de scripts.
# Fichier    : OBS_BUFR.tcl
# Creation   : Mai 2008 - J.P. Gauthier - CMC/CMOE
# Description: Test des fonctions BUFR bufrdataset et bufrtemplate
#
# Parametres :
#
# Retour:
#
# Remarques  :
#
#============================================================================

package require TclData
#package require TclGeoEER
package require Logger

Log::Start [info script] 0.1

#----- Read standard table set
metobs table -readcmc

##################################
#----- Create a new BUFR file
##################################

#----- Create a new template and a new dataset based ont this template
bufrtemplate create TEMPLATE_TEST DataIn/isxx01_cytr_010000_0.template
bufrdataset create DATASET_TEST TEMPLATE_TEST

puts "   Template edition: [bufrtemplate define TEMPLATE_TEST -BUFR_EDITION]"
puts "   Template codes  : [bufrtemplate define TEMPLATE_TEST -DESCRIPTOR]"

#----- Read in data dump that fit dataset
bufrdataset read DATASET_TEST DataIn/isxx01_cytr_010000_0.dump

#----- Add a dataset from tcl
bufrdataset define DATASET_TEST -subsetadd [list {1204 ACYTR} {1204 BCYTR} {1204 CCYTR} {1204 DCYTR} {205023 {second basic smoke test}}]

#----- Change the value of 5th code of first dataset
bufrdataset define DATASET_TEST -subset 0 4 {205023 {changed code} }

#----- Define various message parameters
bufrdataset define DATASET_TEST -BUFR_EDITION 4 -BUFR_MASTER_TABLE 0 -ORIG_CENTER 0 -ORIG_SUB_CENTER 0 -UPDATE_SEQUENCE 0 \
   -DATA_CATEGORY 0 -INTERN_SUB_CATEGORY 0 -LOCAL_SUB_CATEGORY 0 -MASTER_TABLE_VERSION 2 -LOCAL_TABLE_VERSION 0 \
   -YEAR 2008 -MONTH 5 -DAY 13 -HOUR 23 -MINUTE 2 -SECOND 0 -DATA_FLAG 0

#----- Write dataset to ASCII dump and BUFR
bufrdataset write DATASET_TEST DataOut/OBS_BUFR.bufr BUFR
bufrdataset write DATASET_TEST DataOut/OBS_BUFR.txt ASCII

#----- Output all dataset info
puts "\n   File: DataIn/isxx01_cytr_010000_0.dump"
puts "      Found [bufrdataset define DATASET_TEST -subsetnb] subset(s)"
for { set i 0  } { $i<[bufrdataset define DATASET_TEST -subsetnb] } { incr i } {
   puts "   SubDataset $i: [bufrdataset define DATASET_TEST -subset $i]"
}

#----- Output 5th code of first dataset info
puts "\n      SubDataset 0 Code 4 : [bufrdataset define DATASET_TEST -subset 0 4]"

#----- Free objects
bufrdataset free DATASET_TEST
bufrtemplate free TEMPLATE_TEST

##################################
#----- Open a BUFR file
##################################

puts "\n   File: DataIn/lamwest_buf12.bufr"

while { [bufrdataset create DATASET_LAM DataIn/lamwest_buf12.bufr] } {

   #----- Print header info
   puts "      BUFR_EDITION        : [bufrdataset define DATASET_LAM -BUFR_EDITION]"
   puts "      BUFR_MASTER_TABLE   : [bufrdataset define DATASET_LAM -BUFR_MASTER_TABLE]"
   puts "      ORIG_CENTER         : [bufrdataset define DATASET_LAM -ORIG_CENTER]"
   puts "      ORIG_SUB_CENTER     : [bufrdataset define DATASET_LAM -ORIG_SUB_CENTER]"
   puts "      UPDATE_SEQUENCE     : [bufrdataset define DATASET_LAM -UPDATE_SEQUENCE]"
   puts "      DATA_CATEGORY       : [bufrdataset define DATASET_LAM -DATA_CATEGORY]"
   puts "      INTERN_SUB_CATEGORY : [bufrdataset define DATASET_LAM -INTERN_SUB_CATEGORY]"
   puts "      LOCAL_SUB_CATEGORY  : [bufrdataset define DATASET_LAM -LOCAL_SUB_CATEGORY]"
   puts "      MASTER_TABLE_VERSION: [bufrdataset define DATASET_LAM -MASTER_TABLE_VERSION]"
   puts "      LOCAL_TABLE_VERSION : [bufrdataset define DATASET_LAM -LOCAL_TABLE_VERSION]"
   puts "      YEAR                : [bufrdataset define DATASET_LAM -YEAR]"
   puts "      MONTH               : [bufrdataset define DATASET_LAM -MONTH]"
   puts "      DAY                 : [bufrdataset define DATASET_LAM -DAY]"
   puts "      HOUR                : [bufrdataset define DATASET_LAM -HOUR]"
   puts "      MINUTE              : [bufrdataset define DATASET_LAM -MINUTE]"
   puts "      SECOND              : [bufrdataset define DATASET_LAM -SECOND]"
   puts "      DATA_FLAG           : [bufrdataset define DATASET_LAM -DATA_FLAG]"

   #----- Get message template and save it to file
   bufrtemplate write [bufrdataset define DATASET_LAM -template] DataOut/OBS_BUFR.template

   #----- Print data info
   puts "\n      Found [bufrdataset define DATASET_LAM -subsetnb] subset(s)"
   for { set i 0  } { $i<[bufrdataset define DATASET_LAM -subsetnb] } { incr i } {
      puts "      SubDataset $i: [bufrdataset define DATASET_LAM -subset $i]"
   }
}

bufrdataset free DATASET_LAM

##################################
#----- Test socket communication
##################################

if { [catch { set sock [socket goodenough.cmc.ec.gc.ca 9093] }] } {
   puts "   Could not connect to server"
} else {
   fconfigure $sock -translation binary

   while { [bufrdataset create DATASET_SOCK $sock] } {

      #----- Print data info
      puts "\n      Found [bufrdataset define DATASET_SOCK -subsetnb] subset(s)"
      for { set i 0  } { $i<[bufrdataset define DATASET_SOCK -subsetnb] } { incr i } {
         puts "      SubDataset $i: [bufrdataset define DATASET_SOCK -subset $i]"
      }
   }
}

Log::End