/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Chargement de fichiers SQLite en TMetObs
 * Fichier      : tclMetObs_SQLite.c
 * Creation     : Juin 2018 - Philippe Carphin
 *
 * Description  : Fichier d'entete du module Obs.
 *
 * Remarques    :
 *
 * License      :
 *    This library is free software; you can redistribute it and/or
 *    modify it under the terms of the GNU Lesser General Public
 *    License as published by the Free Software Foundation,
 *    version 2.1 of the License.
 *
 *    This library is distributed in the hope that it will be useful,
 *    but WITHOUT ANY WARRANTY; without even the implied warranty of
 *    MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 *    Lesser General Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser General Public
 *    License along with this library; if not, write to the
 *    Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 *
 *=========================================================
 */
#include "App.h"
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include "tclMetObs.h"
#include "tclMetObs_SQLite.h"
#include "tclMetObs_Test.h"
#include "sqlite3.h"
#include "tclMetObs_SQLiteConfigParser.h"
#include <inttypes.h>

static const int INSTRUCTIONS_PER_CALL = 9000;
enum SpiSQLiteElements {
                        SPI_ID_RAPPORT,
                        SPI_ID_STN,
                        SPI_LAT,
                        SPI_LON,
                        SPI_ELEV,
                        SPI_DATE_VALIDITE,
                        SPI_VALEUR,
                        SPI_ELEMENT
};

static const char *CONFIG_FILE = "/users/dor/afsm/pca/Documents/GitHub/SPI_PHIL/LibTkGL/TclGeoEER/MetObsTest/spi_queries.txt";
static const char *obs_query = NULL;
static const char *elem_query = NULL;
/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_LoadSQLite>
 * Creation     : Mai 2018 Philippe Carphin
 *
 * But          : Charger un fichier SQLite en mémoire dans un objet TMetObs
 *
 * Parametres   :
 *    <obs>     : Objet TMetObs qui sera rempli avec les données de la BD
 *                L'objet TMetObs doit avoir été initialisé au préalable
 *                Pour l'instant, c'ets fait dans la fonction MetObs_
 *
 *   <Filename> : Nom du fichier à ouvrir
 *
 * Retour       : Code d'erreur TCL
 *
 * Remarques :
 *
 *--------------------------------------------------------------------------------------------------------------*/
static int loop_over_join(TMetObs *Obs, sqlite3 *Db);
static int set_obs_elements(Tcl_Interp *Interp, TMetObs *obs, sqlite3 *Db);
static int get_obs_query(const char *Filename);
static int query_progress_callback(void *params);
int MetObs_LoadSQLite(Tcl_Interp *Interp, const char *Filename, TMetObs *Obs)
{
   (void) Interp;
   int retval = TCL_OK;
   sqlite3 *db;
   if(sqlite3_open(Filename, &db)){
      fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(db));
      retval = TCL_ERROR;
      goto out_close;
   }

   int nb_instr = 0;
   sqlite3_progress_handler(db, INSTRUCTIONS_PER_CALL, query_progress_callback, &nb_instr);

   if(get_obs_query(Filename)){
      App_Log(ERROR, "Could not find the right query for your database based on the filename %s\n", Filename);
      retval = TCL_ERROR;
      goto out_close;
   }

   if(loop_over_join(Obs, db)){
      App_Log(ERROR, "Something went wrong with loop_over_join()\n");
      retval = TCL_ERROR;
      goto out_close;
   }

   if(set_obs_elements(Interp, Obs, db)){
      App_Log(ERROR, "Somthing went wrong when setting Obs->Elems\n");
      retval = TCL_ERROR;
      goto out_close;
   }

 out_close:
   fputs("\33[2K", stderr);
   if(sqlite3_close(db) == SQLITE_BUSY){
      App_Log(ERROR, "Couldn't close database connection\n");
      retval = TCL_ERROR;
   }
   return retval;
}

static int get_obs_query(const char *Filename)
{
  char *key = NULL;

  if(strstr(Filename, "acars"))
    key = "acars";
  else if(strstr(Filename, "other"))
    key = "other";
  else if(strstr(Filename, "2018"))
    key = "other";

  return MetObsSQLite_GetQueries(CONFIG_FILE, key,(char **)&(obs_query), (char **)&elem_query);
}

/*******************************************************************************
 * set_obs_elements(Tcl_Interp *Interp, TMetObs *obs, sqlite3 *db)
 *
 * Adds all element codes present in the file to Obs->Elems.
*******************************************************************************/
static int add_obs_element(Tcl_Interp *Interp, TMetObs *Obs, sqlite3_stmt *Element);
static int set_obs_elements(Tcl_Interp *Interp, TMetObs *Obs, sqlite3 *Db)
{
   int retval = TCL_ERROR;
   sqlite3_stmt *unique_elements;
   if(sqlite3_prepare_v2(Db, elem_query, -1, &unique_elements, NULL)){
      App_Log(ERROR, "Could not compile query %s\nSQL_ERROR_MESSAGE:%s\n",obs_query, sqlite3_errmsg(Db));
      retval = TCL_ERROR;
      goto out;
   }

   while(sqlite3_step(unique_elements) != SQLITE_DONE){
      if(add_obs_element(Interp, Obs, unique_elements) != TCL_OK){
         goto out;
      }
   }
   retval = TCL_OK;

out:
   sqlite3_finalize(unique_elements);
   return retval;
}

static int add_obs_element(Tcl_Interp *Interp, TMetObs *Obs, sqlite3_stmt *Element)
{
   EntryTableB *eb;
   unsigned int code = atoi((const char *)sqlite3_column_text(Element, 0));

   if((eb = MetObs_BUFRFindTableCode(code)) == NULL){
      App_Log(ERROR, "%s(): MetObs_BUFRFindTableCode(%u) failed\n", __func__, code);
      return TCL_ERROR;
   }

   Tcl_Obj *element = Tcl_NewIntObj(eb->descriptor);
   if (TclY_ListObjFind(Interp,Obs->Elems,element)==-1) {
      Tcl_ListObjAppendElement(Interp, Obs->Elems, element);
   }

   return TCL_OK;
}

/*******************************************************************************
 * loop_over_join(TMetObs *obs, sqlite3 *db)
 *
 * Iterate over all observations.  The data of each line in the natural join of
 * Rapport and Observation.
 * Since the observations are ordered by id_rapport, we can add consecutive
 * entries that have the same id_rapport to the same TMetLoc.
*******************************************************************************/
static TMetLoc *get_loc_join(TMetObs *Obs, sqlite3_stmt *Row);
static int read_observation_join(TMetLoc *loc, sqlite3_stmt *Row);
static int loop_over_join(TMetObs *Obs, sqlite3 *Db)
{
   sqlite3_stmt *jro_stmt;
   int retval = TCL_OK;
   App_Log(INFO, "MetObs_SQLite : Running query \n%s\n", obs_query);

   if(sqlite3_prepare_v2(Db, obs_query, -1, &jro_stmt, NULL)){
      App_Log(ERROR, "Could not compile query %s\nSQL_ERROR_MESSAGE:%s\n",obs_query, sqlite3_errmsg(Db));
     retval = TCL_ERROR;
     goto out;
   }

   /*
    * We have to read one row before the loop because the loop because the loop
    * has to check whether the next row has the same id_rapport as the
    * current_loc
    */
   sqlite3_step(jro_stmt);
   TMetLoc *current_loc = get_loc_join(Obs, jro_stmt);
   current_loc->id_rapport = sqlite3_column_int64(jro_stmt, SPI_ID_RAPPORT);

   do {
     uint64_t id_rapport = sqlite3_column_int64(jro_stmt, SPI_ID_RAPPORT);
     if( current_loc->id_rapport != id_rapport){
       current_loc = get_loc_join(Obs, jro_stmt);
     }

     if(read_observation_join(current_loc, jro_stmt) != TCL_OK){
       retval = TCL_ERROR;
       goto out;
     }

   } while(sqlite3_step(jro_stmt) != SQLITE_DONE);

out:
   sqlite3_finalize(jro_stmt);
   return retval;
}

/*******************************************************************************
 * get_loc_join(TMetObs *Obs, sqlite3_stmt *jro_stmt)
 *
 * Get a TMetLoc object corresponding to the current row of the table
 * Rapport NATURAL JOIN Observation
*******************************************************************************/
static double get_elev_join(sqlite3_stmt *Row);
static TMetLoc *get_loc_join(TMetObs *Obs, sqlite3_stmt *Row)
{
   TMetLoc *loc = NULL;
   const unsigned char *stnid = sqlite3_column_text(Row, SPI_ID_STN);
   uint64_t id_rapport = sqlite3_column_int64(Row, SPI_ID_RAPPORT);
   double lat = sqlite3_column_double(Row, SPI_LAT);
   double lon =  sqlite3_column_double(Row, SPI_LON);
   double elev = get_elev_join(Row);

   char multi; // La fonction le met a 0 ou 1 alors pourquoi c'est pas un int?
   loc = TMetLoc_FindWithCoordIndex(
         Obs,  // TMetObs *Obs,
         NULL, // TMetLoc *From,
         (char *)stnid,   // char *Id,
         lat,  // double Lat,
         lon,  // double Lon,
         elev, // double Elev,
         MET_TYPEID, //int Type, ???????????????????
         &multi   // char *Multi
   );

   if(!loc){
      loc = TMetLoc_New(
            Obs,  // TMetObs *Obs,
            (char *)stnid,   // char *Id,
            NULL, // char *No, ?????????????????????
            lat,  // double Lat,
            lon,  // double Lon,
            elev  // double Elev
      );
   }
   // TODO Talk about this addition of id_rapport to the TMetLoc structure.
   loc->id_rapport = id_rapport;

   return loc;
}
/*******************************************************************************
 * Get the correct column from the table 
 * Rapport NATURAL JOIN Observation
 * We can have a bunch of small functions like this, and we can use function
 * pointers: if the choice is made once per file, it doesn't make any sense to
 * check 6 conditions for each of the 1 000 000 rows of a table.
*******************************************************************************/
static double get_elev_join(sqlite3_stmt *Row)
{
   // if( type is flight data )
   return sqlite3_column_double(Row, SPI_ELEV);
   // else if(type is something else)
   // ...
}

/*******************************************************************************
 * Read the information from a row of Rapport NATURAL JOIN Observation into the
 * supplied TMetLoc.
*******************************************************************************/
static int get_eb_code(sqlite3_stmt *Row, EntryTableB **Eb_out);
static int read_observation_join(TMetLoc *loc, sqlite3_stmt *Row)
{
   time_t dt = 0; // un genre de temps minimal
   time_t time = sqlite3_column_int64(Row, SPI_DATE_VALIDITE);

   /*
    * On met rien la dedans
    */
   int fam = 0;
   int type = 0;
   int stype = 0;

   /*
    * Tableau avec un seul élément pour l'instant
    */
   int ne = 1, nv = 1, nt = 1;
   const int nb_val = ne*nv*nt;
   float fval[nb_val];
   int val[nb_val];
   fval[0] = sqlite3_column_double(Row, SPI_VALEUR);

   /*
    * EntryTableB stuff
    */
   EntryTableB *ebCodes[nb_val];
   if(get_eb_code(Row, &ebCodes[0])){
      return TCL_ERROR;
   }

   TMetElem_Insert(loc, dt, time, fam, type, stype, ne, nv, nt, fval, val, ebCodes);
  
   return TCL_OK;
}

/*******************************************************************************
 *
*******************************************************************************/
static int get_eb_code(sqlite3_stmt *Row, EntryTableB **Eb_out)
{
   EntryTableB* eb;
   const char *ebCodeStr = (const char *)sqlite3_column_text(Row, SPI_ELEMENT);
   unsigned int ebCode;
   if(sscanf(ebCodeStr, "%u", &ebCode) != 1){
      App_Log(ERROR, "could not convert elemnt code string %s to integer with sscanf\n", ebCodeStr);
      return TCL_ERROR;
   }
   if((eb = MetObs_BUFRFindTableCode(ebCode)) == NULL){
      App_Log(ERROR, "Could not obtain EntryTableB* for ebCode=%u\n", ebCode);
      return TCL_ERROR;
   }
   *Eb_out = eb;
   return TCL_OK;
}

/*
 * Callback that that will be run every INSTRUCTIONS_PER_CALL instructions of
 * the database virtual machine
 */
static int query_progress_callback(void *params)
{
   int *nb_instr = (int *) params;
   *nb_instr += INSTRUCTIONS_PER_CALL;

   static int i = 0;
   static int calls = 0;
   static char spinner[] = "-\\|/";
   static int spinner_chars = sizeof(spinner)/sizeof(char) - 1;
   calls++;
   i = (i+1) % spinner_chars;
   char c = spinner[i];
   fprintf(stderr,
           "\r %c ===== progress_callback_calls : %d === dbvm Instructions: %d ====== %c       \r",
           c, calls, *nb_instr, c
   );
   return 0;
}
