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
struct SQLiteHelper {
   char *db_filename;
   sqlite3 *db;
   char *obs_query;
   char *elem_query;
   sqlite3_stmt *obs_stmt;
   sqlite3_stmt *elem_stmt;
   char *cfg_file;
};
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
static int sqlite_helper_init(struct SQLiteHelper *sqlh, const char *Filename);
static int sqlite_helper_finalize(struct SQLiteHelper *sqlh);
static int loop_over_observations(TMetObs *Obs, struct SQLiteHelper *sqlh);
static int set_obs_elements(Tcl_Interp *Interp, TMetObs *obs, struct SQLiteHelper *sqlh);
int MetObs_LoadSQLite(Tcl_Interp *Interp, const char *Filename, TMetObs *Obs)
{
   (void) Interp;
   int retval = TCL_OK;
   struct SQLiteHelper sqlh;

   if(sqlite_helper_init(&sqlh, Filename)){
      App_Log(ERROR, "Could not initialize SQLite for %s\n", Filename);
      retval = TCL_ERROR;
      goto out_close;
   }

   if(loop_over_observations(Obs, &sqlh)){
      App_Log(ERROR, "Something went wrong with loop_over_observations()\n");
      retval = TCL_ERROR;
      goto out_close;
   }

   if(set_obs_elements(Interp, Obs, &sqlh)){
      App_Log(ERROR, "Somthing went wrong when setting Obs->Elems\n");
      retval = TCL_ERROR;
      goto out_close;
   }

 out_close:
   if(sqlite_helper_finalize(&sqlh)){
      retval = TCL_ERROR;
   }
   return retval;
}

static int sqlite_helper_finalize(struct SQLiteHelper *sqlh)
{
   int retval = TCL_OK;
   sqlite3_finalize(sqlh->obs_stmt);
   sqlite3_finalize(sqlh->elem_stmt);
   if(sqlite3_close(sqlh->db) == SQLITE_BUSY){
      App_Log(ERROR, "Couldn't close database connection\n");
      retval = TCL_ERROR;
   }
   free(sqlh->obs_query);
   free(sqlh->elem_query);
   free(sqlh->db_filename);
   return retval;
}

static int query_progress_callback(void *Params);
static int sqlite_helper_init(struct SQLiteHelper *sqlh, const char *Filename)
{
   sqlh->db_filename = strdup(Filename);
  char *key = NULL;
  if(strstr(Filename, "acars"))
    key = "acars";
  else if(strstr(Filename, "other"))
    key = "other";
  else if(strstr(Filename, "2018"))
    key = "other";


  if(sqlite3_open(Filename, &(sqlh->db))){
     fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(sqlh->db));
     return TCL_ERROR;
  }

  int nb_instr = 0;
  sqlite3_progress_handler(sqlh->db, INSTRUCTIONS_PER_CALL, query_progress_callback, &nb_instr);

  sqlh->cfg_file = CONFIG_FILE;
  if(MetObsSQLite_GetQueries(sqlh->cfg_file, key,(char **)&(sqlh->obs_query), (char **)&(sqlh->elem_query))){
     App_Log(ERROR, "Could not find the right query for your database based on the filename %s\n", Filename);
     return TCL_ERROR;
  }

  if(sqlite3_prepare_v2(sqlh->db, sqlh->obs_query, -1, &sqlh->obs_stmt, NULL)){
     App_Log(ERROR, "Could not compile query %s\nSQL_ERROR_MESSAGE:%s\n", sqlh->obs_query, sqlite3_errmsg(sqlh->db));
     return TCL_ERROR;
  }

  if(sqlite3_prepare_v2(sqlh->db, sqlh->elem_query, -1, &sqlh->elem_stmt, NULL)){
     App_Log(ERROR, "Could not compile query %s\nSQL_ERROR_MESSAGE:%s\n", sqlh->elem_query, sqlite3_errmsg(sqlh->db));
     return TCL_ERROR;
  }

  return TCL_OK;

}

/*******************************************************************************
 * set_obs_elements(Tcl_Interp *Interp, TMetObs *obs, sqlite3 *db)
 * Adds all element codes present in the file to Obs->Elems.
*******************************************************************************/
static int add_obs_element(Tcl_Interp *Interp, TMetObs *Obs, sqlite3_stmt *Element);
static int set_obs_elements(Tcl_Interp *Interp, TMetObs *Obs, struct SQLiteHelper *sqlh)
{
   int retval = TCL_ERROR;
   sqlite3_stmt *unique_elements = sqlh->elem_stmt;

   while(sqlite3_step(unique_elements) != SQLITE_DONE){
      if(add_obs_element(Interp, Obs, unique_elements) != TCL_OK){
         goto out;
      }
   }
   retval = TCL_OK;

out:
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
 * loop_over(TMetObs *obs, sqlite3 *db)
 ********************************************************************************/
static TMetLoc *get_loc(TMetObs *Obs, sqlite3_stmt *Row);
static int read_observation(TMetObs *Obs, sqlite3_stmt *Row);
static int loop_over_observations(TMetObs *Obs, struct SQLiteHelper *sqlh)
{
   sqlite3_stmt *observations = sqlh->obs_stmt;

   while(sqlite3_step(observations) != SQLITE_DONE){
     if(read_observation(Obs, observations) != TCL_OK){
       return TCL_ERROR;
     }
   }

   return TCL_OK;
}


/*******************************************************************************
 * 
 *******************************************************************************/
static double get_elev(sqlite3_stmt *Row);
static TMetLoc *get_loc(TMetObs *Obs, sqlite3_stmt *Row)
{
   TMetLoc *loc = NULL;
   const unsigned char *stnid = sqlite3_column_text(Row, SPI_ID_STN);
   uint64_t id_rapport = sqlite3_column_int64(Row, SPI_ID_RAPPORT);
   double lat = sqlite3_column_double(Row, SPI_LAT);
   double lon =  sqlite3_column_double(Row, SPI_LON);
   double elev = get_elev(Row);

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
static double get_elev(sqlite3_stmt *Row)
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
static int read_observation(TMetObs *Obs, sqlite3_stmt *Row)
{
   TMetLoc *loc = get_loc(Obs, Row);
   // TODO : See todo in loop_over_observations TMetLoc *loc = get_loc(Row);
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

   // TODO Add error handling here and with caller of this function.
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
      App_Log(ERROR, "could not convert element code string %s to integer with sscanf\n", ebCodeStr);
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
static int query_progress_callback(void *Params)
{
   static int i = 0;
   static char spinner[] = {'-', '\\', '|', '/'};
   fprintf(stderr, "\r %c\r", spinner[i++ % sizeof(spinner)]);
   return 0; // This function could interact with the GUI, it could return -1 if
             // the user presses a cancel button.  The function could check if
             // the user has pressed a cancel button and return -1.  SQLite
             // looks at the return value of this function to determine whether
             // to continue or not.  I.E. if this function return non-zero, it
             // makes SQLite abort.
             // A WORD OF CAUTION : It's not clear when SQLite calls this, so if
             // whatever logic you have checks some shared memory and notices
             // that it should now return non-zero, then you have no idea where
             // what you'll be doing after the stop.  An sqlite3_column_somthing
             // could return NULL where that was impossible if this function
             // always returns 0.  You could get NULL from a
             // sqlite3_column_text() for a column marked NOT NULL.
}
