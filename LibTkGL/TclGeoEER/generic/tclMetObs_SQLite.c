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
 *                L'objet TMetObs doit avoir été initialisé au préalable.
 *
 *   <Filename> : Nom du fichier à ouvrir
 *
 * Retour       : Code d'erreur TCL
 *
 * Remarques    :
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

/********************************************************************************
 * Name        : static int sqlite_helper_finalize(struct SQLiteHelper *sqlh)
 * Description : Destroy database related objects
 * Arguments   : this : the object to destroy.
 * Return      : TCL error code
 * Remarks     : The order is important here, the statements have to be
 *               destroyed for us to be able to close the database connection.
 *******************************************************************************/
static int sqlite_helper_finalize(struct SQLiteHelper *this)
{
   int retval = TCL_OK;
   sqlite3_finalize(this->obs_stmt);
   sqlite3_finalize(this->elem_stmt);
   if(sqlite3_close(this->db) == SQLITE_BUSY){
      App_Log(ERROR, "Couldn't close database connection\n");
      retval = TCL_ERROR;
   }
   free(this->obs_query);
   free(this->elem_query);
   free(this->db_filename);
   return retval;
}

/********************************************************************************
 * Name        : sqlite_helper_init(struct SQLiteHelper *this, const char *Filename)
 * Description : Initialize database objects related to parsing weather database
 * Arguments   : this : Object to initialize
 *               Filename : Filename of the database file
 * Return      : TCL error code
 * Remarks     : The code to look at the filename is mock code, we haven't
 *               figured out how different database types will be identified.
 *               See MetObs_Cmd also, it was monkey patched (in 2018-07) to
 *               dispatch to MetObs_LoadSQLite() based on the file containing
 *               the string '.sqlite'.
 *******************************************************************************/
static int query_progress_callback(void *Params);
static int sqlite_helper_init(struct SQLiteHelper *this, const char *Filename)
{
   this->db_filename = strdup(Filename);
  char *key = NULL;
  if(strstr(Filename, "acars"))
    key = "acars";
  else if(strstr(Filename, "other"))
    key = "other";
  else if(strstr(Filename, "2018"))
    key = "other";


  if(sqlite3_open(Filename, &(this->db))){
     fprintf(stderr, "Can't open database: %s\n", sqlite3_errmsg(this->db));
     return TCL_ERROR;
  }

  sqlite3_progress_handler(this->db, INSTRUCTIONS_PER_CALL, query_progress_callback, NULL);

  this->cfg_file = (char *)CONFIG_FILE;
  if(MetObsSQLite_GetQueries(this->cfg_file, key,(char **)&(this->obs_query), (char **)&(this->elem_query))){
     App_Log(ERROR, "Could not find the right query for your database based on the filename %s\n", Filename);
     return TCL_ERROR;
  }

  if(sqlite3_prepare_v2(this->db, this->obs_query, -1, &this->obs_stmt, NULL)){
     App_Log(ERROR, "Could not compile query %s\nSQL_ERROR_MESSAGE:%s\n", this->obs_query, sqlite3_errmsg(this->db));
     return TCL_ERROR;
  }

  if(sqlite3_prepare_v2(this->db, this->elem_query, -1, &this->elem_stmt, NULL)){
     App_Log(ERROR, "Could not compile query %s\nSQL_ERROR_MESSAGE:%s\n", this->elem_query, sqlite3_errmsg(this->db));
     return TCL_ERROR;
  }

  return TCL_OK;

}

/********************************************************************************
 * Name        : set_obs_elements(Tcl_Interp *Interp, TMetObs *Obs, struct SQLiteHelper *sqlh)
 * Description : Iterate over the results of the element query
 * Arguments   : Interp : The TCL interpreter
 *             : Obs : The output obs for the elements
 *             : sqlh : The SQLite helper struct.
 * Remarks     : Since the elements get added in Obs->Elems which is a Tcl_Obj
 *               by the Tcl interpreter needs to be passed down to it.
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

/********************************************************************************
 * Name        : static int add_obs_element(Tcl_Interp *Interp, TMetObs *Obs, sqlite3_stmt *Element)
 * Description : Add a result from the element query to Obs->Elems.
 * Arguments   : Interp : The TCL interpreter
 *             : Obs : The output obs for the elements
 *             : Element : An sqlite_stmt taken as a current row
 * Remarks     : The database contains an integer representing a BUFR code.
 *               This is open to change because some codes are just strings
 *               and/or cannot be mapped to a BUFR code.  See documentation
 *               about ideas for dealing with this.
 *******************************************************************************/
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

/********************************************************************************
 * Name        : loop_over_observations(TMetObs *Obs, struct SQLiteHelper *sqlh)
 * Description : Drive the iteration over the rows of the query for weather data
 * Arguments   : Obs : The TMetObs that will be populated by the data
 *               sqlh : The database things
 * Return      : TCL error code
 * Remarks     : Isn't making small functions nice!
 *******************************************************************************/
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

/********************************************************************************
 * Name        : get_loc(TMetObs *Obs, sqlite3_stmt *Row)
 * Description : Get appropriate location for the data in the row
 * Arguments   : Obs : TMetObs whose TMetLocs we are going to search
 *               Row : SQLite statement treated as a result row
 * Return      : TMetLoc * loc : The location
 * Remarks     : 
 *******************************************************************************/
static TMetLoc *get_loc(TMetObs *Obs, sqlite3_stmt *Row)
{
   TMetLoc *loc = NULL;
   const unsigned char *stnid = sqlite3_column_text(Row, SPI_ID_STN);
   double lat = sqlite3_column_double(Row, SPI_LAT);
   double lon =  sqlite3_column_double(Row, SPI_LON);
   double elev = sqlite3_column_double(Row, SPI_ELEV);

   char multi; // La fonction le met a 0 ou 1 alors pourquoi c'est pas un int?
   loc = TMetLoc_FindWithCoordIndex(
         Obs,  // TMetObs *Obs,
         NULL, // TMetLoc *From,
         (char *)stnid,   // char *Id,
         lat,  // double Lat,
         lon,  // double Lon,
         elev, // double Elev,
         MET_TYPEID, //int Type,
         &multi   // char *Multi
   );

   if(!loc){
      loc = TMetLoc_New(
            Obs,  // TMetObs *Obs,
            (char *)stnid,   // char *Id,
            NULL, // char *No,
            lat,  // double Lat,
            lon,  // double Lon,
            elev  // double Elev
      );
   }

   return loc;
}

/********************************************************************************
 * Name        : read_observation(TMetObs *Obs, sqlite3_stmt *Row)
 * Description : Read data from a result row into the TMetObs object.
 * Arguments   : Obs : Where the data is inserted
 *               Row : Data source (SQLite statement treated as a row)
 * Return      : TCL error code
 * Remarks     : For simplicity's sake, we only put one datum per TMetElemData
 *               which is not efficient.  See documentation for ideas about
 *               changing this.
 *******************************************************************************/
static int get_eb_code(sqlite3_stmt *Row, EntryTableB **Eb_out);
static int read_observation(TMetObs *Obs, sqlite3_stmt *Row)
{
   TMetLoc *loc;

   if((loc = get_loc(Obs, Row)) == NULL){
      App_Log(ERROR, "Could not find or create a TMetLoc matching Row's data\n");
      return TCL_ERROR;
   }

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

   if(TMetElem_Insert(loc, dt, time, fam, type, stype, ne, nv, nt, fval, val, ebCodes) == NULL){
      App_Log(DEBUG,
              "No data inserted, possibly \"same data\" in %s()\n"
              "     loc=%p\n"
              "     time=%lu\n"
              "     fval[0]=%f\n"
              "     val[0]=%d\n",
              __func__, loc, time, fval[0], val[0]);
      // return TCL_ERROR;
   }

   return TCL_OK;
}

/********************************************************************************
 * Name        : get_eb_code(sqlite3_stmt *Row, EntryTableB **Eb_out)
 * Description : Get an EntryTableB* corresponding to the row's Element
 *               column.
 * Arguments   : Row : An sqlite3_stmt treated as a row.
 *               Eb_Out : Out param for the EntryTableB*.
 * Return      : TCL error code
 * Remarks     : This relies on the query providing element codes as strings.
 *               If element codes are stored as ints, then the query will have
 *               user a cast (https://stackoverflow.com/a/12419894/5795941) or
 *               the other way around, queries could be made to produce integers
 *               by possibly casting but in that case, this code would have to
 *               be changed and all previous queries edited.
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

/********************************************************************************
 * Name        : query_progress_callback(void *Params)
 * Description : The most basic progress spinner in the world
 * Arguments   : None
 * Return      : 0 to continue, 1 to tell SQLite to abort what it's doing.
 * Remarks     : As stated above, you can halt an SQLite operation by making
 *               this function return non-zero. For example if a user presses a
 *               cancel button. This will ONLY STOP SQLite but it WILL NOT stop
 *               whatever you were doing with SQLite. I.E. if you halt SQLite
 *               but keep doing sqlite3_column_text(so-and-so), you're not going
 *               to be have a hard time finding out why you're getting
 *               segfaults.
 *******************************************************************************/
static int query_progress_callback(void *Params)
{
   static int i = 0;
   static char spinner[] = {'-', '\\', '|', '/'};
   fprintf(stderr, "\r %c\r", spinner[i++ % sizeof(spinner)]);
   return 0;
}
