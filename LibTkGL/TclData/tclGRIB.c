/*=============================================================================
* Environnement Canada
* Centre Meteorologique Canadian
* 2100 Trans-Canadienne
* Dorval, Quebec
*
* Projet    : Librairie Tcl de fichiers GRIB.
* Fichier   : tclGRIB.c
* Creation  : Decembre 2002 - J.P. Gauthier - CMC/CMOE
*
* Description: Utilisation des fichiers GRIB dans des scripts Tcl et
*              dans les projections.
*
* Remarques :
*
*==============================================================================
*/
#include "tclGRIB.h"

static Tcl_HashTable GRIB_FileTable;

static int GRIB_FieldCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int GRIB_FileCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FileCmd>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relies aux fichiers CDF.
 *
 * Parametres     :
 *  <clientData>  : Donnees du module.
 *  <Interp>      : Interpreteur TCL.
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int GRIB_FileCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int         id,idx;
   static CONST char *sopt[] = { "open","close",NULL };
   enum                opt { OPEN,CLOSE };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case OPEN:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id mode filename");
            return(TCL_ERROR);
         }
         return(GRIB_FileOpen(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3])[0],Tcl_GetString(Objv[4])));
         break;

      case CLOSE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         return(GRIB_FileClose(Interp,Tcl_GetString(Objv[2])));
         break;
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldCmd>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Appel des commandes relies aux fichiers standards.
 *
 * Parametres     :
 *  <clientData>  : Donnees du module.
 *  <Interp>      : Interpreteur TCL.
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int GRIB_FieldCmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]){

   int        idx;
   long       key;
   TData     *field0,*field1;
   TDataSpec *spec;

   static CONST char *mode[] = { "NEAREST","LINEAR","CUBIC","NORMALIZED_CONSERVATIVE","CONSERVATIVE","MAXIMUM","MINIMUM","SUM","AVERAGE","NORMALIZED_COUNT","COUNT","NOP",NULL };
   static CONST char *type[] = { "MASL","SIGMA","PRESSURE","UNDEFINED","MAGL","HYBRID","THETA","ETA","GALCHEN",NULL };
   static CONST char *sopt[] = { "read","configure","stats",NULL };
   enum                opt { READ,CONFIGURE,STATS };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return TCL_ERROR;
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return TCL_ERROR;
   }

   switch ((enum opt)idx) {
      case READ:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld file Index");
            return TCL_ERROR;
         }
         Tcl_GetLongFromObj(Interp,Objv[4],&key);
         return(GRIB_FieldRead(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3]),key));
         break;

      case CONFIGURE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld ?option?");
            return TCL_ERROR;
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field",(char*)NULL);
            return TCL_ERROR;
         }

         if (!field0->Stat)
            Data_GetStat(field0);

         if (strcmp(Tcl_GetString(Objv[3]),"-dataspec")==0) {
            if (Objc==4) {
               if (field0->Spec) {
                  field0->Spec->NRef++;
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(field0->Spec->Name,-1));
               }
            } else {
               if ((spec=DataSpec_Get(Tcl_GetString(Objv[4])))) {
                  if (field0->Spec) {
                     DataSpec_FreeHash(Interp,field0->Spec->Name);
                  }
                  field0->Spec=spec;
                  spec->NRef++;
               } else {
                  Tcl_AppendResult(Interp,"GRIB_FieldCmd: invalid configuration object",(char*)NULL);
                  return(TCL_ERROR);
               }
            }
         } else {
            if (DataSpec_Config(Interp,field0->Spec,Objc-3,Objv+3)==TCL_OK) {
               return(TCL_OK);
            } else {
               return(TCL_ERROR);
            }
         }
         break;

      case STATS:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld ?option?");
            return TCL_ERROR;
         }
         field0=Data_Get(Tcl_GetString(Objv[2]));
         if (!field0) {
            Tcl_AppendResult(Interp,"invalid field",(char*)NULL);
            return TCL_ERROR;
         }
         return Data_Stat(Interp,field0,Objc-3,Objv+3);
         break;
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FileClose>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Ferme le fichier GRIB.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *
 * Retour:
 *  <TCL_...> : Code de reussite.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GRIB_FileClose(Tcl_Interp *Interp,char *Id){

   GRIB_File *file=NULL;

   if ((file=(GRIB_File*)Tcl_HashDel(&GRIB_FileTable,Id))) {
      fclose(file->Handle);
      free(file->Path);
      free(file->Id);
      free(file);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_headGet>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait l'adresse d'un champs de la liste des champs
 *            connu (HashTable) et en retourne la valeur.
 *
 * Parametres     :
 *  <Name>        : Nom du champ
 *
 * Retour:
 *  <GRIB_head>  : Pointeur sur la structure du champs (char)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
GRIB_File* GRIB_FileGet(char *Id){

   Tcl_HashEntry *entry;

   if (Id && strlen(Id)>0) {
      entry=Tcl_FindHashEntry(&GRIB_FileTable,Id);
      if (entry) {
         return (GRIB_File*)(Tcl_GetHashValue(entry));
      }
   }
   return(NULL);
}

int GRIB_FilePut(Tcl_Interp *Interp,GRIB_File *File){

   Tcl_HashEntry *entry;
   int            new;

   entry=Tcl_CreateHashEntry(&GRIB_FileTable,File->Id,&new);

   if (!new) {
      Tcl_AppendResult(Interp,"\n   GRIB_FilePut: File already open: \"",File->Path, "\"",(char *)NULL);
      return(TCL_ERROR);
   }

   Tcl_SetHashValue(entry,File);

   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FileOpen>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'ouverture d'un fichier GRIB.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *  <Mode>    : Mode d'ouverture (R ou W)
 *  <Name>    : Non du fichier
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *  <tcl>     : Liste des parametres des enregistrements contenue dans le fichier
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GRIB_FileOpen(Tcl_Interp *Interp,char* Id,char Mode,char* Name){

   GRIB_Head  head;
   GRIB_File *file;
   char       buf[1024];

   FILE               *fi;
   grib_handle        *handle;
   int                 err=0;
   long                date,time,ni,nj,offset=0,size=0;
   time_t              valid;
   size_t              len;
   long                lval,lval2,lev;

   if (GRIB_FileGet(Id)) {
      Tcl_AppendResult(Interp,"GRIB_FileOpen: Cannot reuse openned file identificator ",Id,(char*)NULL);
      return TCL_ERROR;
   }

   if (!(fi=fopen(Name,"r"))) {
      Tcl_AppendResult(Interp,"GRIB_FileOpen: Cannot open grib file ",Name,(char*)NULL);
      return(TCL_ERROR);
   }

   /*Loop on the messages*/
   while(handle=grib_handle_new_from_file(0,fi,&err)) {

      err=grib_get_long(handle,"date",&date);
      err=grib_get_long(handle,"time",&time);
      err=grib_get_long(handle,"numberOfPointsAlongAParallel",&ni);
      err=grib_get_long(handle,"numberOfPointsAlongAMeridian",&nj);
      err=grib_get_long(handle,"lev",&lval);
      err=grib_get_long(handle,"typeOfLevel",&lval2);
      lval2=lval2==100?LVL_PRES:(lval2==1?LVL_MAGL:(lval2==105?LVL_MAGL:(lval2==105?LVL_MASL:LVL_UNDEF)));

      lev=FSTD_Level2IP(lval,lval2);
      valid=System_DateTime2Seconds(date,time*100);
      err=grib_get_long(handle,"GRIBEditionNumber",&lval);
      sprintf(buf,"%s %i VAR TV %ld 0 0 GRIB%i %ld %ld %ld %ld 0",Id,offset,lev,lval,valid,valid,ni,nj);
      Tcl_AppendElement(Interp,buf);
      size+=offset=ftell(fi);
   }

   /*Error on handle access*/
   if (handle) {
      grib_handle_delete(handle);

      if (err!=GRIB_SUCCESS) {
         Tcl_AppendResult(Interp,"GRIB_FileOpen: Unable to get grib handle on file ",Name,(char*)NULL);
         return(TCL_ERROR);
      }
   }

   file=(GRIB_File*)malloc(sizeof(GRIB_File));
   file->Path=(char*)strdup(Name);
   file->Id=(char*)strdup(Id);
   file->Mode=Mode;
   file->Handle=fi;
   file->Size=size;

   GRIB_FilePut(Interp,file);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <tclGRIB_Init>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser le package lors de l'inclusion par Tcl.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int TclGRIB_Init(Tcl_Interp *Interp) {

   Tcl_CreateObjCommand(Interp,"gribfile",GRIB_FileCmd,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);
   Tcl_CreateObjCommand(Interp,"gribfield",GRIB_FieldCmd,(ClientData)NULL,(Tcl_CmdDeleteProc *)NULL);

   Tcl_InitHashTable(&GRIB_FileTable,TCL_STRING_KEYS);

   return(TCL_OK);
}

