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
#ifdef HAVE_GRIB

#include "tclGRIB.h"

static int           GRIBInit=0;
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

   int        n,idx,type;
   TGRIBFile *file=NULL;

   static CONST char *types[] = { "NONE","SPI","ALL","NOMVAR","DATEV","IP1" };
   static CONST char *sopt[] = { "is","open","close","filename","info",NULL };
   enum                opt { IS,OPEN,CLOSE,FILENAME,INFO };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"filename");
            return(TCL_ERROR);
         }
#ifdef HAVE_RMN
         type=f77name(wkoffit)(Tcl_GetString(Objv[2]),strlen(Tcl_GetString(Objv[2])));
         if (type==7) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
#endif
         return(TCL_OK);
         break;

      case OPEN:
         if(Objc!=5 && Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id mode filename [index]");
            return(TCL_ERROR);
         }
         type=2;

         if(Objc==6) {
            if (Tcl_GetIndexFromObj(Interp,Objv[5],types,"type",0,&type)!=TCL_OK) {
               return(TCL_ERROR);
            }
         }
         return(GRIB_FileOpen(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3])[0],Tcl_GetString(Objv[4]),type));
         break;

      case CLOSE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fileid");
            return(TCL_ERROR);
         }
         for(n=2;n<Objc;n++) {
            GRIB_FileClose(Interp,Tcl_GetString(Objv[n]));
         }
         return(TCL_OK);
         break;

      case FILENAME:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id");
            return(TCL_ERROR);
         }
         if (!(file=GRIB_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         Tcl_SetObjResult(Interp,Tcl_NewStringObj(file->Path,-1));
         return(TCL_OK);
         break;

      case INFO:
         if(Objc!=4 && Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"id mode [var]");
            return(TCL_ERROR);
         }
         if (!(file=GRIB_FileGet(Interp,Tcl_GetString(Objv[2])))) {
            return(TCL_ERROR);
         }
         if (Tcl_GetIndexFromObj(Interp,Objv[3],types,"type",0,&type)!=TCL_OK) {
            return(TCL_ERROR);
         }
         return(GRIB_FieldList(Interp,file,type,Objc==5?Tcl_GetString(Objv[4]):NULL));
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

   int        ni,nj,nk,idx,npack,compress=0;
   long       key;
   char      *sample;
   TData     *field,*rfield;
   
   static CONST char *sopt[] = { "create","read","write","gridinterp","import",NULL };
   enum                opt { CREATE,READ,WRITE,GRIDINTERP,IMPORT };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(Data_FieldCmd(clientData,TD_GRIB,Interp,Objc,Objv));
   }

   switch ((enum opt)idx) {
      case READ:
         if(Objc!=5) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld file Index");
            return(TCL_ERROR);
         }
         Tcl_GetLongFromObj(Interp,Objv[4],&key);
         return(GRIB_FieldRead(Interp,Tcl_GetString(Objv[2]),Tcl_GetString(Objv[3]),key));
         break;
         
      case WRITE:
         if(Objc!=5 && Objc!=6) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld file npack [compress]");
            return(TCL_ERROR);
         }
         if (!(field=Data_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"GRIB_FieldCmd: Invalid field (",Tcl_GetString(Objv[2]),")",(char*)NULL);
            return(TCL_ERROR);
         }
         Tcl_GetIntFromObj(Interp,Objv[4],&npack);
         if (Objc==6) {
            Tcl_GetBooleanFromObj(Interp,Objv[5],&compress);
         }

         return(GRIB_FieldWrite(Interp,Tcl_GetString(Objv[3]),field,npack,compress));
         break;
         
      case CREATE:
         if(Objc!=6 && Objc!=7) {
            Tcl_WrongNumArgs(Interp,2,Objv,"fld ni nj nk [sample]");
            return(TCL_ERROR);
         }
         ni=nj=nk=-1;

         Tcl_GetIntFromObj(Interp,Objv[3],&ni);
         Tcl_GetIntFromObj(Interp,Objv[4],&nj);
         Tcl_GetIntFromObj(Interp,Objv[5],&nk);

         if (ni<=0 || nj<=0 || nk<=0) {
             Tcl_AppendResult(Interp,"GRIB_FieldCmd: wrong dimensions",(char *)NULL);
             return(TCL_ERROR);
         }

         sample=NULL;
         if (Objc==7) {
           sample=Tcl_GetString(Objv[6]);
         }
         if (!GRIB_FieldCreate(Interp,Tcl_GetString(Objv[2]),sample,ni,nj,nk,TD_Float32)) {
            return(TCL_ERROR);
         } else {
            return(TCL_OK);
         }
         break;

      case IMPORT:
         if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,2,Objv,"grib rpn");
            return(TCL_ERROR);
         }   
         if (!(field=Data_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"GRIB_FieldCmd: Invalid field (",Tcl_GetString(Objv[2]),")",(char*)NULL);
            return(TCL_ERROR);
         }
         if (!(rfield=Data_Get(Tcl_GetString(Objv[3])))) {
            Tcl_AppendResult(Interp,"GRIB_FieldCmd: Invalid field (",Tcl_GetString(Objv[3]),")",(char*)NULL);
            return(TCL_ERROR);
         }
         return(GRIB_FieldImport(Interp,field,rfield));
         break;
      case GRIDINTERP:
         return(FSTD_FieldCmd(clientData,Interp,Objc,Objv));
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

   TGRIBFile *file=NULL;
   int        t;

   if ((file=(TGRIBFile*)TclY_HashDel(&GRIB_FileTable,Id))) {
      fclose(file->Handle);
      free(file->Path);
      free(file->Id);
      free(file);

      if (file->Table) {
         for(t=0;t<GRIB_TABLESIZE;t++) {
            if (file->Table[t].Handle) {
               grib_handle_delete(file->Table[t].Handle);
            } else {
               break;
            }
         }
         free(file->Table);
      }
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
TGRIBFile* GRIB_FileGet(Tcl_Interp *Interp,char *Id){

   Tcl_HashEntry *entry;

   if (Id && strlen(Id)>0) {
      entry=TclY_FindHashEntry(&GRIB_FileTable,Id);
      if (entry) {
         return (TGRIBFile*)(Tcl_GetHashValue(entry));
      }
   }
   return(NULL);
}

int GRIB_FilePut(Tcl_Interp *Interp,TGRIBFile *File){

   Tcl_HashEntry *entry;
   int            new;

   entry=TclY_CreateHashEntry(&GRIB_FileTable,File->Id,&new);

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
 *  <Index>   : Format de l'index
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *  <tcl>     : Liste des parametres des enregistrements contenue dans le fichier
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GRIB_FileOpen(Tcl_Interp *Interp,char* Id,char Mode,char* Name,int Index){

   TGRIBFile *file;
   FILE      *fi;
   char      mode[2];
   
  if (GRIB_FileGet(Interp,Id)) {
      Tcl_AppendResult(Interp,"GRIB_FileOpen: Cannot reuse openned file identificator ",Id,(char*)NULL);
      return TCL_ERROR;
   }

   mode[0]=Mode;mode[1]='\0';
   if (!(fi=fopen(Name,mode))) {
      Tcl_AppendResult(Interp,"GRIB_FileOpen: Cannot open grib file ",Name,(char*)NULL);
      return(TCL_ERROR);
   }

   file=(TGRIBFile*)malloc(sizeof(TGRIBFile));
   file->Path=(char*)strdup(Name);
   file->Id=(char*)strdup(Id);
   file->Mode=Mode;
   file->Handle=fi;
   file->Table=NULL;

   // Force multi field support
   grib_multi_support_on(grib_context_get_default());

   GRIB_FilePut(Interp,file);

   if (Mode=='r') {
      return(GRIB_FieldList(Interp,file,Index,NULL));
   } else {
      return(TCL_OK);
   }
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

   if (!GRIBInit++) {
      Tcl_InitHashTable(&GRIB_FileTable,TCL_STRING_KEYS);
   }
   return(TCL_OK);
}

#endif
