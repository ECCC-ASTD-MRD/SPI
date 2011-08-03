/*=============================================================================
* Environnement Canada
* Centre Meteorologique Canadian
* 2100 Trans-Canadienne
* Dorval, Quebec
*
* Projet    : Librairie Tcl de fichiers standards.
* Fichier   : GRIB_Field.c
* Creation  : Aout 1998 - J.P. Gauthier - CMC/CMOE
*
* Description: Manipulation de champs standards RPN dans des scripts Tcl.
*
* Remarques :
*
*==============================================================================
*/

#include "tclGRIB.h"
#include "Projection.h"

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FieldSet>
 * Creation : Janvier 2007 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser la structure specificque a ce type de donnee.
 *
 * Parametres   :
 *  <Data>      : Pointeur sur la donnee
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GRIB_FieldSet(TData *Data){

   GRIB_Head *head;

   if (Data->Head && Data->Free)
      Data->Free(Data);

   head=(GRIB_Head*)malloc(sizeof(GRIB_Head));

   head->FID=NULL;
   head->Handle=NULL;
   head->Version=0;
   head->NOMVAR[0]='\0';
   head->CENTER[0]='\0';
   head->KEY=0;
   head->IP1=0;
   head->DATEV=0;
   head->DATEO=0;

   /*Initialiser les parametres de definition du champs*/
   Data->Head=head;
   Data->Set=GRIB_FieldSet;
   Data->Free=GRIB_FieldFree;
   Data->Copy=GRIB_HeadCopy;
   Data->Grid=GRIB_Grid;
   Data->ReadCube=NULL;
}

void GRIB_HeadCopy(void *To,void *From) {
   memcpy((GRIB_Head*)To,(GRIB_Head*)From,sizeof(GRIB_Head));
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_Grid>
 * Creation : Janvier 2010 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Calculer la position des points de grille (3D) dans la projection.
 *
 * Parametres   :
 *  <Field>     : Champs de donnees
 *  <Proj>      : Projection
 *
 * Retour:
 *  <Vect3d*>   : Pointeur sur les positions (NULL si invalide)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
Vect3d* GRIB_Grid(TData *Field,void *Proj,int Level) {

   GRIB_Head *head=(GRIB_Head*)Field->Head;
   Coord      coord;
   double     z;
   int        i,j,idx;

   /*Verifier la validite de la grille*/
   if (!Field->Ref || Field->Ref->Type==GRID_NONE)
      return(NULL);

   if (Field->Ref->Pos && Field->Ref->Pos[Level])
      return(Field->Ref->Pos[Level]);

   /*Allocate memory for various levels*/
   if (!Field->Ref->Pos)
      Field->Ref->Pos=(Vect3d**)calloc(Field->Ref->LevelNb,sizeof(Vect3d*));

   if (!Field->Ref->Pos[Level]) {
      Field->Ref->Pos[Level]=(Vect3d*)malloc(FSIZE2D(Field->Def)*sizeof(Vect3d));
      if (!Field->Ref->Pos[Level]) {
         fprintf(stderr,"(ERROR) GRIB_Grid: Not enough memory to calculate gridpoint location");
         return(NULL);
      }
   }

   z=Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[Level]);
   for (i=0;i<Field->Def->NI;i++) {
      for (j=0;j<Field->Def->NJ;j++) {

         idx=j*Field->Def->NI+i;
         coord.Elev=0.0;

         /*Reproject coordinates if needed*/
         Field->Ref->Project(Field->Ref,i,j,&coord.Lat,&coord.Lon,1,1);

         if (Field->Ref->Hgt) {
            coord.Elev+=Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Hgt[idx]);
         } else {
            coord.Elev+=z;
         }
         coord.Elev*=Field->Spec->TopoFactor;

         /*Si les positions sont hors domaine, outter space*/
         if (coord.Lat<-900.0 || coord.Lon<-900.0) {
            coord.Elev=1e32;
         }
         Vect_Init(Field->Ref->Pos[Level][idx],coord.Lon,coord.Lat,coord.Elev);
      }
   }
   ((Projection*)Proj)->Type->Project(Proj,Field->Ref->Pos[Level],NULL,FSIZE2D(Field->Def));

   return(Field->Ref->Pos[Level]);
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FieldDefine>
 * Creation : Janvier 2010 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la configuration des parametres RPN du champ et le retour des valeurs de
 *            configuration si il n'y a pas de valeur specifie (seulement le token).
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Field>       : Pointeur sur les donnees du champs
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
int GRIB_FieldDefine(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj     *obj;
   GRIB_Head   *head=(GRIB_Head*)Field->Head;
   TGeoRef     *ref;
   int          i,j,idx,nidx;
   double       tra[6],inv[6],*tm,*im;
   const char **list;

   static CONST char *sopt[] = { "-DATEO","-DATEV","-FID","-KEY","-NI","-NJ","-NK","-IP1","-NOMVAR","-CENTER","-DATA","-projection","-transform","-georef",NULL };
   enum        opt { DATEO,DATEV,FID,KEY,NI,NJ,NK,IP1,NOMVAR,CENTER,DATA,PROJECTION,TRANSFORM,GEOREF };

   if (!Field) {
      Tcl_AppendResult(Interp,"Invalid field",(char*)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return TCL_ERROR;
      }

      switch ((enum opt)idx) {

         case DATEO:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(head->DATEO));
            } else {
               i++;
               Tcl_GetLongFromObj(Interp,Objv[i],&head->DATEO);
            }
            break;

         case DATEV:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewLongObj(head->DATEV));
            } else {
               i++;
               Tcl_GetLongFromObj(Interp,Objv[i],&head->DATEV);
            }
            break;

         case FID:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(head->FID->Id,-1));
            } else {
               head->FID=GRIB_FileGet(Interp,Tcl_GetString(Objv[++i]));
            }
            break;

         case KEY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->KEY));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->KEY);
            }
            break;

         case NI:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->Def->NI));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Field->Def->NI);
            }
            break;

         case NJ:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->Def->NJ));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Field->Def->NJ);
            }
            break;

         case NK:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->Def->NK));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&Field->Def->NK);
            }
            break;

         case IP1:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->IP1));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->IP1);
               if (!Field->Ref) {
//                  Field->Ref=GeoRef_RPNSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,(Field->Ref?Field->Ref->LevelType:LVL_UNDEF),(Field->Ref?Field->Ref->Levels:NULL),"X",head->IG1,head->IG2,head->IG3,head->IG4,head->FID?head->FID->Id:-1);
               }
               Field->Ref->Levels[Field->Def->Level]=FSTD_IP2Level(head->IP1,&Field->Ref->LevelType);
            }
            break;

         case NOMVAR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(head->NOMVAR,-1));
            } else {
               strncpy(head->NOMVAR,Tcl_GetString(Objv[++i]),4);
               head->NOMVAR[4]='\0';
               if (Field->Spec->Desc) free(Field->Spec->Desc);
               Field->Spec->Desc=strdup(head->NOMVAR);
            }
            break;

         case CENTER:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(head->CENTER,-1));
            } else {
               strncpy(head->CENTER,Tcl_GetString(Objv[++i]),4);
               head->CENTER[4]='\0';
            }
            break;

         case PROJECTION:
            if (Objc==1) {
               if (Field->Ref && Field->Ref->String)  {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->Ref->String,-1));
               }
            } else {
               ++i;
               if (Field->Ref && Field->Ref->String && strlen(Field->Ref->String)==strlen(Tcl_GetString(Objv[i])) && strcmp(Tcl_GetString(Objv[i]),Field->Ref->String)==0) {
              } else {
                  ref=Field->Ref;
                  if (ref) {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,ref->LevelType,ref->Levels,ref->Grid,ref->IG1,ref->IG2,ref->IG3,ref->IG4,Tcl_GetString(Objv[i]),ref->Transform,ref->InvTransform,NULL);
                     Field->Ref->Grid[1]=ref->Grid[1];
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,LVL_UNDEF,NULL,NULL,0,0,0,0,Tcl_GetString(Objv[i]),NULL,NULL,NULL);
                  }
                  Data_Clean(Field,1,1,1);
               }
            }
            break;

         case TRANSFORM:
            if (Objc==1 && Field->Ref && Field->Ref->Transform) {
               obj=Tcl_NewListObj(0,NULL);
               for(j=0;j<6;j++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Ref->Transform[j]));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Tcl_SplitList(Interp,Tcl_GetString(Objv[++i]),&nidx,&list)==TCL_ERROR) {
                  return TCL_ERROR;
               }

               if (nidx!=6) {
                  Tcl_AppendResult(Interp,"\n   FSTD_FieldDefine: Invalid number of transform element, must be 6 \"",(char*)NULL);
                  return TCL_ERROR;
               }
               for(j=0;j<6;j++) {
                  Tcl_GetDouble(Interp,list[j],&tra[j]);
               }
               Tcl_Free((char*)list);
               tm=tra;
               if (!GDALInvGeoTransform(tra,inv)) {
                  fprintf(stdout,"(WARNING) FSTD_FieldDefine: Unable to generate the inverse transform matrix\n");
                  im=NULL;
               } else {
                  im=inv;
               }
               if (!Field->Ref || !Field->Ref->Transform || memcmp(tm,Field->Ref->Transform,6*sizeof(double))!=0) {
                  ref=Field->Ref;
                  if (ref) {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,ref->LevelType,ref->Levels,ref->Grid,0,0,0,0,ref->String,tm,im,NULL);
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,LVL_UNDEF,NULL,NULL,0,0,0,0,NULL,tm,im,NULL);
                  }
                  Data_Clean(Field,1,1,1);
               }
            }
            break;

         case GEOREF:
            if (Objc==1) {
               if (Field->Ref) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->Ref->Name,-1));
               }
            } else {
               ref=GeoRef_Get(Tcl_GetString(Objv[++i]));
               if (!ref) {
                  Tcl_AppendResult(Interp,"\n   GRIB_FieldDefine: Georef name unknown: \"",Tcl_GetString(Objv[i]),"\"",(char *)NULL);
                  return TCL_ERROR;
               }
               if (Field->Ref && ref!=Field->Ref) {
                  GeoRef_Destroy(Interp,Field->Ref->Name);
                  Data_Clean(Field,1,1,1);
               }
               Field->Ref=ref;
               GeoRef_Incr(Field->Ref);
            }
            break;

         case DATA:
            if (Objc==1) {
               Data_ValGetMatrix(Interp,Field,((FSTD_Head*)Field->Head)->DATYP);
            } else {
               return Data_ValPutMatrix(Interp,Field,Objv[++i]);
            }
            break;
       }
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FieldFree>
 * Creation : Janvier 2007 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Libere l'espace memoire associee a un champ.
 *
 * Parametres :
 *  <Field>   : Pointeur sur une structure de champ.
 *
 * Retour     :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GRIB_FieldFree(TData *Data){

   GRIB_Head *head=(GRIB_Head*)Data->Head;

   if (Data) {
      if (head) {
//         grib_handle_delete(head->Handle);
         free(head);
      }
   }
 }

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FieldRead>
 * Creation : Janvier 2010 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Lit et stocke un enregistrement en memoire de meme qu'initialise
 *            les valeurs de configuraiton par defaut.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *  <Key>     : Cle de l'enregistrement
 *
 * Retour         :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GRIB_FieldRead(Tcl_Interp *Interp,char *Name,char *File,long Key) {

   TData         *field=NULL;
   GRIB_File     *file=NULL;
   GRIB_Head      head;
   grib_keys_iterator *iter;

   OGRSpatialReferenceH         ref,llref=NULL;;
   OGRCoordinateTransformationH func;

   int         err=0;
   long        lval,i,ni=-1,nj=-1,nk=1,date,time,inci,incj;
   size_t      len;
   double      dval;
   char        sval[GRIB_STRLEN];
   double      mtx[6],inv[6],*data;

   /*Get the file*/
   file=GRIB_FileGet(Interp,File);
   if (!file) {
      Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Invalid file \"",File,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

  /*Move to right offset within the file*/
   if (Key>file->Size) {
      Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Invalid field index",(char*)NULL);
      return(TCL_ERROR);
   }
   fseek(file->Handle,Key,SEEK_SET);

   /*Get GRIB handle to message*/
   head.Handle=grib_handle_new_from_file(0,file->Handle,&err);
   if (err!=GRIB_SUCCESS) {
      Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Could not get field handle",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Figure out valid time*/
   grib_get_long(head.Handle,"date",&date);
   grib_get_long(head.Handle,"time",&time);
   head.DATEV=System_DateTime2Seconds(date,time*100,1);

//         fprintf(stderr,"(DEBUG) level :%i %i\n",lval,lval2);
//         head.IP1=FSTD_Level2IP(lval,lval2);
         head.IP1=lval;

   /*Get message info*/
   err=grib_get_long(head.Handle,"numberOfPointsAlongAParallel",&ni);
   err=grib_get_long(head.Handle,"numberOfPointsAlongAMeridian",&nj);
   if (ni==-1) {
      err=grib_get_long(head.Handle,"numberOfPointsAlongXAxis",&ni);
      err=grib_get_long(head.Handle,"numberOfPointsAlongYAxis",&nj);
   }
   err=grib_get_long(head.Handle,"numberOfVerticalCoordinateValues",&nk);
   nk=nk==0?1:nk;

   /*Verifier si le champs existe et est valide*/
   field=Data_Valid(Interp,Name,ni,nj,nk,1,TD_Float32);
   if (!field) {
      return(TCL_ERROR);
   }

   len=FSIZE3D(field->Def);
   data=malloc(len*sizeof(double));
   if ((grib_get_double_array(head.Handle,"values",data,&len))!=GRIB_SUCCESS) {
      Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Could not load data",(char*)NULL);
      return(TCL_ERROR);
   }
   err=grib_get_double(head.Handle,"missingValue",&field->Def->NoData);

   for(i=0;i<FSIZE3D(field->Def);i++) {
      Def_Set(field->Def,0,i,data[i]);
   }
   free(data);

   /*Create grid definition*/
   err=grib_get_long(head.Handle,"gridDefinition",&i);
   memset(mtx,0,6*sizeof(double));
   err=grib_get_double(head.Handle,"latitudeOfFirstGridPointInDegrees",&mtx[3]);
   err=grib_get_double(head.Handle,"longitudeOfFirstGridPointInDegrees",&mtx[0]);

   if (ref=GRIB_WKTProjCS(Interp,head.Handle)) {
      if (OSRIsProjected(ref)) {
         if ((llref=OSRCloneGeogCS(ref))) {
            if ((func=OCTNewCoordinateTransformation(llref,ref))) {
               if (!OCTTransform(func,1,&mtx[0],&mtx[3],NULL)) {
                  fprintf(stderr,"(ERROR) GRIB_FieldRead: unable to project transform origin\n");
               }
            } else {
               fprintf(stderr,"(ERROR) GRIB_FieldRead: Unable to create transform function\n");
            }
            OSRDestroySpatialReference(llref);
         } else {
            fprintf(stderr,"(ERROR) GRIB_FieldRead: Unable to create latlon transform\n");
         }
         err=grib_get_double(head.Handle,"DxInMetres",&mtx[1]);
         err=grib_get_double(head.Handle,"DyInMetres",&mtx[5]);
     } else {
         err=grib_get_double(head.Handle,"iDirectionIncrementInDegrees",&mtx[1]);
         err=grib_get_double(head.Handle,"jDirectionIncrementInDegrees",&mtx[5]);

         /*GRIB1 stored the direction of increment in other parameters and the i-j test are inversed ... so brainless*/
         inci=incj=0;
         err=grib_get_long(head.Handle,"iScansNegatively",&inci);
         err=grib_get_long(head.Handle,"jScansPositively",&incj);

         mtx[1]=inci?-mtx[1]:mtx[1];
         mtx[5]=incj?mtx[1]:-mtx[1];
         /*Patch for a GRIB1 inversion*/
//         if (mtx[3]==90.0) mtx[5]=-mtx[5];
     }

      GDALInvGeoTransform(mtx,inv);
      field->Ref=GeoRef_WKTSetup(ni,nj,nk,LVL_MASL,NULL,NULL,0,0,0,0,NULL,mtx,inv,ref);
      GeoRef_Qualify(field->Ref);

#ifdef DEBUG
      fprintf(stderr,"(DEBUG) GRIB_FieldRead: Dim      : %i %i %i\n",ni,nj,nk);
      fprintf(stderr,"(DEBUG) GRIB_FieldRead: WKTString: '%s'\n",field->Ref->String);
      fprintf(stderr,"(DEBUG) GRIB_FieldRead: WKTMatrix: %f %f %f %f %f %f\n",mtx[0],mtx[1],mtx[2],mtx[3],mtx[4],mtx[5]);
#endif
   }

   err=grib_get_long(head.Handle,"level",&lval);
   field->Ref->Levels[0]=lval;
   err=grib_get_long(head.Handle,"typeOfLevel",&lval);
   switch(lval) {
      case   1:
      case 103:
      case 106: field->Ref->LevelType=LVL_MAGL; break;
      case 100:
      case 108: field->Ref->LevelType=LVL_PRES; break;
      case 101:
      case 102:
      case 160: field->Ref->LevelType=LVL_MASL; break;
      case 104: field->Ref->LevelType=LVL_SIGMA; break;
      case 105: field->Ref->LevelType=LVL_HYBRID; break;
      case 107: field->Ref->LevelType=LVL_THETA; break;
      case 111: field->Ref->LevelType=LVL_ETA; break;
      default: field->Ref->LevelType=LVL_UNDEF;
   }

   len=GRIB_STRLEN;
   grib_get_string(head.Handle,"shortName",head.NOMVAR,&len);

   len=GRIB_STRLEN;
   grib_get_string(head.Handle,"parameterName",sval,&len);
   field->Spec->Desc=strdup(sval);

   len=GRIB_STRLEN;
   grib_get_string(head.Handle,"centre",head.CENTER,&len);

   GRIB_FieldSet(field);
   memcpy(field->Head,&head,sizeof(GRIB_Head));

/*
   fprintf(stderr,"\n\n\n-------------------\n");
   iter=grib_keys_iterator_new(head.Handle,0,NULL);
   while(grib_keys_iterator_next(iter)){
      switch(grib_keys_iterator_get_native_type(iter)) {
         case GRIB_TYPE_LONG:
            grib_keys_iterator_get_long(iter,&lval,&len);
            fprintf(stderr,"%s=%ld\n",grib_keys_iterator_get_name(iter),lval);
            break;
         case GRIB_TYPE_DOUBLE:
            grib_keys_iterator_get_double(iter,&dval,&len);
            fprintf(stderr,"%s=%e\n",grib_keys_iterator_get_name(iter),dval);
            break;
         case GRIB_TYPE_STRING:
            grib_get_string(head.Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"string ----------- <<<<<<<<< %s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
         case GRIB_TYPE_BYTES:
            grib_get_string(head.Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"bytes ----------- >>>>>>>> %s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
         case GRIB_TYPE_SECTION:
            grib_get_string(head.Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"%s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
         case GRIB_TYPE_LABEL:
            grib_keys_iterator_get_string(iter,sval,&len);
            grib_get_string(head.Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"label ---------- %s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
      }
   }
   grib_keys_iterator_delete(iter);
*/
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FieldList>
 * Creation : Janvier 2010 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liste les champs disponibles dans un fichier standard.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <File>    : Fichier grib
 *  <Mode>    : Type d'information
 *  <Var>     : Variable specifique requise
 *
 * Retour         :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GRIB_FieldList(Tcl_Interp *Interp,GRIB_File *File,int Mode,char *Var){

   GRIB_Head      head;
   grib_handle   *handle;
   Tcl_Obj       *list,*obj;
   int            err=0;
   size_t         len;
   long           offset=0,size=0,date,time,lval,lval2,ni=-1,nj=-1,nk=1,type;
   char           buf[1024];

   list=Tcl_NewListObj(0,NULL);
   obj=Tcl_NewObj();

   /*Loop on the messages*/
   while(handle=grib_handle_new_from_file(0,File->Handle,&err)) {

      len=GRIB_STRLEN;
      grib_get_string(handle,"shortName",head.NOMVAR,&len);

      /*Check for var if provided*/
      if (!Var || strcmp(Var,head.NOMVAR)==0) {

         err=grib_get_long(handle,"GRIBEditionNumber",&lval);
         head.Version=lval;

         err=grib_get_long(handle,"date",&date);
         err=grib_get_long(handle,"time",&time);
         err=grib_get_long(handle,"numberOfPointsAlongAParallel",&ni);
         err=grib_get_long(handle,"numberOfPointsAlongAMeridian",&nj);
         if (ni==-1) {
            err=grib_get_long(handle,"numberOfPointsAlongXAxis",&ni);
            err=grib_get_long(handle,"numberOfPointsAlongYAxis",&nj);
         }
         err=grib_get_long(handle,"numberOfVerticalCoordinateValues",&nk);
         nk=nk==0?1:nk;

         err=grib_get_long(handle,"typeOfGeneratingProcess",&type);
         switch(type) {
            case 0: type='A';
            case 1: type='I';
            case 2: type='P';
            case 3: type='P';
            case 4: type='P';
            case 5: type='P';
            case 6: type='P';
            case 7: type='E';
            case 8: type='O';
            default: type='X';
         }

         err=grib_get_long(handle,"level",&lval);
         err=grib_get_long(handle,"typeOfLevel",&lval2);
         switch(lval2) {
            case   1:
            case 103:
            case 106: lval2=LVL_MAGL; break;
            case 100:
            case 108: lval2=LVL_PRES; break;
            case 101:
            case 102:
            case 160: lval2=LVL_MASL; break;
            case 104: lval2=LVL_SIGMA; break;
            case 105: lval2=LVL_HYBRID; break;
            case 107: lval2=LVL_THETA; break;
            case 111: lval2=LVL_ETA; break;
            default: lval2=LVL_UNDEF;
         }

//         fprintf(stderr,"(DEBUG) level :%i %i\n",lval,lval2);
//         head.IP1=FSTD_Level2IP(lval,lval2);
         head.IP1=lval;

         /*Calculer la date de validitee du champs*/
         date=date<=1231?date+19800000:date;
         head.DATEV=System_DateTime2Seconds(date,time*100,1);

         switch(Mode) {
            case FSTD_LISTALL:
               snprintf(buf,1024,"%s %li {%s} {%li} %i %i %i GRIB%i %09li %09li %li %li %li",
                  File->Id,offset,head.NOMVAR,type,head.IP1,0,0,head.Version,head.DATEV,head.DATEV,ni,nj,nk);
               Tcl_SetStringObj(obj,buf,-1);
               Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               break;

            case FSTD_LISTVAR:
               Tcl_SetStringObj(obj,head.NOMVAR,-1);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                  Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;

            case FSTD_LISTDATEV:
               Tcl_SetLongObj(obj,head.DATEV);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                  Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;

            case FSTD_LISTIP1:
               Tcl_SetIntObj(obj,head.IP1);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                     Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;
         }
      }
      size=offset=ftell(File->Handle);
   }
   File->Size=size;

   /*Error on handle access*/
   if (handle) {
      err=grib_handle_delete(handle);

      if (err!=GRIB_SUCCESS) {
         Tcl_AppendResult(Interp,"GRIB_FileOpen: Unable to free grib handle ",File->Id,(char*)NULL);
         return(TCL_ERROR);
      }
   }

   Tcl_DecrRefCount(obj);
   Tcl_SetObjResult(Interp,list);
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_WKTProjCS>
 * Creation : Novembre 2010 - E. Legault-Ouellet - J.P. Gauthier - CMC/CMOE
 *
 * But      : Point d'entree pour generer un string WKT.
 *
 * Parametres :
 *  <Interp>      : Interpreteur TCL
 *  <grib_handle> : Handle sur le message grib.
 *  <WKTstr>      : String WKT a remplir.
 *  <WKTlen>      : Longueur de WKTstr.
 *
 * Retour:
 *    <TCL_OK>    : Le tag a ete ajoute avec succes.
 *    <TCL_ERROR> : Une erreur est survenue avant l'ajout du tag.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
OGRSpatialReferenceH GRIB_WKTProjCS(Tcl_Interp* Interp,grib_handle* Handle) {

   OGRSpatialReferenceH ref;
   int    err,opt=0;
   size_t len=64;
   char   gridType[64],buf[32];
   double lat,lon,scale,scale2;
   long   gribVer,lval;

   enum gridOpt { REGULAR_LL,REDUCED_LL,ROTATED_LL,STRETCHED_LL,STRETCHED_ROTATED_LL,MERCATOR,POLAR_STEREOGRAPHIC,LAMBERT,ALBERS,REGULAR_GG,REDUCED_GG,ROTATED_GG,STRETCHED_GG,STRETCHED_ROTATED_GG,SH,ROTATED_SH,STRETCHED_SH,STRETCHED_ROTATED_SH,SPACE_VIEW,TRIANGULAR_GRID,EQUATORIAL_AZIMUTHAL_EQUIDISTANT,AZIMUTH_RANGE,IRREGULAR_LATLON,LAMBERT_AZIMUTHAL_EQUAL_AREA,CROSS_SECTION,HOVMOLLER,TIME_SECTION,UNKNOWN,UNKNOWN_PLPRESENT };
   static CONST char *gridTypes[] = { "regular_ll","reduced_ll","rotated_ll","stretched_ll","stretched_rotated_ll","mercator","polar_stereographic","lambert","albers","regular_gg","reduced_gg","rotated_gg","stretched_gg","stretched_rotated_gg","sh","rotated_sh","stretched_sh","stretched_rotated_sh","space_view","triangular_grid","equatorial_azimuthal_equidistant","azimuth_range","irregular_latlon","lambert_azimuthal_equal_area","cross_section","Hovmoller","time_section","unknown","unknown_PLPresent",NULL };


   // GRIB version is needed since we do not deal the same way with both file types
   if (grib_get_long(Handle,"GRIBEditionNumber",&gribVer)!=GRIB_SUCCESS) {
      Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get GRIB version.",(char*)NULL);
      return(NULL);
   }

   // Get gridType Index
   if (err=grib_get_string(Handle,"gridType",gridType, &len)) {
      Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Could not get gridType",(char*)NULL);
      return(NULL);
   }

   for (opt=0;gridTypes[opt]!=NULL;opt++) {
      if (!strncmp(gridType,gridTypes[opt],64)) {
         break;
      }
   }

   if (!gridTypes[opt]) {
      Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Invalid gridType \"",gridType,"\"",(char*)NULL);
      return(NULL);
   }

   ref=OSRNewSpatialReference(NULL);

   switch(opt) {
      case REDUCED_LL :             /* same as REGULAR_LL, but extra parameters are unsupported at the moment */
      case ROTATED_LL :             /* same as REGULAR_LL, but extra parameters are unsupported at the moment */
      case STRETCHED_LL :           /* same as REGULAR_LL, but extra parameters are unsupported at the moment */
      case STRETCHED_ROTATED_LL :   /* same as REGULAR_LL, but extra parameters are unsupported at the moment */
      case REGULAR_LL :
          /* Equirectangular spherical (EPSG:9823) or elliptical (EPSG:9842)*/
         if (grib_get_double(Handle,"latitudeOfFirstGridPointInDegrees",&lat)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get latitudeOfFirstGridPointInDegrees",(char*)NULL);
            return(NULL);
         }
         if (grib_get_double(Handle,"longitudeOfFirstGridPointInDegrees",&lon)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get longitudeOfFirstGridPointInDegrees",(char*)NULL);
            return(NULL);
         }
         //OSRSetEquirectangular2(ref,lat,lon,0.0,0.0,0.0);
         break;

      case MERCATOR :
         /* Mercator 1SP (ESPG:9804) or Transverse Mercator (ESPG:9807) */
        if (grib_get_double(Handle,"LaDInDegrees",&lat)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get LaDInDegrees",(char*)NULL);
            return(NULL);
         }
         if (grib_get_double(Handle,"orientationOfTheGridInDegrees",&lon)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get orientationOfTheGridInDegrees",(char*)NULL);
            return(NULL);
         }
         OSRSetMercator(ref,lat,lon,1.0,0.0,0);
         break;

      case LAMBERT :
        if (grib_get_double(Handle,"Latin1InDegrees",&scale)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get Latin1InDegrees",(char*)NULL);
            return(NULL);
         }
        if (grib_get_double(Handle,"LaDtin2InDegrees",&scale2)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get LaDtin2InDegreess",(char*)NULL);
            return(NULL);
         }
         if (grib_get_double(Handle,"orientationOfTheGridInDegrees",&lon)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get orientationOfTheGridInDegrees",(char*)NULL);
            return(NULL);
         }
         OSRSetLCC(ref,scale,scale2,0.0,lon,0.0,0.0);
         break;

      case POLAR_STEREOGRAPHIC :
         /* Equirectangular spherical (EPSG:9823) or elliptical (EPSG:9842)*/
         if (grib_get_double(Handle,"LaDInDegrees",&lat)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get LaDInDegrees",(char*)NULL);
            return(NULL);
         }
         if (grib_get_double(Handle,"orientationOfTheGridInDegrees",&lon)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get orientationOfTheGridInDegrees",(char*)NULL);
            return(NULL);
         }
         if (grib_get_double(Handle,"projectionCentreFlag",&scale)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get projectionCentreFlag",(char*)NULL);
            return(NULL);
         }
         scale=scale==1?-90:90;
         OSRSetPS(ref,lat,lon,scale,0.0,0.0);
         break;

      case ALBERS :
      case REGULAR_GG :
      case REDUCED_GG :
      case ROTATED_GG :
      case STRETCHED_GG :
      case STRETCHED_ROTATED_GG :
      case SH :
      case ROTATED_SH :
      case STRETCHED_SH :
      case STRETCHED_ROTATED_SH :
      case SPACE_VIEW :
      case TRIANGULAR_GRID :
      case EQUATORIAL_AZIMUTHAL_EQUIDISTANT :
      case AZIMUTH_RANGE :
      case IRREGULAR_LATLON :
      case LAMBERT_AZIMUTHAL_EQUAL_AREA :
      case CROSS_SECTION :
      case HOVMOLLER :
      case TIME_SECTION :
      case UNKNOWN :
      case UNKNOWN_PLPRESENT :
      default :
         Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Unsupported gridType \"",gridTypes[opt],"\"",(char*)NULL);
         return(NULL);
   }

   // Get semiMajorAxis and inverseFlattening values
   if (gribVer==14) {
      if (grib_get_long(Handle,"earthIsOblate",&lval)!=GRIB_SUCCESS) {
         Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get earthIsOblate.",(char*)NULL);
         return(NULL);
      }
   } else {
      if (grib_get_long(Handle,"shapeOfTheEarth",&lval)!=GRIB_SUCCESS) {
         Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get shapeOfTheEarth",(char*)NULL);
         return(NULL);
      }

      switch(lval) {
         case 0 :
            OSRSetGeogCS(ref,"Coordinate System imported from GRIB","Unknown","Sphere",6367470.0,0.0,NULL,0.0,NULL,0.0);
            break;
         case 2 :
            OSRSetGeogCS(ref,"Coordinate System imported from GRIB","Unknown","IAU 1965",6378160.0,297.0,NULL,0.0,NULL,0.0);
            break;
         case 4 :
            OSRSetGeogCS(ref,"Coordinate System imported from GRIB","Unknown","IAG-GRS80",6378137.0,298.257222101,NULL,0.0,NULL,0.0);
            break;
         case 5 :
            OSRSetWellKnownGeogCS(ref,"WGS84");
            break;
         case 6 :
            OSRSetGeogCS(ref,"Coordinate System imported from GRIB","Unknown","Sphere",6371229.0,0.0,NULL,0.0,NULL,0.0);
            break;
         case 1 :
            /* User defined (w/ scale factor); earth is spherical (1/f=1.0) */
         case 3 :
            /* User defined major+minor axis IN KM (1/f = 1/((major-minor)/major) */
         case 7 :
            /* User defined major+minor axis IN M (1/f = 1/((major-minor)/major) */
         case 8 :
            OSRSetGeogCS(ref,"Coordinate System imported from GRIB","Unknown","Sphere",6371200.0,0.0,NULL,0.0,NULL,0.0);
         default :
            sprintf(buf,"%ld", lval);
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Unsupported shapeOfTheEarth \"",buf,"\"",(char*)NULL);
            return(NULL);
      }
   }

   return(ref);
}
