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
#ifdef HAVE_GRIB

#include "App.h"
#include "tclGRIB.h"
#include "tclGeoRef.h"
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

   TGRIBHeader *head;

   if (Data->Head && Data->Free)
      Data->Free(Data);

   head=(TGRIBHeader*)malloc(sizeof(TGRIBHeader));

   if (head) {
      head->FID=NULL;
      head->Handle=NULL;
      head->Version=0;
      head->NOMVAR[0]='\0';
      head->KEY=0;
      head->IP1=0;
      head->DATEV=0;
      head->DATEO=0;
   }
   
   /*Initialiser les parametres de definition du champs*/
   Data->Type=TD_GRIB;
   Data->Head=head;
   Data->Set=GRIB_FieldSet;
   Data->Free=GRIB_FieldFree;
   Data->Copy=GRIB_HeadCopy;
   Data->Grid=GRIB_Grid;
   Data->ReadCube=NULL;
   Data->Define=GRIB_FieldDefine;
}

void GRIB_HeadCopy(void *To,void *From) {
   memcpy((TGRIBHeader*)To,(TGRIBHeader*)From,sizeof(TGRIBHeader));
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

   Coord      coord;
   double     z;
   int        i,j,idx;

   /*Verifier la validite de la grille*/
   if (!Field->GRef || Field->GRef->Type==GRID_NONE)
      return(NULL);

   if (Field->GPos && Field->GPos->Pos[Level])
      return(Field->GPos->Pos[Level]);

   // Allocate memory for gridpoint positions
   if (!Field->GPos)
      Field->GPos=GeoPos_Find(Field->GRef,Field->ZRef);

   if (!Field->GPos)
      return(NULL);

   if (!Field->GPos->Pos[Level]) {
      Field->GPos->Pos[Level]=(Vect3d*)malloc(FSIZE2D(Field->Def)*sizeof(Vect3d));
      if (!Field->GPos->Pos[Level]) {
         App_Log(ERROR,"%s: Not enough memory to calculate gridpoint location",__func__);
         return(NULL);
      }
   }

   z=ZRef_Level2Meter(Field->ZRef->Levels[Level],Field->ZRef->Type);
   for (i=0;i<Field->Def->NI;i++) {
      for (j=0;j<Field->Def->NJ;j++) {

         idx=j*Field->Def->NI+i;
         coord.Elev=0.0;

         /*Reproject coordinates if needed*/
         Field->GRef->Project(Field->GRef,i,j,&coord.Lat,&coord.Lon,1,1);

         if (Field->GRef->Hgt) {
            coord.Elev+=ZRef_Level2Meter(Field->GRef->Hgt[idx],Field->ZRef->Type);
         } else {
            coord.Elev+=z;
         }
         coord.Elev*=Field->Spec->TopoFactor;

         /*Si les positions sont hors domaine, outter space*/
         if (coord.Lat<-900.0 || coord.Lon<-900.0) {
            coord.Elev=1e32;
         }
         Vect_Init(Field->GPos->Pos[Level][idx],coord.Lon,coord.Lat,coord.Elev);
      }
   }
   ((Projection*)Proj)->Type->Project(Proj,(GeoVect*)Field->GPos->Pos[Level],NULL,FSIZE2D(Field->Def));

   return(Field->GPos->Pos[Level]);
}

static int GRIB_KeySet(Tcl_Interp *Interp,grib_handle *Handle,char *Key,Tcl_Obj *Val) {
   
   char   sval[1024];
   double dval;
   long   lval;
   size_t len;
   
   if (Tcl_GetLongFromObj(Interp,Val,&lval)==TCL_OK) {
      grib_set_long(Handle,Key,lval);
      grib_get_long(Handle,Key,&lval);
      Tcl_SetObjResult(Interp,Tcl_NewIntObj(lval));                  
   } else if (Tcl_GetDoubleFromObj(Interp,Val,&dval)==TCL_OK) {
      grib_set_double(Handle,Key,dval);
      grib_get_double(Handle,Key,&dval);
      Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(dval));
   } else {
      grib_set_string(Handle,Key,Tcl_GetString(Val),&len);
      len=1024;
      grib_get_string(Handle,Key,sval,&len);
      Tcl_SetObjResult(Interp,Tcl_NewStringObj(sval,-1));                  
   }
   return(TCL_OK);
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
   TGRIBHeader *head=(TGRIBHeader*)Field->Head;
   TGeoRef     *ref;
   int          i,j,idx,nidx,n,nobj,type=0;
   double       tra[6],inv[6],*tm,*im;
   const char **list;
 
   unsigned long key_iterator_filter_flags=GRIB_KEYS_ITERATOR_SKIP_READ_ONLY || GRIB_KEYS_ITERATOR_SKIP_COMPUTED;
//               char* name_space="ls";
   grib_keys_iterator* kiter=NULL;
   char   *key,sval[1024];
   double dval;
   long   lval;
   size_t len;

   static CONST char *sopt[] = { "-DATEO","-DATEV","-FID","-NI","-NJ","-NK","-IP1","-NOMVAR","-GRTYP","-DATA","-projection","-transform","-georef","-key",NULL };
   enum        opt { DATEO,DATEV,FID,NI,NJ,NK,IP1,NOMVAR,GRTYP,DATA,PROJECTION,TRANSFORM,GEOREF,KEY };

   if (!Field) {
      Tcl_AppendResult(Interp,"Invalid field",(char*)NULL);
      return TCL_ERROR;
   }

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
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
               Field->ZRef->Levels[Field->Def->Level]=ZRef_IP2Level(head->IP1,&Field->ZRef->Type);
            }
            break;

         case NOMVAR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(head->NOMVAR,-1));
            } else {
               strncpy(head->NOMVAR,Tcl_GetString(Objv[++i]),GRIB_STRLEN);
               if (Field->Spec && !Field->Spec->Desc)
                  Field->Spec->Desc=strdup(head->NOMVAR);
            }
            break;

         case GRTYP:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->GRef->Grid,-1));
            } else {
            }
            break;
            
         case PROJECTION:
            if (Objc==1) {
               if (Field->GRef && Field->GRef->String)  {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->GRef->String,-1));
               }
            } else {
               ++i;
               if (Field->GRef && Field->GRef->String && strlen(Field->GRef->String)==strlen(Tcl_GetString(Objv[i])) && strcmp(Tcl_GetString(Objv[i]),Field->GRef->String)==0) {
              } else {
                  ref=Field->GRef;
                  if (ref) {
                     Field->GRef=GeoRef_Find(GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,ref->Grid,ref->IG1,ref->IG2,ref->IG3,ref->IG4,Tcl_GetString(Objv[i]),ref->Transform,ref->InvTransform,NULL));
                     Field->GRef->Grid[1]=ref->Grid[1];
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     Field->GRef=GeoRef_Find(GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,NULL,0,0,0,0,Tcl_GetString(Objv[i]),NULL,NULL,NULL));
                  }
                  Data_Clean(Field,1,1,1);
               }
            }
            break;

         case TRANSFORM:
            if (Objc==1 && Field->GRef && Field->GRef->Transform) {
               obj=Tcl_NewListObj(0,NULL);
               for(j=0;j<6;j++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->GRef->Transform[j]));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Tcl_SplitList(Interp,Tcl_GetString(Objv[++i]),&nidx,&list)==TCL_ERROR) {
                  return(TCL_ERROR);
               }

               if (nidx!=6) {
                  Tcl_AppendResult(Interp,"GRIB_FieldDefine: Invalid number of transform element, must be 6 \"",(char*)NULL);
                  return(TCL_ERROR);
               }
               for(j=0;j<6;j++) {
                  Tcl_GetDouble(Interp,list[j],&tra[j]);
               }
               Tcl_Free((char*)list);
               tm=tra;
               if (!GDALInvGeoTransform(tra,inv)) {
                  App_Log(WARNING,"%s: Unable to generate the inverse transform matrix\n",__func__);
                  im=NULL;
               } else {
                  im=inv;
               }
               if (!Field->GRef || !Field->GRef->Transform || memcmp(tm,Field->GRef->Transform,6*sizeof(double))!=0) {
                  ref=Field->GRef;
                  if (ref) {
                     Field->GRef=GeoRef_Find(GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,ref->Grid,0,0,0,0,ref->String,tm,im,NULL));
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     Field->GRef=GeoRef_Find(GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,NULL,0,0,0,0,NULL,tm,im,NULL));
                  }
                  Data_Clean(Field,1,1,1);
               }
            }
            break;

         case GEOREF:
            if (Objc==1) {
               if (Field->GRef) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->GRef->Name,-1));
               }
            } else {
               ref=GeoRef_Get(Tcl_GetString(Objv[++i]));
               if (!ref) {
                  Tcl_AppendResult(Interp,"GRIB_FieldDefine: Georef name unknown: \"",Tcl_GetString(Objv[i]),"\"",(char *)NULL);
                  return(TCL_ERROR);
               }
               if (Field->GRef && ref!=Field->GRef) {
                  GeoRef_Destroy(Interp,Field->GRef->Name);
                  Data_Clean(Field,1,1,1);
               }
               Field->GRef=ref;
               GeoRef_Incr(Field->GRef);
            }
            break;

         case DATA:
            nidx=0;
            
            if (Objc>1) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&nidx);
            }
            
            if (Objc>2) {
               return Data_ValPutMatrix(Interp,Field,nidx,Objv[++i]);
            } else {
               Data_ValGetMatrix(Interp,Field,nidx,0);
            }
            break;
            
         case KEY:
            if (Objc==1) {
              
               obj=Tcl_NewListObj(0,NULL);
               kiter=grib_keys_iterator_new(head->Handle,key_iterator_filter_flags,NULL);
               if (!kiter) {
                  Tcl_AppendResult(Interp,"GRIB_FieldDefine: Unable to initialize key iterator",(char *)NULL);
                  return(TCL_ERROR);
               }
         
               while(grib_keys_iterator_next(kiter)) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(grib_keys_iterator_get_name(kiter),-1));
               }
         
               grib_keys_iterator_delete(kiter);
               Tcl_SetObjResult(Interp,obj);
               return(TCL_OK);
            }
            
            // Check for a list of key/value pair
            Tcl_ListObjLength(Interp,Objv[++i],&nobj);
            
            if (nobj>1) {
               if (nobj%2) {
                  Tcl_AppendResult(Interp,"GRIB_FieldDefine: Invalid key/value pairing",(char*)NULL);
                  return(TCL_ERROR);
               }
               
               for(n=0;n<nobj;n+=2) {
                  Tcl_ListObjIndex(Interp,Objv[i],n,&obj);
                  key=Tcl_GetString(obj);
                  Tcl_ListObjIndex(Interp,Objv[i],n+1,&obj);
                  GRIB_KeySet(Interp,head->Handle,key,obj);
               }
            } else {
               
               key=Tcl_GetString(Objv[i]);
               
               // Check fo type overloading
               len=(long)key+strlen(key);
               if (key[0]=='(') {
                  // Check for specified type
                  if (key[1]=='S') type=1;   // Strings
                  if (key[1]=='I') type=2;   // Integer
                  if (key[1]=='R') type=3;   // Real
                  while(*(key++)!=')' && (size_t)key<len);
               } else {
                  // Try to get the native key type
                  grib_get_native_type(head->Handle,key,&type);               
               }

               if (Objc==2) {
                  // Only one key, provide value
                  switch(type) {
                     case 1: grib_get_string(head->Handle,key,sval,&len);
                           Tcl_SetObjResult(Interp,Tcl_NewStringObj(sval,-1));
                           break;
                           
                     case 2: grib_get_long(head->Handle,key,&lval);
                           Tcl_SetObjResult(Interp,Tcl_NewLongObj(lval));
                           break;
                           
                     case 3: 
                     default:grib_get_double(head->Handle,key,&dval);
                           Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(dval));
                           break;
                  }
               } else if (Objc==3) {
                  // A key and a value
                  GRIB_KeySet(Interp,head->Handle,key,Objv[++i]);
               }
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
 *  <Datad>   : Pointeur sur une structure de champ.
 *
 * Retour     :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void GRIB_FieldFree(TData *Data){

   TGRIBHeader *head=(TGRIBHeader*)Data->Head;

   if (Data) {
      if (head) {
//         grib_handle_delete(head->Handle);
         free(head);
      }
   }
 }

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_GetLevel>
 * Creation : Mai 2012 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Libere l'espace memoire associee a un champ.
 *
 * Parametres :
 *  <Head>      : GRIB field header object
 *  <Level>     : Returned level
 *  <LevelType> : Return type of level
 *
 * Retour     :
 *  <Ok>      : Code d'erreur
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GRIB_GetLevel(TGRIBHeader *Head,float *Level,int *LevelType){

   long   lval,lval2;
   int    err=0;
   float  lvl;

   if ((err=grib_get_long(Head->Handle,"vertical.level",&lval))) {
      return(0);
   }
   lvl=lval;

   if (Head->Version==1) {
      err=grib_get_long(Head->Handle,"indicatorOfTypeOfLevel",&lval2);
      switch(lval2) {
         case   1: lval2=LVL_MAGL; break;
         case 125: lval2=LVL_MAGL; lvl/=100; break;
         case 111:
         case 112: lval2=LVL_MAGL; lvl=-lvl/100; break;
         case 100:
         case 101:
         case 115:
         case 116: lval2=LVL_PRES; break;
         case 121: lval2=LVL_PRES; lvl=lvl+1100;break;
         case 210: lval2=LVL_PRES; lvl*=100;break;
         case 102:
         case 103:
         case 104: lval2=LVL_MASL; break;
         case 160: lval2=LVL_MASL; lvl=-lvl; break;
         case 107: lval2=LVL_SIGMA; lvl/=10000.0; break;
         case 108: lval2=LVL_SIGMA; lvl/=100.0;   break;
         case 128: lval2=LVL_SIGMA; lvl=1.1-lvl/1000.0;   break;
         case 109:
         case 110: lval2=LVL_HYBRID; break;
         case 113:
         case 114: lval2=LVL_THETA;  break;
         case 119: lval2=LVL_ETA;  lvl/=10000.0; break;
         case 120: lval2=LVL_ETA;  lvl/=100.0; break;
         default: lval2=LVL_UNDEF;
      }
   } else {
      err=grib_get_long(Head->Handle,"vertical.typeOfLevel",&lval2);
      if (!lval2)
         err=grib_get_long(Head->Handle,"typeOfFirstFixedSurface",&lval2);

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
   }
   *Level=lvl;
   *LevelType=lval2;

   App_Log(DEBUG,"%s: %f %li\n",__func__,lvl,lval2);

   return(err==0);
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_GetData>
 * Creation : Mai 2012 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Read the data matrix
 *
 * Parametres :
 *  <Head>      : GRIB field header object
 *  <Def>       : Data definition object
 *  <Idx>       : Vectorial data index
 *  <Factor>    : Factor to be aplied to data
 *
 * Retour     :
 *  <Ok>      : Code d'erreur
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GRIB_GetData(TGRIBHeader *Head,TDef *Def,int Idx,double Factor){

   size_t  i,len;
   double *data;

   len=FSIZE3D(Def);
   if (!(data=(double*)malloc(len*sizeof(double))))
      return(0);
   
   if ((grib_get_double_array(Head->Handle,"values",data,&len))!=GRIB_SUCCESS) {
      return(0);
   }
   grib_get_double(Head->Handle,"missingValue",&Def->NoData);

   for(i=0;i<FSIZE3D(Def);i++) {
      if (Factor!=0.0)
         data[i]*=Factor;
      Def_Set(Def,Idx,i,data[i]);
   }
   free(data);

   return(1);
}

TGRIBHeader *GRIB_FieldFind(TGRIBFile *File,int DATEV,int IP1,char* NOMVAR) {

   TGRIBHeader *head=NULL;
   int  h=0;

   while(File->Table[h].KEY!=-1) {
      if (File->Table[h].DATEV==DATEV && File->Table[h].IP1==IP1 && strcmp(File->Table[h].NOMVAR,NOMVAR)==0) {
         head=&File->Table[h];
         break;
      }
      h++;
   }

   return(head);
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
   TDataVector   *uvw=NULL;;
   TGRIBFile     *file=NULL;
   TGRIBHeader   *head,*h;

   int         idx;
   long        ni=-1,nj=-1,nk=1;
   size_t      len;
   char        sval[GRIB_STRLEN];
//   grib_keys_iterator *iter;
//   long        lval
//   double      dval;

  /*Get the file*/
   file=GRIB_FileGet(Interp,File);
   if (!file) {
      Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Invalid file \"",File,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Get GRIB handle to message*/
   head=&file->Table[Key];

   /*Move to right offset within the file*/
   if (head->KEY>file->Size) {
      Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Invalid field index",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Get dimensions*/
   grib_get_long(head->Handle,"numberOfPointsAlongAParallel",&ni);
   grib_get_long(head->Handle,"numberOfPointsAlongAMeridian",&nj);
   if (ni==-1) {
      grib_get_long(head->Handle,"numberOfPointsAlongXAxis",&ni);
      grib_get_long(head->Handle,"numberOfPointsAlongYAxis",&nj);
   }
   grib_get_long(head->Handle,"numberOfVerticalCoordinateValues",&nk);
   nk=nk==0?1:nk;

   /*Champs vectoriel ???*/
   if ((uvw=Data_VectorTableCheck(head->NOMVAR,&idx)) && uvw->VV) {
      field=Data_Valid(Interp,Name,ni,nj,nk,(uvw->WW?3:2),TD_Float32);
      if (!field) {
         return(TCL_ERROR);
      }

      /*Recuperer les donnees du champs*/
      if (!GRIB_GetData(head,field->Def,idx,0.0)) {
         Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Could not load data",(char*)NULL);
         return(TCL_ERROR);
      }
      strcpy(head->NOMVAR,uvw->UU);

      if (uvw->UU && idx!=0) {
         if ((h=GRIB_FieldFind(file,head->DATEV,head->IP1,uvw->UU))) {
            if (!GRIB_GetData(h,field->Def,0,0.0)) {
               Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Could not load data: ",uvw->UU,(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            Tcl_AppendResult(Interp,"GRIB_FieldRead: Could not find first component field: ",uvw->UU,(char*)NULL);
            return(TCL_ERROR);
         }
      }
      if (uvw->VV && idx!=1) {
         if ((h=GRIB_FieldFind(file,head->DATEV,head->IP1,uvw->VV))) {
            if (!GRIB_GetData(h,field->Def,1,0.0)) {
               Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Could not load data: ",uvw->VV,(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            Tcl_AppendResult(Interp,"GRIB_FieldRead: Could not find second component field: ",uvw->VV,(char*)NULL);
            return(TCL_ERROR);
         }
      }
      if (uvw->WW && idx!=2) {
         if ((h=GRIB_FieldFind(file,head->DATEV,head->IP1,uvw->WW))) {
            if (!GRIB_GetData(h,field->Def,2,uvw->WWFactor)) {
               Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Could not load data: ",uvw->WW,(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            Tcl_AppendResult(Interp,"GRIB_FieldRead: Could not find third component field: ",uvw->WW,(char*)NULL);
            return(TCL_ERROR);
         }
      }
   } else {
      /*Verifier si le champs existe et est valide*/
      field=Data_Valid(Interp,Name,ni,nj,nk,1,TD_Float32);
      if (!field) {
         return(TCL_ERROR);
      }
      if (!GRIB_GetData(head,field->Def,0,0.0)) {
         Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Could not load data",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   GRIB_FieldSet(field);
   
   memcpy(field->Head,head,sizeof(TGRIBHeader));
   if (field->Spec->Desc)
      free(field->Spec->Desc);

   // Create grid definition
   GRIB_GridGet(Interp,field,ni,nj,nk);

   len=GRIB_STRLEN;
   grib_get_string(head->Handle,"parameterName",sval,&len);
   field->Spec->Desc=strdup(sval);

   len=GRIB_STRLEN;
   grib_get_string(head->Handle,"parameterUnits",sval,&len);
   field->Spec->Unit=strdup(sval);

#ifdef DEBUG
/*
   fprintf(stderr,"\n\n\n-------------------\n");
   iter=grib_keys_iterator_new(head->Handle,0,NULL);
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
            grib_get_string(head->Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"string ----------- <<<<<<<<< %s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
         case GRIB_TYPE_BYTES:
            grib_get_string(head->Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"bytes ----------- >>>>>>>> %s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
         case GRIB_TYPE_SECTION:
            grib_get_string(head->Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"%s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
         case GRIB_TYPE_LABEL:
            grib_keys_iterator_get_string(iter,sval,&len);
            grib_get_string(head->Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"label ---------- %s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
      }
   }
   grib_keys_iterator_delete(iter);
*/
#endif

   return(TCL_OK);
}

int GRIB_GridGet(Tcl_Interp *Interp,TData *Field,int NI,int NJ,int NK) {

   TGRIBHeader *head=(TGRIBHeader*)Field->Head;

   OGRSpatialReferenceH         ref,llref=NULL;;
   OGRCoordinateTransformationH func;

   long        i,inci,incj;
   double      mtx[6],inv[6];
   
   // Create grid definition
   grib_get_long(head->Handle,"gridDefinition",&i);
   mtx[0]=mtx[1]=mtx[2]=mtx[3]=mtx[4]=mtx[5]=0.0;
   grib_get_double(head->Handle,"latitudeOfFirstGridPointInDegrees",&mtx[3]);
   grib_get_double(head->Handle,"longitudeOfFirstGridPointInDegrees",&mtx[0]);

   // Fix for matrix transformation to work correclty
   if (mtx[0]==180.0) mtx[0]=-180;
   
   if ((ref=GRIB_WKTProjCS(Interp,head->Handle))) {
      if (OSRIsProjected(ref)) {
         if ((llref=OSRCloneGeogCS(ref))) {
            if ((func=OCTNewCoordinateTransformation(llref,ref))) {
               if (!OCTTransform(func,1,&mtx[0],&mtx[3],NULL)) {
                  App_Log(WARNING,"%s: unable to project transform origin\n",__func__);
               }
            } else {
               App_Log(WARNING,"%s: Unable to create transform function\n",__func__);
            }
            OSRDestroySpatialReference(llref);
         } else {
            App_Log(WARNING,"%s: GRIB_GridGet: Unable to create latlon transform\n",__func__);
         }
         grib_get_double(head->Handle,"DxInMetres",&mtx[1]);
         grib_get_double(head->Handle,"DyInMetres",&mtx[5]);
      } else {
         grib_get_double(head->Handle,"iDirectionIncrementInDegrees",&mtx[1]);
         grib_get_double(head->Handle,"jDirectionIncrementInDegrees",&mtx[5]);
         CLAMPLON(mtx[0]);
      }

      // Stored the direction of increment in other parameters and the i-j test are inversed ... so brainless
      inci=incj=0;
      grib_get_long(head->Handle,"iScansNegatively",&inci);
      grib_get_long(head->Handle,"jScansPositively",&incj);
      mtx[1]=inci?-mtx[1]:mtx[1];
      mtx[5]=incj?mtx[5]:-mtx[5];

      if (!GDALInvGeoTransform(mtx,inv)) {
         App_Log(WARNING,"%s: Unable to create inverse transform function\n",__func__);
      }
      Field->GRef=GeoRef_Find(GeoRef_WKTSetup(NI,NJ,NULL,0,0,0,0,NULL,mtx,inv,ref));
      Field->ZRef=ZRef_Define(LVL_MASL,NK,NULL);
      Field->ZRef->Levels[0]=ZRef_IP2Level(head->IP1,&Field->ZRef->Type);

      GeoRef_Size(Field->GRef,0,0,NI-1,NJ-1,0);
      GeoRef_Qualify(Field->GRef);
      Field->GRef->Type|=GRID_NUNORTH;

      App_Log(DEBUG,"%s: WKTString: '%s'\n",__func__,Field->GRef->String);
      App_Log(DEBUG,"%s: WKTMatrix: %f %f %f %f %f %f\n",__func__,mtx[0],mtx[1],mtx[2],mtx[3],mtx[4],mtx[5]);
   } else {
      return(TCL_ERROR);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FieldWrite>
 * Creation : Fevrier 2015 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Inscrit un enregistrement.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *  <Field>   : Champ a ecrire
 *
 * Retour     :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GRIB_FieldWrite(Tcl_Interp *Interp,char *Id,TData *Field,int NPack,int Compress) {

   TGRIBFile   *file;
   TGRIBHeader *head=(TGRIBHeader*)Field->Head;
   size_t      i,size;
   const void *buffer=NULL;   
   double     *data;
   
   // Verifier l'existence du champs
   if (!Field) {
      Tcl_AppendResult(Interp,"GRIB_FieldWrite: Invalid field",(char*)NULL);
      return(TCL_ERROR);
   }
  
    // Verifier l'existence du fichier
   if (!(file=GRIB_FileGet(Interp,Id))) {
      return(TCL_ERROR);
   }
    
   // Update data array
   size=FSIZE2D(Field->Def);
   if (!(data=(double*)malloc(size*sizeof(double)))) {
      Tcl_AppendResult(Interp,"GRIB_FieldWrite: Unable to allocate temporary array",(char*)NULL);
      return(TCL_ERROR);
   }

   for(i=0;i<size;i++) {
      Def_Get(Field->Def,0,i,data[i]);
   }
   grib_set_double_array(head->Handle,"values",data,size);
   grib_set_long(head->Handle,"bitsPerValue",NPack);
   free(data);

   /* write the buffer to a file */
   grib_get_message(head->Handle,&buffer,&size);
   if(fwrite(buffer,1,size,file->Handle)!=size) {
      Tcl_AppendResult(Interp,"GRIB_FieldWrite: Could not write field ",(char*)NULL);
      return(TCL_ERROR);
   }
    
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FieldCreate>
 * Creation : Fevrier 2015 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Creer un enregistrement grib a partir d'une template.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Name>    : Identificateur du champs
 *  <Sample>  : Nom de la template
 *  <NI>      : Dimension en X
 *  <NJ>      : Dimension en Y
 *  <NK>      : Dimension en Z
 *  <Type>    : Type des donnees
 *
 * Retour     :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TData *GRIB_FieldCreate(Tcl_Interp *Interp,char *Name,char *Sample,int NI,int NJ,int NK,TDef_Type Type){

   TData       *field;
   TGRIBHeader *head;
   size_t       len;
   
   field=Data_Valid(Interp,Name,NI,NJ,NK,1,Type==TD_Binary?TD_Byte:Type);

   if (!field)
     return(NULL);

   GRIB_FieldSet(field);
   
   // Initialize from template
   head=(TGRIBHeader*)field->Head;
   if (!(head->Handle=grib_handle_new_from_samples(0,Sample?Sample:"GRIB1"))) {
      Tcl_AppendResult(Interp,"GRIB_FieldCreate: Unable to access template ",Sample,(char*)NULL);
      return(NULL);
      
   }

   // Set some default keys
   grib_set_long(head->Handle,"Ni",field->Def->NI);
   grib_set_long(head->Handle,"Nj",field->Def->NJ);
   grib_set_long(head->Handle,"bitsPerValue",0);
   
   len=4;grib_set_string(head->Handle,"centre","cwao",&len);

   // Create grid definition
   GRIB_GridGet(Interp,field,NI,NJ,NK);

   return(field);
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FieldImport>
 * Creation : Fevrier 2015 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Importer un FSTD dans un GRIB.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Field>   : Identificateur du champs GRIB
 *  <RPN>     : Identificateur du champss FSTD
 *
 * Retour     :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int GRIB_FieldImport(Tcl_Interp *Interp,TData *Field,TData *RPN) {

#ifdef HAVE_RMN   
   TGRIBHeader *head=(TGRIBHeader*)Field->Head;
   TRPNHeader  *rhead=(TRPNHeader*)RPN->Head;
   TCoord co;
   float  xg[4];
   int    yyyy,mm,dd,h,m,s,tmp;
   size_t len;

   // Copy data
   Def_Free(Field->Def);
   Field->Def=Def_Copy(RPN->Def);
  
   // Define time parameters   
   System_StampDecode(rhead->DATEO,&yyyy,&mm,&dd,&h,&m,&s);
   grib_set_long(head->Handle,"dataDate",yyyy*10000+mm*100+dd);
   grib_set_long(head->Handle,"dataTime",h*100+m);
   grib_set_long(head->Handle,"stepUnits",1);
   grib_set_long(head->Handle,"stepRange",rhead->IP2);
   grib_set_long(head->Handle,"startStep",rhead->IP2);
   grib_set_long(head->Handle,"endStep",rhead->IP2);
   grib_set_long(head->Handle,"timeRangeIndicator",10); 

   // Define vertical parameters   
   switch(RPN->ZRef->Type) {
      case LVL_MASL:         tmp=103; break;  //  Meters above sea level
      case LVL_SIGMA:        tmp=107; break;  //  P/Ps
      case LVL_PRES:         tmp=100; break;  //  Pressure mb
      case LVL_MAGL:         tmp=105; break;  //  Meters above ground level
      case LVL_HYBRID:       tmp=109; break;  //  Hybrid levels
      case LVL_THETA:        tmp=113; break;  //  ?
      case LVL_ETA:          tmp=119; break;  //  (Pt-P)/(Pt-Ps) -not in convip
      case LVL_GALCHEN:      tmp=105; break;  //  Original Gal-Chen -not in convip (JP Defined)
          
      case LVL_UNDEF:        tmp=255; break;  //  units are user defined
      case LVL_ANGLE:        tmp=255; break;  //  Radar angles (JP defined)
      case LVL_MPRES:        tmp=255; break;  //  Metres-pression   
      default:               tmp=255; break;
   }
   
   grib_set_long(head->Handle,"indicatorOfTypeOfLevel",tmp); 
   grib_set_double(head->Handle,"level",Field->ZRef->Levels[Field->Def->Level]); 
   
   // Define grid parameters
   grib_set_long(head->Handle,"Ni",RPN->Def->NI);
   grib_set_long(head->Handle,"Nj",RPN->Def->NJ);
   grib_set_long(head->Handle,"projectionCentreFlag",0);
   grib_set_long(head->Handle,"iScansNegatively",0);
   grib_set_long(head->Handle,"jScansPositively",1);
   grib_set_long(head->Handle,"jPointsAreConsecutive",0);
   grib_set_long(head->Handle,"earthIsOblate",0);
   grib_set_long(head->Handle,"uvRelativeToGrid",1);
         
   switch(RPN->GRef->Grid[0]) {
      case 'N':
      case 'S':
         f77name(cigaxg)(RPN->GRef->Grid,&xg[0],&xg[1],&xg[2],&xg[3],&rhead->IG1,&rhead->IG2,&rhead->IG3,&rhead->IG4);
         RPN->GRef->Project(RPN->GRef,0,0,&co.Lat,&co.Lon,0,1);
        
         xg[3]=RPN->GRef->Grid[0]=='N'?(270.0-xg[3]):xg[3]+90.0;
         while(xg[3]<0)    xg[3]+=360;
         while(xg[3]>360 ) xg[3]-=360;
         
         if (co.Lon<0) co.Lon+=360.0;
         
         len=19;
         grib_set_string(head->Handle,"gridType","polar_stereographic",&len);
         grib_set_double(head->Handle,"latitudeOfFirstGridPointInDegrees",co.Lat);
         grib_set_double(head->Handle,"longitudeOfFirstGridPointInDegrees",co.Lon);
         grib_set_double(head->Handle,"LaDInDegrees",60.0);
         grib_set_double(head->Handle,"DxInMetres",xg[2]);
         grib_set_double(head->Handle,"DyInMetres",xg[2]);
         grib_set_double(head->Handle,"orientationOfTheGridInDegrees",xg[3]);
         break;   
         
      case 'L':
         f77name(cigaxg)(RPN->GRef->Grid,&xg[0],&xg[1],&xg[2],&xg[3],&rhead->IG1,&rhead->IG2,&rhead->IG3,&rhead->IG4);
         
         len=10;
         grib_set_string(head->Handle,"gridType","regular_ll",&len);
         grib_set_double(head->Handle,"latitudeOfFirstGridPointInDegrees",xg[0]);
         grib_set_double(head->Handle,"longitudeOfFirstGridPointInDegrees",xg[1]);
         grib_set_double(head->Handle,"latitudeOfLastGridPointInDegrees",xg[0]+xg[2]*(RPN->Def->NJ-1));
         grib_set_double(head->Handle,"longitudeOfLastGridPointInDegrees",xg[1]+xg[3]*(RPN->Def->NI-1));
         grib_set_double(head->Handle,"jDirectionIncrementInDegrees",xg[2]);
         grib_set_double(head->Handle,"iDirectionIncrementInDegrees",xg[3]);
         break;   
         
   }
#else
   App_ErrorSet("%s: Need RMNLIB",__func__);
#endif
   
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
int GRIB_FieldList(Tcl_Interp *Interp,TGRIBFile *File,int Mode,char *Var){

   TGRIBHeader  head,*table;
   Tcl_Obj     *list,*obj;
   int          err=0,lvtyp=0,nb;
   size_t       len;
   long         date,time,step,unit,lval,ni=-1,nj=-1,nk=1,type;
   int          idate,itime;
   char         buf[1024];
   float        lvl;
   const char **units;

   if (Mode==FSTD_LISTNONE) {
      return(TCL_OK);
   }

   list=Tcl_NewListObj(0,NULL);
   obj=Tcl_NewObj();

   /*Loop on the messages*/
   nb=0;
   head.KEY=0;
   head.FID=File;

   units=ZRef_LevelUnits();

   table=File->Table;
   if (!File->Table) {
      File->Table=(TGRIBHeader*)calloc(GRIB_TABLESIZE,sizeof(TGRIBHeader));
   }
//   grib_count_in_file(NULL,File->Handle,&err);
//   App_Log(DEBUG,"%s: %i messages\n",__func__,err);

   while((head.Handle=grib_handle_new_from_file(NULL,File->Handle,&err))) {

      len=GRIB_STRLEN;
      grib_get_string(head.Handle,"shortName",head.NOMVAR,&len);

      /*Check for var if provided*/
      if (!Var || strcmp(Var,head.NOMVAR)==0) {

         grib_get_long(head.Handle,"GRIBEditionNumber",&lval);
         head.Version=lval;

         grib_get_long(head.Handle,"dataDate",&date);
         grib_get_long(head.Handle,"dataTime",&time);
         grib_get_long(head.Handle,"stepUnits",&unit);
         grib_get_long(head.Handle,"stepRange",&step);
         
         grib_get_long(head.Handle,"numberOfPointsAlongAParallel",&ni);
         grib_get_long(head.Handle,"numberOfPointsAlongAMeridian",&nj);
         if (ni==-1) {
            grib_get_long(head.Handle,"numberOfPointsAlongXAxis",&ni);
            grib_get_long(head.Handle,"numberOfPointsAlongYAxis",&nj);
         }
         grib_get_long(head.Handle,"numberOfVerticalCoordinateValues",&nk);
         nk=nk==0?1:nk;

         grib_get_long(head.Handle,"typeOfGeneratingProcess",&type);
         switch(type) {
            case 0: type='A';break;
            case 1: type='I';break;
            case 2: type='P';break;
            case 3: type='P';break;
            case 4: type='P';break;
            case 5: type='P';break;
            case 6: type='P';break;
            case 7: type='E';break;
            case 8: type='O';break;
            case 9: type='C';break;
            case 10: type='P';break;
            case 11: type='P';break;
            default: type='X';
         }

         GRIB_GetLevel(&head,&lvl,&lvtyp);
         head.IP1=ZRef_Level2IP(lvl,lvtyp,DEFAULT);

         /*Calculer la date de validitee du champs*/
         date=date<=1231?date+19800000:date;
         
         switch(unit) {
            case   0: unit=60;break;
            case   1: unit=3600;break;
            case   2: unit=86400;break;
            case  10: unit=10800;break;
            case  11: unit=21600;break;
            case  12: unit=43200;break;
            case 254: unit=1;break;
            default : unit=0;
         }
         
         head.DATEO=System_DateTime2Seconds(date,time*100,1);
         head.DATEV=head.DATEO+step*unit;
         System_Seconds2DateTime(head.DATEV,&idate,&itime,1);

         switch(Mode) {
            case FSTD_LISTSPI:
               sprintf(buf,"%-4s %-2c  ",head.NOMVAR,(char)type);
               switch(lvtyp) {
                  case LVL_MASL  : sprintf(strend(buf)," %8.1f %-2s",lvl,units[lvtyp]); break;
                  case LVL_SIGMA : sprintf(strend(buf)," %8.4f %-2s",lvl,units[lvtyp]); break;
                  case LVL_PRES  : sprintf(strend(buf)," %8.1f %-2s",lvl,units[lvtyp]); break;
                  case LVL_UNDEF : sprintf(strend(buf)," %8.1f %-2s",lvl,units[lvtyp]); break;
                  case LVL_MAGL  : sprintf(strend(buf)," %8.1f %-2s",lvl,units[lvtyp]); break;
                  case LVL_HYBRID: sprintf(strend(buf)," %8.6f %-2s",lvl,units[lvtyp]); break;
                  case LVL_THETA : sprintf(strend(buf)," %8.4f %-2s",lvl,units[lvtyp]); break;
                  case LVL_HOUR  : sprintf(strend(buf)," %8.1f %-2s",lvl,units[lvtyp]); break;
               }
               sprintf(strend(buf)," %8i %-2s %8i %-2s GRIB%-8i %08i%04i %s %i %i %i %i gribfield",0,units[LVL_HOUR],0,units[LVL_UNDEF],head.Version,idate,itime/100,File->Id,nb,head.IP1,0,0);
               Tcl_SetStringObj(obj,buf,-1);
               Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               break;

            case FSTD_LISTALL:
               snprintf(buf,1024,"%s %i {%s} {%c} %i %i %i GRIB%i %09li %09li %li %li %li",
                  File->Id,nb,head.NOMVAR,(char)type,head.IP1,0,0,head.Version,head.DATEO,head.DATEV,ni,nj,nk);
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
      head.KEY=ftell(File->Handle);

      if (!table) {
         if (nb>=GRIB_TABLESIZE-1) {
            App_Log(WARNING,"%s: Number of records higher than table size\n",__func__);
         }
         memcpy(&File->Table[nb],&head,sizeof(TGRIBHeader));
         File->Table[nb+1].KEY=-1;
      }
      nb++;
   }
   if (err) {
      App_Log(ERROR,"%s: %s\n",__func__,grib_get_error_message(err));
   }

   File->Size=head.KEY;

   /*Error on handle access*/
   if (!table && head.Handle) {
      err=grib_handle_delete(head.Handle);

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
   double lat,lon,lat1,lon1,scale,scale2;
   long   gribVer,lval;

   enum gridOpt { REGULAR_LL,REDUCED_LL,ROTATED_LL,STRETCHED_LL,STRETCHED_ROTATED_LL,MERCATOR,POLAR_STEREOGRAPHIC,LAMBERT,ALBERS,REGULAR_GG,REDUCED_GG,ROTATED_GG,STRETCHED_GG,STRETCHED_ROTATED_GG,SH,ROTATED_SH,STRETCHED_SH,STRETCHED_ROTATED_SH,SPACE_VIEW,TRIANGULAR_GRID,EQUATORIAL_AZIMUTHAL_EQUIDISTANT,AZIMUTH_RANGE,IRREGULAR_LATLON,LAMBERT_AZIMUTHAL_EQUAL_AREA,CROSS_SECTION,HOVMOLLER,TIME_SECTION,UNKNOWN,UNKNOWN_PLPRESENT };
   static CONST char *gridTypes[] = { "regular_ll","reduced_ll","rotated_ll","stretched_ll","stretched_rotated_ll","mercator","polar_stereographic","lambert","albers","regular_gg","reduced_gg","rotated_gg","stretched_gg","stretched_rotated_gg","sh","rotated_sh","stretched_sh","stretched_rotated_sh","space_view","triangular_grid","equatorial_azimuthal_equidistant","azimuth_range","irregular_latlon","lambert_azimuthal_equal_area","cross_section","Hovmoller","time_section","unknown","unknown_PLPresent",NULL };


   // GRIB version is needed since we do not deal the same way with both file types
   if (grib_get_long(Handle,"GRIBEditionNumber",&gribVer)!=GRIB_SUCCESS) {
      Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get GRIB version.",(char*)NULL);
      return(NULL);
   }

   // Get gridType Index
   if ((err=grib_get_string(Handle,"gridType",gridType, &len))) {
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
         if (grib_get_double(Handle,"latitudeOfLastGridPointInDegrees",&lat1)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get latitudeOfLastGridPointInDegrees",(char*)NULL);
            return(NULL);
         }
         if (grib_get_double(Handle,"longitudeOfLastGridPointInDegrees",&lon1)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get longitudeOfLastGridPointInDegrees",(char*)NULL);
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
        if (grib_get_double(Handle,"Latin2InDegrees",&scale2)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get Latin2InDegrees",(char*)NULL);
            return(NULL);
         }
         if (grib_get_double(Handle,"LoVInDegrees",&lon)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get LoVInDegrees",(char*)NULL);
            return(NULL);
         }
         if (grib_get_double(Handle,"LaDInDegrees",&lat)!=GRIB_SUCCESS) {
            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get LaDInDegrees",(char*)NULL);
            return(NULL);
         }
//         if (grib_get_double(Handle,"orientationOfTheGridInDegrees",&lon)!=GRIB_SUCCESS) {
//            Tcl_AppendResult(Interp,"\n   GRIB_WKTProjCS: Couldn't get orientationOfTheGridInDegrees",(char*)NULL);
//            return(NULL);
//         }
         OSRSetLCC(ref,scale,scale2,lat,lon,0.0,0.0);
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
         case 1 :  /* User defined (w/ scale factor); earth is spherical (1/f=1.0) */
         case 0 :
            OSRSetGeogCS(ref,"Coordinate System imported from GRIB","Unknown","Sphere",6367470.0,0.0,NULL,0.0,NULL,0.0);
            break;
         case 3 :
            /* User defined major+minor axis IN KM (1/f = 1/((major-minor)/major) */
         case 7 :
            /* User defined major+minor axis IN M (1/f = 1/((major-minor)/major) */
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

#endif
