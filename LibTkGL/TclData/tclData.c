/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de divers fichiers de donnees
 * Fichier      : tclData.c
 * Creation     : Fevrier 2003 - J.P. Gauthier
 *
 * Description  : Fonctions generales applicables a divers types de donnees.
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
 *    Boston, MA 02111-1307, USA.
 *
 *=========================================================
 */

#include "tclData.h"
#include "Projection.h"

TCL_DECLARE_MUTEX(MUTEX_DATACALC)

static Tcl_HashTable TData_Table;
static int           TDataInit=0;
int                  TData_Size[]={ 0,0,1,1,2,2,4,4,8,8,4,8 };

static int Data_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
int        Data_Stat(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Tcldata_Init>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Initialisation des commandes Tcl pour utilisation des divers types de donnees
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *
 * Retour       : Code de retour Tcl
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Tcldata_Init(Tcl_Interp *Interp) {

   /*Initialisation du package viewport*/
   if (Projection_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package des cameras*/
   if (ProjCam_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package model*/
   if (Model_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package de palette de couleur*/
   if (TclCMap_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package de vecteur*/
   if (TclVector_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package de configuration des donnees*/
   if (TclDataSpec_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

#ifdef LNK_FSTD
   /*Initialisation du package de fichier standard*/
   if (TclFSTD_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);
#endif

#ifdef LNK_GRIB
   /*Initialisation du package de fichier standard*/
   if (TclGRIB_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);
#endif

#ifdef LNK_GDALOGR
   /*Initialisation du package GDAL*/
   if (TclGDAL_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package OGR*/
   if (TclOGR_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);
#endif

  /*Initialisation du package d'observations*/
   if (TclObs_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package trajectoires*/
   if (TclTraj_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package radar*/
#ifdef LNK_URP
   if (TclRadar_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);
#endif

   /*Initialisation du package metobs*/
   if (TclMetObs_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   if (TclMetModel_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation du package metobs*/
   if (GeoRef_Init(Interp)==TCL_ERROR)
      return(TCL_ERROR);

   /*Initialisation de la commande de calculatrice*/
   Tcl_CreateObjCommand(Interp,"vexpr",Data_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   if (!TDataInit++) {
      Tcl_InitHashTable(&TData_Table,TCL_STRING_KEYS);
   }

   Tcl_PkgProvide(Interp,"TclData",TCLDATA_VERSION);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_FreeHash>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Libere la hashtable et l'espace memoire associee a un champ.
 *
 * Parametres :
 *  <Name>    : Identificateur du champs.
 *
 * Retour     :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_FreeHash(Tcl_Interp *Interp,char *Name) {

   TData *data;

   if ((data=(TData*)TclY_HashDel(&TData_Table,Name))) {
      Data_Free(data);
      DataSpec_FreeHash(Interp,data->Spec->Name);
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Get>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait l'adresse d'un champs de la liste des champs
 *            connu (HashTable) et en retourne la valeur.
 *
 * Parametres     :
 *  <Name>        : Nom du champ
 *
 * Retour:
 *  <Interp> : Interpreteur Tcl
 *  <TData>  : Pointeur sur la structure du champs (char)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TData* Data_Get(char *Name) {
   return((TData*)TclY_HashGet(&TData_Table,Name));
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_GetShell>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait l'adresse d'un champs de la liste des champs
 *            connu (HashTable) et en retourne la valeur.
 *
 * Parametres     :
 *  <Name>        : Nom du champ
 *
 * Retour:
 *  <Interp> : Interpreteur Tcl
 *  <TData>  : Pointeur sur la structure du champs (char)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TData* Data_GetShell(Tcl_Interp *Interp,char *Name){

   TData          *field=NULL;
   Tcl_HashEntry  *entry;
   int             new;

   entry=Tcl_CreateHashEntry(&TData_Table,Name,&new);

   if (!new) {
      Tcl_AppendResult(Interp,"Data_GetShell: Field already exist",(char *)NULL);
   } else {
      field=(TData*)malloc(sizeof(TData));
      Tcl_SetHashValue(entry,field);
   }
   return(field);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_GetStat>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait des statistiques d'un champ.
 *            (Minimum,Maximum,Moyenne,LatMin,LatMax,LonMin,LonMax)
 *
 * Parametres :
 *  <Field>   : Champs a utiliser
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_GetStat(TData *Field){

   double        val,mode;
   int           i,j,k,d,imin=0,jmin=0,imax=0,jmax=0,kmin=0,kmax=0;
   unsigned long idxk,idx,n=0;

   if (Field->Ref && Field->Ref->Type&GRID_SPARSE)
      FSTD_FieldReadMesh(Field);

   /*Calculate vector module if needed*/
   if (Field->Def->NC>1) {
      if (!Field->Def->Mode || Field->Def->Mode==Field->Def->Data[0]) {
         Field->Def->Mode=(char*)malloc(FSIZE3D(Field->Def)*TData_Size[Field->Def->Type]);
      } else {
         Field->Def->Mode=(char*)realloc(Field->Def->Mode,FSIZE3D(Field->Def)*TData_Size[Field->Def->Type]);
      }
      for (k=0;k<Field->Def->NK;k++) {
        idxk=FSIZE2D(Field->Def)*k;
        for (j=0;j<Field->Def->NJ;j++) {
            idx=idxk+j*Field->Def->NI;
            for (i=0;i<Field->Def->NI;i++,idx++) {
               mode=0;
               for (d=0;d<Field->Def->NC;d++) {
                  Def_Get(Field->Def,d,idx,val);
                  mode+=val*val;
               }
               Def_SetMod(Field->Def,idx,sqrt(mode));
            }
         }
      }
   } else {
      Field->Def->Mode=Field->Def->Data[0];
   }


   /*Initialiser la structure*/
   if (!Field->Stat)
      Field->Stat=(TDataStat*)malloc(sizeof(TDataStat));

   Field->Stat->Min=1e200;
   Field->Stat->Max=-1e200;
   Field->Stat->Avg=0.0;

   for (k=0;k<Field->Def->NK;k++) {
      idxk=FSIZE2D(Field->Def)*k;

      /*Calculer les statistiques*/
      for (j=0;j<Field->Def->NJ;j++) {
         idx=idxk+j*Field->Def->NI;
         for (i=0;i<Field->Def->NI;i++,idx++) {

            Def_GetMod(Field->Def,idx,val);
            if (val!=Field->Def->NoData) {
               n++;
               Field->Stat->Avg+=val;

               if (val<Field->Stat->Min && val!=0.0) {
                  Field->Stat->Min=val;
                  imin=i;
                  jmin=j;
                  kmin=k;
               }
               if (val>Field->Stat->Max) {
                  Field->Stat->Max=val;
                  imax=i;
                  jmax=j;
                  kmax=k;
               }
            }
         }
      }
   }

   if (n)
      Field->Stat->Avg/=n;

   if (Field->Stat->Min==1e200 || Field->Stat->Min==Field->Stat->Max) Field->Stat->Min=0.0;

   /*Recuperer les coordonnees latlon des min max*/
   Field->Stat->MinLoc.lat=0;
   Field->Stat->MinLoc.lon=0;
   Field->Stat->MinLoc.elev=0;
   Field->Stat->MaxLoc.lat=0;
   Field->Stat->MaxLoc.lon=0;
   Field->Stat->MaxLoc.elev=0;

   if (Field->Ref && Field->Ref->Grid[0]!='V') {
      if (Field->Ref->Lat && Field->Ref->Lon) {
         Field->Stat->MinLoc.lat=Field->Ref->Lat[FIDX2D(Field->Def,imin,jmin)];
         Field->Stat->MinLoc.lon=Field->Ref->Lon[FIDX2D(Field->Def,imin,jmin)];
         Field->Stat->MaxLoc.lat=Field->Ref->Lat[FIDX2D(Field->Def,imax,jmax)];
         Field->Stat->MaxLoc.lon=Field->Ref->Lon[FIDX2D(Field->Def,imax,jmax)];
      } else if (Field->Ref->Project) {
         Field->Ref->Project(Field->Ref,imin,jmin,&Field->Stat->MinLoc.lat,&Field->Stat->MinLoc.lon,1,1);
         Field->Ref->Project(Field->Ref,imax,jmax,&Field->Stat->MaxLoc.lat,&Field->Stat->MaxLoc.lon,1,1);
      }
      if (Field->Ref->Hgt) {
         Field->Stat->MinLoc.elev=Field->Ref->Hgt[FIDX2D(Field->Def,imin,jmin)];
         Field->Stat->MaxLoc.elev=Field->Ref->Hgt[FIDX2D(Field->Def,imax,jmax)];
      }  else {
         Field->Stat->MinLoc.elev=Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[kmin]);
         Field->Stat->MaxLoc.elev=Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[kmax]);
      }
   }
}

int Data_Free(TData *Field) {

   if (Field) {

      /*Liberer l'espace specifique au type de donnees*/
      Field->Free(Field);

      /*Liberer l'espace de donnees*/
      Data_Clean(Field,1,1,1);
      Data_DefFree(Field->Def);

      /*Liberer l'espace du descriptif*/
      if (Field->Stat) free(Field->Stat);
      if (Field->Ref)  GeoRef_Destroy(NULL,Field->Ref->Name);
      if (Field->Tag)  Tcl_DecrRefCount(Field->Tag);

      free(Field);
   }
   return TCL_OK;
}

TData* Data_Copy(Tcl_Interp *Interp,TData *Field,char *Name,int Def){

   TData     *field;
   int        i;

   /* Verifier que le champs n'est pas lui-meme*/
   field=Data_Get(Name);
   if (field==Field) {
      if (!Def && field->Def) {
         Data_DefFree(field->Def);
         field->Def=NULL;
      }
      return(field);
   }

   /* Est-ce que le champs existe et si oui, verifier les dimensions */
   if (Def) {
      field=Data_Valid(Interp,Name,Field->Def->NI,Field->Def->NJ,Field->Def->NK,DSIZE(Field->Def->Data),Field->Def->Type);
   } else {
      field=Data_Valid(Interp,Name,0,0,0,0,Field->Def->Type);
   }

   if (!field)
      return(NULL);

   /*Copier le champs par bloc de memoire*/
   Field->Set(field);
   Field->Copy(field->Head,Field->Head);

   field->Ref=GeoRef_Copy(Field->Ref);
   field->Spec->Map=Field->Spec->Map;

   if (Field->Spec->Desc) field->Spec->Desc=strdup(Field->Spec->Desc);
   if (Field->Spec->Topo) field->Spec->Topo=strdup(Field->Spec->Topo);

   if (Def) {
      for(i=0;i<4;i++) {
         if (Field->Def->Data[i])
            memcpy(field->Def->Data[i],Field->Def->Data[i],FSIZE3D(Field->Def)*sizeof(float));
      }
   }

   return(field);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Cut>
 * Creation : Avril 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue un coupe verticale aux coordonnes specifiees.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Stream de Champs
 *  <Cut>     : Identificateur du champs de coupe
 *  <Lat>     : Stream de Latitude
 *  <Lon>     : Stream de Longitude
 *  <Nb>      : Nombre de coordonnees
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_Cut(Tcl_Interp *Interp,TData **Field,char *Cut,double *Lat,double *Lon,int NbF,int NbC) {

   TData *cut;
   unsigned int  n,k,f;
   unsigned long idx;
   double  i,j,i0=-1.0,j0=-1.0,theta,zeta,vi,vj,vij;

   /*Recuperer la grille dans l'espace des champs de base*/
   for(f=0;f<NbF;f++) {
      if (!Field[f] || Field[f]->Ref->Grid[0]=='V') {
         Tcl_AppendResult(Interp,"Data_Cut:  Invalid Field or Grid",(char*)NULL);
         return(TCL_ERROR);
      }
      if (Field[f]->ReadCube)
         Field[f]->ReadCube(Interp,Field[f],0);
   }

   cut=Data_Valid(Interp,Cut,NbF*NbC,Field[0]->Def->NK,1,DSIZE(Field[0]->Def->Data),Field[0]->Def->Type);
   Field[0]->Set(cut);
   Field[0]->Copy(cut->Head,Field[0]->Head);
   cut->Ref=GeoRef_Reference(Field[0]->Ref);
   cut->Ref->Grid[0]='V';
   cut->Ref->LevelType=Field[0]->Ref->LevelType;

   cut->Ref->Levels=(float*)malloc(Field[0]->Def->NK*sizeof(float));
   memcpy(cut->Ref->Levels,Field[0]->Ref->Levels,Field[0]->Def->NK*sizeof(float));

   if (Field[0]->Spec) {
      if (Field[0]->Spec->Desc) cut->Spec->Desc=strdup(Field[0]->Spec->Desc);
      if (Field[0]->Spec->Topo) cut->Spec->Topo=strdup(Field[0]->Spec->Topo);
   }

   GeoRef_Size(cut->Ref,0,0,0,NbF*NbC-1,Field[0]->Def->NK-1,0,0);
   GeoRef_Qualify(cut->Ref);

   if (!NbC || !NbF)
      return;

   cut->Ref->Lat=(float*)malloc(NbC*sizeof(float));
   cut->Ref->Lon=(float*)malloc(NbC*sizeof(float));

   if (!cut->Ref->Lat || !cut->Ref->Lon) {
      Tcl_AppendResult(Interp,"Data_Cut: Unable to allocate memory for coordinate caching",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Recuperer la grille dans l'espace*/
   for(n=0;n<NbC;n++) {

      /*Figure out the grid coordinates*/
      if (Lat[n]!=-999.0 && Lon[n]!=-999.0) {

         cut->Ref->Lat[n]=Lat[n];
         cut->Ref->Lon[n]=Lon[n];

         for(f=0;f<NbF;f++) {

            Field[f]->Ref->UnProject(Field[f]->Ref,&i,&j,Lat[n],Lon[n],0,1);

            /*Vectorial data needs to be referenced along the cut*/
            if (cut->Def->Data[1]) {
               if (i0==-1.0) {
                  Field[f]->Ref->UnProject(Field[f]->Ref,&i0,&j0,Lat[n+1],Lon[n+1],0,1);
                  theta=atan2(j0-j,i0-i);
               } else {
                  theta=atan2(j-j0,i-i0);
               }
            }

            for(k=0;k<Field[0]->Def->NK;k++) {

               idx=k*NbF*NbC+n*NbF+f;

               if (cut->Def->Data[1]) {

                  vi=VertexValN(Field[f]->Ref,Field[f]->Def,0,i,j,k);
                  vj=VertexValN(Field[f]->Ref,Field[f]->Def,1,i,j,k);
                  vij=hypot(vi,vj);
                  zeta=atan2(vj,vi)-theta;

                  Def_Set(cut->Def,0,idx,vij*cos(zeta));
                  Def_Set(cut->Def,1,idx,vij*sin(zeta));

                  if (cut->Def->Data[2]) {
                     vi=VertexValN(Field[f]->Ref,Field[f]->Def,2,i,j,k);
                     Def_Set(cut->Def,2,idx,vi);
                  }
#ifdef DEBUG
                  fprintf(stderr,"(DEBUG) %f (%f,%f) [%f,%f] =%f %f (%f) v=%f %f (%f)\n",theta*57.295779,Lat[n],Lon[n],i,j,vi,vj,vij,
                  vij*cos(zeta),vij*sin(zeta),hypot(vij*cos(zeta),vij*sin(zeta)));
#endif
               } else {
                  vij=VertexVal(Field[f]->Ref,Field[f]->Def,i,j,k);
                  Def_Set(cut->Def,0,idx,vij);
               }
            }
            i0=i;
            j0=j;
         }
      }
   }
#ifdef DEBUG
   fprintf(stderr,"(DEBUG) FSTD_FieldCut: Vertical grid size (%i,%i)\n",cut->Def->NI,cut->Def->NJ);
#endif

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_PreInit>
 * Creation : Juillet 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la pre-initialisation des parametres avant le rendue.
 *
 * Parametres :
 *  <Data>    : Champs
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_PreInit(TData *Data) {

   if (!Data->Stat)
      Data_GetStat(Data);

   if (!(Data->Spec->MinMax&DATASPEC_MINSET)) Data->Spec->Min=Data->Stat->Min;
   if (!(Data->Spec->MinMax&DATASPEC_MAXSET)) Data->Spec->Max=Data->Stat->Max;
   if (!(Data->Spec->MinMax&DATASPEC_MINSET)) Data->Spec->Min=Data->Spec->Max<Data->Spec->Min?Data->Spec->Max:Data->Spec->Min;

   if (Data->Spec->InterMode) {
      DataSpec_Intervals(Data->Spec,Data->Spec->Min,Data->Spec->Max);
   }
   DataSpec_Define(Data->Spec);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Valid>
 * Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner la validite d'un champ de par ses dimension ou en
 *            creer un nouveau si necessaire.
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Name>        : Nom du champ
 *  <NI>          : Dimension en I
 *  <NJ>          : Dimension en J
 *  <NK>          : Dimension en K
 *  <Dim>         : Champs vectoriel
 *  <Type>        : Type de donnees
 *
 * Retour:
 *  <Field>       : Champs valide pour les parametres demandees
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TData *Data_Valid(Tcl_Interp *Interp,char *Name,int NI,int NJ,int NK,int Dim,TData_Type Type){

   TData          *field=NULL;
   Tcl_HashEntry  *entry;
   int             new,i;

   entry=Tcl_CreateHashEntry(&TData_Table,Name,&new);

   if (!new) {
      field=(TData*)Tcl_GetHashValue(entry);

      if (NI*NJ*NK==0) {
         Data_DefFree(field->Def);
         field->Def=NULL;
      } else {
         /* Si les dimensions sont correctes et les composantes sont ls memes*/
         if (NI!=field->Def->NI || NJ!=field->Def->NJ || NK!=field->Def->NK || TData_Size[field->Def->Type]!=TData_Size[Type]) {
            Data_DefFree(field->Def);
            if (!(field->Def=Data_DefNew(NI,NJ,NK,Dim,Type))) {
               Tcl_AppendResult(Interp,"Data_Valid: Not enough memory to allocate data",(char *)NULL);
               return(NULL);
            }
         } else if (Dim!=field->Def->NC) {
           for(i=0;i<4;i++) {
               if (i<Dim && !field->Def->Data[i]) {
                  if (!(field->Def->Data[i]=(char*)calloc(NI*NJ*NK,TData_Size[Type]))) {
                     Tcl_AppendResult(Interp,"Data_Valid: Not enough memory to allocate complementary field",(char *)NULL);
                     return(NULL);
                  }
               }
               if (i>=Dim && field->Def->Data[i]) {
                  free(field->Def->Data[i]);
                  field->Def->Data[i]=NULL;
               }
            }
            field->Def->NC=Dim;
         }
      }
      /*Liberer les donnees secondaires*/
      Data_Clean(field,1,1,1);
      field->Free(field);                                   field->Head=NULL;
      if (field->Tag)         Tcl_DecrRefCount(field->Tag); field->Tag=NULL;
      if (field->Stat)        free(field->Stat);            field->Stat=NULL;
      GeoRef_Destroy(NULL,field->Ref->Name);                field->Ref=NULL;
   }

   /*Allouer la memoire pour les structures*/
   if (!field) {
       if (!(field=(TData*)malloc(sizeof(TData)))) {
         Tcl_AppendResult(Interp,"Data_Valid: Not enough memory to allocate field",(char *)NULL);
         return(NULL);
      }
      if (!(field->Spec=DataSpec_Create(Interp,NULL))) {
         Tcl_AppendResult(Interp,"Data_Valid: Not enough memory to allocate field",(char *)NULL);
         return(NULL);
      }

      field->Def=NULL;
      field->Stat=NULL;
      field->Ref=NULL;
      field->Head=NULL;
      field->Free=NULL;
      field->ReadCube=NULL;
      field->Tag=NULL;
      field->Segments=NULL;
      field->Map=NULL;

      if (NI*NJ*NK) {
         if (!(field->Def=Data_DefNew(NI,NJ,NK,Dim,Type))) {
            Tcl_AppendResult(Interp,"Data_Valid: Not enough memory to allocate data",(char *)NULL);
            return(NULL);
         }
      }
      Tcl_SetHashValue(entry,field);
   }
   return(field);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Data_GridInterpolate>
 * Creation     : Aout 2006 J.P. Gauthier - CMC/CMOE
 *
 * But          : Interpolate uneExporter bande raster dans un champs de donnees
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <ToRef>    : Reference du champs destination
 *   <ToDef>    : Description du champs destination
 *   <FromRef>  : Reference du champs source
 *   <FromDef>  : Description du champs source
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Data_GridInterpolate(Tcl_Interp *Interp,TGeoRef *ToRef,TDataDef *ToDef,TGeoRef *FromRef,TDataDef *FromDef) {

   double lat,lon,i,j;
   float val,val1;
   int x,y,x0,y0,x1,y1,idx,ex,grd=0;

   ex=!isnan(ToDef->NoData);

   if (!ToRef || !FromRef) {
      if (Interp)
         Tcl_AppendResult(Interp,"Data_GridInterpolate: No georeference defined",(char*)NULL);
      return(TCL_ERROR);
   }

   if (!GeoRef_Intersect(FromRef,ToRef,&x0,&y0,&x1,&y1,0)) {
      return(TCL_OK);
   }

   if (GeoRef_Equal(ToRef,FromRef)) {
      grd=1;
   }

   for(x=x0;x<=x1;x++){
      for(y=y0;y<=y1;y++){

         if (grd) {
            idx=FIDX2D(ToDef,x,y);
            Def_Get(FromDef,0,idx,val);
            Def_Set(ToDef,0,idx,val);
         } else {
            /*Get the latlon coordinate of grid point*/
            ToRef->Project(ToRef,x,y,&lat,&lon,0,1);
            /*Get the value of the gdalband at this latlon coordinate*/
            if (FromRef->UnProject(FromRef,&i,&j,lat,lon,0,1)) {
               FromRef->Value(FromRef,FromDef,'L',0,i,j,0,&val,&val1);
               if (val!=FromDef->NoData && !isnan(val)) {
                  idx=FIDX2D(ToDef,x,y);
                  Def_Set(ToDef,0,idx,val);
               }
            } else {
               /*Set as nodata
               if (ex) {
                  Def_Set(ToDef,0,idx,ToDef->NoData);
               }*/
            }
         }
      }
   }
  return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Wipe>
 * Creation : Juin 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer toutes la memoire allouee par ce package.
 *
 * Parametres     :
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_Wipe() {

   Tcl_HashSearch  ptr;
   Tcl_HashEntry  *entry=NULL;

   entry=Tcl_FirstHashEntry(&TData_Table,&ptr);

   while (entry) {
      Data_Free((TData*)Tcl_GetHashValue(entry));
      Tcl_DeleteHashEntry(entry);
      entry=Tcl_FirstHashEntry(&TData_Table,&ptr);
   }
//   Tcl_DeleteHashTable(&TData_Table);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Data_Cmd>
 * Creation     : Fevrier 2003 J.P. Gauthier
 *
 * But          : Execution de la commande d'expression
 *
 * Parametres   :
 *  <clientData>  : Donnees du module.
 *  <Interp>      : Interpreteur TCL.
 *  <Objc>        : Nombre d'arguments
 *  <Objv>        : Liste des arguments
 *
 * Retour       : Code de retour Tcl
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int Data_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   char expr[PARSE_LEN],*fld;
   int  i,len,tlen=0;
   TData_Type type=TD_Unknown;

   /*Clear the expression*/
   memset(expr,0,PARSE_LEN);
   expr[0]='\0';

   fld=Tcl_GetString(Objv[1]);
   if (strncmp(fld,"(Binary)",8)==0) {
      fld=&fld[8];type=TD_Binary;
   } else if (strncmp(fld,"(UByte)",7)==0) {
      fld=&fld[7];type=TD_UByte;
   } else if (strncmp(fld,"(Byte)",6)==0) {
      fld=&fld[6];type=TD_Byte;
   } else if (strncmp(fld,"(UInt16)",8)==0) {
      fld=&fld[8];type=TD_UInt16;
   } else if (strncmp(fld,"(Int16)",7)==0) {
      fld=&fld[7];type=TD_Int16;
   } else if (strncmp(fld,"(UInt32)",8)==0) {
      fld=&fld[8];type=TD_UInt32;
   } else if (strncmp(fld,"(Int32)",7)==0) {
      fld=&fld[7];type=TD_Int32;
   } else if (strncmp(fld,"(UInt64)",8)==0) {
      fld=&fld[8];type=TD_UInt64;
   } else if (strncmp(fld,"(Int64)",7)==0) {
      fld=&fld[7];type=TD_Int64;
   } else if (strncmp(fld,"(Float32)",9)==0) {
      fld=&fld[9];type=TD_Float32;
   } else if (strncmp(fld,"(Float64)",9)==0) {
      fld=&fld[9];type=TD_Float64;
   }

   /*Build the expression by concatenation of the args*/
   for(i=2;i<Objc;i++) {
      strcat(expr,Tcl_GetStringFromObj(Objv[i],&len));
      if ((len+=tlen)>=PARSE_LEN) {
         Tcl_AppendResult(Interp,"Data_Cmd: expression string too long",(char*)NULL);
         return TCL_ERROR;
      }
   }

   Tcl_MutexLock(&MUTEX_DATACALC);
   len=Calc_Parse(Interp,0,fld,type,expr);  /*Lexing and Parsing yeyeye*/
   Tcl_MutexUnlock(&MUTEX_DATACALC);

   return(len);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Clean>
 * Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Reinitialiser les structures du champ.
 *
 * Parametres :
 *  <Field>   : Reference du champs
 *  <Map>     : Clean de la Texture
 *  <Pos>     : Clean des Positions
 *  <Seg>     : Clean des Contours
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_CleanAll(TDataSpec *Spec,int Map,int Pos,int Seg) {

   TData          *data;
   Tcl_HashSearch  ptr;
   Tcl_HashEntry  *entry=NULL;

   entry=Tcl_FirstHashEntry(&TData_Table,&ptr);
   while (entry) {
      data=Tcl_GetHashValue(entry);

      if (data->Spec && data->Spec==Spec) {
         Data_Clean(data,Map,Pos,Seg);
      }
      entry=Tcl_NextHashEntry(&ptr);
   }
}

void Data_Clean(TData *Data,int Map,int Pos,int Seg){

   if (Data) {
      if (Data->Ref && Data->Ref->Pos && Pos) {
         free(Data->Ref->Pos);
         Data->Ref->Pos=NULL;
     }

      if (Map && Data->Map) {
         free(Data->Map);
         Data->Map=NULL;
      }

      if (Seg && Data->Segments) {
         TList_Clear(Data->Segments);
         Data->Segments=NULL;
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_DefSort>
 * Creation : Octobre 2005- J.P. Gauthier - CMC/CMOE
 *
 * But      : Trier une liste de TDataDef pour diverses stats.
 *
 * Parametres :
 *  <Interp>  : Dimension du champs a allouer
 *  <List>    : Liste de Champs/Obs
 *
 * Retour:
 *  <Status>  :   Code d'erreur Tcl
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_DefSort(Tcl_Interp *Interp,Tcl_Obj *List){

   Tcl_Obj   *obj;
   TDataDef **def;
   TData     *fld;
   TObs      *obs;
   int        i=0,n,nobj;
   double    *v;

   if (!List) {
      Tcl_AppendResult(Interp,"\n   Data_DefSort: Empty list",(char*)NULL);
      return TCL_ERROR;
   }
   Tcl_ListObjLength(Interp,List,&nobj);

   v=(double*)malloc(nobj*sizeof(double));
   def=(TDataDef**)malloc(nobj*sizeof(TDataDef*));

   for(n=0;n<nobj;n++) {
      Tcl_ListObjIndex(Interp,List,n,&obj);
      if (fld=Data_Get(Tcl_GetString(obj))) {
         def[n]=fld->Def;
      } else if (obs=Obs_Get(Tcl_GetString(obj))) {
         def[n]=obs->Def;
      } else {
         Tcl_AppendResult(Interp,"\n   Data_DefSort: Invalid field of observation",(char*)NULL);
         return TCL_ERROR;
      }
      if (i!=0 && i!=FSIZE2D(def[n])) {
         Tcl_AppendResult(Interp,"\n   Data_DefSort: Invalid dimensions",(char*)NULL);
         return TCL_ERROR;
      }
      i=FSIZE2D(def[n]);
   }

   for(i=0;i<FSIZE2D(def[0]);i++) {
      for(n=0;n<nobj;n++) {
         Def_Get(def[n],0,i,v[n]);
      }
      qsort(v,nobj,sizeof(double),QSort_Double);
      for(n=0;n<nobj;n++) {
         Def_Set(def[n],0,i,v[n]);
      }
   }

   free(v);
   free(def);
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_DefNew>
 * Creation : Fevrier 2003- J.P. Gauthier - CMC/CMOE
 *
 * But      : Initialiser la structure TDataDef.
 *
 * Parametres :
 *  <NI>      : Dimension du champs a allouer
 *  <NJ>      : Dimension du champs a allouer
 *  <NK>      : Dimension du champs a allouer
 *  <Dim>     : Nombre de composantes
 *  <Type>    : Type de donnnes
 *
 * Retour:
 *  <Def>:      Nouvelle structure
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TDataDef *Data_DefNew(int NI,int NJ,int NK,int Dim,TData_Type Type){

   int       i;
   TDataDef *def;

   def=(TDataDef*)malloc(sizeof(TDataDef));
   if (!def)
     return(NULL);

   def->NI=NI;
   def->NJ=NJ;
   def->NK=NK;
   def->NC=abs(Dim);
   def->Container=abs(Dim)==0;
   def->NoData=nan("NaN");
   def->Level=0;

   def->Limits[0][0]=0;
   def->Limits[0][1]=NI-1;
   def->Limits[1][0]=0;
   def->Limits[1][1]=NJ-1;
   def->Limits[2][0]=0;
   def->Limits[2][1]=NK-1;

   /* Allocate data vector */
   def->Data[0]=NULL;
   def->Data[1]=NULL;
   def->Data[2]=NULL;
   def->Data[3]=NULL;
   def->Type=Type;
   def->Buffer=NULL;
   def->Accum=NULL;
   def->Pick=def->Poly=NULL;

   for(i=0;i<Dim;i++) {
      def->Data[i]=(char*)calloc(NI*NJ*NK,TData_Size[Type]);
      if (!def->Data[i]) {
         free(def);
         return(NULL);
      }
   }
   def->Mode=def->Data[0];

   return(def);
}


/*----------------------------------------------------------------------------
 * Nom      : <Data_DefResize>
 * Creation : Avril 2004- J.P. Gauthier - CMC/CMOE
 *
 * But      : Redimensionner le champs de donnees.
 *
 * Parametres :
 *  <Def>     : Definition a redimensionner
 *  <NI>      : Dimension du champs a allouer
 *  <NJ>      : Dimension du champs a allouer
 *  <NK>      : Dimension du champs a allouer
 *
 * Retour:
 *  <Def>:      Nouvelle structure
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TDataDef *Data_DefResize(TDataDef *Def,int NI,int NJ,int NK){

   int i;

   if (!Def)
     return(NULL);

   if (!Def->Container && (Def->NI!=NI || Def->NJ!=NJ || Def->NK!=NK)) {
      Def->NI=NI;
      Def->NJ=NJ;
      Def->NK=NK;

      Def->Limits[0][0]=0;
      Def->Limits[0][1]=NI-1;
      Def->Limits[1][0]=0;
      Def->Limits[1][1]=NJ-1;
      Def->Limits[2][0]=0;
      Def->Limits[2][1]=NK-1;

      if (Def->Mode && Def->Mode!=Def->Data[0])
         free(Def->Mode);

      for(i=0;i<4;i++) {
         if (Def->Data[i]) {
            Def->Data[i]=(char*)realloc(Def->Data[i],NI*NJ*NK*sizeof(float));
            if (!Def->Data[i]) {
               free(Def);
               return(NULL);
            }
         }
      }
      Def->Mode=Def->Data[0];

      if (Def->Buffer)     free(Def->Buffer); Def->Buffer=NULL;
      if (Def->Accum)      free(Def->Accum);  Def->Accum=NULL;
   }
   return(Def);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_DefFree>
 * Creation : Fevrier 2003- J.P. Gauthier - CMC/CMOE
 *
 * But      : Liberer la structure TDataDef.
 *
 * Parametres :
 *  <Def>     : Structure a liberer
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_DefFree(TDataDef *Def){

   if (Def) {
      if (!Def->Container) {
         if (Def->Mode && Def->Mode!=Def->Data[0]) free(Def->Mode);
         if (Def->Data[0])            free(Def->Data[0]);
         if (Def->Data[1])            free(Def->Data[1]);
         if (Def->Data[2])            free(Def->Data[2]);
         if (Def->Data[3])            free(Def->Data[3]);
      }

      if (Def->Buffer)     free(Def->Buffer);
      if (Def->Accum)      free(Def->Accum);
      if (Def->Poly)       OGR_G_DestroyGeometry(Def->Poly);
//      if (Def->Pick)       OGR_G_DestroyGeometry(Def->Pick);

      free(Def);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_DefCopy>
 * Creation : Fevrier 2003- J.P. Gauthier - CMC/CMOE
 *
 * But      : Copier une structure TDataDef.
 *
 * Parametres :
 *  <Def>     : Structure a copier
 *
 * Retour:
 *  <def>     : Pointeur sur le copie de la structure
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TDataDef *Data_DefCopy(TDataDef *Def){

   int       i;
   TDataDef *def;

   if (Def) {
      def=(TDataDef*)malloc(sizeof(TDataDef));
      def->Container=Def->Container;
      def->NI=Def->NI;
      def->NJ=Def->NJ;
      def->NK=Def->NK;
      def->NC=Def->NC;
      def->NoData=Def->NoData;
      def->Type=Def->Type;
      def->Level=Def->Level;
      def->Buffer=NULL;
      def->Accum=NULL;
      def->Pick=def->Poly=NULL;

      memcpy(def->Limits,Def->Limits,6*sizeof(int));

      for(i=0;i<4;i++) {
         if (def->Container) {
            def->Data[i]=Def->Data[i];
         } else {
            if (Def->Data[i]) {
               def->Data[i]=(char*)malloc(Def->NI*Def->NJ*Def->NK*TData_Size[Def->Type]);
//               def->Data[i]=(char*)calloc(Def->NI*Def->NJ*Def->NK,Def->Type>=6?TData_Size[Def->Type]:TData_Size[6]);
//            def->Data[i]=(char*)calloc(Def->NI*Def->NJ*Def->NK,sizeof(float));
               memcpy(def->Data[i],Def->Data[i],Def->NI*Def->NJ*Def->NK*TData_Size[Def->Type]);
            } else {
               def->Data[i]=NULL;
            }
         }
      }
      def->Mode=def->Data[0];
   }

   return(def);
}

TDataDef *Data_DefCopyPromote(TDataDef *Def,TData_Type Type){

   int       i;
   TDataDef *def;

   if (Def) {
      def=(TDataDef*)malloc(sizeof(TDataDef));
      def->Container=0;
      def->NI=Def->NI;
      def->NJ=Def->NJ;
      def->NK=Def->NK;
      def->NC=Def->NC;
      def->NoData=Def->NoData;
      def->Type=Type;
      def->Level=Def->Level;
      def->Buffer=NULL;
      def->Accum=NULL;
      def->Pick=def->Poly=NULL;

      memcpy(def->Limits,Def->Limits,6*sizeof(int));

      for(i=0;i<4;i++) {
         if (Def->Data[i]) {
            def->Data[i]=(char*)calloc(Def->NI*Def->NJ*Def->NK,TData_Size[def->Type]);
         } else {
            def->Data[i]=NULL;
         }
      }
      def->Mode=def->Data[0];
   }

   return(def);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_DefTile>
 * Creation : Novembre 2007- J.P. Gauthier - CMC/CMOE
 *
 * But      : Copier les donnees d'un TDataDef ands un autres (Tiling).
 *
 * Parametres :
 *  <DefTo>   : Destination
 *  <DefTile> : Tuile
 *  <X0>      : Point de depart de la tuile en X
 *  <Y0>      : Point de depart de la tuile en Y
 *
 * Retour:
 *  <Code>    : In(1) ou Out(0)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_DefTile(TDataDef *DefTo,TDataDef *DefTile,int X0, int Y0) {

   int    x,y,dx,dy,x0,y0,x1,y1,c;
   unsigned long idxf,idxd;
   double val;

   x0=X0<0?-X0:0;
   y0=Y0<0?-Y0:0;

   x1=DefTile->NI+X0>DefTo->NI?DefTo->NI:DefTile->NI;
   y1=DefTile->NJ+Y0>DefTo->NJ?DefTo->NJ:DefTile->NJ;

   if (x0>DefTo->NI || x1<0 || y0>DefTo->NJ || y1<0) {
      return(0);
   }

   dy=Y0;
   for (y=y0;y<=y1;y++) {
      dx=X0;
      for (x=x0;x<=x1;x++) {
         for(c=0;c<DefTile->NC;c++) {
            if (DefTo->Data[c]) {
               idxf=FIDX2D(DefTile,x,y);
               Def_Get(DefTile,0,idxf,val);

               idxd=FIDX2D(DefTo,dx,dy);
               Def_Set(DefTo,0,idxd,val);
            }
         }
         dx++;
      }
      dy++;
   }
   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_GetImage>
 * Creation : Janvier 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer une image d'un champs (snapshot).
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Field>       : Champs
 *  <Img>         : Image
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_GetImage(Tcl_Interp *Interp,TData *Field,char* Img){

   double incri,incrj,val;
   int idx,cidx,x,y,i,j,nidx;

   Tk_PhotoImageBlock data;
   Tk_PhotoHandle     handle;

   if (!Field || !Field->Spec || !Field->Spec->Map) {
      Tcl_AppendResult(Interp,"Data_GetImage: Invalid field or missing colormap",(char*)NULL);
      return(TCL_ERROR);
   }

   Data_PreInit(Field);

   /*Recuperer le handle de l'image specifie*/
   handle=Tk_FindPhoto(Interp,Img);

   /*Definire les parametres du bock de donnees*/
   Tk_PhotoGetSize(handle,&data.width,&data.height);

   data.pitch=data.width*4;
   data.pixelSize=4;
   data.offset[0]=0;
   data.offset[1]=1;
   data.offset[2]=2;
   data.offset[3]=3;
   data.pixelPtr=(unsigned char*)calloc(data.width*data.height*4,sizeof(unsigned char));

   /*Creer l'image de la palette*/
   incrj=(double)(Field->Def->NJ)/(data.height+1);
   incri=(double)(Field->Def->NI)/(data.width+1);

   for(y=0,idx=0;y<data.height;y++) {
      for(x=0;x<data.width;x++) {
         i=(double)x*incri;
         j=Field->Def->NJ-(double)y*incrj;
         nidx=j*Field->Def->NI+i;
         Def_GetMod(Field->Def,nidx,val);
         if (val!=Field->Def->NoData) {
            VAL2COL(cidx,Field->Spec,val);

            data.pixelPtr[idx++]=Field->Spec->Map->Color[cidx][0];
            data.pixelPtr[idx++]=Field->Spec->Map->Color[cidx][1];
            data.pixelPtr[idx++]=Field->Spec->Map->Color[cidx][2];
            data.pixelPtr[idx++]=Field->Spec->Map->Color[cidx][3];
         }
      }
   }

   /*Envoyer le data dans l'image Tk*/
   Tk_PhotoPutBlock(handle,&data,0,0,data.width,data.height,TK_PHOTO_COMPOSITE_SET);
   free(data.pixelPtr);

   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_HighLow>
 * Creation : Octobre 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer les Highs ou Lows.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Champs
 *  <VP>      : Parametres du viewport
 *  <Proj>    : Parametres de la projection
 *  <High>    : Recherche de (1:high -1:Low)
 *  <Tile>    : Dimension des tuiles d'evaluation
 *
 * Retour:
 *
 * Remarques :
 *     -base sur l'algorithme de Michel Grenier
 *
 *----------------------------------------------------------------------------
*/
Tcl_Obj* Data_HighLow(Tcl_Interp *Interp,TData *Field,int High,int Tile){

   Tcl_Obj *obj,*sub;
   double zm,zmm,zn,zv;
   int    high;
   int    ichk,jchk;
   int    is,js,jt,it;
   int    ip,jp,ik,jk;
   int    ni,nj;

   ni=Field->Def->NI;
   nj=Field->Def->NJ;

   /* ichk and jchk indicates the covering area under which the extrema is to be evaluated */
   ichk=ni/Tile;ichk=(2>=ichk?2:(2*Tile-1<=ichk?2*Tile-1:ichk));
   jchk=nj/Tile;jchk=(2>=jchk?2:(2*Tile-1<=jchk?2*Tile-1:jchk));

   obj=Tcl_NewListObj(0,NULL);

   /* loop in all the lines */
   for (ip=0;ip<ni;ip++) {

      /* as long as the function is increasing or decreasing move along the y axe */
      Def_GetMod(Field->Def,ip,zm);
      Def_GetMod(Field->Def,ip+1,zmm);
      high=(zm<=zmm);
      for (jp=0;jp<nj-1;jp++) {
         if (jp!=nj-1) {
            Def_GetMod(Field->Def,ip+(jp+1)*ni,zn);
            if (zm==zn || ((zm<zn) && high) || ((zm>zn) && !high)) {
               zm=zn;
               continue;
            }
         }

         /* check if it is really a max or a min over the area */
         js=(0>=jp-jchk?0:jp-jchk);
         jt=(nj-1<=jp+jchk?nj-1:jp+jchk);
         is=(0>=ip-ichk?0:ip-ichk);
         it=(ni-1<=ip+ichk?ni-1:ip+ichk);

         for (jk=js;jk<=jt;jk++) {
            for (ik=is;ik<=it;ik++) {
               if (ik!=ip || jk!=jp) {
                  Def_GetMod(Field->Def,ik+jk*ni,zv);
                  if (((zv>=zm) && high) || ((zv<=zm) && !high)) goto nexty;
               }
            }
         }

         /* an extrema was found */
         if (high==High) {
            sub=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,sub,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,zm)));
            Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(ip));
            Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(jp));
            Tcl_ListObjAppendElement(Interp,obj,sub);
         }

         /* continues the search */
         nexty: high=!high;
         zm=zn;
      }
   }
   return(obj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Stat>
 * Creation : Avril 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la configuration des statistiques et le retour des valeurs
 *            si il n'y a pas de valeur specifie (seulement le token).
 *
 * Parametres     :
 *  <Interp>      : Interpreteur TCL
 *  <Field>       : Pointeur sur le champs
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

int Data_Stat(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj *obj,*sub;
   int      n,i,j,ni,nj,index,idx,b,f,tr=1,ex;
   int      nb,len,nobj;
   double   dlat,dlon,dlat0,dlon0,dlat1,dlon1,dx,dy,dval,dl,dv;
   float    val,val1,levels[1024];
   char     buf[32];

   extern Vect3d GDB_VBuf[];
   extern int FFStreamLine(TGeoRef *Ref,TDataDef *Def,ViewportItem *VP,Vect3d *Stream,float *Map,double X,double Y,double Z,int MaxIter,double Step,double Min,double Res,int Mode,int ZDim);

   static CONST char *type[] = { "MASL","SIGMA","PRESSURE","UNDEFINED","MAGL","HYBRID","THETA","ETA","GALCHEN","ANGLE" };
   static CONST char *sopt[] = { "-tag","-component","-image","-nodata","-max","-min","-avg","-high","-low","-grid","-gridlat","-gridlon","-gridpoint","-coordpoint","-project","-unproject","-gridvalue","-coordvalue",
      "-gridstream","-coordstream","-within","-level","-levels","-leveltype","-limits","-matrix",NULL };
   enum        opt {  TAG,COMPONENT,IMAGE,NODATA,MAX,MIN,AVG,HIGH,LOW,GRID,GRIDLAT,GRIDLON,GRIDPOINT,COORDPOINT,PROJECT,UNPROJECT,GRIDVALUE,COORDVALUE,
      GRIDSTREAM,COORDSTREAM,WITHIN,LEVEL,LEVELS,LEVELTYPE,LIMITS,MATRIX };

   if (!Field ) {
      return(TCL_OK);
   }

   /*Calculer les statistiques si elle ne le sont pas deja*/
   if (!Field->Stat)
      Data_GetStat(Field);

   ex=Field->Spec->ExtrapDegree[0]!='N'?1:0;

   for(i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {

         case TAG:
            if (Objc==1) {
               if (Field->Tag) {
                  Tcl_SetObjResult(Interp,Field->Tag);
               }
            } else {
               if (Field->Tag) {
                  Tcl_DecrRefCount(Field->Tag);
               }
               Field->Tag=Objv[++i];
               Tcl_IncrRefCount(Field->Tag);
            }
            break;

         case NODATA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(Field->Def->NoData));
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Field->Def->NoData);
            }
            break;

         case COMPONENT:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(DSIZE(Field->Def->Data)));
            }
            break;

         case IMAGE:
            if(Objc!=2) {
               Tcl_WrongNumArgs(Interp,2,Objv,"image");
               return(TCL_ERROR);
            }
            return Data_GetImage(Interp,Field,Tcl_GetString(Objv[++i]));
            break;

         case MAX:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,Field->Stat->Max)));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MaxLoc.lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MaxLoc.lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MaxLoc.elev));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Field->Stat->Max);
               Field->Stat->Max=SPEC2VAL(Field->Spec,Field->Stat->Max);
            }
            break;

         case MIN:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,Field->Stat->Min)));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MinLoc.lat));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MinLoc.lon));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Stat->MinLoc.elev));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&Field->Stat->Min);
               Field->Stat->Min=SPEC2VAL(Field->Spec,Field->Stat->Min);
            }
            break;

         case AVG:
            Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,Field->Stat->Avg)));
            break;

         case HIGH:
            Tcl_GetIntFromObj(Interp,Objv[++i],&len);
            Tcl_SetObjResult(Interp,Data_HighLow(Interp,Field,1,len));
            break;

         case LOW:
            Tcl_GetIntFromObj(Interp,Objv[++i],&len);
            Tcl_SetObjResult(Interp,Data_HighLow(Interp,Field,0,len));
            break;

         case GRID:
            if (!Field->Ref) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               if (Field->Ref->Grid[0]!='V') {
                  for (i=0;i<Field->Def->NI;i++) {
                     for (j=0;j<Field->Def->NJ;j++) {
                        Field->Ref->Project(Field->Ref,i,j,&dlat,&dlon,0,1);
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
                     }
                  }
               } else {
                  for (index=0;index<Field->Def->NI;index++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Ref->Lat[index]));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Ref->Lon[index]));
                  }
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
               if (Field->Ref->Grid[0]=='V') {
                  Tcl_ListObjLength(Interp,Objv[++i],&nobj);
                  if ((nobj>>1)!=Field->Def->NI) {
                     Tcl_AppendResult(Interp,"Data_Stat: Invalid number of coordinates",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  if (!Field->Ref->Lat) {
                     Field->Ref->Lat=(float*)malloc(Field->Def->NI*sizeof(float));
                     Field->Ref->Lon=(float*)malloc(Field->Def->NI*sizeof(float));
                  }
                  for (index=0,n=0;index<nobj;index+=2,n++) {
                     Tcl_ListObjIndex(Interp,Objv[i],index,&obj);
                     Tcl_GetDoubleFromObj(Interp,obj,&dlat);
                     Tcl_ListObjIndex(Interp,Objv[i],index+1,&obj);
                     Tcl_GetDoubleFromObj(Interp,obj,&dlon);
                     Field->Ref->Lat[n]=dlat;
                     Field->Ref->Lon[n]=dlon;
                  }
               }
            }
            break;

         case GRIDLAT:
            if (!Field->Ref) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            obj=Tcl_NewListObj(0,NULL);
            if (Field->Ref->Grid[0]!='V') {
               for (i=0;i<Field->Def->NI;i++) {
                  for (j=0;j<Field->Def->NJ;j++) {
                     Field->Ref->Project(Field->Ref,i,j,&dlat,&dlon,0,1);
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
                  }
               }
            } else {
               for (index=0;index<Field->Def->NI;index++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Ref->Lat[index]));
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case GRIDLON:
            if (!Field->Ref) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            obj=Tcl_NewListObj(0,NULL);
            if (Field->Ref->Grid[0]!='V') {
               for (i=0;i<Field->Def->NI;i++) {
                  for (j=0;j<Field->Def->NJ;j++) {
                     Field->Ref->Project(Field->Ref,i,j,&dlat,&dlon,0,1);
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
                  }
               }
            } else {
               for (index=0;index<Field->Def->NI;index++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Ref->Lon[index]));
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case PROJECT:
            tr=0;
            ex=1;
         case GRIDPOINT:
            if (!Field->Ref) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dx);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dy);
            if (Field->Ref->Grid[0]=='Y') {
               if (!Field->Ref->Lat || !Field->Ref->Lon) {
                  Tcl_AppendResult(Interp,"Data_Stat: No positional information",(char*)NULL);
                  return(TCL_ERROR);
               }
               index=FIDX2D(Field->Def,(int)ROUND(dx),(int)ROUND(dy));
               dlat=Field->Ref->Lat[index];
               dlon=Field->Ref->Lon[index];
            } else {
               Field->Ref->Project(Field->Ref,dx,dy,&dlat,&dlon,ex,tr);
            }
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlat));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dlon));
            Tcl_SetObjResult(Interp,obj);
            break;

         case WITHIN:
            if (!Field->Ref) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }

            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlat0);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlon0);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlat1);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlon1);

            if (dlon0*dlon1<0) {
               dl=dlon1-dlon0;
            } else {
               dl=0;
            }

            obj=Tcl_NewListObj(0,NULL);
            if (Field->Ref->Grid[0]!='V') {
               for (ni=0;ni<Field->Def->NI;ni++) {
                  for (nj=0;nj<Field->Def->NJ;nj++) {
                     if (dlat0==0 && dlat1==0 && dlon0==0 && dlon1==0) {
                        f=1;
                     } else {
                        Field->Ref->Project(Field->Ref,ni,nj,&dlat,&dlon,0,1);
                        f=0;
                        if (dlat>=dlat0 && dlat<=dlat1) {
                           if (dl<=180) {
                              if (dlon>=dlon0 && dlon<=dlon1) {
                                 f=1;
                              }
                           } else {
                              if ((dlon<=dlon0 && dlon>-180) || (dlon>=dlon1 && dlon<180)) {
                                 f=1;
                              }
                           }
                        }
                     }
                     if (f) {
                        sub=Tcl_NewListObj(0,NULL);
                        Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(ni));
                        Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(nj));
                        Tcl_ListObjAppendElement(Interp,obj,sub);
                     }
                  }
               }
            } else {
               for (index=0;index<Field->Def->NI;index++) {
                  if (dlat0==0 && dlat1==0 && dlon0==0 && dlon1==0) {
                     f=1;
                  } else {
                     f=0;
                     if (Field->Ref->Lat[index]>=dlat0 && Field->Ref->Lat[index]<=dlat1) {
                        if (dl<=180) {
                           if (Field->Ref->Lon[index]>=dlon0 && Field->Ref->Lon[index]<=dlon1) {
                              f=1;
                           }
                        } else {
                           if ((Field->Ref->Lon[index]<=dlon0 && dlon>-180) || (Field->Ref->Lon[index]>=dlon1 && dlon<180)) {
                              f=1;
                           }
                        }
                     }
                  }
                  if (f) {
                     sub=Tcl_NewListObj(0,NULL);
                     Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(index));
                     Tcl_ListObjAppendElement(Interp,sub,Tcl_NewIntObj(0));
                     Tcl_ListObjAppendElement(Interp,obj,sub);
                  }
               }
            }
            Tcl_SetObjResult(Interp,obj);
            break;

         case UNPROJECT:
           tr=0;
           ex=1;
         case COORDPOINT:
            if (!Field->Ref) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlat);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlon);
            Field->Ref->UnProject(Field->Ref,&dx,&dy,dlat,dlon,ex,tr);
            obj=Tcl_NewListObj(0,NULL);
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dx));
            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(dy));
            Tcl_SetObjResult(Interp,obj);
           break;

         case GRIDVALUE:
            if (Objc==1) {
               for (index=0;index<FSIZE2D(Field->Def);index++) {
                  Def_GetMod(Field->Def,index,dval);
                  Tcl_SetObjResult(Interp,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,dval)));
               }
            } else {

               Tcl_GetDoubleFromObj(Interp,Objv[++i],&dx);
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&dy);

               if (Objc==4) {
                  Tcl_GetDoubleFromObj(Interp,Objv[++i],&dval);
                  Data_ValSet(Field,dx,dy,dval);
               } else {
#ifdef LNK_FSTD
                  c_ezsetopt("INTERP_DEGREE",Field->Spec->InterpDegree);
#endif
                  obj=Tcl_NewListObj(0,NULL);
                  if (Field->Def->NC>=2) {
                     if (Field->Ref->Value(Field->Ref,Field->Def,Field->Spec->InterpDegree[0],0,dx,dy,Field->Def->Level,&val,&val1)) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(val1));
                     } else {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
                     }
                  } else {
                     for(n=0;n<Field->Def->NC;n++) {
                        if (Field->Ref->Value(Field->Ref,Field->Def,Field->Spec->InterpDegree[0],n,dx,dy,Field->Def->Level,&val,&val1)) {
                           Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                        } else {
                           Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
                        }
                     }
                  }
                     Tcl_SetObjResult(Interp,obj);
               }
            }
            break;

         case COORDVALUE:
            if (!Field->Ref) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlat);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dlon);

            Field->Ref->UnProject(Field->Ref,&dx,&dy,dlat,dlon,1,1);

            if (Objc==4) {
               Tcl_GetDoubleFromObj(Interp,Objv[++i],&dval);
               Data_ValSet(Field,dx,dy,dval);
            } else {
#ifdef LNK_FSTD
               c_ezsetopt("INTERP_DEGREE",Field->Spec->InterpDegree);
#endif
               obj=Tcl_NewListObj(0,NULL);
               if (Field->Def->NC>=2) {
                 if (Field->Ref->Value(Field->Ref,Field->Def,Field->Spec->InterpDegree[0],0,dx,dy,Field->Def->Level,&val,&val1)) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(val1));
                  } else {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
                  }
               } else {
                  for(n=0;n<Field->Def->NC;n++) {
                     if (Field->Ref->Value(Field->Ref,Field->Def,Field->Spec->InterpDegree[0],n,dx,dy,Field->Def->Level,&val,&val1)) {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(VAL2SPEC(Field->Spec,val)));
                     } else {
                        Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
                     }
                  }
               }
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case GRIDSTREAM:
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dx);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dy);
            Tcl_GetIntFromObj(Interp,Objv[++i],&len);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dval);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dv);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dl);

            if (Field->Def->Data[1]) {
               b=FFStreamLine(Field->Ref,Field->Def,NULL,GDB_VBuf,NULL,dx,dy,0,len,-dval,dv,dl,REF_GRID,0);
               f=FFStreamLine(Field->Ref,Field->Def,NULL,&GDB_VBuf[len],NULL,dx,0,dy,len,dval,dv,dl,REF_GRID,0);
               obj=Tcl_NewListObj(0,NULL);
               for (nb=0;nb<b+f;nb++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(GDB_VBuf[len-b+nb][0]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(GDB_VBuf[len-b+nb][1]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(GDB_VBuf[len-b+nb][2]));
               }
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case COORDSTREAM:
            if (!Field->Ref) {
               Tcl_AppendResult(Interp,"Data_Stat: No geographic reference defined",(char*)NULL);
               return(TCL_ERROR);
            }
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dx);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dy);
            Tcl_GetIntFromObj(Interp,Objv[++i],&len);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dval);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dv);
            Tcl_GetDoubleFromObj(Interp,Objv[++i],&dl);

            if (Field->Def->Data[1]) {
               b=FFStreamLine(Field->Ref,Field->Def,NULL,GDB_VBuf,NULL,dx,dy,0,len,-dval,dv,dl,REF_COOR,0);
               f=FFStreamLine(Field->Ref,Field->Def,NULL,&GDB_VBuf[len],NULL,dx,dy,0,len,dval,dv,dl,REF_COOR,0);
               obj=Tcl_NewListObj(0,NULL);
               for (nb=0;nb<b+f;nb++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(GDB_VBuf[len-b+nb][0]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(GDB_VBuf[len-b+nb][1]));
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(GDB_VBuf[len-b+nb][2]));
               }
               Tcl_SetObjResult(Interp,obj);
            }
            break;

         case LIMITS:
            if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[0][0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[0][1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[1][0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[1][1]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[2][0]));
               Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Def->Limits[2][1]));
               Tcl_SetObjResult(Interp,obj);
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&nobj);

               if (nobj>6) {
                  Tcl_AppendResult(Interp,"Data_Stat: Invalid number of coordinates must be { i0 i1 j0 j1 k0 k1 }",(char*)NULL);
                  return(TCL_ERROR);
               }
               Tcl_ListObjIndex(Interp,Objv[i],0,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[0][0]);
               if (Field->Def->Limits[0][0]<0) Field->Def->Limits[0][0]=0.0;
               Tcl_ListObjIndex(Interp,Objv[i],1,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[0][1]);
               if (Field->Def->Limits[0][1]>Field->Def->NI-1) Field->Def->Limits[0][1]=Field->Def->NI-1;
               Tcl_ListObjIndex(Interp,Objv[i],2,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[1][0]);
               if (Field->Def->Limits[1][0]<0) Field->Def->Limits[1][0]=0.0;
               Tcl_ListObjIndex(Interp,Objv[i],3,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[1][1]);
               if (Field->Def->Limits[1][1]>Field->Def->NJ-1) Field->Def->Limits[1][1]=Field->Def->NJ-1;
               Tcl_ListObjIndex(Interp,Objv[i],4,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[2][0]);
               if (Field->Def->Limits[2][0]<0) Field->Def->Limits[2][0]=0.0;
               Tcl_ListObjIndex(Interp,Objv[i],5,&obj); Tcl_GetIntFromObj(Interp,obj,&Field->Def->Limits[2][1]);
               if (Field->Def->Limits[2][1]>Field->Def->NK-1) Field->Def->Limits[2][1]=Field->Def->NK-1;
               Data_Clean(Field,0,0,1);
            }
            break;

         case LEVELS:
            if (Objc==1) {
               if (Field->Ref) {
                  nb=(Field->Ref && Field->Ref->Grid[0]=='V')?Field->Def->NJ:Field->Def->NK;

                  obj=Tcl_NewListObj(0,NULL);
                  for (index=0;index<nb;index++) {
                     Tcl_ListObjAppendElement(Interp,obj,Tcl_NewDoubleObj(Field->Ref->Levels[index]));
                  }
                  Tcl_SetObjResult(Interp,obj);
               }
            } else {
               nb=(Field->Ref && Field->Ref->Grid[0]=='V')?Field->Def->NJ:Field->Def->NK;

               Tcl_ListObjLength(Interp,Objv[++i],&nobj);

               if (nobj>nb) {
                  Tcl_AppendResult(Interp,"Data_Stat: Too many levels",(char*)NULL);
                  return(TCL_ERROR);
               }
               for (index=0;index<nobj;index++) {
                  Tcl_ListObjIndex(Interp,Objv[i],index,&obj);
                  Tcl_GetDoubleFromObj(Interp,obj,&dv);
                  levels[index]=dv;
               }
               Field->Ref=GeoRef_Resize(Field->Ref,Field->Def->NI,Field->Def->NJ,Field->Def->NK,(Field->Ref?Field->Ref->LevelType:LVL_UNDEF),levels);
            }
            break;

         case LEVEL:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->Def->Level));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&n);
               if (n<0 || n>Field->Ref->LevelNb) {
                  Tcl_AppendResult(Interp,"Data_Stat: Invalid level index",(char*)NULL);
                  return(TCL_ERROR);
               }
               if (n!=Field->Def->Level) {
                  if (Field->Ref->LevelType==LVL_ANGLE) {
                     Field->Ref->CTH=cos(DEG2RAD(Field->Ref->Levels[n]));
                     Field->Ref->STH=sin(DEG2RAD(Field->Ref->Levels[n]));
                  }
                  Field->Def->Level=n;
                  Data_Clean(Field,0,0,1);
               }
            }
            break;

         case LEVELTYPE:
            if (Objc==1) {
               if (Field->Ref)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(type[Field->Ref->LevelType],-1));
            } else {
               if (Tcl_GetIndexFromObj(Interp,Objv[++i],type,"type",0,&index)!=TCL_OK) {
                  return(TCL_ERROR);
               }
               Field->Ref=GeoRef_Resize(Field->Ref,Field->Def->NI,Field->Def->NJ,Field->Def->NK,index,(Field->Ref?Field->Ref->Levels:NULL));
            }
            break;

         case MATRIX:
            if (Objc!=2) {
               Tcl_WrongNumArgs(Interp,2,Objv,"matrix var");
               return(TCL_ERROR);
            } else {
               i++;
               Tcl_UnsetVar2(Interp,Tcl_GetString(Objv[i]),NULL,0x0);
               for(ni=0;ni<Field->Def->NI;ni++) {
                  for(nj=0;nj<Field->Def->NJ;nj++) {
                     sprintf(buf,"%i,%i",nj+1,ni+1);
                     Def_Get(Field->Def,0,FIDX2D(Field->Def,ni,nj),val);
                     switch(Field->Def->Type) {
                        case TD_UByte:
                        case TD_Byte:
                        case TD_UInt16:
                        case TD_Int16:
                        case TD_UInt32:
                        case TD_Int32:   Tcl_SetVar2Ex(Interp,Tcl_GetString(Objv[i]),buf,Tcl_NewIntObj(val),0x0);break;
                        case TD_Float32:
                        case TD_Float64: Tcl_SetVar2Ex(Interp,Tcl_GetString(Objv[i]),buf,Tcl_NewDoubleObj(val),0x0);
                     }
                  }
               }
            }
            break;
      }
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Level2Meter>
 * Creation : Avril 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Derminer le niveaux en metre a d'un niveaux d'un autre type
 *
 * Parametres :
 *  <Type>    : Type de niveau.
 *  <Level>   : Valeur du niveau.
 *
 * Retour:
 *  <niveau>  : Niveau en metres
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
double Data_Level2Meter(int Type,double Level) {

   /* Dans le cas d'un niveau 0 (Mauvaise habitude premise pour sigma et eta a 0)*/
   if (Type==LVL_PRES && Level==0) {
      return(0);
   }

   switch(Type) {
      case LVL_MASL    : return (Level); break;
      case LVL_ETA     :
      case LVL_SIGMA   : return (SIGMA2METER(Level)); break;
      case LVL_PRES    : return (PRESS2METER(Level)); break;
      case LVL_UNDEF   : return (Level); break;
      case LVL_MAGL    : return (Level); break;
      case LVL_HYBRID  : return (SIGMA2METER(Level)); break;
      case LVL_THETA   : return (SIGMA2METER(Level)); break;
      case LVL_GALCHEN : return (Level); break;
      case LVL_ANGLE   : return (Level); break;
   }

   return(0.0);
}

void Data_ToString(char *String,TDataDef *Def,int Comp,int Idx,int Append) {

   char buf[32];

   switch(Def->Type) {
      case TD_Unknown:sprintf(buf,"");break;
      case TD_Binary: sprintf(buf,"");break;
      case TD_Byte:   sprintf(buf,"%hhi" ,((char*)Def->Data[Comp])[Idx]); break;
      case TD_UByte:  sprintf(buf,"%hhi" ,((unsigned char*)Def->Data[Comp])[Idx]);break;
      case TD_Int16:  sprintf(buf,"%hi"  ,((short*)Def->Data[Comp])[Idx]);break;
      case TD_UInt16: sprintf(buf,"%hi"  ,((unsigned short*)Def->Data[Comp])[Idx]);break;
      case TD_Int32:  sprintf(buf,"%i"   ,((int*)Def->Data[Comp])[Idx]);break;
      case TD_UInt32: sprintf(buf,"%u"   ,((unsigned int*)Def->Data[Comp])[Idx]);break;
      case TD_Float32:sprintf(buf,"%.7f" ,((float*)Def->Data[Comp])[Idx]);break;
      case TD_Float64:sprintf(buf,"%.25e",((double*)Def->Data[Comp])[Idx]);break;
   }
   if (Append) {
      strcat(String," ");
      strcat(String,buf);
   } else {
      sprintf(String,"%s",buf);
   }
}

void Data_FromString(char *String,TDataDef *Def,int Comp,int Idx) {

   switch(Def->Type) {
      case TD_Unknown:break;
      case TD_Binary: break;
      case TD_Byte:   ((char*)Def->Data[Comp])[Idx]=atoi(String); break;
      case TD_UByte:  ((unsigned char*)Def->Data[Comp])[Idx]=atoi(String);break;
      case TD_Int16:  ((short*)Def->Data[Comp])[Idx]=atoi(String);break;
      case TD_UInt16: ((unsigned short*)Def->Data[Comp])[Idx]=atoi(String);break;
      case TD_Int32:  ((int*)Def->Data[Comp])[Idx]=atoi(String);break;
      case TD_UInt32: ((unsigned int*)Def->Data[Comp])[Idx]=atoi(String);break;
      case TD_Float32:((float*)Def->Data[Comp])[Idx]=atof(String);break;
      case TD_Float64:((double*)Def->Data[Comp])[Idx]=atof(String);break;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_ValGet>
 * Creation : Juillet 1999 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Extrait les donnees du champs et les retourne a Tcl en liste
 *            de ligne (NJ ligne de NI donnees)
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <Field>    : Pointeur sur le champs
 *  <Type>     : Type des donnees
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void Data_ValGetMatrix(Tcl_Interp *Interp,TData *Field,int Type){

   int           i,j;
   char          buf[20];

   Tcl_DString data;
   Tcl_DStringInit(&data);

   for(j=0;j<Field->Def->NJ;j++){
      Tcl_DStringStartSublist(&data);
      for(i=0;i<Field->Def->NI;i++){
         Data_ToString(buf,Field->Def,0,j*Field->Def->NI+i,0);
         Tcl_DStringAppendElement(&data,buf);
      }
      Tcl_DStringEndSublist(&data);
   }

   Tcl_DStringResult(Interp,&data);
   Tcl_DStringFree(&data);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_ValPut>
 * Creation : Septembre 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Inserer une liste de valeur dans un champs.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Data>    : Pointeur sur les donnees du champs
 *  <List>    : Matrice de valeurs
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *   La matrice en question est une serie de listes imbriquees comprenant NJ listes
 *   de NI elements.
 *
 *----------------------------------------------------------------------------
*/
int Data_ValPutMatrix(Tcl_Interp *Interp,TData *Field,Tcl_Obj *List){

   Tcl_Obj *objj,*obji;
   int      i,j,nobjj,nobji;
   double   value;

   /*Extraire les nj lignes de donnees de la liste bidimensionnelle*/
   Tcl_ListObjLength(Interp,List,&nobjj);

   for (j=0;j<nobjj;j++){

      /*Extraire les ni points de la nj ieme ligne*/
      Tcl_ListObjIndex(Interp,List,j,&objj);
      Tcl_ListObjLength(Interp,objj,&nobji);

      /*Assigner les valeurs ni de la nj ieme ligne*/
      for (i=0;i<nobji;i++){
         Tcl_ListObjIndex(Interp,objj,i,&obji);
         Tcl_GetDoubleFromObj(Interp,obji,&value);
         Def_Set(Field->Def,0,j*nobji+i,value);
      }
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_ValSet>
 * Creation : Decembre 2002 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Modifier la valeur du champs a une position I,J.
 *
 * Parametres :
 *  <Field>   : Pointeur sur une structure de champ.
 *  <I>       : Coordonnee en I dans le champs
 *  <J>       : Coordonnee en J dans le champs
 *  <Val>     : Valeur a assigner
 *
 * Retour     :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *   -La valeur sera interpolee selon les parametres du champs et peut
 *    donc affecter plus d'un point
 *
 *----------------------------------------------------------------------------
*/
int Data_ValSet(TData *Field,float I,float J,float Val) {

   float dx0,dx1,dy0,dy1;
   int x0,y0,idx;

   if (I<0 || I>Field->Def->NI-1 || J<0 || J>Field->Def->NJ-1)
      return 0;

   if (Field->Stat) {
      free(Field->Stat);
      Field->Stat=NULL;
   }

   idx=ROUND(J)*Field->Def->NI+ROUND(I);
   Val=SPEC2VAL(Field->Spec,Val);
   Def_Set(Field->Def,0,idx,Val);
   return 1;

   if (Field->Spec->InterpDegree[0]=='N') {
      Field->Def->Data[0][(int)(ROUND(J)*Field->Def->NI+ROUND(I))]=Val;
   } else {
      x0=floor(I);
      y0=floor(J);

      if (x0<0 || x0>Field->Def->NI-2 || y0<0 || y0>Field->Def->NJ-2) {
         return 0;
      } else {
         dx1=I-floor(I);
         dx0=1.0-dx1;
         dy1=J-floor(J);
         dy0=1.0-dy1;
         Field->Def->Data[0][y0    *Field->Def->NI+x0]    =dy0*dx0*Val;
         Field->Def->Data[0][(y0+1)*Field->Def->NI+x0]    =dy1*dx0*Val;
         Field->Def->Data[0][y0    *Field->Def->NI+(x0+1)]=dy0*dx1*Val;
         Field->Def->Data[0][(y0+1)*Field->Def->NI+(x0+1)]=dy1*dx1*Val;
         return 1;
      }
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Data_AppendValueObj>
 * Creation     : Novembre 2004 J.P. Gauthier - CMC/CMOE
 *
 * But          : Recuperer la valeur d'une position dans une bance.
 *
 * Parametres  :
 *   <Band>     : Bande a afficher
 *   <X>        : Position en X
 *   <Y>        : Position en Y
 *   <Z>        : Position en Z
 *   <Val>      : Valeur de retour
 *
 * Retour       :
 *   <int>      : Interieur ou exterieur de la bande
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Tcl_Obj* Data_Val2Obj(TDataDef *Def,double Val) {

   Tcl_Obj *obj=NULL;

   switch(Def->Type) {
      case TD_UByte:
      case TD_Byte:
      case TD_UInt16:
      case TD_Int16:
      case TD_UInt32:
      case TD_Int32:   obj=Tcl_NewIntObj(Val);break;
      case TD_Float32:
      case TD_Float64: obj=Tcl_NewDoubleObj(Val);
   }
   return(obj);
}

Tcl_Obj* Data_AppendValueObj(Tcl_Interp *Interp,TDataDef *Def,int X,int Y) {

   Tcl_Obj *obj;
   int      i;
   double   val;

   obj=Tcl_NewListObj(0,NULL);
   for(i=0;i<Def->NC;i++) {
      if (X>=0 && X<Def->NI && Y>=0 && Y<Def->NJ) {
         Def_Get(Def,i,FIDX2D(Def,X,Y),val);
         Tcl_ListObjAppendElement(Interp,obj,Data_Val2Obj(Def,val));
      } else {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj("-",-1));
      }
   }
   return(obj);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_Within>
 * Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner si une valeur est a l'interieur des limites d'affichage du champs.
 *
 * Parametres :
 *  <Field>   : Pointeur sur une structure de champ.
 *  <Val>     : Valeur
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_Within(TData *Field,float Val) {

   if (Field->Spec->InterNb) {
      if (Val>=Field->Spec->Inter[0] && Val<=Field->Spec->Inter[Field->Spec->InterNb-1])
         return(1);
   } else {
      if (Val>=Field->Spec->Min && Val<=Field->Spec->Max)
         return(1);
   }
   return(0);
}

/*----------------------------------------------------------------------------
 * Nom      : <Data_WithinNb>
 * Creation : Novembre 2004 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner le nombre de valeur qui sont a l'interieur des limites d'affichage du champs.
 *
 * Parametres :
 *  <Field>   : Pointeur sur une structure de champ.
 *  <Val>     : Valeur
 *
 * Retour     :
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int Data_WithinNb(TData *Field) {

   int   n,t=0;
   float val;

   for(n=0;n<FSIZE2D(Field->Def);n++) {
      Def_GetMod(Field->Def,n,val);
      if (val>=Field->Spec->Min && val<=Field->Spec->Max)
         t++;
   }
   return(t);
}
