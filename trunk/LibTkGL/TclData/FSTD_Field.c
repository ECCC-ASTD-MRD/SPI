/*==============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de fichiers standards.
 * Fichier   : FSTD_Field.c
 * Creation  : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Manipulation de champs standards RPN dans des scripts Tcl.
 *
 * Remarques :
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
 *==============================================================================
 */

#include "tclFSTD.h"
#include "Projection.h"

TCL_DECLARE_MUTEX(MUTEX_FSTDVI)

typedef struct ThreadSpecificData {
   void *viInterp;
} ThreadSpecificData;

static Tcl_ThreadDataKey threadKey;

/*0=binary 1=real 2=unsigned integer 3=character 4=signed integer 5=IEEE style representation 6=whatever RPN comes with*/
static int FSTD_Type[]={ 1,10,6,2,7,10,10 };

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldSet>
 * Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
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
void FSTD_FieldSet(TData *Data){

   FSTD_Head *head;

   if (Data->Head && Data->Free)
      Data->Free(Data);

   head=(FSTD_Head*)malloc(sizeof(FSTD_Head));

   /*Initialiser les parametres de definition du champs*/
   head->IG1=0;
   head->IG2=0;
   head->IG3=0;
   head->IG4=0;
   head->FID=NULL;
   head->KEY=-1;
   head->DATEO=0;
   head->DATEV=0;
   head->DEET=0;
   head->NPAS=0;
   head->NBITS=16;
   head->DATYP=1;
   head->IP1=-1;
   head->IP2=head->IP3=0;
   head->TYPVAR[0]='\0';
   head->NOMVAR[0]='\0';
   head->ETIKET[0]='\0';

   Data->Head=head;
   Data->Set=FSTD_FieldSet;
   Data->Free=FSTD_FieldFree;
   Data->Copy=FSTD_HeadCopy;
   Data->Grid=FSTD_Grid;
   Data->ReadCube=FSTD_FieldReadLevels;
}

void FSTD_HeadCopy(void *To,void *From) {
   memcpy((FSTD_Head*)To,(FSTD_Head*)From,sizeof(FSTD_Head));
}

void FSTD_Project(Projection *Proj,Vect3d *Grid,unsigned long Nb) {

   float d;
   int   n;

   for (n=0;n<Nb;n++) {

      if (Proj->Params->Ref->AX) {
         Grid[n][0]=Proj->Params->Ref->AX[(int)Grid[n][0]]-Proj->Params->Ref->AX[0];
         Grid[n][1]=Proj->Params->Ref->AY[(int)Grid[n][1]]-Proj->Params->Ref->AY[0];
         d=Proj->Params->L*0.5;
      } else {
         d=(Proj->Params->L-1)*0.5;
      }

      Grid[n][0]=Grid[n][0]/d-Proj->Params->LI;
      Grid[n][1]=Grid[n][1]/d-Proj->Params->LJ;
      Grid[n][2]=1.0+Grid[n][2]*Proj->Params->Scale*Proj->Params->ZFactor;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldReadComp>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la lecture d'un champ complementaire.
 *
 * Parametres :
 *  <Head>    : Entete de la donnee
 *  <Ptr>     : Pointeur sur le vecteur a allouer
 *  <Var>     : Variable a lire
 *  <Grid>    : Utiliser les standard grille
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldReadComp(FSTD_Head *Head,float **Ptr,char *Var,int Grid) {

   int key,ni,nj,nk;

   if (!*Ptr) {
      if (Grid) {
         key=cs_fstinf(Head->FID->Id,&ni,&nj,&nk,-1,"",Head->IG1,Head->IG2,Head->IG3,"",Var);
      } else {
         key=cs_fstinf(Head->FID->Id,&ni,&nj,&nk,Head->DATEV,Head->ETIKET,Head->IP1,Head->IP2,Head->IP3,Head->TYPVAR,Var);
      }
      if (key<0) {
         fprintf(stderr,"(WARNING) FSTD_FieldReadComp: Could not find component field field %s (c_fstinf failed)\n",Var);
         return(0);
      } else {
         if (!(*Ptr=(float*)malloc(ni*nj*nk*sizeof(float)))) {
            fprintf(stderr,"(ERROR) FSTD_FieldReadComp: Not enough memory to read coordinates fields\n");
            return(0);
         }
         cs_fstluk(*Ptr,key,&ni,&nj,&nk);
      }
   }
   return(ni*nj*nk);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldReadMesh>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue les calculs et l'affichage pour le type particule.
 *
 * Parametres :
 *  <Field>   : Adresse des valeurs du champs
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldReadMesh(TData *Field) {

   FSTD_Head *head=(FSTD_Head*)Field->Head;
   int        key,ni,nj,nk;

#ifdef LNK_FSTD
   if (!Field->Ref || !(Field->Ref->Type&(GRID_SPARSE|GRID_VARIABLE|GRID_VERTICAL)))
      return(0);

   if ((!Field->Ref->Lat || !Field->Ref->Lon) && head->FID) {
      FSTD_FileSet(NULL,head->FID);

      switch(Field->Ref->Grid[0]) {
         case 'M':
            if (!Field->Ref->Lat) FSTD_FieldReadComp(head,&Field->Ref->Lat,"^^",1);
            if (!Field->Ref->Lon) FSTD_FieldReadComp(head,&Field->Ref->Lon,">>",1);

            /* Lire le champs d'indexes*/
            if (!Field->Ref->Idx) {
               key=cs_fstinf(head->FID->Id,&ni,&nj,&nk,-1,"",head->IG1,head->IG2,head->IG3,"","##");
               if (key < 0) {
                  fprintf(stderr,"(ERROR) FSTD_ReadMesh: Could not find index field %s (c_fstinf failed)","##");
                  return(0);
               } else {
                  Field->Ref->NIdx=ni*nj*nk;
                  if (!(Field->Ref->Idx=(unsigned int*)malloc(Field->Ref->NIdx*sizeof(unsigned int)))) {
                     fprintf(stderr,"(ERROR) FSTD_ReadMesh: Not enough memory to read coordinates fields");
                     return(0);
                  }
                  cs_fstluk((float*)Field->Ref->Idx,key,&ni,&nj,&nk);
               }
            }
            break;

         case 'W':
            if (Field->Ref->Grid[1]!='Y' && Field->Ref->Grid[1]!='Z')
               break;

         case 'Y':
            if (!Field->Ref->Lat) FSTD_FieldReadComp(head,&Field->Ref->Lat,"^^",1);
            if (!Field->Ref->Lon) FSTD_FieldReadComp(head,&Field->Ref->Lon,">>",1);
            if (!Field->Ref->Hgt) FSTD_FieldReadComp(head,&Field->Ref->Hgt,"ZH",0);
            break;

         case 'X':
            if (!Field->Ref->Lat) FSTD_FieldReadComp(head,&Field->Ref->Lat,"LA",0);
            if (!Field->Ref->Lon) FSTD_FieldReadComp(head,&Field->Ref->Lon,"LO",0);
            if (!Field->Ref->Hgt) FSTD_FieldReadComp(head,&Field->Ref->Hgt,"ZH",0);
            break;

         case 'V':
            if (!Field->Ref->Lat) FSTD_FieldReadComp(head,&Field->Ref->Lat,"^^",1);
            if (!Field->Ref->Lon) FSTD_FieldReadComp(head,&Field->Ref->Lon,">>",1);
            if (Field->Ref->Levels)
               free(Field->Ref->Levels);
            Field->Ref->Levels=NULL;
            Field->Ref->LevelNb=FSTD_FieldReadComp(head,&Field->Ref->Levels,"^>",1);
            break;
      }
      FSTD_FileUnset(NULL,head->FID);
   }
#endif
   return(Field->Ref->Lat && Field->Ref->Lon);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldGetMesh>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer le positionnement.
 *
 * Parametres :
 *  <Field>   : Adresse des valeurs du champs
 *  <Proj>    : Projection
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldGetMesh(TData *Field,Projection *Proj) {

   FSTD_Head *head=(FSTD_Head*)Field->Head;
   Coord      coord;
   int        i,j,k,sz,idx;
   double     z;
   float     *gz=NULL;

   if (!FSTD_FieldReadMesh(Field))
      return(0);

   /*Allouer les tableau de localisations et couleurs*/
   Field->Ref->Pos=(Vect3d*)malloc(FSIZE3D(Field->Def)*sizeof(Vect3d));

   if (!Field->Ref->Pos) {
      fprintf(stderr,"(ERROR) FSTD_FieldGetMesh: Not enough memory to store projected locations");
      return(0);
   } else {
      if (Field->Spec->Topo) {
         FSTD_FileSet(NULL,head->FID);
         EZLock_RPNField();
         idx=c_fstinf(head->FID->Id,&i,&j,&k,head->DATEV,head->ETIKET,head->IP1,head->IP2,head->IP3,head->TYPVAR,Field->Spec->Topo);
         if (idx<0) {
            fprintf(stderr,"(WARNING) FSTD_FieldGetMesh: Warning, Could not load corresponding topo field, trying for any (%s)\n",Field->Spec->Topo);
            idx=c_fstinf(head->FID->Id,&i,&j,&k,-1,"",-1,-1,-1,"",Field->Spec->Topo);
         }
         if (idx<0) {
            fprintf(stderr,"(WARNING) FSTD_FieldGetMesh: Could not load corresponding modulator (GZ)\n");
         } else {
            if (!gz) gz=(float*)malloc(i*j*k*sizeof(float));
            c_fstluk(gz,idx,&i,&j,&k);
         }
         EZUnLock_RPNField();
         FSTD_FileUnset(NULL,head->FID);
      }

      sz=FSIZE2D(Field->Def);
      /*Precalculer les tableaux de particules dans l'espace*/
      if (Field->Ref->Lat && Field->Ref->Lon) {
         for (k=0;k<Field->Def->NK;k++) {
            z=Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[k]);
            for (i=0;i<Field->Def->NI;i++) {
               for (j=0;j<Field->Def->NJ;j++) {

                  idx=j*Field->Def->NI+i;
                  coord.Elev=0.0;

                  /*Reproject coordinates if needed*/
                  if (Field->Ref->Grid[1]=='Z') {
                     Field->Ref->Project(Field->Ref,i,j,&coord.Lat,&coord.Lon,1,1);
                     if (gz) coord.Elev=gz[idx];
                  } else if (Field->Ref->Grid[1]=='Y') {
                     Field->Ref->Project(Field->Ref,i,j,&coord.Lat,&coord.Lon,1,1);
                  } else {
                     coord.Lat=Field->Ref->Lat[idx];
                     coord.Lon=Field->Ref->Lon[idx];
                  }

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
                  Vect_Init(Field->Ref->Pos[k*sz+idx],coord.Lon,coord.Lat,coord.Elev);
               }
            }
         }
         Proj->Type->Project(Proj->Params,Field->Ref->Pos,NULL,FSIZE3D(Field->Def));
      }
   }
   if (gz)
      free(gz);
   return(1);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_DataMap>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Precalcul des index dans la palette de couleur.
 *
 * Parametres   :
 *  <Field>     : Champs de donnees
 *  <Idx>       : Utiliser les index
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void FSTD_DataMap(TData *Field,int Idx) {

   int    i;
   double v;

   if (Field->Map) free(Field->Map);
   Field->Map=(float*)malloc(FSIZE2D(Field->Def)*sizeof(float));

   if (Idx) {
      if (Field->Ref->Idx) free(Field->Ref->Idx);
      Field->Ref->Idx=(unsigned int*)malloc(FSIZE2D(Field->Def)*sizeof(unsigned int));
      Field->Ref->NIdx=0;
   }

   for (i=0;i<FSIZE2D(Field->Def);i++) {
      Def_Get(Field->Def,0,i,v);
      VAL2COL(Field->Map[i],Field->Spec,v);
      Field->Map[i]/=(float)Field->Spec->Map->NbPixels;

      if (Idx && Field->Map[i]>=0) {
         Field->Ref->Idx[Field->Ref->NIdx++]=i;
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_Grid>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
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
Vect3d* FSTD_Grid(TData *Field,void *Proj) {

   FSTD_Head *head=(FSTD_Head*)Field->Head;
   Coord      coord;
   float     *lat,*lon,*gz=NULL,flat,flon,fele;
   int        i,j,k,idx,ni,nj,nk,ip1;
   int        idxi,idxk;

#ifdef LNK_FSTD
   /*Verifier la validite de la grille*/
   if (!Field->Ref || Field->Ref->Type==GRID_NONE)
      return(NULL);

   if (Field->Ref->Pos)
      return(Field->Ref->Pos);

   if (Field->Ref->Type&GRID_SPARSE) {
      FSTD_FieldGetMesh(Field,Proj);
      return(Field->Ref->Pos);
   }

   if (Field->Ref->Grid[0]=='V') {
      FSTD_FieldReadMesh(Field);
      if (!Field->Ref->Lat || !Field->Ref->Lon) {
         fprintf(stderr,"(ERROR) FSTD_Grid: Section coordinates not defined");
         return(NULL);
      }

      /*Localiser les point de grille dans l'espace*/
      Field->Ref->Pos=(Vect3d*)malloc(FSIZE2D(Field->Def)*sizeof(Vect3d));
      if (!Field->Ref->Pos) {
         fprintf(stderr,"(ERROR) FSTD_Grid: Not enough memory to calculate gridpoint location");
         return(NULL);
      }

      FSTD_FileSet(NULL,head->FID);
      for (j=0;j<Field->Def->NJ;j++) {

         /*Essayer de recuperer le modulateur (GZ)*/
         if (head->FID) {
            ip1=FSTD_Level2IP(Field->Ref->Levels[j],Field->Ref->LevelType);
            EZLock_RPNField();
            idx=c_fstinf(head->FID->Id,&ni,&nj,&nk,head->DATEV,head->ETIKET,ip1,head->IP2,head->IP3,head->TYPVAR,"GZ");
            if (idx<0) {
               if (gz) { free(gz); gz=NULL; };
               fprintf(stderr,"(WARNING) FSTD_Grid: Could not load corresponding modulator (GZ) (%f(%i)), using constant pressure\n",Field->Ref->Levels[j],ip1);
            } else {
               if (!gz) gz=(float*)malloc(ni*nj*nk*sizeof(float));
               c_fstluk(gz,idx,&ni,&nj,&nk);
            }
            EZUnLock_RPNField();
         }

         for (i=0;i<Field->Def->NI;i++) {
            flat=coord.Lat=Field->Ref->Lat[i];
            flon=coord.Lon=CLAMPLON(Field->Ref->Lon[i]);
            idx=j*Field->Def->NI+i;
            if (gz && Field->Ref->RefFrom->Id>-1) {
               EZLock_RPNInt();
               c_gdllsval(Field->Ref->RefFrom->Id,&fele,gz,&flat,&flon,1);
               EZUnLock_RPNInt();
               coord.Elev=fele*10.0*Field->Spec->TopoFactor;
            } else {
               coord.Elev=Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[j]);
            }
            coord.Elev*=Field->Spec->TopoFactor;
            Vect_Init(Field->Ref->Pos[idx],Field->Ref->Lon[i],Field->Ref->Lat[i],coord.Elev);
         }
      }
      if (Proj) {
         ((Projection*)Proj)->Type->Project(((Projection*)Proj)->Params,Field->Ref->Pos,NULL,FSIZE2D(Field->Def));
      }
      FSTD_FileUnset(NULL,head->FID);
   } else {

      if (Field->Ref->Id>-1) {
         /*Recuperer les coordonnees des points de grille*/
         lat=(float*)malloc(FSIZE2D(Field->Def)*sizeof(float));
         lon=(float*)malloc(FSIZE2D(Field->Def)*sizeof(float));

         if (!lat || !lon) {
            fprintf(stderr,"(ERROR) FSTD_Grid: Not enough memory to process gridpoint location");
            return(NULL);
         }
         EZLock_RPNInt();
         c_gdll(Field->Ref->Id,lat,lon);
         EZUnLock_RPNInt();
      }

      /*Localiser les point de grille dans l'espace*/
      Field->Ref->Pos=(Vect3d*)malloc(FSIZE3D(Field->Def)*sizeof(Vect3d));
      if (!Field->Ref->Pos) {
         fprintf(stderr,"(ERROR) FSTD_Grid: Not enough memory to calculate gridpoint location");
         return(NULL);
      }

      FSTD_FileSet(NULL,head->FID);
      for (k=0;k<Field->Def->NK;k++) {

         /*Essayer de recuperer le GZ*/
         if (head->FID && Field->Spec->Topo) {

            ip1=FSTD_Level2IP(Field->Ref->Levels[k],Field->Ref->LevelType);
            EZLock_RPNField();
            if (Field->Spec->Topo) {
               idx=c_fstinf(head->FID->Id,&ni,&nj,&nk,head->DATEV,head->ETIKET,ip1,head->IP2,head->IP3,head->TYPVAR,Field->Spec->Topo);
               if (idx<0) {
                   fprintf(stderr,"(WARNING) FSTD_Grid: Could not load corresponding topo field, trying for any (%s)\n",Field->Spec->Topo);
                   idx=c_fstinf(head->FID->Id,&ni,&nj,&nk,-1,"",-1,-1,-1,"",Field->Spec->Topo);
                }
                if (ni!=Field->Def->NI || nj!=Field->Def->NJ || nk!=Field->Def->NK) {
                   idx=-1;
                }
            } else {
               idx=-1;
            }
            if (idx<0) {
               if (gz) { free(gz); gz=NULL; };
               fprintf(stderr,"(WARNING) FSTD_Grid: Could not load corresponding (%s) (%f(%i)), using constant pressure\n",Field->Spec->Topo,Field->Ref->Levels[k],ip1);
            } else {
               if (!gz) gz=(float*)malloc(ni*nj*nk*sizeof(float));
               c_fstluk(gz,idx,&ni,&nj,&nk);
            }
            EZUnLock_RPNField();
         }

         /*For every gridpoints*/
         for (j=0;j<Field->Def->NJ;j++) {
            for (i=0;i<Field->Def->NI;i++) {

               /*Figure out table plane indexes*/
               idxi=j*Field->Def->NI+i;
               idxk=k*Field->Def->NI*Field->Def->NJ+idxi;

               if (gz) {
                  coord.Elev=gz[idxi]*Field->Spec->TopoFactor;
                  if (Field->Spec->Topo[0]=='G' && Field->Spec->Topo[1]=='Z' ) {
                     coord.Elev*=10.0;
                  }
               } else {
                  coord.Elev=Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[k]);
               }
               coord.Elev*=Field->Spec->TopoFactor;

               if (((Projection*)Proj)->Type->Def==PROJPLANE && ((Projection*)Proj)->Params->Ref && ((Projection*)Proj)->Params->Ref->Id==Field->Ref->Id) {
                  Vect_Init(Field->Ref->Pos[idxk],i,j,coord.Elev);
               } else {
                  if (Field->Ref->Id>-1) {
                     coord.Lat=lat[idxi];
                     coord.Lon=CLAMPLON(lon[idxi]);
                  } else {
                     Field->Ref->Project(Field->Ref,i,j,&coord.Lat,&coord.Lon,0,1);
                  }

                  Vect_Init(Field->Ref->Pos[idxk],coord.Lon,coord.Lat,coord.Elev);
               }
            }
         }
      }
      if (((Projection*)Proj)->Type->Def==PROJPLANE && ((Projection*)Proj)->Params->Ref && ((Projection*)Proj)->Params->Ref->Id==Field->Ref->Id) {
         FSTD_Project(((Projection*)Proj),Field->Ref->Pos,FSIZE3D(Field->Def));
      } else {
         ((Projection*)Proj)->Type->Project(((Projection*)Proj)->Params,Field->Ref->Pos,NULL,FSIZE3D(Field->Def));
      }

      FSTD_FileUnset(NULL,head->FID);
      if (Field->Ref->Id>-1) {
         free(lat);
         free(lon);
      }
   }

   if (gz) free(gz);
#endif
   return(Field->Ref->Pos);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_DecodeHybrid>
 * Creation : Novembre 2008 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Decoder les parametres des niveaux hybrides a partir du champs
 *
 * Parametres   :
 *  <Unit>      : Unite du ficher
 *  <IP2>       : IP2
 *  <IP3>       : IP3
 *  <Etiket>    : Etiket
 *  <Datev>     : Date de validitee
 *  <PTop>      : Pression au top
 *  <PRef>      : Pression de reference
 *  <RCoef>     : Coeeficient
 *
 * Retour:
 *  <TCL_...> : (Index du champs HY <0=erreur).
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int FSTD_DecodeHybrid(int Unit,char* Var,int IP2,int IP3,char *Etiket,int DateV,float *PTop,float *PRef,float *RCoef) {

   int   l,deet,ip1a,ip2a,ip3a,ig1a,ig2a,ig3a,ig4a,bit;
   int   idayo,dty,swa,lng,dlf,ubc,ex1,ex2, ex3;
   int   npas,nia,nja,i,j,k,ierr,kind,flag=0,mode=-1;
   char  typ,grda,blk_S;
   char  var[5];
   char  labanl[13];

   l = c_fstinf(Unit,&i,&j,&k,DateV,Etiket,-1,IP2,IP3,"X",Var);
   if (l>=0) {
       ierr= c_fstprm(l,&idayo,&deet,&npas,&nia,&nja,&k,&bit,&dty,&ip1a,&ip2a,&ip3a,&typ,var,labanl,&grda,
                    &ig1a,&ig2a,&ig3a,&ig4a,&swa,&lng,&dlf,&ubc,&ex1,&ex2,&ex3);
       f77name(convip)(&ip1a,PTop,&kind,&mode,&blk_S,&flag);
       *RCoef=ig2a/1000.0f;
       *PRef=ig1a;
   }
   return(l);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldVertInterpolate>
 * Creation : Avril 2003 - S. Gaudreault - CMC/CMOE
 *
 * But      : Effectue l'interpolation d'un champs dans un autre
 *
 * Parametres   :
 *  <Interp>    : Interpreteur TCL
 *  <FieldTo>   : Champs de destination
 *  <FieldFrom> : Champs d'origine
 *  <ZFieldFrom>: Champs GZ d'origine
 *  <ZFieldTo>  : Champs GZ de destination
 *  <PField>    : Champs P0
 *  <Top>       : Plafond
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int FSTD_FieldVertInterpolate(Tcl_Interp *Interp,TData *FieldTo,TData *FieldFrom,TData *ZFieldTo,TData *ZFieldFrom,TData *PField0,TData *PField1,double Top) {

   float  *gzfrom=NULL,*gzto=NULL,*p0=NULL,*p1=NULL,ptop,pref,rcoef;
   int     gridfrom,gridto,i,id;
   void   *pto,*pfrom;

   FSTD_Head *headto=(FSTD_Head*)FieldTo->Head;
   FSTD_Head *headfrom=(FSTD_Head*)FieldFrom->Head;

   ThreadSpecificData *threadData=(ThreadSpecificData*)Tcl_GetThreadData((&threadKey),sizeof(ThreadSpecificData));

   extern void* c_videfine();
   extern void* c_viundefine(void*);
   extern int c_viqkdef(void*,const int numLevel, const int gridType, float *levelList, float top, float pRef, float rCoef, float *gz);
   extern int c_videfset (void*,const int ni, const int nj, int idGrdDest, int idGrdSrc,float *surf, float *top);
   extern int c_visint(void*,float *stateOut, float *stateIn, float *derivOut,float *derivIn,float extrapGuideDown,float extrapGuideUp);
   extern int c_visetopt(void*,const char *option, const char *value);

   if (FieldFrom->Def->Type!=TD_Float32 || FieldTo->Def->Type!=TD_Float32) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid Field data type, must be float",(char*)NULL);
      return TCL_ERROR;
   }

   if (!FieldFrom) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Origin field not valid",(char*) NULL);
      return TCL_ERROR;
   }

   if (!FieldTo) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Destination field not valid",(char*) NULL);
      return TCL_ERROR;
   }

   if (FieldFrom->Def->NI!=FieldTo->Def->NI || FieldFrom->Def->NJ!=FieldTo->Def->NJ) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Grid sizes differ",(char*)NULL);
      return TCL_ERROR;
   }

   if (FieldFrom->Ref->LevelType==LVL_SIGMA || FieldFrom->Ref->LevelType==LVL_HYBRID || FieldFrom->Ref->LevelType==LVL_ETA) {
      if (FieldTo->Ref->LevelType==LVL_MAGL || FieldTo->Ref->LevelType==LVL_MASL ||  FieldTo->Ref->LevelType==LVL_GALCHEN) {
         if (!ZFieldFrom) {
            Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Need GZ source field data",(char*)NULL);
            return TCL_ERROR;
         }
      } else {
         if (!PField0) {
            Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Need Pressure source field data",(char*)NULL);
            return TCL_ERROR;
         }
      }
   } else {
   }

   /*Recuperer tout les niveaux disponibles*/
   if (FieldFrom->ReadCube)
      FieldFrom->ReadCube(Interp,FieldFrom,0);

   if (ZFieldFrom) {
      if (ZFieldFrom->ReadCube)
         ZFieldFrom->ReadCube(Interp,ZFieldFrom,0);

      if (ZFieldFrom->Def->NI!=FieldFrom->Def->NI || ZFieldFrom->Def->NJ!=FieldFrom->Def->NJ) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid GZ source field dimensions",(char*)NULL);
         return TCL_ERROR;
      }
      gzfrom=(float*)(ZFieldFrom->Def->Data[0]);
   }

   if (ZFieldTo) {
      if (ZFieldTo->ReadCube)
         ZFieldTo->ReadCube(Interp,ZFieldTo,0);

      if (ZFieldTo->Def->NI!=FieldTo->Def->NI || ZFieldTo->Def->NJ!=FieldTo->Def->NJ) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid GZ destination field dimensions",(char*)NULL);
         return TCL_ERROR;
      }
      gzto=(float*)(ZFieldTo->Def->Data[0]);
   }

   if (PField0) {
      p0=(float*)(PField0->Def->Data[0]);
   } else {
      p0=NULL;
   }

   if (PField1) {
      p1=(float*)(PField1->Def->Data[0]);
   } else {
      p1=NULL;
   }

   /* Definition des grilles */
   /* Définition des algorithmes d'[inter/extra]polation */
   if (!threadData->viInterp) {
      threadData->viInterp=c_videfine();
   }
   ptop=Top;
   pref=800.0f;
   rcoef=1.0f;

   Tcl_MutexLock(&MUTEX_FSTDVI);
   c_visetopt(threadData->viInterp,"INTERP_DEGREE",FieldTo->Spec->InterpDegree);
   c_visetopt(threadData->viInterp,"VERBOSE","NO");

   /*Try to read HY for hybrid levels*/
   if (FieldFrom->Ref->LevelType==LVL_HYBRID) {
      i=-1;
      id=(((FSTD_Head*)FieldFrom->Head)->FID->Id);
      FSTD_FileSet(NULL,((FSTD_Head*)FieldFrom->Head)->FID);
      if (FSTD_DecodeHybrid(id,"HY   ",i,i,"             ",i,&ptop,&pref,&rcoef)<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: (WARNING) Could not find hybrid definition field HY",(char*) NULL);
      }
      FSTD_FileUnset(NULL,((FSTD_Head*)FieldFrom->Head)->FID);
   }

   if ((gridfrom=c_viqkdef(threadData->viInterp,FieldFrom->Def->NK,FieldFrom->Ref->LevelType,FieldFrom->Ref->Levels,ptop,pref,rcoef,gzfrom))<0) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Could not initialize source grid (c_viqkdef)",(char*) NULL);
      Tcl_MutexUnlock(&MUTEX_FSTDVI);
      return(TCL_ERROR);
   }

   if ((gridto=c_viqkdef(threadData->viInterp,FieldTo->Def->NK,FieldTo->Ref->LevelType,FieldTo->Ref->Levels,ptop,pref,rcoef,gzto))<0) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Could not initialize destination grid (c_viqkdef)",(char*) NULL);
      Tcl_MutexUnlock(&MUTEX_FSTDVI);
      return TCL_ERROR;
   }

   if (c_videfset(threadData->viInterp,FieldFrom->Def->NI,FieldFrom->Def->NJ,gridto,gridfrom,p0,p1)<0) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Incompatible grids (c_videfset)",(char*) NULL);
      Tcl_MutexUnlock(&MUTEX_FSTDVI);
      return TCL_ERROR;
   }

   /* Inter ET/OU Extrapolation */
   for(i=0;i<3;i++) {
      if (FieldFrom->Def->Data[i]) {
         if (!FieldTo->Def->Data[i]) {
            FieldTo->Def->Data[i]=(char*)calloc(FieldTo->Def->NI*FieldTo->Def->NJ*FieldTo->Def->NK,TData_Size[FieldTo->Def->Type]);
            if (!FieldTo->Def->Data[i]) {
               Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Not enough memory to allocate field",(char*)NULL);
               Tcl_MutexUnlock(&MUTEX_FSTDVI);
               return TCL_ERROR;
            }
         }
         Def_Pointer(FieldFrom->Def,i,0,pfrom);
         Def_Pointer(FieldTo->Def,i,0,pto);

         if (c_visint(threadData->viInterp,pto,pfrom,NULL,NULL,0,0)<0) {
           Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Interpolation error (c_visint)",(char*)NULL);
           Tcl_MutexUnlock(&MUTEX_FSTDVI);
           return TCL_ERROR;
         }
      } else {
         if (FieldTo->Def->Data[i]) {
            free(FieldTo->Def->Data[i]);
            FieldTo->Def->Data[i]=NULL;
         }
      }
   }

   if (!FieldTo->Ref || FieldTo->Ref->Id!=FieldFrom->Ref->Id)
      FieldTo->Ref=GeoRef_Resize(FieldFrom->Ref,FieldTo->Def->NI,FieldTo->Def->NJ,FieldTo->Def->NK,FieldTo->Ref->LevelType,FieldTo->Ref->Levels);
   memcpy(headto,headfrom,sizeof(FSTD_Head));

   if (FieldTo->Stat) {
      free(FieldTo->Stat);
      FieldTo->Stat=NULL;
   }
   Tcl_MutexUnlock(&MUTEX_FSTDVI);
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldSetTo>
 * Creation : Mai 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Copier les parametres des champs necessaire dans les interpolations
 *
 * Parametres  :
 *  <FieldTo>  : Champs de destination
 *  <FieldFrom>: Champs d'origine
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void FSTD_FieldSetTo(TData *FieldTo,TData *FieldFrom) {

   FSTD_Head *headto=(FSTD_Head*)FieldTo->Head;
   FSTD_Head *headfrom=(FSTD_Head*)FieldFrom->Head;

   /*Initialiaser les valeurs de descriptions du champs destination*/
   if (FieldTo->Stat) {
      free(FieldTo->Stat);
      FieldTo->Stat=NULL;
   }

   headto->FID=headfrom->FID;
   headto->DATEO=headfrom->DATEO;
   headto->DATEV=headfrom->DATEV;
   headto->DEET=headfrom->DEET;
   headto->NPAS=headfrom->NPAS;
   headto->NBITS=headfrom->NBITS;
   headto->DATYP=headfrom->DATYP;
   headto->IP1=headfrom->IP1;
   headto->IP2=headfrom->IP2;
   headto->IP3=headfrom->IP3;
   strcpy(headto->TYPVAR,headfrom->TYPVAR);
   strcpy(headto->NOMVAR,headfrom->NOMVAR);
   strcpy(headto->ETIKET,headfrom->ETIKET);

   /*This is safe since RPN always allocates float size buffers*/
   FieldTo->Def->Type=FieldFrom->Def->Type;
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldGridInterpolate>
 * Creation : Fevrier 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'interpolation d'un champs dans un autre
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <FieldTo>  : Champs de destination
 *  <FieldFrom>: Champs d'origine
 *  <Mode>     : Mode d'interpolation (0=NEAREST, 1=LINEAR, 6=CUBIC, autre=INTERPDEGREE du champs FieldTo)
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldGridInterpolate(Tcl_Interp *Interp,TData *FieldTo,TData *FieldFrom,int Mode){

   double     val,lat,lon,di,dj;
   float      tmpf;
   int        ez=1,ok=-1,idx,n,i,j,k;
   void      *pf0,*pt0,*pf1,*pt1;

#ifdef LNK_FSTD
   if (!FieldFrom || !FieldFrom->Ref) {
      Tcl_AppendResult(Interp,"FSTD_FieldGridInterpolate: Origin field not valid",(char*)NULL);
      return(TCL_ERROR);
   }
   if (!FieldTo || !FieldTo->Ref) {
      Tcl_AppendResult(Interp,"FSTD_FieldGridInterpolate: Destination field not valid",(char*)NULL);
      return(TCL_ERROR);
   }

   if (FieldTo->Def->Mode && FieldTo->Def->Mode!=FieldTo->Def->Data[0]) {
      free(FieldTo->Def->Mode);
   }
   FieldTo->Def->Mode=NULL;

   /*Verifier la dimension verticale*/
   if (FieldTo->Def->NK!=FieldFrom->Def->NK) {
      if (FieldTo->Def->Data[1]) {
         free(FieldTo->Def->Data[1]);
         FieldTo->Def->Data[1]=NULL;
      }
      free(FieldTo->Def->Data[0]);
      FieldTo->Def->NK=FieldFrom->Def->NK;
      FieldTo->Def->Data[0]=(char*)calloc(FSIZE3D(FieldTo->Def),TData_Size[FieldTo->Def->Type]);
      FieldTo->Ref=GeoRef_Resize(FieldTo->Ref,FieldTo->Def->NI,FieldTo->Def->NJ,FieldTo->Def->NK,FieldFrom->Ref->LevelType,FieldFrom->Ref->Levels);
   } else {
      FieldTo->Ref->LevelType=FieldFrom->Ref->LevelType;
   }

   /*Verifier la 2ieme composantes*/
   if (FieldFrom->Def->Data[1]) {
      if (!FieldTo->Def->Data[1]) {
         FieldTo->Def->Data[1]=(char*)calloc(FSIZE3D(FieldTo->Def),TData_Size[FieldTo->Def->Type]);
      }
      FieldTo->Def->NC=2;
   } else{
     if (FieldTo->Def->Data[1]) {
         free(FieldTo->Def->Data[1]);
         FieldTo->Def->Data[1]=NULL;
      }
      FieldTo->Def->NC=1;
   }

   if (FieldFrom->Def->Type!=TD_Float32) {
      ez=0;
   }

   if (FieldFrom->Ref->Grid[0]=='W' || FieldTo->Ref->Grid[0]=='W') {
      ez=0;
   }

   FSTD_FieldSetTo(FieldTo,FieldFrom);

   EZLock_RPNInt();

   if (Mode==0) {
      c_ezsetopt("INTERP_DEGREE","NEAREST");
   } else if (Mode==1) {
      c_ezsetopt("INTERP_DEGREE","LINEAR");
   } else if (Mode==2) {
      c_ezsetopt("INTERP_DEGREE","CUBIC");
   } else {
      c_ezsetopt("INTERP_DEGREE",FieldTo->Spec->InterpDegree);
   }

   if (FieldTo->Spec->ExtrapDegree[0]=='V') {
      tmpf=FieldTo->Def->NoData;
      c_ezsetval("EXTRAP_VALUE",&tmpf);
   }
   c_ezsetopt("EXTRAP_DEGREE",FieldTo->Spec->ExtrapDegree);

   /*Use ezscint*/
   if (ez) {
      ok=c_ezdefset(FieldTo->Ref->Id,FieldFrom->Ref->Id);

      if (ok<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldGridInterpolate:  EZSCINT internal error, could not define gridset",(char*)NULL);
         EZUnLock_RPNInt();
         return(TCL_ERROR);
      }

      for(k=0;k<FieldTo->Def->NK;k++) {
         /*Effectuer l'interpolation selon le type de champs*/
         if (FieldTo->Def->Data[1]) {
            /*Interpolation vectorielle*/
            Def_Pointer(FieldTo->Def,0,k*FSIZE2D(FieldTo->Def),pt0);
            Def_Pointer(FieldFrom->Def,0,k*FSIZE2D(FieldFrom->Def),pf0);
            Def_Pointer(FieldTo->Def,1,k*FSIZE2D(FieldTo->Def),pt1);
            Def_Pointer(FieldFrom->Def,1,k*FSIZE2D(FieldFrom->Def),pf1);

            /*In case of Y grid, get the speed and dir instead of wind components
              since grid oriented components dont mean much*/
            if (FieldTo->Ref->Grid[0]=='Y') {
               ok=c_ezwdint(pt0,pt1,pf0,pf1);
            } else {
               ok=c_ezuvint(pt0,pt1,pf0,pf1);
            }
        } else{
            /*Interpolation scalaire*/
            Def_Pointer(FieldTo->Def,0,k*FSIZE2D(FieldTo->Def),pt0);
            Def_Pointer(FieldFrom->Def,0,k*FSIZE2D(FieldFrom->Def),pf0);
            ok=c_ezsint(pt0,pf0);
         }
         FieldTo->Ref->Levels[k]=FieldFrom->Ref->Levels[k];
      }
      if (ok<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldGridInterpolate: EZSCINT internal error, interpolation problem",(char*)NULL);
         EZUnLock_RPNInt();
         return(TCL_ERROR);
      }
  } else {
      for(i=0;i<FieldTo->Def->NI;i++) {
         for(j=0;j<FieldTo->Def->NJ;j++) {
            for(k=0;k<FieldTo->Def->NK;k++) {
               idx=FIDX3D(FieldTo->Def,i,j,k);

               FieldTo->Ref->Project(FieldTo->Ref,i,j,&lat,&lon,0,1);
               ok=FieldFrom->Ref->UnProject(FieldFrom->Ref,&di,&dj,lat,lon,0,1);
               n=0;
               while(FieldTo->Def->Data[n]) {
                  if (ok) {
                     val=VertexValN(FieldFrom->Ref,FieldFrom->Def,n,di,dj,k);
                  } else {
                     val=FieldTo->Def->NoData;
                  }
                  Def_Set(FieldTo->Def,n,idx,val);
                  n++;
               }
            }
         }
      }
   }
   EZUnLock_RPNInt();
#endif
   FieldTo->Def->Mode=NULL;
   /*In case of vectorial field, we have to recalculate the module*/
   if (FieldTo->Def->NC>1) {
      Data_GetStat(FieldTo);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldTimeInterpolate>
 * Creation : Septembre 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue l'interpolation d'un champs dans un autre
 *
 * Parametres  :
 *  <Interp>   : Interpreteur TCL
 *  <Stamp>    : Datestamp de retour
 *  <Interp>   : Champs de destination
 *  <Field0>   : Champs initial
 *  <Field1>   : Champs terminal
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldTimeInterpolate(Tcl_Interp *Interp,int Stamp,char *Name,TData *Field0,TData *Field1) {

   int   i,n,idx=0;
   double delay,dt,v0,v1,vr;

   TData     *field;
   FSTD_Head *head0=(FSTD_Head*)Field0->Head;
   FSTD_Head *head1=(FSTD_Head*)Field1->Head;

   extern difdatr();

#ifdef LNK_FSTD

   if (!Field0) {
      Tcl_AppendResult(Interp,"FSTD_FieldTimeInterpolate: Initial field not valid",(char*)NULL);
      return TCL_ERROR;
   }
   if (!Field1) {
      Tcl_AppendResult(Interp,"FSTD_FieldTimeInterpolate: Terminal field not valid",(char*)NULL);
      return TCL_ERROR;
   }
   if (Field0->Def->NI!=Field1->Def->NI || Field0->Def->NJ!=Field1->Def->NJ || Field0->Def->NK!=Field1->Def->NK) {
      Tcl_AppendResult(Interp,"FSTD_FieldTimeInterpolate: Incompatible size",(char*)NULL);
      return TCL_ERROR;
   }

   /* Est-ce que le champs existe et si oui, verifier les dimensions */
   field=Data_Valid(Interp,Name,Field0->Def->NI,Field0->Def->NJ,Field0->Def->NK,Field0->Def->Data[2]?3:Field0->Def->Data[1]?2:1,Field0->Def->Type);
   if (!field)
      return TCL_ERROR;

   Field0->Set(field);

   /*Figure out le delai entre les deux champs*/
   f77name(difdatr)(&head1->DATEV,&head0->DATEV,&delay);
   f77name(difdatr)(&Stamp,&head0->DATEV,&dt);

   dt=dt/delay;

  /*Interpoler le champs*/
   for(i=0;i<FSIZE3D(Field0->Def);i++) {
      for(n=0;n<3;n++) {
         if (Field0->Def->Data[n]) {
            Def_Get(Field0->Def,n,idx,v0);
            Def_Get(Field1->Def,n,idx,v1);
            vr=ILIN(v0,v1,dt);
            Def_Set(field->Def,n,idx,vr);
         }
      }
      idx++;
   }

   /*Initialiser les valeurs de descriptions du champs destination*/
   if (field->Stat) {
      free(field->Stat);
      field->Stat=NULL;
   }

   memcpy(field->Head,head0,sizeof(FSTD_Head));
   field->Spec->Map=Field0->Spec->Map;

   /*Modifier les parametres de dates*/
   head0=(FSTD_Head*)field->Head;
   head0->DATEV=Stamp;
   head0->DATEO=Stamp;
   head0->NPAS=0;
   head0->DEET=0;
   head0->IP2=0;

   if (field->Ref)
      GeoRef_Destroy(Interp,field->Ref->Name);
  field->Ref=GeoRef_Copy(Field0->Ref);
#endif
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldDefine>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
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
int FSTD_FieldDefine(Tcl_Interp *Interp,TData *Field,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj     *obj;
   TData       *fieldAX,*fieldAY;
   FSTD_Head   *head=(FSTD_Head*)Field->Head;
   TGeoRef     *ref;
   int          i,j,idx,nidx;
   char         buf[64],*grtyp;
   double       dxg1,dxg2,dxg3,dxg4;
   float        xg1,xg2,xg3,xg4;
   double       tra[6],inv[6],*tm,*im;
   const char **list;

   static CONST char *sopt[] = { "-DATEO","-DATEV","-DEET","-FID","-KEY","-NPAS","-NI","-NJ","-NK","-NBITS","-DATYP","-IP1","-IP2","-IP3",
                                 "-TYPVAR","-NOMVAR","-ETIKET","-GRIDID","-GRTYP","-IG1","-IG2","-IG3","-IG4","-SWA","-LNG","-DLTF",
                                 "-UBC","-EX1","-EX2","-EX3","-DATA","-positional","-projection","-transform","-georef",NULL };
   enum        opt { DATEO,DATEV,DEET,FID,KEY,NPAS,NI,NJ,NK,NBITS,DATYP,IP1,IP2,IP3,TYPVAR,NOMVAR,ETIKET,GRIDID,GRTYP,
                     IG1,IG2,IG3,IG4,SWA,LNG,DLTF,UBC,EX1,EX2,EX3,DATA,POSITIONAL,PROJECTION,TRANSFORM,GEOREF };

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
               sprintf(buf,"%09i",head->DATEO);
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
            } else {
               i++;
               TclY_Get0IntFromObj(Interp,Objv[i],&head->DATEO);
            }
            break;

         case DATEV:
            if (Objc==1) {
               sprintf(buf,"%09i",head->DATEV);
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(buf,-1));
            } else {
               i++;
               TclY_Get0IntFromObj(Interp,Objv[i],&head->DATEV);
            }
            break;

         case DEET:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->DEET));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->DEET);
            }
            break;

         case FID:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(head->FID->CId,-1));
            } else {
               head->FID=FSTD_FileGet(Interp,Tcl_GetString(Objv[++i]));
            }
            break;

         case KEY:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->KEY));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->KEY);
            }
            break;

         case NPAS:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->NPAS));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->NPAS);
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

         case NBITS:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->NBITS));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->NBITS);
            }
            break;

         case DATYP:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->DATYP));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->DATYP);
            }
            break;

         case IP1:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->IP1));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->IP1);
               if (!Field->Ref) {
                  Field->Ref=GeoRef_RPNSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,(Field->Ref?Field->Ref->LevelType:LVL_UNDEF),(Field->Ref?Field->Ref->Levels:NULL),"X",head->IG1,head->IG2,head->IG3,head->IG4,head->FID?head->FID->Id:-1);
               }
               Field->Ref->Levels[Field->Def->Level]=FSTD_IP2Level(head->IP1,&Field->Ref->LevelType);
            }
            break;

         case IP2:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->IP2));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->IP2);
            }
            break;

         case IP3:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->IP3));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->IP3);
            }
            break;

         case TYPVAR:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(head->TYPVAR,-1));
            } else {
               strncpy(head->TYPVAR,Tcl_GetString(Objv[++i]),2);
               head->TYPVAR[2]='\0';
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

         case ETIKET:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(head->ETIKET,-1));
            } else {
               strncpy(head->ETIKET,Tcl_GetString(Objv[++i]),12);
               head->ETIKET[12]='\0';
            }
            break;

         case GRIDID:
            if (Objc==1 && Field->Ref) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->Ref->Id));
            } else {
            }
            break;

         case PROJECTION:
             if (Objc==1) {
               if (Field->Ref && Field->Ref->String)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->Ref->String,-1));
            } else {
               ++i;
               if (Field->Ref && Field->Ref->String && strlen(Field->Ref->String)==strlen(Tcl_GetString(Objv[i])) && strcmp(Tcl_GetString(Objv[i]),Field->Ref->String)==0) {
               } else {
                  ref=Field->Ref;
                  if (ref) {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,ref->LevelType,ref->Levels,Tcl_GetString(Objv[i]),tm,im,NULL);
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,LVL_UNDEF,NULL,Tcl_GetString(Objv[i]),tm,im,NULL);
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
                  fprintf(stderr,"(WARNING) FSTD_FieldDefine: Unable to generate the inverse transform matrix\n");
                  im=NULL;
               } else {
                  im=inv;
               }
               if (!Field->Ref || !Field->Ref->Transform || memcmp(tm,Field->Ref->Transform,6*sizeof(double))!=0) {
                  ref=Field->Ref;
                  if (ref) {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,ref->LevelType,ref->Levels,Field->Ref->String,tm,im,NULL);
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,LVL_UNDEF,NULL,NULL,tm,im,NULL);
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
                  Tcl_AppendResult(Interp,"\n   FSTD_FieldDefine: Georef name unknown: \"",Tcl_GetString(Objv[i]),"\"",(char *)NULL);
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

         case POSITIONAL:
            if (Objc<3) {
               Tcl_WrongNumArgs(Interp,0,Objv,"Field FieldAX FieldAY");
               return(TCL_ERROR);
            } else {
               fieldAX=Data_Get(Tcl_GetString(Objv[1]));
               if (!fieldAX) {
                  Tcl_AppendResult(Interp,"invalid AX Axis field :",Tcl_GetString(Objv[1]),(char*)NULL);
                  return(TCL_ERROR);
               }
               fieldAY=Data_Get(Tcl_GetString(Objv[2]));
               if (!fieldAY) {
                  Tcl_AppendResult(Interp,"invalid AY Axis field :",Tcl_GetString(Objv[2]),(char*)NULL);
                  return(TCL_ERROR);
               }
               if (Field->Ref->Grid[0]!='Z' && Field->Ref->Grid[0]!='Y')
                  return(0);

               head=(FSTD_Head*)fieldAX->Head;
               if (Field->Ref->Lat) free(Field->Ref->Lat);
               if (Field->Ref->Lon) free(Field->Ref->Lon);

               Field->Ref->Lat=(float*)malloc(FSIZE2D(fieldAY->Def)*sizeof(float));
               Field->Ref->Lon=(float*)malloc(FSIZE2D(fieldAX->Def)*sizeof(float));
               memcpy(Field->Ref->Lat,fieldAY->Def->Data[0],FSIZE2D(fieldAY->Def)*sizeof(float));
               memcpy(Field->Ref->Lon,fieldAX->Def->Data[0],FSIZE2D(fieldAX->Def)*sizeof(float));
               EZLock_RPNInt();
               Field->Ref->Id=c_ezgdef_fmem(Field->Def->NI,Field->Def->NJ,Field->Ref->Grid,fieldAX->Ref->Grid,head->IG1,head->IG2,head->IG3,head->IG4,fieldAX->Def->Data[0],fieldAY->Def->Data[0]);
               EZUnLock_RPNInt();
               GeoRef_Qualify(Field->Ref);
               Data_Clean(Field,1,1,1);
               return(TCL_OK);
            }
            break;

         case GRTYP:

            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->Ref->Grid,-1));
            } else {
#ifdef LNK_FSTD
               grtyp=Tcl_GetString(Objv[++i]);

               if (Objc==2 && (Field->Ref && grtyp[0]==Field->Ref->Grid[0])) {
                  return(TCL_OK);
               }
               Data_Clean(Field,1,1,1);
               ref=Field->Ref;
               if (ref && ref->Id>-1 && grtyp[0]!='V') {
                  EZLock_RPNInt();
                  c_gdrls(ref->Id);
                  EZUnLock_RPNInt();
               }
               if (grtyp[0]=='W') {
                  Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,(ref?ref->LevelType:LVL_UNDEF),(ref?ref->Levels:NULL),Tcl_GetString(Objv[++i]),NULL,NULL,NULL);
               } else {
                  if (grtyp[0]=='L' || grtyp[0]=='N' || grtyp[0]=='S') {
                     if (Objc>3 && (Tcl_GetDoubleFromObj(Interp,Objv[i+1],&dxg1)!=TCL_ERROR)) {
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&dxg1);
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&dxg2);
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&dxg3);
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&dxg4);

                        xg1=dxg1;xg2=dxg2;xg3=dxg3;xg4=dxg4;
                        f77name(cxgaig)(grtyp,&head->IG1,&head->IG2,&head->IG3,&head->IG4,&xg1,&xg2,&xg3,&xg4);
                     }
                  } else if (grtyp[0]=='A' || grtyp[0]=='B' || grtyp[0]=='G') {
                     if (Objc>3 && (Tcl_GetIntFromObj(Interp,Objv[i+1],&j)!=TCL_ERROR)) {
                        Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG1);
                        Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG2);
                        Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG3);
                        Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG4);
                     }
                  } else if (grtyp[0]=='V') {
                     if (!Field->Ref) {
                         Field->Ref=GeoRef_Reference(GeoRef_RPNSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,LVL_UNDEF,NULL,"A ",0,0,0,0,-1));
                     }
                     Field->Ref->Levels=(float*)realloc(Field->Ref->Levels,Field->Def->NJ*sizeof(float));
                  }
                  Field->Ref=GeoRef_RPNSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,(ref?ref->LevelType:LVL_UNDEF),(ref?ref->Levels:NULL),grtyp,head->IG1,head->IG2,head->IG3,head->IG4,head->FID?head->FID->Id:-1);
                  GeoRef_Qualify(Field->Ref);
               }
               if (ref)
                  GeoRef_Destroy(Interp,ref->Name);
#endif
            }
            break;

         case IG1:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->IG1));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG1);
            }
            break;

         case IG2:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->IG2));
             } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG2);
            }
            break;

         case IG3:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->IG3));
            } else {
              Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG3);
            }
            break;

         case IG4:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->IG4));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG4);
            }
            break;

         case SWA:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->SWA));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->SWA);
            }
            break;

         case LNG:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->LNG));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->LNG);
            }
            break;

         case DLTF:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->DLTF));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->DLTF);
            }
            break;

         case UBC:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->UBC));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->UBC);
            }
            break;

         case EX1:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->EX1));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->EX1);
            }
            break;

         case EX2:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->EX2));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->EX2);
            }
            break;

         case EX3:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(head->EX3));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&head->EX3);
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
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldFree>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
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
void FSTD_FieldFree(TData *Field){

   FSTD_Head *head=(FSTD_Head*)Field->Head;

   if (Field && head)
      free(head);
 }

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldCreate>
 * Creation : Septembre 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Creer un nouvel enregistrement en allouant l'espace memoire necessaire
 *            specifie par les NI,Nj et NK.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Name>    : Nom du nouveau champs
 *  <NI>      : Nombre de points de grille en I
 *  <NJ>      : Nombre de points de grille en J
 *  <NK>      : Nombre de points de grille en K
 *  <Type>    : Type de donnees
 *
 * Retour         :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
TData *FSTD_FieldCreate(Tcl_Interp *Interp,char *Name,int NI,int NJ,int NK,int Type){

   TData  *field;

   field=Data_Valid(Interp,Name,NI,NJ,NK,1,TD_Float32);

   if (!field)
     return(NULL);

   FSTD_FieldSet(field);

   field->Def->Type=FSTD_Type[Type];
   ((FSTD_Head*)field->Head)->DATYP=Type;

   return field;
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldFind>
 * Creation : Mai 2001 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recherche tout les enregistrement satisfaisant les criteres de
 *            selection et retourne leurs indexes.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *  <Max>     : Nombre maximum a retourner
 *  <DateV>   : Date de validite du champ
 *  <Eticket> : Etiquette du champ
 *  <IP1>     : Valeur du IP1
 *  <IP2>     : Valeur du IP2
 *  <IP3>     : Valeur du IP3
 *  <TypVar>  : Type de champ
 *  <NomVaR>  : Nom du champ
 *
 * Retour         :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldFind(Tcl_Interp *Interp,char *Id,int Max,int DateV,char* Eticket,int IP1,int IP2,int IP3,char* TypVar,char* NomVar){

   FSTD_File *file;
   int        ni,nj,nk,*idlst,idnb=0;
   char       buf[32];

   /*Recuperer les index de tout les champs satisfaisant*/
#ifdef LNK_FSTD
   file=FSTD_FileGet(Interp,Id);
   if(FSTD_FileSet(Interp,file)<0)
      return(TCL_ERROR);

   if (!(idlst=(int*)malloc(Max*sizeof(int)))) {
      Tcl_AppendResult(Interp,"FSTD_FieldFind: unable to allocate find array",(char*)NULL);
      return(TCL_ERROR);
   }
   cs_fstinl(file->Id,&ni,&nj,&nk,DateV,Eticket,IP1,IP2,IP3,TypVar,NomVar,idlst,&idnb,Max);

   FSTD_FileUnset(Interp,file);

   for(ni=0;ni<idnb;ni++) {
      sprintf(buf,"%i",idlst[ni]);
      Tcl_AppendElement(Interp,buf);
   }
   free(idlst);
#endif
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldReadHead>
 * Creation : Janvier 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Recuperer les informations d'un champs sur disque sans le lire.
 *            Pendant de la fonction c_fstprm
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
int FSTD_FieldReadHead(Tcl_Interp *Interp,char *Id,int Key){

   FSTD_File *file;
   FSTD_Head  h;
   int        ok,ni,nj,nk;
   char       buf[1024],grtyp[2];

#ifdef LNK_FSTD

   file=FSTD_FileGet(Interp,Id);
   if(FSTD_FileSet(Interp,file)<0)
      return TCL_ERROR;

   strcpy(h.NOMVAR,"    ");
   strcpy(h.TYPVAR,"  ");
   strcpy(h.ETIKET,"            ");
   h.KEY=Key;

   ok=cs_fstprm(h.KEY,&h.DATEO,&h.DEET,&h.NPAS,&ni,&nj,&nk,&h.NBITS,
         &h.DATYP,&h.IP1,&h.IP2,&h.IP3,h.TYPVAR,h.NOMVAR,h.ETIKET,
         grtyp,&h.IG1,&h.IG2,&h.IG3,&h.IG4,&h.SWA,&h.LNG,&h.DLTF,
         &h.UBC,&h.EX1,&h.EX2,&h.EX3);

   FSTD_FileUnset(Interp,file);

   if (ok!=0) {
      Tcl_AppendResult(Interp,"FSTD_FieldReadHead: Could not get field information (c_fstprm failed)",(char*)NULL);
      return TCL_ERROR;
   }

   /*Supprimer les espaces inutiles*/
   strtrim(h.NOMVAR,' ');
   strtrim(h.TYPVAR,' ');
   strtrim(h.ETIKET,' ');

   /*Retourner la liste des parametres obtenus*/
   sprintf(buf,"%s %i {%s} {%s} %i %i %i {%s} %09i %i %i %i %i %i %i %i %c %i %i %i %i %i %i %i %i %i %i %i",
      Id,h.KEY,h.NOMVAR,h.TYPVAR,h.IP1,h.IP2,h.IP3,h.ETIKET,h.DATEO,h.DEET,h.NPAS,
      ni,nj,nk,h.NBITS,h.DATYP,grtyp[0],h.IG1,h.IG2,h.IG3,h.IG4,h.SWA,
      h.LNG,h.DLTF,h.UBC,h.EX1,h.EX2,h.EX3);
   Tcl_AppendResult(Interp,buf,(char*)NULL);
#endif
   return TCL_OK;
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldList>
 * Creation : Octobre 2005 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Liste les champs disponibles dans un fichier standard.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <File>    : Fichier standard
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
int FSTD_FieldList(Tcl_Interp *Interp,FSTD_File *File,int Mode,char *Var){

   FSTD_Head      head;
   Tcl_Obj       *list,*obj;
   int            i,nb,ni,nj,nk;
   char           buf[1024],grtyp[2];
   double         nhour;

#ifdef LNK_FSTD

   nb=FSTD_FileSet(Interp,File);

   list=Tcl_NewListObj(0,NULL);
   obj=Tcl_NewObj();

   if (nb>=0) {
      EZLock_RPNField();
      head.KEY=c_fstinf(File->Id,&ni,&nj,&nk,-1,"",-1,-1,-1,"","");

      for (i=0;i<nb;i++) {
          /* On saute les enregistrements invalides (Pour les cas ou ca arrive ????) */
          if (head.KEY>=0) {

            strcpy(head.NOMVAR,"    ");
            strcpy(head.TYPVAR,"  ");
            strcpy(head.ETIKET,"            ");
            c_fstprm(head.KEY,&head.DATEO,&head.DEET,&head.NPAS,&ni,&nj,&nk,&head.NBITS,
                    &head.DATYP,&head.IP1,&head.IP2,&head.IP3,head.TYPVAR,head.NOMVAR,head.ETIKET,
                    grtyp,&head.IG1,&head.IG2,&head.IG3,&head.IG4,&head.SWA,&head.LNG,&head.DLTF,
                    &head.UBC,&head.EX1,&head.EX2,&head.EX3);

            strtrim(head.NOMVAR,' ');
            strtrim(head.TYPVAR,' ');
            strtrim(head.ETIKET,' ');

            /*Check for var if provided*/
            if (!Var || strcmp(Var,head.NOMVAR)==0) {

               /*Calculer la date de validitee du champs*/
               nhour=(head.NPAS*head.DEET)/3600.0;
               if (head.DATEO==0) {
                  head.DATEV=0;
               } else {
                  f77name(incdatr)(&head.DATEV,&head.DATEO,&nhour);
               }
               if (head.DATEV==101010101) head.DATEV=0;

               switch(Mode) {
                  case FSTD_LISTALL:
                     sprintf(buf,"%s %i {%s} {%s} %i %i %i {%s} %09i %09i %i %i %i",
                        File->CId,head.KEY,head.NOMVAR,head.TYPVAR,head.IP1,head.IP2,head.IP3,head.ETIKET,head.DATEO,head.DATEV,ni,nj,nk);
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
                     Tcl_SetLongObj(obj,System_Stamp2Seconds(head.DATEV));
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
            head.KEY=c_fstsui(File->Id,&ni,&nj,&nk);
         }
      }
      EZUnLock_RPNField();
   } else {
      return(TCL_ERROR);
   }

#endif
   Tcl_SetObjResult(Interp,list);

   FSTD_FileUnset(Interp,File);
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldRead>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Lit et stocke un enregistrement en memoire de meme qu'initialise
 *            les valeurs de configuraiton par defaut.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *  <Key>     : Cle de l'enregistrement
 *  <DateV>   : Date de validite du champ
 *  <Eticket> : Etiquette du champ
 *  <IP1>     : Valeur du IP1
 *  <IP2>     : Valeur du IP2
 *  <IP3>     : Valeur du IP3
 *  <TypVar>  : Type de champ
 *  <NomVaR>  : Nom du champ
 *
 * Retour         :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldRead(Tcl_Interp *Interp,char *Name,char *Id,int Key,int DateV,char* Eticket,int IP1,int IP2,int IP3,char* TypVar,char* NomVar){

   FSTD_File  *file;
   TData *field=NULL;
   FSTD_Head   h;
   TFSTDVector *uvw;
   int         ok,ni,nj,nk,i,type,idx;
   float       lvl;
   char        nomvar[5],typvar[2],grtyp[2],etik[13],*proj=NULL;
   double      nhour;

#ifdef LNK_FSTD

   file=FSTD_FileGet(Interp,Id);
   if(FSTD_FileSet(Interp,file)<0)
      return(TCL_ERROR);

   EZLock_RPNField();
   /*Rechercher et lire l'information de l'enregistrement specifie*/
   if (Key==-1) {
      Key=c_fstinf(file->Id,&ni,&nj,&nk,DateV,Eticket,IP1,IP2,IP3,TypVar,NomVar);
      if (Key<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldRead: Specified field does not exist (c_fstinf failed)",(char*)NULL);
         FSTD_FileUnset(Interp,file);
         EZUnLock_RPNField();
         return(TCL_ERROR);
      }
   }

   h.KEY=Key;
   h.FID=file;
   strcpy(h.NOMVAR,"    ");
   strcpy(h.TYPVAR,"  ");
   strcpy(h.ETIKET,"            ");

   ok=c_fstprm(h.KEY,&h.DATEO,&h.DEET,&h.NPAS,&ni,&nj,&nk,&h.NBITS,
         &h.DATYP,&h.IP1,&h.IP2,&h.IP3,h.TYPVAR,h.NOMVAR,h.ETIKET,
         &grtyp,&h.IG1,&h.IG2,&h.IG3,&h.IG4,&h.SWA,&h.LNG,&h.DLTF,
         &h.UBC,&h.EX1,&h.EX2,&h.EX3);

   h.DATYP=h.DATYP<=0?1:h.DATYP;
//   h.DATYP=h.DATYP>=5?1:h.DATYP;
   h.DATYP=h.DATYP>128?h.DATYP-128:h.DATYP;

   if (ok<0) {
      Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not get field information for ",Name," (c_fstprm failed)",(char*)NULL);
      FSTD_FileUnset(Interp,file);
      EZUnLock_RPNField();
      return(TCL_ERROR);
   }

   /*Calculer la date de validitee du champs*/
   if (h.DATEO!=0) {
      nhour=(h.NPAS*h.DEET)/3600.0;
      f77name(incdatr)(&h.DATEV,&h.DATEO,&nhour);
      if (h.DATEV==101010101) h.DATEV=0;
   } else {
      h.DATEV=0;
   }

   /*Supprimer les espaces inutiles*/
   strtrim(h.NOMVAR,' ');
   strtrim(h.TYPVAR,' ');
   strtrim(h.ETIKET,' ');

   /*Champs vectoriel ???*/
   if ((uvw=FSTD_VectorTableCheck(h.NOMVAR,&idx)) && uvw->VV) {
      field=Data_Valid(Interp,Name,ni,nj,nk,(uvw->WW?3:2),TD_Float32);
      if (!field) {
         FSTD_FileUnset(Interp,file);
         EZUnLock_RPNField();
         return(TCL_ERROR);
      }

      /*Recuperer les donnees du champs*/
      c_fstluk(field->Def->Data[idx],h.KEY,&ni,&nj,&nk);
      strcpy(h.NOMVAR,uvw->UU);

      if (uvw->UU && idx!=0) {
         /*Rechercher et lire l'information de l'enregistrement complementaire*/
         ok=c_fstinf(h.FID->Id,&ni,&nj,&nk,h.DATEV,h.ETIKET,h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->UU);

         if (ok<0) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not find first component field ",uvw->UU," (c_fstinf failed)",(char*)NULL);
            FSTD_FileUnset(Interp,file);
            EZUnLock_RPNField();
            return(TCL_ERROR);
         } else {
            c_fstluk(field->Def->Data[0],ok,&ni,&nj,&nk);
         }
      }
      if (uvw->VV && idx!=1) {
         /*Rechercher et lire l'information de l'enregistrement complementaire*/
         ok=c_fstinf(h.FID->Id,&ni,&nj,&nk,h.DATEV,h.ETIKET,h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->VV);

         if (ok<0) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not find second component field ",uvw->VV," (c_fstinf failed)",(char*)NULL);
            FSTD_FileUnset(Interp,file);
            EZUnLock_RPNField();
            return(TCL_ERROR);
         } else {
            c_fstluk(field->Def->Data[1],ok,&ni,&nj,&nk);
         }
      }
      if (uvw->WW && idx!=2) {
         /*Rechercher et lire l'information de l'enregistrement complementaire*/
         ok=c_fstinf(h.FID->Id,&ni,&nj,&nk,h.DATEV,h.ETIKET,h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->WW);

         if (ok<0) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not find third component field ",uvw->WW," (c_fstinf failed)",(char*)NULL);
            FSTD_FileUnset(Interp,file);
            EZUnLock_RPNField();
            return(TCL_ERROR);
         } else {
            c_fstluk(field->Def->Data[2],ok,&ni,&nj,&nk);
         }
      }
   } else {
      /*Verifier si le champs existe et est valide*/
      field=Data_Valid(Interp,Name,ni,nj,nk,1,TD_Float32);
      if (!field) {
         EZUnLock_RPNField();
         return(TCL_ERROR);
      }

      /*Recuperer les donnees du champs*/
      c_fstluk(field->Def->Data[0],h.KEY,&ni,&nj,&nk);
   }

   /*Recuperer les type de niveaux*/
   lvl=FSTD_IP2Level(h.IP1,&type);

   /*Override le type de niveaux pour ZH is ip1=0*/
   if (h.NOMVAR[0]=='Z' && h.NOMVAR[1]=='H' && h.IP1==0){
      type=LVL_MASL;
   }

   /*Creer une grille comme definie dans l'enregistrement*/
   if (grtyp[0]=='W' || grtyp[0]=='Y' || grtyp[0]=='Z') {
      int pni,pnj,ig1,ig2,ig3,ig4;
      float tmpv[6];
      double mtx[6],inv[6],*tm,*im;
      char   t='\0';

      ig1=h.IG1;
      ig2=h.IG2;
      ig3=h.IG3;

      if (grtyp[0]=='Y' || grtyp[0]=='Z') {
         t=grtyp[0];
         Key=c_fstinf(file->Id,&pni,&pnj,&nk,-1,"",h.IG1,h.IG2,h.IG3,"",">>");
         ok=c_fstprm(Key,&pni,&pni,&pni,&pni,&pni,&pni,&pni,
               &pni,&pni,&pni,&pni,typvar,nomvar,etik,
               &grtyp,&ig1,&ig2,&ig3,&ig4,&pni,&pni,&pni,
               &pni,&pni,&pni,&pni);

         if (grtyp[0]!='W') {
            grtyp[0]=t;
         }
      }

      if (grtyp[0]=='W') {

         Key=c_fstinf(file->Id,&pni,&pnj,&nk,-1,"",ig1,ig2,ig3,"","PROJ");
         if (Key<0) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Projection description field does not exist (c_fstinf failed)",(char*)NULL);
         } else {
            proj=(char*)malloc(pni*pnj*4);
            c_fstluk(proj,Key,&pni,&pnj,&nk);
         }

         Key=c_fstinf(file->Id,&pni,&pnj,&nk,-1,"",ig1,ig2,ig3,"","MTRX");
         if (Key<0) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Tranform matrix field does not exist (c_fstinf failed)",(char*)NULL);
         } else {
            c_fstluk(tmpv,Key,&pni,&pnj,&nk);
            for(i=0;i<pni;i++) {
               mtx[i]=tmpv[i];
            }
            tm=mtx;
            if (!GDALInvGeoTransform(mtx,inv)) {
               im=NULL;
            } else {
               im=inv;
            }
         }

         if (proj && im) {
            field->Ref=GeoRef_WKTSetup(ni,nj,nk,type,&lvl,proj,tm,im,NULL);
            field->Ref->Grid[1]=t;
         }
         if (proj) free(proj);
      }
   }
   if (grtyp[0]!='W') {
      field->Ref=GeoRef_RPNSetup(ni,nj,nk,type,&lvl,grtyp,h.IG1,h.IG2,h.IG3,h.IG4,h.FID->Id);
   }
   EZUnLock_RPNField();

   FSTD_FieldSet(field);

//   if (grtyp[0]!='X' || (h.NOMVAR[0]=='Z' && h.NOMVAR[1]=='H')){
      GeoRef_Qualify(field->Ref);
//   }
   field->Spec->Desc=strdup(h.NOMVAR);
   field->Def->Type=FSTD_Type[h.DATYP];
   memcpy(field->Head,&h,sizeof(FSTD_Head));

#endif
   FSTD_FileUnset(Interp,file);
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldReadLevels>
 * Creation : Decembrre 2000 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Lit et stocke les divers niveaux d'un enregistrement en memoire.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Field>   : Pointeur sur une structure de champ.
 *  <Invert>  : Inversion des niveaux (Top down).
 *
 * Retour         :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *   -On s'assure que l'on list seulement un type de niveau si plusieurs sont
 *    present, soit le niveau selectionne
 *   -On trie les niveaux en ordre croissant pour s'aasurer de la consistance
 *    au cas ou ls ne serait pas dans le bon ordre dans le fichier
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldReadLevels(Tcl_Interp *Interp,TData *Field,int Invert){

   FSTD_Head   *head=(FSTD_Head*)Field->Head;
   TFSTDVector *uvw;
   TGeoRef     *ref;
   int          idxs[512],tmp[512],i,k,k2,idx,ok,idump,ni,nj,nk,type;
   char         cdump[16];
   void        *p;
   float        levels[512];

   if (Field->Def->NK>1 || Field->Ref->Grid[0]=='V')
      return(1);

#ifdef LNK_FSTD
   if (FSTD_FileSet(Interp,head->FID)<0)
      return TCL_ERROR;

   /*Recuperer les indexes de tout les niveaux*/
   EZLock_RPNField();
   c_fstinl(head->FID->Id,&ni,&nj,&nk,head->DATEV,head->ETIKET,-1,head->IP2,head->IP3,head->TYPVAR,head->NOMVAR,idxs,&nk,512);

   if (nk<1) {
      fprintf(stderr,"(WARNING) FSTD_FieldReadLevels: Could not find any other levels\n");
      FSTD_FileUnset(Interp,head->FID);
      EZUnLock_RPNField();
      return(0);
   }

   uvw=FSTD_VectorTableCheck(head->NOMVAR,NULL);

   /*Determiner les niveaux disponibles*/
   k2=0;
   for(k=0;k<nk;k++) {
      ok=c_fstprm(idxs[k],&idump,&idump,&idump,&idump,&idump,&idump,&idump,
         &idump,&tmp[k],&idump,&idump,cdump,cdump,cdump,cdump,&idump,&idump,
         &idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump);

      /*Verifier que l'on garde le meme type de niveau*/
      FSTD_IP2Level(tmp[k],&type);
      if (type==Field->Ref->LevelType) {

        /*Verifier que l'on a pas deja ce niveau (niveau en double)*/
         for(i=0;i<k2;i++) {
            if (tmp[k]==tmp[i]) break;
         }

         /*Garder ce niveau en metre pour le tri*/
         if (i>=k2) {
            levels[k2]=FSTD_IP2Meter(tmp[k]);
            idxs[k2]=idxs[k];
            k2++;
         }
      }
   }
   nk=k2;

   /*Trier les niveaux*/
   for(k=0;k<nk;k++) {
      tmp[k]=nk-1;
      for(k2=0;k2<nk;k2++) {
         if (Invert) {
            if (levels[k]>levels[k2]) {
               tmp[k]--;
            }
         } else {
            if (levels[k]<levels[k2]) {
               tmp[k]--;
            }
         }
      }
   }

   /*Augmenter la dimension du tableau*/
   if (!Data_DefResize(Field->Def,ni,nj,nk)) {
      fprintf(stderr,"(ERROR) FSTD_FieldReadLevels: Not enough memory to allocate levels\n");
      FSTD_FileUnset(Interp,head->FID);
      EZUnLock_RPNField();
      return(0);
   }

#ifdef DEBUG
   fprintf(stderr,"(DEBUG) FSTD_FieldReadLevels: found %i levels\n",Field->Def->NK);
#endif
   /*Recuperer le data*/
   for(k=0;k<Field->Def->NK;k++) {
      idx=FSIZE2D(Field->Def)*tmp[k];
      Def_Pointer(Field->Def,0,idx,p);
      c_fstluk(p,idxs[k],&ni,&nj,&idump);

      /*Recuperer le data seulement*/
      ok=c_fstprm(idxs[k],&idump,&idump,&idump,&idump,&idump,&idump,&idump,
         &idump,&i,&idump,&idump,cdump,cdump,cdump,cdump,&idump,&idump,
         &idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump);

      /*Champs vectoriel ???*/
      if (uvw) {
         if (uvw->VV) {
            ok=c_fstinf(head->FID->Id,&idump,&idump,&idump,head->DATEV,head->ETIKET,i,head->IP2,head->IP3,head->TYPVAR,uvw->VV);
            Def_Pointer(Field->Def,1,idx,p);
            c_fstluk(p,ok,&idump,&idump,&idump);
         }
         if (uvw->WW) {
            ok=c_fstinf(head->FID->Id,&idump,&idump,&idump,head->DATEV,head->ETIKET,i,head->IP2,head->IP3,head->TYPVAR,uvw->WW);
            Def_Pointer(Field->Def,2,idx,p);
            c_fstluk(p,ok,&idump,&idump,&idump);
         }
      }

      /*Assigner le niveaux courant*/
      if (i==head->IP1) {
         Field->Def->Level=tmp[k];
      }
      levels[tmp[k]]=FSTD_IP2Level(i,&type);

      if (ok<0) {
         fprintf(stderr,"(ERROR) FSTD_FieldReadLevels: Something really wrong here (c_fstprm failed (%i))",ok);
         FSTD_FileUnset(Interp,head->FID);
         EZUnLock_RPNField();
         return(0);
      }
   }

   ref=Field->Ref;
   if (Field->Ref->Grid[0]=='W') {
       Field->Ref=GeoRef_WKTSetup(ni,nj,nk,type,levels,Field->Ref->String,Field->Ref->Transform,Field->Ref->InvTransform,NULL);
   } else {
       Field->Ref=GeoRef_RPNSetup(ni,nj,nk,type,levels,Field->Ref->Grid,head->IG1,head->IG2,head->IG3,head->IG4,head->FID->Id);
   }
   Field->Ref->Grid[1]=ref->Grid[1];
   GeoRef_Destroy(Interp,ref->Name);
   GeoRef_Qualify(Field->Ref);

   if (Field->Ref->NRef>1) {
      Data_Clean(Field,1,0,1);
   } else {
      Data_Clean(Field,1,1,1);
   }

   FSTD_FileUnset(Interp,head->FID);
   EZUnLock_RPNField();

   Data_GetStat(Field);
#endif
   return 1;
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldWrite>
 * Creation : Aout 1998 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Inscrit un enregistrement.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Id>      : Identificateur a donner au fichier
 *  <Field>   : Champ a ecrire
 *  <NPack>   : Facteur de compaction
 *  <Rewrite> : Reecrire le champs ou pas
 *
 * Retour     :
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldWrite(Tcl_Interp *Interp,char *Id,TData *Field,int NPack,int Rewrite,int Compress){

   FSTD_File *file;
   FSTD_Head *head=(FSTD_Head*)Field->Head;
   int        ok=-1,k,idx,ip1,datyp;
   char       nv[5];
   void      *p;

#ifdef LNK_FSTD
   /*Verifier l'existence du champs*/
   if (!Field) {
      Tcl_AppendResult(Interp,"FSTD_FieldWrite: Invalid field",(char*)NULL);
      return TCL_ERROR;
   }

   file=FSTD_FileGet(Interp,Id);
   if (FSTD_FileSet(Interp,file)<0)
      return TCL_ERROR;

   head->FID=file;
   datyp=NPack==-32?5:head->DATYP;
   NPack=NPack==0?-head->NBITS:NPack;

   /*Check for compression flag and adjust datyp accordingly*/
   if (Compress) {
      switch (head->DATYP) {
         case 2: datyp=130; break;
         case 4: datyp=132; break;
         case 5: datyp=133; break;
         case 1: datyp=134; break;
      }
   }

   EZLock_RPNField();

   for(k=0;k<Field->Def->NK;k++) {
      idx=k*FSIZE2D(Field->Def);

//      ip1=Field->Def->NK==1?((FSTD_Head*)Field->Head)->IP1:FSTD_Level2IP(Field->Ref->Levels[k],Field->Ref->LevelType);
      /*If IP1 is set, use it otherwise, convert it from levels array*/
      if ((ip1=((FSTD_Head*)Field->Head)->IP1)==-1 || Field->Def->NK>1) {
         ip1=FSTD_Level2IP(Field->Ref->Levels[k],Field->Ref->LevelType);
      }

      /*Inscription de l'enregistrement*/
      Def_Pointer(Field->Def,0,idx,p);
      ok=c_fstecr(p,NULL,NPack,file->Id,head->DATEO,head->DEET,head->NPAS,Field->Def->NI,Field->Def->NJ,1,
                  ip1,head->IP2,head->IP3,head->TYPVAR,head->NOMVAR,head->ETIKET,
                  (Field->Ref?(Field->Ref->Grid[1]!='\0'?&Field->Ref->Grid[1]:Field->Ref->Grid):"X"),head->IG1,head->IG2,head->IG3,head->IG4,datyp,Rewrite);

      /*Inscription du champs complementaire*/
      if (Field->Def->Data[1]) {
         if (strncmp(head->NOMVAR,"UU",2)==0) {
            strcpy(nv,"VV");
         } else if (strncmp(head->NOMVAR,"UD",2) == 0) {
            strcpy(nv,"VD");
         }

         Def_Pointer(Field->Def,1,idx,p);
         ok=c_fstecr(p,NULL,NPack,file->Id,head->DATEO,head->DEET,head->NPAS,Field->Def->NI,Field->Def->NJ,1,
                     ip1,head->IP2,head->IP3,head->TYPVAR,nv,head->ETIKET,
                     (Field->Ref?(Field->Ref->Grid[1]!='\0'?&Field->Ref->Grid[1]:Field->Ref->Grid):"X"),head->IG1,head->IG2,head->IG3,head->IG4,datyp,Rewrite);
      }
   }

   EZUnLock_RPNField();

   FSTD_FileUnset(Interp,file);
#endif

   if (ok>=0){
      return TCL_OK;
   } else {
      Tcl_AppendResult(Interp,"FSTD_FieldWrite: Could not write field (c_fstecr failed)",(char*)NULL);
      return TCL_ERROR;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_ZGrid>
 * Creation : Septembre 2007 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Creer les tic-tac pour une grille Z.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL.
 *  <Tic>     : Nom du champs >>
 *  <Tac>     : Nom du champs ^^
 *  <Set>     : Array Tcl du contenue de namelist (gem_settings.nml)
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
/*
* lagrd         limited-area flag
* grdtyp        3 options only: 'G'=PHI grid, 'U'=U grid, 'V'=V grid
* digfil        if .true. then apply digital filter
* dgfm          2*(dgfm-1) = maximum number of neighbour points to be used by the digital filter (dgfm >= 1)
* lcfac         factor that controls the critical wavelength
* mlr           minimum value of mesh-length ratio necessary to activate the digital filter (mlr >= 1.0)
* mapfac        if .true. then consider the map-scale factor
* norm          if .true. then use normalized coefficients for digital filter
* tdxfil        if .true. then apply 2-delta-xy filter
* frco          2-delta-xy filter coefficient (0.0 <= frco <= 0.5)
*/

int FSTD_ZFilterTopo(Tcl_Interp *Interp,TData *Field,Tcl_Obj *Set) {

   Tcl_Obj *obj;

   float *fld,lcfac,mlr,frco;
   int    idx,i,j,nio,njo,dgfm;
   int    lagrd=0,digfil=0,tdxfil=0,mapfac=0,norm=0;
   char   grtyp[2]="GU";

   if (!Field) {
      Tcl_AppendResult(Interp,"FSTD_ZFilterTopo: Invalid topography field",(char*)NULL);
      return(TCL_ERROR);
   }
   GeoRef_Expand(Field->Ref);

   dgfm=5;
   lcfac=2.0;
   mlr=3.0;
   norm=TRUE;
   frco=0.5;
   lagrd=FALSE;

   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_TYP_S",0x0))    { grtyp[0]=Tcl_GetString(obj)[0];grtyp[1]=Tcl_GetString(obj)[1]; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"TOPO_DGFMS_L",0x0)) { Tcl_GetBooleanFromObj(Interp,obj,&mapfac); }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"TOPO_DGFMX_L",0x0)) { Tcl_GetBooleanFromObj(Interp,obj,&digfil); }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"TOPO_FILMX_L",0x0)) { Tcl_GetBooleanFromObj(Interp,obj,&tdxfil); }

   if (!digfil && !tdxfil) {
      return(TCL_OK);
   }

   if ( grtyp[0]=='L' && grtyp[1]=='U' ) {
     lagrd=TRUE;
   }

   nio=lagrd?Field->Def->NI:Field->Def->NI-1;
   njo=Field->Def->NJ;

   fld=(float*)malloc(nio*njo*sizeof(float));
   for(i=0;i<nio;i++) {
      for(j=0;j<njo;j++) {
         idx=j*nio+i;
         Def_Get(Field->Def,0,FIDX2D(Field->Def,i,j),fld[idx]);
         if (fld[idx]<0.0)
            fld[idx]=0.0;
      }
   }


   /*Apply digital filter*/
   if (digfil) {
      f77name(smp_digt_flt)(fld,Field->Ref->AX,Field->Ref->AY,&nio,&njo,&lagrd,grtyp,&dgfm,&lcfac,&mlr,&mapfac,&norm);
   }

   /*Apply 2-delta-xy filter*/
   if (tdxfil) {
      f77name(smp_2del_flt)(fld,Field->Ref->AX,Field->Ref->AY,&nio,&njo,&lagrd,grtyp,&frco);
   }

   for(j=0;j<Field->Def->NJ;j++) {
      for(i=0;i<nio;i++) {
         idx=j*nio+i;
         Def_Set(Field->Def,0,FIDX2D(Field->Def,i,j),fld[idx]);
      }
   }
   if (!lagrd) {
      for(j=0;j<Field->Def->NJ;j++) {
         idx=j*nio;
         Def_Set(Field->Def,0,FIDX2D(Field->Def,Field->Def->NI,j),fld[idx]);
      }
   }

   free(fld);
   return(TCL_OK);
}

int FSTD_ZGrid(Tcl_Interp *Interp,Tcl_Obj *Tic,Tcl_Obj *Tac,Tcl_Obj *Set) {

   Tcl_Obj *obj;
   double   dval;
   TData   *field[2];

   char Grd_typ_S[2]="GU",Grd_proj_S[2]="L\0";
   int Grd_ni=0,Grd_nj=0,Grd_nila=0,Grd_njla=0,Grd_iref=0,Grd_jref=0,Grd_roule=0,Grd_uniform_L=0;
   float Grd_latr=0.0f,Grd_lonr=0.0f,Grd_dx=0.0f,Grd_dy=0.0f,Grd_dxmax=360.0f,Grd_dymax=180.0f,Grd_phir=22.5f,Grd_dgrw=10.0f;
   float Grd_xlat1=0.0f,Grd_xlon1=180.0f,Grd_xlat2=0.0f,Grd_xlon2=270.0f,Grd_x0=0.0f,Grd_y0=0.0f,Grd_xl=0.0f,Grd_yl=0.0f;

   float  stretch=-1.0;
   int    debug=0;
   char   ns[3];
   int    i,j,ni,nila,nleft,nimax,nbelo,njmax,np,gauss=0,stag;
   int    ig1,ig2,ig3,ig4;
   float  *xp,*yp;
   double *x_8,*y_8;

   float r1,s1,x,y,maxlat,minlat,v1,v2,v3;
   double epsilon=1.0e-3,epsilat=90.0,dx_8,latr_8,lonr_8,phidg_8;

   FSTD_Head h;

   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_TYP_S",0x0))     { Grd_typ_S[0]=Tcl_GetString(obj)[0];Grd_typ_S[1]=Tcl_GetString(obj)[1]; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_PROJ_S",0x0))    { Grd_proj_S[0]=Tcl_GetString(obj)[0]; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_NI",0x0))        { Tcl_GetIntFromObj(Interp,obj,&Grd_ni); }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_NJ",0x0))        { Tcl_GetIntFromObj(Interp,obj,&Grd_nj); }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_NILA",0x0))      { Tcl_GetIntFromObj(Interp,obj,&Grd_nila); }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_NJLA",0x0))      { Tcl_GetIntFromObj(Interp,obj,&Grd_njla); }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_IREF",0x0))      { Tcl_GetIntFromObj(Interp,obj,&Grd_iref); }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_JREF",0x0))      { Tcl_GetIntFromObj(Interp,obj,&Grd_jref); }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_ROULE",0x0))     { Tcl_GetBooleanFromObj(Interp,obj,&Grd_roule); }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_UNIFORM_L",0x0)) { Tcl_GetBooleanFromObj(Interp,obj,&Grd_uniform_L); }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_LATR",0x0))      { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_latr=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_LONR",0x0))      { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_lonr=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_DX",0x0))        { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_dx=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_DY",0x0))        { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_dy=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_DXMAX",0x0))     { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_dxmax=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_DYMAX",0x0))     { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_dymax=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_PHIR",0x0))      { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_phir=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_DGRW",0x0))      { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_dgrw=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_XLAT1",0x0))     { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_xlat1=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_XLAT2",0x0))     { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_xlat2=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_XLON1",0x0))     { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_xlon1=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_XLON2",0x0))     { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_xlon2=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_X0",0x0))        { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_x0=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_Y0",0x0))        { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_y0=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_XL",0x0))        { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_xl=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"GRD_YL",0x0))        { Tcl_GetDoubleFromObj(Interp,obj,&dval); Grd_yl=dval; }

   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"STRETCH",0x0))       { Tcl_GetDoubleFromObj(Interp,obj,&dval); stretch=dval; }
   if (obj=Tcl_GetVar2Ex(Interp,Tcl_GetString(Set),"DEBUG",0x0))         { Tcl_GetBooleanFromObj(Interp,obj,&debug); }

   if (Grd_ni<=0 || Grd_nj<=0) {
      Tcl_AppendResult(Interp,"FSTD_ZGrid: GRD_NI or GRD_NJ are invalid",(char*)NULL);
      return(TCL_ERROR);
   }

   if (Grd_proj_S[0]=='P')
      Grd_proj_S[0]='N';

   ns[1]='\0';
   ni=Grd_ni;

   if (Grd_typ_S[0]=='G') {
      if (Grd_typ_S[1]=='U') {
         Grd_nila=Grd_ni;
         Grd_njla=Grd_nj;
      } else {
         if ((Grd_nila*Grd_njla*Grd_dx*Grd_dy)==0) {
            Tcl_AppendResult(Interp,"FSTD_ZGrid: GRD_NILA, GRD_NJLA, GRD_DX & GRD_DY are invalid",(char*)NULL);
            return(TCL_ERROR);
         }
      }
      Grd_x0=0.0;
      Grd_xl=360.0;
      Grd_y0=-90.0;
      Grd_yl=90.0;
      nila=Grd_nila;
      ni++;

      if (ni==nila+1) nila++;

      Grd_proj_S[0]='L';
   } else {
      if ((Grd_proj_S[0]!='N') && (Grd_proj_S[0]!='S')) {

         if (Grd_xlon1<0) Grd_xlon1=360.0+Grd_xlon1;
         if (Grd_xlon2<0) Grd_xlon2=360.0+Grd_xlon2;
/*
         np=1;
         EZLock_RPNInt();
         f77name(ez_gfxyfll)(&Grd_lonr,&Grd_latr,&x,&y,&np,&Grd_xlat1,&Grd_xlon1,&Grd_xlat2,&Grd_xlon2);
         EZUnLock_RPNInt();
         Grd_lonr=x;
         Grd_latr=y;
 */
      }
      if ((Grd_iref>Grd_ni) || (Grd_iref<1)) {
         Tcl_AppendResult(Interp,"FSTD_ZGrid: Error with GRD_IREF larger than GRD_NI",(char*)NULL);
         return(TCL_ERROR);
      }
      if ((Grd_jref>Grd_nj) || (Grd_jref<1)) {
         Tcl_AppendResult(Interp,"FSTD_ZGrid: Error with GRD_IREF larger than GRD_NJ",(char*)NULL);
         return(TCL_ERROR);
      }
      if ((Grd_lonr>360.0) || (Grd_lonr<0.0)) {
         Tcl_AppendResult(Interp,"FSTD_ZGrid: Error GRD_LONR must be within 0 and 360",(char*)NULL);
         return(TCL_ERROR);
      }
      if ((Grd_latr>90.0) || (Grd_latr<-90.0)) {
         Tcl_AppendResult(Interp,"FSTD_ZGrid: Error GRD_LATR must be within -90 and 90",(char*)NULL);
         return(TCL_ERROR);
      }

      if (Grd_dy<=0.0)
         Grd_dy=Grd_dx;

      if (Grd_dx<=0 || Grd_dy<=0) {
         Tcl_AppendResult(Interp,"FSTD_ZGrid: Error GRD_DX or GRD_DY are invalid",(char*)NULL);
         return(TCL_ERROR);
      }

      Grd_nila=Grd_ni;
      Grd_njla=Grd_nj;

      if (Grd_proj_S[0]=='L') {
         Grd_x0=Grd_lonr-(Grd_iref-1)*Grd_dx;
         Grd_y0=Grd_latr-(Grd_jref-1)*Grd_dy;
         Grd_xl=Grd_x0  +(Grd_ni  -1)*Grd_dx;
         Grd_yl=Grd_y0  +(Grd_nj  -1)*Grd_dy;

         if ((Grd_x0<0.0) || (Grd_y0<-90.0) || (Grd_xl>360.0) || (Grd_y0>90.0)) {
            Tcl_AppendResult(Interp,"FSTD_ZGrid: Grid limits are invalid",(char*)NULL);
            return(TCL_ERROR);
         }
      }
   }

   x_8=(double*)malloc((ni+2)*sizeof(double));
   y_8=(double*)malloc((Grd_nj+2)*sizeof(double));

   ns[0]='E';
   f77name(cxgaig)(ns,&h.IG1,&h.IG2,&h.IG3,&h.IG4,&Grd_xlat1,&Grd_xlon1,&Grd_xlat2,&Grd_xlon2);

   r1=0.0;
   s1=0.0;

   if (Grd_typ_S[0]=='G') {
      if ((Grd_typ_S[1]=='V') && (stretch>0)) {
         free(x_8);
         free(y_8);

         x_8=(double*)malloc((10*nila)*sizeof(double));
         y_8=(double*)malloc((10*Grd_njla)*sizeof(double));
         ni=nila+0.1*nila;

         stag=FALSE;
         while(1) {
            if (f77name(stretch_axis2)(x_8,&Grd_dx,&Grd_x0,&Grd_xl,&nleft,&ni,&nila,&r1,&stag,&debug,&Grd_dxmax,&nimax,&gauss)!=0) {
               ni+=1;
            } else {
               break;
            }
            if (r1-stretch>epsilon) {
               ni+=2;
            } else {
               break;
            }
         }
         Grd_ni=ni;
         Grd_nj=Grd_njla+0.1*Grd_njla;

         stag=TRUE;
         while(1) {
            if (f77name(stretch_axis2)(y_8,&Grd_dy,&Grd_y0,&Grd_yl,&nbelo,&Grd_nj,&Grd_njla,&s1,&stag,&debug,&Grd_dymax,&njmax,&gauss)!=0) {
               Grd_nj+=1;
            } else {
               break;
            }

            if (s1-stretch>epsilon) {
               Grd_nj+=2;
            } else {
               break;
            }
         }
      } else {
         stag=FALSE;
         if (f77name(stretch_axis2)(x_8,&Grd_dx,&Grd_x0,&Grd_xl,&nleft,&ni,&nila,&r1,&stag,&debug,&Grd_dxmax,&nimax,&gauss)!=0) {
            Tcl_AppendResult(Interp,"FSTD_ZGrid: Stretching error",(char*)NULL);
            return(TCL_ERROR);
         }

         stag=TRUE;
         if (f77name(stretch_axis2)(y_8,&Grd_dy,&Grd_y0,&Grd_yl,&nbelo,&Grd_nj,&Grd_njla,&s1,&stag,&debug,&Grd_dymax,&njmax,&gauss)!=0) {
            Tcl_AppendResult(Interp,"FSTD_ZGrid: Stretching error",(char*)NULL);
            return(TCL_ERROR);
         }
      }
   } else {
      dx_8  =Grd_dx;
      latr_8=Grd_latr;
      lonr_8=Grd_lonr;

      if (Grd_proj_S[0]=='M') {
         phidg_8=Grd_phir;
         f77name(xpyp_m)(x_8,y_8,&Grd_iref,&Grd_jref,&latr_8,&lonr_8,&dx_8,&phidg_8,&Grd_ni,&Grd_nj);
      } else if ((Grd_proj_S[0]=='N') || (Grd_proj_S[0]=='S')) {
         phidg_8=Grd_dgrw;
         ns[0]  =Grd_proj_S[0];
         f77name(xpyp_p)(x_8,y_8,&Grd_iref,&Grd_jref,&Grd_latr,&Grd_lonr,&dx_8,&Grd_dgrw,ns,&Grd_ni,&Grd_nj);
         f77name(cxgaig)(ns,&h.IG1,&h.IG2,&h.IG3,&h.IG4,0.0,0.0,1000.0,&Grd_dgrw);
      } else if (Grd_proj_S[0]=='L') {
         if ((Grd_dx*Grd_ni-360.0-Grd_dx)>epsilon) {
            Tcl_AppendResult(Interp,"FSTD_ZGrid: Given grid resolution, it would be wiser to run a globalgrid",(char*)NULL);
            return(TCL_ERROR);
         }

         maxlat=Grd_latr+(float)(Grd_nj-Grd_jref)*Grd_dy;
         minlat=Grd_latr+(float)(1-Grd_jref)*Grd_dy;
         if ((maxlat>epsilat) || (minlat<-epsilat)) {
            Tcl_AppendResult(Interp,"FSTD_ZGrid: This grid extends too close or goes further than at least one of the numerical pole",(char*)NULL);
            return(TCL_ERROR);
         }

         nila=ni;
         stag=FALSE;
         if ((f77name(stretch_axis2)(x_8,&Grd_dx,&Grd_x0,&Grd_xl,&nleft,&ni,&nila,&r1,&stag,&debug,&Grd_dxmax,&nimax,&gauss))!=0) {
            Tcl_AppendResult(Interp,"FSTD_ZGrid: Stretching error",(char*)NULL);
            return(TCL_ERROR);
         }
         if ((f77name(stretch_axis2)(y_8,&Grd_dy,&Grd_y0,&Grd_yl,&nbelo,&Grd_nj,&Grd_njla,&s1,&stag,&debug,&Grd_dymax,&njmax,&gauss))!=0) {
            Tcl_AppendResult(Interp,"FSTD_ZGrid: Stretching error",(char*)NULL);
            return(TCL_ERROR);
         }
     } else {
         Tcl_AppendResult(Interp,"FSTD_ZGrid: Wrong projection",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   field[0]=Data_Valid(Interp,Tcl_GetString(Tic),ni,1,1,1,TD_Float32);
   if (!field[0]) {
      Tcl_AppendResult(Interp,"FSTD_ZGrid: Unable to allocate >> field",(char*)NULL);
      return(TCL_ERROR);
   }
   field[1]=Data_Valid(Interp,Tcl_GetString(Tac),1,Grd_nj,1,1,TD_Float32);
   if (!field[1]) {
      Tcl_AppendResult(Interp,"FSTD_ZGrid: Unable to allocate ^^ field",(char*)NULL);
      return(TCL_ERROR);
   }
   xp=(float*)field[0]->Def->Data[0];
   yp=(float*)field[1]->Def->Data[0];
   for(i=0;i<ni;i++)     { xp[i]=x_8[i]; }
   for(j=0;j<Grd_nj;j++) { yp[j]=y_8[j]; }

   if (Grd_dx<100) {
      v1=20.0*Grd_dx;
   } else if (Grd_dx<10000.0) {
      v1=0.2*Grd_dx;
   } else {
      v1=0.0004*Grd_dx-4.0;
   }

   if (Grd_ni<1000) {
      v2=2.0*Grd_ni;
   } else if (Grd_ni<10000) {
      v2=0.2*Grd_ni;
   } else {
      v2=0.02*Grd_ni;
   }

   if (Grd_nj<1000) {
      v3=2.0*Grd_nj;
   } else if (Grd_ni<10000) {
      v3=0.2*Grd_nj;
   } else {
      v3=0.02*Grd_nj;
   }
   v1=FMIN(2040.0,FMAX(0.0,v1));
   v2=FMIN(2040.0,FMAX(0.0,v2));
   v3=FMIN(2040.0,FMAX(0.0,v3));

   h.IP1=(v1+v2+v3)/3.0;
   h.IP2=((h.IG1+h.IG2+h.IG3/321.0+h.IG4/321.0)/4.0+v1+v2)/3.0;
   h.IP3=((Grd_dxmax*5+Grd_dymax*5.0+r1*10.0+s1*10)+2.0*v1+2.0*v3)/3.0;
   h.NPAS=0;
   h.DATEO=0;
   h.DEET=0;
   h.TYPVAR[0]='X';h.TYPVAR[1]='\0';

   FSTD_FieldSet(field[0]);
   FSTD_FieldSet(field[1]);

   field[0]->Spec->Desc=strdup(">>");
   field[1]->Spec->Desc=strdup("^^");
   strcpy(h.NOMVAR,">>");
   strcpy(h.ETIKET,"POSX");
   memcpy(field[0]->Head,&h,sizeof(FSTD_Head));
   strcpy(h.NOMVAR,"^^");
   strcpy(h.ETIKET,"POSY");
   memcpy(field[1]->Head,&h,sizeof(FSTD_Head));

   field[0]->Ref=GeoRef_RPNSetup(field[0]->Def->NI,field[0]->Def->NJ,field[0]->Def->NK,0,NULL,ns,h.IG1,h.IG2,h.IG3,h.IG4,0);
   field[1]->Ref=GeoRef_RPNSetup(field[1]->Def->NI,field[1]->Def->NJ,field[1]->Def->NK,0,NULL,ns,h.IG1,h.IG2,h.IG3,h.IG4,0);

   return(TCL_OK);
}
