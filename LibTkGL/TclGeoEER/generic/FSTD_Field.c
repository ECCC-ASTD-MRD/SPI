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

#ifdef HAVE_RMN

#include "tclFSTD.h"
#include "Projection.h"
#include "EZTile.h"
#include "ZRefInterp.h"

TCL_DECLARE_MUTEX(MUTEX_FSTDVI)

int      FSTD_UNTILE=0;
Tcl_Obj *FSTD_HIDELIST=NULL;

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_TypeCheck>
 * Creation : Mars 2009 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner le type (TData_Type) de donnees RPN.
 *
 * Parametres   :
 *  <Type>      : DATYP
 *  <Size>      : NBIT
 *
 * Retour:
 *
 * Remarques :
 *   0=binary 1=real 2=unsigned integer 3=character 4=signed integer 5=IEEE style representation 6=whatever RPN comes with
 *  16 bit integer don't exist in RPN, only 32bit, 64 bit ???
 *----------------------------------------------------------------------------
*/
TData_Type FSTD_TypeCheck(int Type,int Size) {

   switch(Type) {
//      case 0: Type=TD_Binary;                                                          break;  
      case 0: Type=Size>1?(Size>8?(Size>16?(Size>32?TD_UInt64:TD_UInt32):TD_UInt16):TD_UByte):TD_Binary; break;
      case 7: Type=TD_UByte;                                                           break;
      case 2: Type=Size<8?TD_UInt32:(Size>8?(Size>16?(Size>32?TD_UInt64:TD_UInt32):TD_UInt16):TD_UByte);  break;
      case 4: Type=Size<8?TD_UInt32:(Size>8?(Size>16?(Size>32?TD_Int64:TD_Int32):TD_Int16):TD_Byte);      break;
      case 1:
      case 6:
      case 5: Type=Size>32?TD_Float64:TD_Float32;                                      break;
   }
   return(Type);
}

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

   FSTD_Head *head=NULL;

   if (Data->Head && Data->Free && Data->Set!=FSTD_FieldSet)
      Data->Free(Data);

   if (!Data->Head) {
      head=(FSTD_Head*)malloc(sizeof(FSTD_Head));
   } else {
      head=(FSTD_Head*)Data->Head;
   }

   /*Initialiser les parametres de definition du champs*/
   if (head) {
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
      head->NBITS=-1;
      head->DATYP=-1;
      head->IP1=-1;
      head->IP2=head->IP3=0;
      head->TYPVAR[0]='\0';
      head->NOMVAR[0]='\0';
      head->ETIKET[0]='\0';
   }
   Data->Type=TD_RPN;
   Data->Head=head;
   Data->Set=FSTD_FieldSet;
   Data->Free=FSTD_FieldFree;
   Data->Copy=FSTD_HeadCopy;
   Data->Grid=FSTD_Grid;
   Data->ReadCube=FSTD_FieldReadLevels;
   Data->Define=FSTD_FieldDefine;
}

void FSTD_HeadCopy(void *To,void *From) {
   memcpy((FSTD_Head*)To,(FSTD_Head*)From,sizeof(FSTD_Head));
}

int FSTD_FieldSubBuild(TData *Field) {

   unsigned long long dij=0;
   int ni,nj,ig;
   char grtyp[2];
   int i,c;

   if (Field->Def) {
      
      // Allocate subgrid array ans set index 0 to supergrid
      if (!Field->SDef) {
         Field->SDef=(TDataDef**)calloc((Field->Ref->NbId+1),sizeof(TDataDef*));
      }
      if (Field->SDef) {
         Field->SDef[0]=Field->Def;

         // Loop on subgrids
         for(i=1;i<=Field->Ref->NbId;i++) {
            c_ezgprm(Field->Ref->Ids[i],grtyp,&ni,&nj,&ig,&ig,&ig,&ig);

            if (Field->SDef[i])
               DataDef_Free(Field->SDef[i]);

            // Allocate a container
            if ((Field->SDef[i]=DataDef_New(ni,nj,Field->Def->NK,-Field->Def->NC,Field->Def->Type))) {

               // Point to subgrid data within global data array
               for(c=0;c<Field->Def->NC;c++) {
                  Field->SDef[i]->Idx=dij;
                  Field->SDef[i]->NIJ=Field->Def->NI*Field->Def->NJ;
                  Field->SDef[i]->Level=Field->Def->Level;
                  Field->SDef[i]->Data[c]=&Field->Def->Data[c][dij*TData_Size[Field->Def->Type]];
               }
               // Increment after global grid
               dij+=ni*nj;
            }
         }
      } else {
         fprintf(stderr,"(ERROR) FSTD_FieldBuildSub: Unable to allocate subgrid array\n");
         return(0);
      }
   }
   return(1);
}

int FSTD_FieldSubSelect(TData *Field,int N) {

   int ni,nj,ig;
   char grtyp[2];

   // If the subgrid index is different from thte current
   if (Field->Ref->Grid[0]=='U' && N!=Field->Ref->NId && N<=Field->Ref->NbId) {
      // Clean positionnal data
      Data_Clean(Field,1,1,1);

      Field->Ref->NId=N;

      // Point to subgrid data within global data array
      Field->Def=Field->SDef[N];

      // Define grid limits
      c_ezgprm(Field->Ref->Ids[N],grtyp,&ni,&nj,&ig,&ig,&ig,&ig);
      Field->Ref->X0=0;    Field->Ref->Y0=0;
      Field->Ref->X1=ni-1; Field->Ref->Y1=nj-1;

      return(1);
   }
   return(0);
}

void FSTD_Project(Projection *Proj,Vect3d *Grid,unsigned long Nb) {

   float d;
   int   n,di,dj;

   if (Proj->Ref) {
      for (n=0;n<Nb;n++) {

         if (Proj->Ref->AX) {
            Grid[n][0]=Proj->Ref->AX[(int)Grid[n][0]]-Proj->Ref->AX[0];
            Grid[n][1]=Proj->Ref->AY[(int)Grid[n][1]]-Proj->Ref->AY[0];
         }
         d=Proj->L*0.5;

         di=dj=0;
         //TODO: this is not general enough
         if (Proj->Ref->NId==2) {
            dj-=(Proj->Ref->Y1+1);
         }
         Grid[n][0]=(di+Grid[n][0])/d-Proj->LI;
         Grid[n][1]=(dj+Grid[n][1])/d-Proj->LJ;
         Grid[n][2]=1.0+Grid[n][2]*Proj->Scale*Proj->ZFactor;
      }
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldReadComp>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Effectue la lecture d'un champ complementaire.
 *
         d=Proj->L*0.5;
         d=Proj->L*0.5;
 * Parametres :
 *  <Head>    : Entete de la donnee
 *  <Ptr>     : Pointeur sur le vecteur a allouer
 *  <Var>     : Variable a lire
 *  <Grid>    : Utiliser les standard grille (1=IP<->IG,0=IP<->IP,-1=IP<->-1);
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldReadComp(FSTD_Head *Head,float **Ptr,char *Var,int Grid,int Force) {

   int key=0,ni=0,nj=0,nk=0;

   if (Var && (!*Ptr || Force)) {
      if (Grid==1) {
         // Look for corresponding time and if not use any time
         if ((key==cs_fstinf(Head->FID->Id,&ni,&nj,&nk,Head->DATEV,"",Head->IG1,Head->IG2,Head->IG3,"",Var))<=0) {
            key=cs_fstinf(Head->FID->Id,&ni,&nj,&nk,-1,"",Head->IG1,Head->IG2,Head->IG3,"",Var);
         }
      } else if (Grid==0) {
         key=cs_fstinf(Head->FID->Id,&ni,&nj,&nk,Head->DATEV,Head->ETIKET,Head->IP1,Head->IP2,Head->IP3,Head->TYPVAR,Var);
      } else {
         key=cs_fstinf(Head->FID->Id,&ni,&nj,&nk,Head->DATEV,Head->ETIKET,-1,Head->IP2,Head->IP3,Head->TYPVAR,Var);
      }

      if (key<0) {
         // Too many warnings so we catch it later
         // fprintf(stdout,"(WARNING) FSTD_FieldReadComp: Could not find component field %s (c_fstinf failed)\n",Var);
         return(0);
      } else {
         if (!*Ptr) {
            if (!(*Ptr=(float*)malloc(ni*nj*nk*sizeof(float)))) {
               fprintf(stderr,"(ERROR) FSTD_FieldReadComp: Not enough memory to read coordinates fields\n");
               return(0);
            }
         }
         cs_fstluk(*Ptr,key,&ni,&nj,&nk);
      }
   }
   return(ni*nj*nk);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_FieldReadVLevels>
 * Creation : Aout 2014 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Lire la coordonnee verticale pour un profile/xsection.
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
int FSTD_FieldReadVLevels(TData *Field) {

   int nj=0;

   if (Field->Ref->ZRef.Levels)
      free(Field->Ref->ZRef.Levels);

   Field->Ref->ZRef.Levels=NULL;
   Field->Ref->ZRef.LevelNb=FSTD_FieldReadComp((FSTD_Head*)Field->Head,&Field->Ref->ZRef.Levels,Field->Spec->Extrude,1,0);

   // If we don't find any level definition, use level index
   if (!Field->Ref->ZRef.Levels) {
      if (!(Field->Ref->ZRef.Levels=(float*)malloc(Field->Def->NJ*sizeof(float)))) {
         fprintf(stderr,"(ERROR) FSTD_FieldReadComp: Not enough memory to read coordinates fields\n");
      } else {
         for(nj=0;nj<Field->Def->NJ;nj++) {
            Field->Ref->ZRef.Levels[nj]=nj+1;
         }
         Field->Ref->ZRef.LevelNb=Field->Def->NJ;
      }
   }
   return(nj);
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
   int        key,ni,nj,nk,nijk=0;

   if (!Field->Ref || !(Field->Ref->Type&(GRID_SPARSE|GRID_VARIABLE|GRID_VERTICAL)))
      return(0);

   if ((!Field->Ref->Lat || !Field->Ref->Lon) && head->FID) {
      if (FSTD_FileSet(NULL,head->FID)<0) {
         return(0);
      }

      switch(Field->Ref->Grid[0]) {
         case 'M':
            if (!Field->Ref->Lat) FSTD_FieldReadComp(head,&Field->Ref->Lat,"^^",1,0);
            if (!Field->Ref->Lon) nijk=FSTD_FieldReadComp(head,&Field->Ref->Lon,">>",1,0);

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
            if (Field->Ref->Grid[1]=='X' || Field->Ref->Grid[1]=='Y' || Field->Ref->Grid[1]=='Z') {
               if (!Field->Ref->Lat) FSTD_FieldReadComp(head,&Field->Ref->Lat,"^^",1,0);
               if (!Field->Ref->Lon) nijk=FSTD_FieldReadComp(head,&Field->Ref->Lon,">>",1,0);
            }

            if (Field->Ref->Grid[1]=='Y') {
               if (!Field->Ref->Lat) FSTD_FieldReadComp(head,&Field->Ref->Lat,"LA",0,0);
               if (!Field->Ref->Lon) nijk=FSTD_FieldReadComp(head,&Field->Ref->Lon,"LO",0,0);
               if (!Field->Ref->Hgt) FSTD_FieldReadComp(head,&Field->Ref->Hgt,"ZH",0,0);
            }
            break;

         case 'Y':
            if (!Field->Ref->Lat) FSTD_FieldReadComp(head,&Field->Ref->Lat,"LA",0,0);
            if (!Field->Ref->Lat) FSTD_FieldReadComp(head,&Field->Ref->Lat,"^^",1,0);
            if (!Field->Ref->Lon) nijk=FSTD_FieldReadComp(head,&Field->Ref->Lon,"LO",0,0);
            if (!Field->Ref->Lon) nijk=FSTD_FieldReadComp(head,&Field->Ref->Lon,">>",1,0);
            if (!Field->Ref->Hgt) FSTD_FieldReadComp(head,&Field->Ref->Hgt,"ZH",0,0);
            break;

         case 'V':
            if (!Field->Ref->Lat) FSTD_FieldReadComp(head,&Field->Ref->Lat,"^^",1,0);
            if (!Field->Ref->Lon) nijk=FSTD_FieldReadComp(head,&Field->Ref->Lon,">>",1,0);
            FSTD_FieldReadVLevels(Field);
            break;
      }
      FSTD_FileUnset(NULL,head->FID);
   }

   // Make sure longitude go from -180 - 180, unless WKT grid
   if (Field->Ref->Grid[0]!='W') {
      for(ni=0;ni<nijk;ni++) {
         if (Field->Ref->Lon[ni]>180) Field->Ref->Lon[ni]-=360.0;
      }
   }
   
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
Vect3d** FSTD_FieldGetMesh(TData *Field,Projection *Proj,int Level) {

   FSTD_Head *head=(FSTD_Head*)Field->Head;
   Coord         coord;
   int           i,j,k;
   unsigned long idx;
   double        z;
   float        *gz=NULL;

   if (!FSTD_FieldReadMesh(Field)) {
      fprintf(stderr,"(Warning) FSTD_FieldGetMesh: Could not find grid definition components");
      return(NULL);
   }

   /*Allocate memory for various levels*/
   if (!Field->Ref->Pos)
      Field->Ref->Pos=(Vect3d**)calloc(Field->Ref->ZRef.LevelNb,sizeof(Vect3d*));

   if (!Field->Ref->Pos)
      return(NULL);

   if (!Field->Ref->Pos[Level]) {
      Field->Ref->Pos[Level]=(Vect3d*)malloc(FSIZE2D(Field->Def)*sizeof(Vect3d));
      if (!Field->Ref->Pos[Level]) {
         fprintf(stderr,"(ERROR) FSTD_FieldGetMesh: Not enough memory to calculate gridpoint location");
         return(NULL);
      }
   }

   if (Field->Spec->Topo && head->FID) {
      if (FSTD_FileSet(NULL,head->FID)<0) {
         return(NULL);
      }
      EZLock_RPNField();
      idx=c_fstinf(head->FID->Id,&i,&j,&k,head->DATEV,head->ETIKET,head->IP1,head->IP2,head->IP3,head->TYPVAR,Field->Spec->Topo);
      if (idx<0) {
         fprintf(stdout,"(WARNING) FSTD_FieldGetMesh: Warning, Could not load corresponding topo field, trying for any (%s)\n",Field->Spec->Topo);
         idx=c_fstinf(head->FID->Id,&i,&j,&k,-1,"",-1,-1,-1,"",Field->Spec->Topo);
      }
      if (idx<0) {
         fprintf(stdout,"(WARNING) FSTD_FieldGetMesh: Could not load corresponding modulator (%s)\n",Field->Spec->Topo);
      } else {
         if (!gz) gz=(float*)malloc(i*j*k*sizeof(float));
         if (gz)  c_fstluk(gz,idx,&i,&j,&k);
      }
      EZUnLock_RPNField();
      FSTD_FileUnset(NULL,head->FID);
   }

   /*Precalculer les tableaux de particules dans l'espace*/
   if (Field->Ref->Lat && Field->Ref->Lon) {
      z=ZRef_Level2Meter(Field->Ref->ZRef.Levels[Level],Field->Ref->ZRef.Type);
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
               coord.Elev+=ZRef_Level2Meter(Field->Ref->Hgt[idx],Field->Ref->ZRef.Type);
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
      Proj->Type->Project(Proj,(GeoVect*)Field->Ref->Pos[Level],NULL,FSIZE2D(Field->Def));
   }

   if (gz)
      free(gz);

   return(Field->Ref->Pos);
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
 *  <Level>     : Niveau a calculer
 *
 * Retour:
 *  <Vect3d*>   : Pointeur sur les positions (NULL si invalide)
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
Vect3d* FSTD_Grid(TData *Field,void *Proj,int Level) {

   FSTD_Head *head=(FSTD_Head*)Field->Head;
   TDataDef  *def;
   Coord      coord;
   float     *lat=NULL,*lon=NULL,*gz=NULL,flat,flon,fele;
   int        i,j,idx,ni,nj,nk,ip1;
   int        idxi;
   char       tile;

   def=Field->SDef?Field->SDef[0]:Field->Def;
   
   /*Verifier la validite de la grille*/
   if (!Field->Ref)
      return(NULL);

   /*Verifier la validite de grille non geographique*/
   if (Field->Ref->Grid[0]=='X' && ((Projection*)Proj)->Type->Def!=PROJPLANE)
      return(NULL);

   if (Field->Ref->Pos && Field->Ref->Pos[Level])
      return(Field->Ref->Pos[Level]);

   if (Field->Ref->Type&GRID_SPARSE) {
      FSTD_FieldGetMesh(Field,Proj,Level);
      return(Field->Ref->Pos[Level]);
   }
   
   /*Allocate memory for various levels*/
   if (!Field->Ref->Pos)
      Field->Ref->Pos=(Vect3d**)calloc(Field->Ref->ZRef.LevelNb,sizeof(Vect3d*));

   if (!Field->Ref->Pos[Level]) {
      Field->Ref->Pos[Level]=(Vect3d*)malloc(FSIZE2D(def)*sizeof(Vect3d));
      if (!Field->Ref->Pos[Level]) {
         fprintf(stderr,"(ERROR) FSTD_Grid: Not enough memory to calculate gridpoint location");
         return(NULL);
      }
   }

   // Check for auto untile
   tile=(Field->Ref->Grid[1]=='#')&&FSTD_UNTILE?'#':0;

   if (Field->Ref->Grid[0]=='V') {
      FSTD_FieldReadMesh(Field);
      
      if (!Field->Ref->Lat || !Field->Ref->Lon) {
         fprintf(stderr,"(ERROR) FSTD_Grid: Section coordinates not defined");
         return(NULL);
      }

      FSTD_FileSet(NULL,head->FID);
      
      for (j=0;j<def->NJ;j++) {

         /*Essayer de recuperer le modulateur (GZ)*/
         if (head->FID && Field->Spec->Topo) {
            ip1=ZRef_Level2IP(Field->Ref->ZRef.Levels[j],Field->Ref->ZRef.Type);
            idx=cs_fstinf(head->FID->Id,&ni,&nj,&nk,head->DATEV,head->ETIKET,ip1,head->IP2,head->IP3,head->TYPVAR,Field->Spec->Topo);
            if (idx<0) {
               if (gz) { free(gz); gz=NULL; };
               fprintf(stdout,"(WARNING) FSTD_Grid: Could not load corresponding modulator (%s) (%f(%i)), using constant pressure\n",Field->Spec->Topo,Field->Ref->ZRef.Levels[j],ip1);
            } else {
               if (!gz) gz=(float*)malloc(ni*nj*nk*sizeof(float));
               if (gz)  cs_fstluk(gz,idx,&ni,&nj,&nk);
            }
         }

         coord.Elev=ZRef_Level2Meter(Field->Ref->ZRef.Levels[j],Field->Ref->ZRef.Type)*Field->Spec->TopoFactor;
         for (i=0;i<def->NI;i++) {
            flat=coord.Lat=Field->Ref->Lat[i];
            flon=coord.Lon=CLAMPLON(Field->Ref->Lon[i]);
            idx=j*def->NI+i;
            if (gz && Field->Ref->RefFrom && Field->Ref->RefFrom->Ids[0]>-1) {
               EZLock_RPNInt();
               c_gdllsval(Field->Ref->RefFrom->Ids[0],&fele,gz,&flat,&flon,1);
               EZUnLock_RPNInt();
               coord.Elev=fele*10.0*Field->Spec->TopoFactor;
            }
            Vect_Init(Field->Ref->Pos[Level][idx],Field->Ref->Lon[i],Field->Ref->Lat[i],coord.Elev);
         }
      }

      if (Proj) {
         ((Projection*)Proj)->Type->Project(((Projection*)Proj),(GeoVect*)Field->Ref->Pos[Level],NULL,FSIZE2D(def));
      }
      FSTD_FileUnset(NULL,head->FID);
   } else {
      
      if (Field->Ref->Ids && Field->Ref->Ids[0]>-1) {
         /*Recuperer les coordonnees des points de grille*/
         lat=(float*)malloc(FSIZE2D(def)*sizeof(float));
         lon=(float*)malloc(FSIZE2D(def)*sizeof(float));

         if (!lat || !lon) {
            fprintf(stderr,"(ERROR) FSTD_Grid: Not enough memory to process gridpoint location");
            free(Field->Ref->Pos[Level]);
            return(Field->Ref->Pos[Level]=NULL);
         }
         EZLock_RPNInt();
         c_gdll(Field->Ref->Ids[0],lat,lon);
         EZUnLock_RPNInt();
      }

      /*Essayer de recuperer le GZ*/
      if (Field->Spec->Topo && head->FID && FSTD_FileSet(NULL,head->FID)>=0) {

         ip1=ZRef_Level2IP(Field->Ref->ZRef.Levels[Level],Field->Ref->ZRef.Type);
         if (Field->Spec->Topo) {
            idx=cs_fstinf(head->FID->Id,&ni,&nj,&nk,head->DATEV,head->ETIKET,ip1,head->IP2,head->IP3,head->TYPVAR,Field->Spec->Topo);
            if (idx<0) {
               fprintf(stdout,"(WARNING) FSTD_Grid: Could not load corresponding topo field, trying for any (%s)\n",Field->Spec->Topo);
               idx=cs_fstinf(head->FID->Id,&ni,&nj,&nk,-1,"",-1,-1,-1,"",Field->Spec->Topo);
            }
            if (!tile && (ni!=def->NI || nj!=def->NJ)) {
               idx=-1;
               }
         } else {
            idx=-1;
         }
         if (idx<0) {
            if (gz) { free(gz); gz=NULL; };
            fprintf(stdout,"(WARNING) FSTD_Grid: Could not load corresponding (%s) (%f(%i)), using constant pressure\n",Field->Spec->Topo,Field->Ref->ZRef.Levels[Level],ip1);
         } else {
            if (!gz) gz=(float*)malloc(def->NI*def->NJ*nk*sizeof(float));
            if (gz)  cs_fstlukt(gz,head->FID->Id,idx,&tile,&ni,&nj,&nk);
         }
         FSTD_FileUnset(NULL,head->FID);
      }

      coord.Elev=ZRef_Level2Meter(Field->Ref->ZRef.Levels[Level],Field->Ref->ZRef.Type)*Field->Spec->TopoFactor;
      /*For every gridpoints*/
      for (j=0;j<def->NJ;j++) {
         for (i=0;i<def->NI;i++) {

            /*Figure out table plane indexes*/
            idxi=j*def->NI+i;

            /*Get height from topographic field*/
            if (gz) {
               coord.Elev=gz[idxi]*Field->Spec->TopoFactor;
               if (Field->Spec->Topo[0]=='G' && Field->Spec->Topo[1]=='Z' ) {
                  coord.Elev*=10.0;
               }
            }

            if (((Projection*)Proj)->Type->Def==PROJPLANE && !Field->Ref->Ids) {
               Vect_Init(Field->Ref->Pos[Level][idxi],i,j,coord.Elev);
            } else if (((Projection*)Proj)->Type->Def==PROJPLANE && ((Projection*)Proj)->Ref && ((Projection*)Proj)->Ref->Ids[0]==Field->Ref->Ids[0]) {
               Vect_Init(Field->Ref->Pos[Level][idxi],i,j,coord.Elev);
            } else {
              if (Field->Ref->Ids && Field->Ref->Ids[0]>-1) {
                  // Fix for G grids which seems to have inverted lat on IG2=1
                  coord.Lat=(Field->Ref->Grid[0]=='G' && head->IG2==1)?-lat[idxi]:lat[idxi];
                  coord.Lon=CLAMPLON(lon[idxi]);
               } else {
                  Field->Ref->Project(Field->Ref,i,j,&coord.Lat,&coord.Lon,0,1);
               }
               Vect_Init(Field->Ref->Pos[Level][idxi],coord.Lon,coord.Lat,coord.Elev);
            }
         }
      }
     
      if (((Projection*)Proj)->Type->Def==PROJPLANE && !Field->Ref->Ids) {
         FSTD_Project(((Projection*)Proj),Field->Ref->Pos[Level],FSIZE2D(def));
      } else if (((Projection*)Proj)->Type->Def==PROJPLANE && ((Projection*)Proj)->Ref && ((Projection*)Proj)->Ref->Ids[0]==Field->Ref->Ids[0]) {
         FSTD_Project(((Projection*)Proj),Field->Ref->Pos[Level],FSIZE2D(def));
      } else {
         ((Projection*)Proj)->Type->Project(((Projection*)Proj),(GeoVect*)Field->Ref->Pos[Level],NULL,FSIZE2D(def));
      }

      if (Field->Ref->Ids && Field->Ref->Ids[0]>-1) {
         free(lat);
         free(lon);
      }
   }

   if (gz) free(gz);

   return(Field->Ref->Pos[Level]);
}

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_DecodeRPNLevelParams>
 * Creation : Fevrier 2009 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Lite le champs de definitions des niveaux hybrides
 *
 * Parametres   :
 *  <Field>     : Champ
 *
 * Retour:
 *  <OK...>     : 0 ou 1
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
 */
int FSTD_DecodeRPNLevelParams(TData *Field) {

   FSTD_File *fid;
   int        i=1;

   if (Field->Ref->ZRef.Type==LVL_UNDEF || Field->Ref->ZRef.Type==LVL_SIGMA || Field->Ref->ZRef.Type==LVL_HYBRID || Field->Ref->ZRef.Type==LVL_ETA) {
      if (Field->Ref->ZRef.PTop==0.0) {
         if ((fid=((FSTD_Head*)Field->Head)->FID)) {
            i=0;

            if (FSTD_FileSet(NULL,fid)<0)
               return(i);

            EZLock_RPNField();
            i=ZRef_DecodeRPN(&Field->Ref->ZRef,fid->Id);
            EZUnLock_RPNField();

            FSTD_FileUnset(NULL,fid);
         }
      }
   }

   return(i);
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
int FSTD_FieldVertInterpolate(Tcl_Interp *Interp,TData *FieldTo,TData *FieldFrom,TData *ZFieldTo,TData *ZFieldFrom) {

   char        *from,*to;
   int          i,ip1;
   TZRefInterp *interp;
   FSTD_Head   *headto=(FSTD_Head*)FieldTo->Head;
   FSTD_Head   *headfrom=(FSTD_Head*)FieldFrom->Head;

   if (FieldFrom->Def->Type!=TD_Float32 || FieldTo->Def->Type!=TD_Float32) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid Field data type, must be Float32",(char*)NULL);
      return(TCL_ERROR);
   }

   if (!FieldFrom) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Origin field not valid",(char*) NULL);
      return(TCL_ERROR);
   }

   if (!FieldTo) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Destination field not valid",(char*) NULL);
      return(TCL_ERROR);
   }

   if (FieldFrom->Def->NI!=FieldTo->Def->NI || FieldFrom->Def->NJ!=FieldTo->Def->NJ) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Horizontal grid sizes differ",(char*)NULL);
      return(TCL_ERROR);
   }

   if (FieldFrom->Ref->ZRef.Type==LVL_GALCHEN || FieldFrom->Ref->ZRef.Type==LVL_MASL || FieldFrom->Ref->ZRef.Type==LVL_MAGL ||
      FieldTo->Ref->ZRef.Type==LVL_GALCHEN || FieldTo->Ref->ZRef.Type==LVL_MASL ||  FieldTo->Ref->ZRef.Type==LVL_MAGL) {
      if (!ZFieldFrom && FieldFrom->Ref->ZRef.Type!=LVL_MASL) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid GZ source field data",(char*)NULL);
         return(TCL_ERROR);
      }
      if (!ZFieldTo && FieldTo->Ref->ZRef.Type!=LVL_MASL) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid GZ destination field data",(char*)NULL);
         return(TCL_ERROR);
      }
   } else {
      if (!ZFieldFrom && FieldFrom->Ref->ZRef.Type!=LVL_PRES) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid P0 source field data",(char*)NULL);
         return(TCL_ERROR);
      }
      if (!ZFieldTo && FieldTo->Ref->ZRef.Type!=LVL_PRES) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid P0 destination field data",(char*)NULL);
         return(TCL_ERROR);
      }
   }

   /*Recuperer tout les niveaux disponibles*/
   if (FieldFrom->ReadCube)
      FieldFrom->ReadCube(Interp,FieldFrom,0,0.0,0.0,NULL);

   if (ZFieldFrom) {
      if (ZFieldFrom->Def->NI!=FieldFrom->Def->NI || ZFieldFrom->Def->NJ!=FieldFrom->Def->NJ) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid source Z field dimensions",(char*)NULL);
         return(TCL_ERROR);
      }
      if (ZFieldFrom->ReadCube)
         ZFieldFrom->ReadCube(Interp,ZFieldFrom,0,0.0,0.0,NULL);
   }

   if (ZFieldTo) {
      if (ZFieldTo->Def->NI!=FieldTo->Def->NI || ZFieldTo->Def->NJ!=FieldTo->Def->NJ) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid destination Z field dimensions",(char*)NULL);
         return(TCL_ERROR);
      }
      if (ZFieldTo->ReadCube)
         ZFieldTo->ReadCube(Interp,ZFieldTo,0,0.0,0.0,NULL);
   }

   Tcl_MutexLock(&MUTEX_FSTDVI);
   ZRefInterp_SetOption("INTERP_DEGREE",FieldTo->Spec->InterpDegree);
   ZRefInterp_SetOption("VERBOSE","NO");

   /*Try to read HY for hybrid levels*/
   FSTD_DecodeRPNLevelParams(FieldFrom);

   if (FieldFrom->Ref->ZRef.Type==LVL_UNDEF && ZFieldFrom->Def->NK==1) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid vertical dimension for source Z field",(char*)NULL);
      Tcl_MutexUnlock(&MUTEX_FSTDVI);
      return(TCL_ERROR);
   }
   if (FieldTo->Ref->ZRef.Type==LVL_UNDEF && ZFieldTo->Def->NK==1) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid vertical dimension for destination Z field",(char*)NULL);
      Tcl_MutexUnlock(&MUTEX_FSTDVI);
      return(TCL_ERROR);
   }

   FieldFrom->Ref->ZRef.P0=ZFieldFrom?(float*)(ZFieldFrom->Def->Data[0]):NULL;
   FieldTo->Ref->ZRef.P0=ZFieldTo?(float*)(ZFieldTo->Def->Data[0]):NULL;

   if (!FSTD_DecodeRPNLevelParams(FieldTo)) {
      Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: (WARNING) Could not find destination hybrid definition field HY",(char*)NULL);
   }

   // Initialize verticap interpolator
   if (!(interp=ZRefInterp_Define(&FieldTo->Ref->ZRef,&FieldFrom->Ref->ZRef,FieldFrom->Def->NI,FieldFrom->Def->NJ))) {
      Tcl_AppendResult(Interp,"Unable to initialize vertical dataset (ZRefInterp_Define)\n");
      Tcl_MutexUnlock(&MUTEX_FSTDVI);
      return(TCL_ERROR);
   }

   /* Inter ET/OU Extrapolation */
   for(i=0;i<3;i++) {
      if (FieldFrom->Def->Data[i]) {
         if (!FieldTo->Def->Data[i]) {
            FieldTo->Def->Data[i]=(char*)calloc(FSIZE3D(FieldTo->Def),TData_Size[FieldTo->Def->Type]);
            if (!FieldTo->Def->Data[i]) {
               Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Not enough memory to allocate field",(char*)NULL);
               Tcl_MutexUnlock(&MUTEX_FSTDVI);
               return(TCL_ERROR);
            }
         }
         Def_Pointer(FieldFrom->Def,i,0,from);
         Def_Pointer(FieldTo->Def,i,0,to);

         // Interpolate datacube
         if (!ZRefInterp(interp,(float*)to,(float*)from,NULL,NULL,0,0)) {
           Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Interpolation error (ZRefInterp)",(char*)NULL);
           Tcl_MutexUnlock(&MUTEX_FSTDVI);
           return(TCL_ERROR);
         }
      } else {
         if (FieldTo->Def->Data[i]) {
            free(FieldTo->Def->Data[i]);
            FieldTo->Def->Data[i]=NULL;
         }
      }
   }

   FieldFrom->Ref->ZRef.P0=NULL;
   FieldTo->Ref->ZRef.P0=NULL;

   if (FieldTo->Ref->NbId!=FieldFrom->Ref->NbId) {
      FieldTo->Ref->Ids=(int*)realloc(FieldTo->Ref->Ids,FieldFrom->Ref->NbId*sizeof(int));
   }
   if (FieldTo->Ref->Ids[FieldTo->Ref->NId]!=FieldFrom->Ref->Ids[FieldFrom->Ref->NId]) {
      FieldTo->Ref->Grid[0]=FieldFrom->Ref->Grid[0];
      FieldTo->Ref->Project=FieldFrom->Ref->Project;
      FieldTo->Ref->UnProject=FieldFrom->Ref->UnProject;
      FieldTo->Ref->Value=FieldFrom->Ref->Value;
      FieldTo->Ref->Type=FieldFrom->Ref->Type;
      memcpy(FieldTo->Ref->Ids,FieldFrom->Ref->Ids,FieldFrom->Ref->NbId*sizeof(int));
      FieldTo->Ref->NId=FieldFrom->Ref->NId;
   }

   ip1=((FSTD_Head*)FieldTo->Head)->IP1;
   memcpy(headto,headfrom,sizeof(FSTD_Head));
   ((FSTD_Head*)FieldTo->Head)->IP1=ip1;

   if (FieldTo->Stat) {
      free(FieldTo->Stat);
      FieldTo->Stat=NULL;
   }

   Tcl_MutexUnlock(&MUTEX_FSTDVI);
   return(TCL_OK);
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
   int        ez=1,ok=-1,idx,n,i,j,k;
   void      *pf0,*pt0,*pf1,*pt1;

   if (!FieldFrom || !FieldFrom->Ref) {
      Tcl_AppendResult(Interp,"FSTD_FieldGridInterpolate: Origin field not valid",(char*)NULL);
      return(TCL_ERROR);
   }
   if (!FieldTo || !FieldTo->Ref) {
      Tcl_AppendResult(Interp,"FSTD_FieldGridInterpolate: Destination field not valid",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Verifier la compatibilite entre source et destination*/
   if (!DataDef_Compat(FieldTo->Def,FieldFrom->Def)) {
      FieldTo->Ref=GeoRef_Resize(FieldTo->Ref,FieldTo->Def->NI,FieldTo->Def->NJ,FieldTo->Def->NK,FieldFrom->Ref->ZRef.Type,FieldFrom->Ref->ZRef.Levels);
   }
   FieldTo->Ref->ZRef.Type=FieldFrom->Ref->ZRef.Type;

   if (FieldFrom->Def->Type!=TD_Float32) {
      ez=0;
   }

   if (FieldFrom->Ref->Grid[0]=='R' || FieldTo->Ref->Grid[0]=='R' || FieldFrom->Ref->Grid[0]=='W' || FieldTo->Ref->Grid[0]=='W' || FieldTo->Ref->Hgt) {
      ez=0;
   }

   if (FieldFrom->Ref->Grid[0]!='R' && FieldTo->Ref->Grid[0]!='R') {
      FSTD_FieldSetTo(FieldTo,FieldFrom);
   }

   /*Use ezscint*/
   if (ez) {
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
         c_ezsetval("EXTRAP_VALUE",FieldTo->Def->NoData);
      }
      c_ezsetopt("EXTRAP_DEGREE",FieldTo->Spec->ExtrapDegree);

      ok=c_ezdefset(FieldTo->Ref->Ids[FieldTo->Ref->NId],FieldFrom->Ref->Ids[FieldFrom->Ref->NId]);

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
        FieldTo->Ref->ZRef.Levels[k]=FieldFrom->Ref->ZRef.Levels[k];
      }
      if (ok<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldGridInterpolate: EZSCINT internal error, interpolation problem",(char*)NULL);
         EZUnLock_RPNInt();
         return(TCL_ERROR);
      }
      EZUnLock_RPNInt();
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
                     val=VertexVal(FieldFrom->Ref,FieldFrom->Def,n,di,dj,k);
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

   if (!Field0) {
      Tcl_AppendResult(Interp,"FSTD_FieldTimeInterpolate: Initial field not valid",(char*)NULL);
      return(TCL_ERROR);
   }
   if (!Field1) {
      Tcl_AppendResult(Interp,"FSTD_FieldTimeInterpolate: Terminal field not valid",(char*)NULL);
      return(TCL_ERROR);
   }
   if (Field0->Def->NI!=Field1->Def->NI || Field0->Def->NJ!=Field1->Def->NJ || Field0->Def->NK!=Field1->Def->NK) {
      Tcl_AppendResult(Interp,"FSTD_FieldTimeInterpolate: Incompatible size",(char*)NULL);
      return(TCL_ERROR);
   }

   /* Est-ce que le champs existe et si oui, verifier les dimensions */
   field=Data_Valid(Interp,Name,Field0->Def->NI,Field0->Def->NJ,Field0->Def->NK,Field0->Def->Data[2]?3:Field0->Def->Data[1]?2:1,Field0->Def->Type);
   if (!field)
      return(TCL_ERROR);

   Field0->Set(field);

   /*Figure out le delai entre les deux champs*/
   f77name(difdatr)(&head1->DATEV,&head0->DATEV,&delay);
   f77name(difdatr)(&Stamp,&head0->DATEV,&dt);

   dt=dt/delay;
   v0=v1=0.0;

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

   return(TCL_OK);
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
   TGeoRef     *ref=NULL;
   int          i,j,idx,nidx;
   char         buf[64],*grtyp=NULL;
   double       dxg1,dxg2,dxg3,dxg4;
   float        xg1,xg2,xg3,xg4;
   double       tra[6],inv[6],*tm,*im;
   const char **list;

   static CONST char *sopt[] = { "-DATEO","-DATEV","-DEET","-FID","-KEY","-NPAS","-NI","-NJ","-NK","-NBITS","-DATYP","-IP1","-IP2","-IP3",
                                 "-TYPVAR","-NOMVAR","-ETIKET","-GRIDID","-GRTYP","-IG1","-IG2","-IG3","-IG4","-SWA","-LNG","-DLTF",
                                 "-UBC","-EX1","-EX2","-EX3","-DATA","-grid","-positional","-projection","-transform","-georef",NULL };
   enum        opt { DATEO,DATEV,DEET,FID,KEY,NPAS,NI,NJ,NK,NBITS,DATYP,IP1,IP2,IP3,TYPVAR,NOMVAR,ETIKET,GRIDID,GRTYP,
                     IG1,IG2,IG3,IG4,SWA,LNG,DLTF,UBC,EX1,EX2,EX3,DATA,GRID,POSITIONAL,PROJECTION,TRANSFORM,GEOREF };

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
               if (head->FID) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(head->FID->CId,-1));
               }
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
                  Field->Ref=GeoRef_RPNSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,(Field->Ref?Field->Ref->ZRef.Type:LVL_UNDEF),(Field->Ref?Field->Ref->ZRef.Levels:NULL),"X",head->IG1,head->IG2,head->IG3,head->IG4,head->FID?head->FID->Id:-1);
               }
               Field->Ref->ZRef.Levels[Field->Def->Level]=ZRef_IP2Level(head->IP1,&Field->Ref->ZRef.Type);
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
               if (Field->Spec && !Field->Spec->Desc)
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

        case GRID:
            if (Objc==1 && Field->Ref) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->Ref->NId));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&nidx);
               if (Field->Ref->NbId>1) {
                  if (nidx>Field->Ref->NbId) {
                     Tcl_AppendResult(Interp,"\n   FSTD_FieldDefine: Invalid subgrid index",(char*)NULL);
                     return(TCL_ERROR);
                  } else {
                     FSTD_FieldSubSelect(Field,nidx);
                  }
               }
            }
            break;

         case GRIDID:
            if (Objc==1 && Field->Ref) {
               obj=Tcl_NewListObj(0,NULL);
               for(nidx=0;nidx<Field->Ref->NbId;nidx++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->Ref->Ids[nidx]));
               }
               Tcl_SetObjResult(Interp,obj);
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
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,ref->Type,ref->ZRef.Levels,ref->Grid,ref->IG1,ref->IG2,ref->IG3,ref->IG4,Tcl_GetString(Objv[i]),ref->Transform,ref->InvTransform,NULL);
                     Field->Ref->Grid[1]=ref->Grid[1];
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,LVL_UNDEF,NULL,NULL,0,0,0,0,Tcl_GetString(Objv[i]),NULL,NULL,NULL);
                  }
                  ref=NULL;
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
                  return(TCL_ERROR);
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
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,ref->Type,ref->ZRef.Levels,ref->Grid,ref->IG1,ref->IG2,ref->IG3,ref->IG4,ref->String,tm,im,NULL);
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,LVL_UNDEF,NULL,NULL,0,0,0,0,NULL,tm,im,NULL);
                  }
                  ref=NULL;
                  Data_Clean(Field,1,1,1);
               }
            }
            break;

         case GEOREF:
            if (Objc==1) {
               if (Field->Ref) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->Ref->Name,-1));
                  GeoRef_Incr(Field->Ref);
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
               ref=NULL;
               GeoRef_Incr(Field->Ref);
            }
            break;

         case POSITIONAL:
            if (Objc<3) {
               Tcl_WrongNumArgs(Interp,0,Objv,"Field FieldAX FieldAY");
               return(TCL_ERROR);
            } else {
               fieldAX=Data_Get(Tcl_GetString(Objv[++i]));
               if (!fieldAX) {
                  Tcl_AppendResult(Interp,"invalid AX Axis field :",Tcl_GetString(Objv[i]),(char*)NULL);
                  return(TCL_ERROR);
               }
               fieldAY=Data_Get(Tcl_GetString(Objv[++i]));
               if (!fieldAY) {
                  Tcl_AppendResult(Interp,"invalid AY Axis field :",Tcl_GetString(Objv[i]),(char*)NULL);
                  return(TCL_ERROR);
               }

               if (!GeoRef_Positional(Field->Ref,fieldAX->Def,fieldAY->Def)) {
                  Tcl_AppendResult(Interp,"unable to initialize positional data",(char*)NULL);
                  return(TCL_ERROR);
               }

               if (Field->Ref->Grid[0]=='Z' || Field->Ref->Grid[0]=='Y') {
                  head=(FSTD_Head*)fieldAX->Head;
                  EZLock_RPNInt();
                  if (!Field->Ref->Ids) 
                     Field->Ref->Ids=(int*)malloc(1*sizeof(int));

                  Field->Ref->Ids[Field->Ref->NId]=c_ezgdef_fmem(Field->Def->NI,Field->Def->NJ,Field->Ref->Grid,fieldAX->Ref->Grid,head->IG1,head->IG2,head->IG3,head->IG4,Field->Ref->Lon,Field->Ref->Lat);
                  EZUnLock_RPNInt();
                  EZGrid_IdIncr(Field->Ref->Ids[Field->Ref->NId]);
               }
               if (Field->Stat) { free(Field->Stat); Field->Stat=NULL; }

               GeoRef_Qualify(Field->Ref);
               Data_Clean(Field,1,1,1);
            }
            break;

         case GRTYP:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->Ref->Grid,-1));
            } else {
               grtyp=Tcl_GetString(Objv[++i]);

               if (Objc==2 && (Field->Ref && grtyp[0]==Field->Ref->Grid[0])) {
                  return(TCL_OK);
               }
               Data_Clean(Field,1,1,1);
               ref=Field->Ref;

               if (grtyp[0]=='W' || grtyp[1]=='W') {
                  if (ref) {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,ref->Type,ref->ZRef.Levels,grtyp,ref->IG1,ref->IG2,ref->IG3,ref->IG4,ref->String,ref->Transform,ref->InvTransform,NULL);
                  } else {
                     Field->Ref=GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,LVL_UNDEF,NULL,grtyp,0,0,0,0,NULL,NULL,NULL,NULL);
                  }
                  grtyp=NULL;
               } else {
                  if (grtyp[0]=='L' || grtyp[0]=='N' || grtyp[0]=='S') {
                     if (i+4<Objc && (Tcl_GetDoubleFromObj(Interp,Objv[i+1],&dxg1)!=TCL_ERROR)) {
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&dxg1);
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&dxg2);
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&dxg3);
                        Tcl_GetDoubleFromObj(Interp,Objv[++i],&dxg4);

                        xg1=dxg1;xg2=dxg2;xg3=dxg3;xg4=dxg4;
                        f77name(cxgaig)(grtyp,&head->IG1,&head->IG2,&head->IG3,&head->IG4,&xg1,&xg2,&xg3,&xg4);
                     } else {
                        Tcl_AppendResult(Interp,"Invalid number of arguments, must be -GRTYP type xg1 xg2 xg3 xg4",(char*)NULL);
                        return(TCL_ERROR);
                     }
                  } else if (grtyp[0]=='A' || grtyp[0]=='B' || grtyp[0]=='G') {
                     if (i+4<Objc && (Tcl_GetIntFromObj(Interp,Objv[i+1],&j)!=TCL_ERROR)) {
                        Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG1);
                        Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG2);
                        Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG3);
                        Tcl_GetIntFromObj(Interp,Objv[++i],&head->IG4);
                     } else {
                        Tcl_AppendResult(Interp,"Invalid number of arguments, must be -GRTYP type ig1 ig2 ig3 ig4",(char*)NULL);
                        return(TCL_ERROR);
                     }
                  } else if (grtyp[0]=='V') {
                     if (!Field->Ref) {
                         Field->Ref=GeoRef_Reference(GeoRef_RPNSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,LVL_UNDEF,NULL,"A ",0,0,0,0,-1));
                     }
                     Field->Ref->ZRef.Levels=(float*)realloc(Field->Ref->ZRef.Levels,Field->Def->NJ*sizeof(float));
                  }
               }
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
            nidx=-1;
            obj=NULL;
            
            if (Objc>1) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&nidx);
               
               if (strcmp(Objv[i]->typePtr->name,"bytearray")==0) {
                  obj=Objv[i];
                  nidx=-1;
               }
            }
           
            if (Objc>2) {
               obj=Objv[++i];
            }
            
            if (obj) {
               return Data_ValPutMatrix(Interp,Field,nidx,obj);
            } else {
               Data_ValGetMatrix(Interp,Field,nidx,0);
            }
            break;
       }
   }
   
   if (grtyp) {
      Field->Ref=GeoRef_RPNSetup(Field->Def->NI,Field->Def->NJ,Field->Def->NK,(ref?ref->Type:LVL_UNDEF),(ref?ref->ZRef.Levels:NULL),grtyp,head->IG1,head->IG2,head->IG3,head->IG4,head->FID?head->FID->Id:-1);
      GeoRef_Qualify(Field->Ref);
   }
   if (ref) {
      GeoRef_Destroy(Interp,ref->Name);
   }
   
   return(TCL_OK);
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
TData *FSTD_FieldCreate(Tcl_Interp *Interp,char *Name,int NI,int NJ,int NK,TData_Type Type){

   TData *field;

   field=Data_Valid(Interp,Name,NI,NJ,NK,1,Type==TD_Binary?TD_Byte:Type);

   if (!field)
     return(NULL);

   FSTD_FieldSet(field);
   field->Ref=GeoRef_RPNSetup(NI,NJ,NK,LVL_UNDEF,NULL,"X",0,0,0,0,-1);

   return(field);
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
   file=FSTD_FileGet(Interp,Id);
   if (FSTD_FileSet(Interp,file)<0)
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

   file=FSTD_FileGet(Interp,Id);
   if (FSTD_FileSet(Interp,file)<0)
      return(TCL_ERROR);

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
      return(TCL_ERROR);
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

   return(TCL_OK);
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

   FSTD_Head    head;
   Tcl_Obj     *list,*obj,*tmp;
   int          nobj,nb,ni,nj,nk;
   int          yyyy,mm,dd,h,m,s,type;
   char         buf[1024],grtyp[2];
   double       nhour,lvl;
   const char **units;

   if (Mode==FSTD_LISTNONE) {
      return(TCL_OK);
   }

   if((nb=FSTD_FileSet(Interp,File))<0)
      return(TCL_ERROR);

   list=Tcl_NewListObj(0,NULL);
   obj=Tcl_NewObj();

   EZLock_RPNField();
   head.KEY=c_fstinf(File->Id,&ni,&nj,&nk,-1,"",-1,-1,-1,"","");

   units=ZRef_LevelUnits();
   
   /* Boucle sur tout les enregistrements */
   while (head.KEY>=0) {

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
         nhour=((double)head.NPAS*head.DEET)/3600.0;
         if (head.DATEO==0) {
            head.DATEV=0;
         } else {
            f77name(incdatr)(&head.DATEV,&head.DATEO,&nhour);
         }
         if (head.DATEV==101010101) head.DATEV=0;
         switch(Mode) {
            case FSTD_LISTSPI:
               if (head.NOMVAR[0]=='\0') {
                  head.NOMVAR[0]='-'; head.NOMVAR[1]='\0';
               }
                              
               if (head.TYPVAR[0]=='\0') {
                  head.TYPVAR[0]='-'; head.TYPVAR[1]='\0';
               }
               if (head.ETIKET[0]=='\0') {
                  head.ETIKET[0]='-'; head.ETIKET[1]='\0';
               }
               
               // Check for descriptor
               if (FSTD_HIDELIST) {
                  Tcl_ListObjLength(Interp,FSTD_HIDELIST,&nobj);
                  for(s=0;s<nobj;s++) {
                     Tcl_ListObjIndex(Interp,FSTD_HIDELIST,s,&tmp);
                     if (Tcl_StringMatch(head.NOMVAR,Tcl_GetString(tmp))) {                     
//                     if (!strncmp(head.NOMVAR,Tcl_GetString(tmp),4)) {
                        nobj=9999;
                        break;
                     }
                  }
                  // If it is, skip
                  if (nobj==9999) {
                     break;
                  }
               }
              
               sprintf(buf,"%-4s %-2s  ",head.NOMVAR,head.TYPVAR);
               lvl=ZRef_IP2Level(head.IP1,&type);
               switch(type) {
                  case LVL_MASL  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                  case LVL_SIGMA : sprintf(buf,"%s %8.4f %-2s",buf,lvl,units[type]); break;
                  case LVL_PRES  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                  case LVL_UNDEF : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                  case LVL_MAGL  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                  case LVL_HYBRID: sprintf(buf,"%s %8.6f %-2s",buf,lvl,units[type]); break;
                  case LVL_THETA : sprintf(buf,"%s %8.4f %-2s",buf,lvl,units[type]); break;
                  case LVL_HOUR  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
               }

               if (head.IP2>32000) {
                  lvl=ZRef_IP2Level(head.IP2,&type);
                  switch(type) {
                     case LVL_MASL  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                     case LVL_SIGMA : sprintf(buf,"%s %8.4f %-2s",buf,lvl,units[type]); break;
                     case LVL_PRES  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                     case LVL_UNDEF : sprintf(buf,"%s %8.0f %-2s",buf,lvl,units[type]); break;
                     case LVL_MAGL  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                     case LVL_HYBRID: sprintf(buf,"%s %8.6f %-2s",buf,lvl,units[type]); break;
                     case LVL_THETA : sprintf(buf,"%s %8.4f %-2s",buf,lvl,units[type]); break;
                     case LVL_HOUR  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                  }
               } else {
                  sprintf(buf,"%s %8i %-2s",buf,head.IP2,units[LVL_UNDEF]);
               }

               if (head.IP3>32000) {
                  lvl=ZRef_IP2Level(head.IP3,&type);
                  switch(type) {
                     case LVL_MASL  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                     case LVL_SIGMA : sprintf(buf,"%s %8.4f %-2s",buf,lvl,units[type]); break;
                     case LVL_PRES  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                     case LVL_UNDEF : sprintf(buf,"%s %8.0f %-2s",buf,lvl,units[type]); break;
                     case LVL_MAGL  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                     case LVL_HYBRID: sprintf(buf,"%s %8.6f %-2s",buf,lvl,units[type]); break;
                     case LVL_THETA : sprintf(buf,"%s %8.4f %-2s",buf,lvl,units[type]); break;
                     case LVL_HOUR  : sprintf(buf,"%s %8.1f %-2s",buf,lvl,units[type]); break;
                  }
               } else {
                  sprintf(buf,"%s %8i %-2s",buf,head.IP3,units[LVL_UNDEF]);
               }

               System_StampDecode(head.DATEV,&yyyy,&mm,&dd,&h,&m,&s);
               sprintf(buf,"%s %-12s %04i%02i%02i%02i%02i %s %i %i %i %i fstdfield",buf,head.ETIKET,yyyy,mm,dd,h,m,File->CId,head.KEY,head.IP1,head.IP2,head.IP3);
               Tcl_SetStringObj(obj,buf,-1);
               Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               break;

            case FSTD_LISTALL:
               sprintf(buf,"%s %i {%s} {%s} %i %i %i {%s} %09i %09i %i %i %i",
                  File->CId,head.KEY,head.NOMVAR,head.TYPVAR,head.IP1,head.IP2,head.IP3,head.ETIKET,head.DATEO,head.DATEV,ni,nj,nk);
               Tcl_SetStringObj(obj,buf,-1);
               Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               break;

             case FSTD_LISTEXTENDED:
               sprintf(buf,"%s %i {%s} {%s} %i %i %i {%s} %09i %i %i %09i %i %i %i %c %i %i %i %i %i %i",
                  File->CId,head.KEY,head.NOMVAR,head.TYPVAR,head.IP1,head.IP2,head.IP3,head.ETIKET,head.DATEO,head.DEET,head.NPAS,head.DATEV,ni,nj,nk,grtyp[0],head.IG1,head.IG2,head.IG3,head.IG4,head.DATYP,head.NBITS);
               Tcl_SetStringObj(obj,buf,-1);
               Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               break;

           case FSTD_LISTVAR:
               Tcl_SetStringObj(obj,head.NOMVAR,-1);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                  Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;

            case FSTD_LISTTYPVAR:
               Tcl_SetStringObj(obj,head.TYPVAR,-1);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                  Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;

            case FSTD_LISTETIKET:
               Tcl_SetStringObj(obj,head.ETIKET,-1);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                  Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;

            case FSTD_LISTDATEV:
               if (head.DATEV>0) {
                  Tcl_SetLongObj(obj,System_Stamp2Seconds(head.DATEV));
                  if (TclY_ListObjFind(Interp,list,obj)==-1) {
                     Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
                  }
               }
               break;

            case FSTD_LISTIP1:
               Tcl_SetIntObj(obj,head.IP1);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                     Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;

            case FSTD_LISTIP2:
               Tcl_SetIntObj(obj,head.IP2);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                     Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;

            case FSTD_LISTIP3:
               Tcl_SetIntObj(obj,head.IP3);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                     Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;
               
            case FSTD_LISTIG1:
               Tcl_SetIntObj(obj,head.IG1);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                     Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;
               
            case FSTD_LISTIG2:
               Tcl_SetIntObj(obj,head.IG2);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                     Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;
               
            case FSTD_LISTIG3:
               Tcl_SetIntObj(obj,head.IG3);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                     Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;
               
            case FSTD_LISTIG4:
               Tcl_SetIntObj(obj,head.IG4);
               if (TclY_ListObjFind(Interp,list,obj)==-1) {
                     Tcl_ListObjAppendElement(Interp,list,Tcl_DuplicateObj(obj));
               }
               break;              
         }
      }
      head.KEY=c_fstsui(File->Id,&ni,&nj,&nk);
   }
   EZUnLock_RPNField();
   FSTD_FileUnset(Interp,File);

   Tcl_DecrRefCount(obj);
   Tcl_SetObjResult(Interp,list);
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

   FSTD_File   *file;
   TData       *field=NULL;
   TDataVector *uvw=NULL;
   TData_Type   dtype;
   FSTD_Head    h;
   int          ok,ni,nj,nk,i,type,idx,datyp,mni,mnj,mnk;
   int          pni,pnj,ig1,ig2,ig3,ig4;
   float        lvl,*tmp;
   char         nomvar[5],typvar[2],grtyp[3],tile,etik[13],*proj=NULL;
   double       nhour,val=0.0;

   file=FSTD_FileGet(Interp,Id);
   if (FSTD_FileSet(Interp,file)<0)
      return(TCL_ERROR);
   
   /*Rechercher et lire l'information de l'enregistrement specifie*/
   if (Key==-1) {
      Key=cs_fstinf(file->Id,&ni,&nj,&nk,DateV,Eticket,IP1,IP2,IP3,TypVar,NomVar);
      if (Key<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldRead: Specified field does not exist (c_fstinf failed)",(char*)NULL);
         EZUnLock_RPNField();
         FSTD_FileUnset(Interp,file);
         return(TCL_ERROR);
      }
   }

   grtyp[0]=grtyp[1]=grtyp[2]='\0';
   
   h.KEY=Key;
   h.FID=file;
   strcpy(h.NOMVAR,"    ");
   strcpy(h.TYPVAR,"  ");
   strcpy(h.ETIKET,"            ");
   ok=cs_fstprm(h.KEY,&h.DATEO,&h.DEET,&h.NPAS,&mni,&mnj,&mnk,&h.NBITS,
         &h.DATYP,&h.IP1,&h.IP2,&h.IP3,h.TYPVAR,h.NOMVAR,h.ETIKET,
         grtyp,&h.IG1,&h.IG2,&h.IG3,&h.IG4,&h.SWA,&h.LNG,&h.DLTF,
         &h.UBC,&h.EX1,&h.EX2,&h.EX3);

   if (ok<0) {
      Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not get field information for ",Name," (c_fstprm failed)",(char*)NULL);
      FSTD_FileUnset(Interp,file);
      return(TCL_ERROR);
   }

   // Check for auto untile
   tile=(grtyp[0]=='#')&&FSTD_UNTILE?'#':0;
   
   if (tile) {
      Key=cs_fstinf(file->Id,&mni,&pnj,&nk,-1,"",h.IG1,h.IG2,-1,"",">>");
      Key=cs_fstinf(file->Id,&pni,&mnj,&nk,-1,"",h.IG1,h.IG2,-1,"","^^");
      if (Key<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldRead: Projection description field does not exist (c_fstinf failed)",(char*)NULL);
      }
   }  
   datyp=h.DATYP>128?h.DATYP-128:h.DATYP;

   // have to boost nbit to 32 for nbit=1, not sure why (X32) ????
   if (h.NBITS==32 && datyp==0) datyp=5;
   dtype=FSTD_TypeCheck(datyp,h.NBITS==1?32:h.NBITS);
//   fprintf(stderr,"---- %i\n",dtype);
//dtype=TD_UInt32;

   /*Calculer la date de validitee du champs*/
   if (h.DATEO!=0) {
      nhour=((double)h.NPAS*h.DEET)/3600.0;
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
   if ((uvw=Data_VectorTableCheck(h.NOMVAR,&idx)) && uvw->VV) {
      field=Data_Valid(Interp,Name,mni,mnj,mnk,(uvw->WW?3:2),dtype);
      if (!field) {
         FSTD_FileUnset(Interp,file);
         return(TCL_ERROR);
      }

      /*Recuperer les donnees du champs*/
      c_fst_data_length(TData_Size[field->Def->Type]);
      if (cs_fstlukt(field->Def->Data[idx],h.FID->Id,h.KEY,&tile,&ni,&nj,&nk)<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not read field data (c_fstluk failed)",(char*)NULL);
         FSTD_FileUnset(Interp,file);
         return(TCL_ERROR);
      }
      strcpy(h.NOMVAR,uvw->UU);

      if (uvw->UU && idx!=0) {
         /*Rechercher et lire l'information de l'enregistrement complementaire*/
         ok=cs_fstinf(h.FID->Id,&ni,&nj,&nk,h.DATEV,h.ETIKET,h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->UU);
         if (ok<0) {
            ok=cs_fstinf(h.FID->Id,&ni,&nj,&nk,h.DATEV,"",h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->UU);
         }

         if (ok<0 || (!tile && (ni!=mni || nj!=mnj))) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not find first component field ",uvw->UU," (c_fstinf failed)",(char*)NULL);
            FSTD_FileUnset(Interp,file);
            return(TCL_ERROR);
         } else {
            c_fst_data_length(TData_Size[field->Def->Type]);
            if (cs_fstlukt(field->Def->Data[0],h.FID->Id,ok,&tile,&ni,&nj,&nk)<0) {
               Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not read field data (c_fstluk failed)",(char*)NULL);
               FSTD_FileUnset(Interp,file);
               return(TCL_ERROR);
            }
         }
      }
      if (uvw->VV && idx!=1) {
         /*Rechercher et lire l'information de l'enregistrement complementaire*/
         ok=cs_fstinf(h.FID->Id,&ni,&nj,&nk,h.DATEV,h.ETIKET,h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->VV);
         if (ok<0) {
            ok=cs_fstinf(h.FID->Id,&ni,&nj,&nk,h.DATEV,"",h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->VV);
         }

         if (ok<0 || (!tile && (ni!=mni || nj!=mnj))) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not find second component field ",uvw->VV," (c_fstinf failed)",(char*)NULL);
            FSTD_FileUnset(Interp,file);
            return(TCL_ERROR);
         } else {
            c_fst_data_length(TData_Size[field->Def->Type]);
            if (cs_fstlukt(field->Def->Data[1],h.FID->Id,ok,&tile,&ni,&nj,&nk)<0) {
               Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not read field data (c_fstluk failed)",(char*)NULL);
               FSTD_FileUnset(Interp,file);
               return(TCL_ERROR);
            }
         }
      }
      if (uvw->WW && idx!=2) {
         /*Rechercher et lire l'information de l'enregistrement complementaire*/
         ok=cs_fstinf(h.FID->Id,&ni,&nj,&nk,h.DATEV,h.ETIKET,h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->WW);
         if (ok<0) {
            ok=cs_fstinf(h.FID->Id,&ni,&nj,&nk,h.DATEV,"",h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->WW);
         }

         if (ok<0 || (!tile && (ni!=mni || nj!=mnj))) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not find third component field ",uvw->WW," (c_fstinf failed)",(char*)NULL);
            FSTD_FileUnset(Interp,file);
            return(TCL_ERROR);
         } else {
            c_fst_data_length(TData_Size[field->Def->Type]);
            if (cs_fstlukt(field->Def->Data[2],h.FID->Id,ok,&tile,&ni,&nj,&nk)<0) {
               Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not read field data (c_fstluk failed)",(char*)NULL);
               FSTD_FileUnset(Interp,file);
               return(TCL_ERROR);
            }
         }
      }
      if (uvw->WW && uvw->WWFactor!=0.0) {
         for(i=0;i<FSIZE2D(field->Def);i++) {
             Def_Get(field->Def,2,i,val);
             val*=uvw->WWFactor;
             Def_Set(field->Def,2,i,val); 
          }
      }
   } else {
      /*Verifier si le champs existe et est valide*/
      field=Data_Valid(Interp,Name,mni,mnj,mnk,1,dtype);
      if (!field) {
         FSTD_FileUnset(Interp,file);
         return(TCL_ERROR);
      }

      /*Recuperer les donnees du champs*/
      c_fst_data_length(TData_Size[field->Def->Type]);
      if ((ok=cs_fstlukt(field->Def->Data[0],h.FID->Id,h.KEY,&tile,&ni,&nj,&nk))<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not read field data (c_fstluk failed)",(char*)NULL);
         FSTD_FileUnset(Interp,file);
         return(TCL_ERROR);
      }
   }
   
   /*Check for mask (TYPVAR==@@)*/
   if (!(h.TYPVAR[0]=='@' && h.TYPVAR[1]=='@')) {
      ok=cs_fstinf(h.FID->Id,&ni,&nj,&nk,h.DATEV,h.ETIKET,h.IP1,h.IP2,h.IP3,"@@",h.NOMVAR);
      if (ok>0 && (tile || (ni==mni && nj==mnj && nk==mnk))) {
         if ((field->Def->Mask=(char*)malloc(ni*nj))) {
            if ((tmp=(float*)malloc(ni*nj*sizeof(float)))) {
               cs_fstlukt(tmp,h.FID->Id,ok,&tile,&ni,&nj,&nk);
               for(i=0;i<ni*nj;i++) {
                  field->Def->Mask[i]=tmp[i]!=0.0;
               }
               free(tmp);
            } else {
               free(field->Def->Mask);
               field->Def->Mask=NULL;
               fprintf(stdout,"(WARNING) FSTD_FieldRead: Could not allocate memory to read mask");
            }
         } else {
            fprintf(stdout,"(WARNING) FSTD_FieldRead: Could not allocate memory for mask");
         }
      }
   }

   /*Recuperer les type de niveaux et forcer ETA pour SIGMA*/
   lvl=ZRef_IP2Level(h.IP1,&type);
   type=type==LVL_SIGMA?LVL_ETA:type;

   /*Override le type de niveaux pour ZH is ip1=0*/
   if (h.NOMVAR[0]=='Z' && h.NOMVAR[1]=='H') {
      grtyp[0]='Y';
      if (h.IP1==0){
         type=LVL_MASL;
      }
   }

   /*Creer une grille comme definie dans l'enregistrement*/
   if (grtyp[0]=='W' || grtyp[0]=='Y' || grtyp[0]=='X' || grtyp[0]=='Z') {
      float tmpv[6];
      double mtx[6],inv[6],*tm=NULL,*im=NULL;
      char   t='\0';

      ig1=h.IG1;
      ig2=h.IG2;
      ig3=h.IG3;
      ig4=h.IG4;

      /*Here we test for W grids, by getting the subgrid format from the >> field*/
      if (grtyp[0]!='W') {
         t=grtyp[0];
         ok=-1;
         Key=cs_fstinf(file->Id,&pni,&pnj,&nk,-1,"",h.IG1,h.IG2,h.IG3,"",">>");
         if (Key>0) {
            ok=cs_fstprm(Key,&pni,&pni,&pni,&pni,&pni,&pni,&pni,
                  &pni,&pni,&pni,&pni,typvar,nomvar,etik,
                  grtyp,&ig1,&ig2,&ig3,&ig4,&pni,&pni,&pni,
                  &pni,&pni,&pni,&pni);
         }

         /*This is an X grid but with associated >> ^^, so we force it to W for proper processing*/
         if (ok>=0 && t=='X') {
            grtyp[0]='W';
         }

         /*This is no W grid so keep previous grtyp*/
         if (grtyp[0]!='W') {
            grtyp[0]=t;
         }
      }

      /*If it is a W grid, look for PROJ and MATRIX fields*/
      if (grtyp[0]=='W') {

         Key=cs_fstinf(file->Id,&pni,&pnj,&nk,-1,"",ig1,ig2,ig3,"","PROJ");
         if (Key<0) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Projection description field does not exist (c_fstinf failed)",(char*)NULL);
         } else {
            if ((proj=(char*)malloc(pni*pnj*4))) {
               cs_fstluk(proj,Key,&pni,&pnj,&nk);
            }
         }

         Key=cs_fstinf(file->Id,&pni,&pnj,&nk,-1,"",ig1,ig2,ig3,"","MTRX");
         if (Key<0) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Tranform matrix field does not exist (c_fstinf failed)",(char*)NULL);
         } else {
            cs_fstluk(tmpv,Key,&pni,&pnj,&nk);
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
         grtyp[1]=t;
         field->Ref=GeoRef_WKTSetup(ni,nj,nk,type,&lvl,grtyp,h.IG1,h.IG2,h.IG3,h.IG4,proj,tm,im,NULL);

         if (proj) free(proj);
      }
   }

   if (tile) {
      grtyp[0]='Z';grtyp[1]='#';
   }
   
   if (grtyp[0]!='W') {
      field->Ref=GeoRef_RPNSetup(ni,nj,nk,type,&lvl,grtyp,h.IG1,h.IG2,h.IG3,h.IG4,h.FID->Id);
   }

   if (grtyp[0]=='U') {
      FSTD_FieldSubBuild(field);
   }
   
//TODO   h.FID->NRef++;

   FSTD_FieldSet(field);
   GeoRef_Qualify(field->Ref);

   if (field->Spec->Desc)
      free(field->Spec->Desc);

   field->Spec->Desc=strdup(h.NOMVAR);
   memcpy(field->Head,&h,sizeof(FSTD_Head));

   // If it's a profile/xsection, read descriptors
   if (grtyp[0]=='V') {
      field->Spec->Extrude=strdup("^>");
      FSTD_FieldReadMesh(field);
   }

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
int FSTD_FieldReadLevels(Tcl_Interp *Interp,TData *Field,int Invert,double LevelFrom,double LevelTo,Tcl_Obj *List){

   FSTD_Head   *head=(FSTD_Head*)Field->Head;
   TDataVector *uvw;
   TGeoRef     *ref;
   Tcl_Obj     *obj;
   int          idxs[FSTD_NKMAX],tmp[FSTD_NKMAX],i,k,k2,idx,ok,idump,ni,nj,nk,type,ip1;
   char         cdump[16],grtyp[2],tile;
   char        *p;
   float        levels[FSTD_NKMAX];
   double       level,val;

   if (Field->Def->NK>1 || Field->Ref->Grid[0]=='V')
      return(1);

   if (FSTD_FileSet(Interp,head->FID)<0)
      return(0);

   if (LevelFrom>LevelTo) {
      level=LevelTo;
      LevelTo=LevelFrom;
      LevelFrom=level;
   }
   
   // Check for auto untile
   tile=(Field->Ref->Grid[1]=='#')&&FSTD_UNTILE?'#':0;

   if (List) {
      Tcl_ListObjLength(Interp,List,&nk);
      for(k=0;k<nk;k++) {
         Tcl_ListObjIndex(Interp,List,k,&obj);
         Tcl_GetDoubleFromObj(Interp,obj,&level);
         ip1=ZRef_Level2IP(level,Field->Ref->ZRef.Type);
         cs_fstinl(head->FID->Id,&ni,&nj,&k2,head->DATEV,head->ETIKET,ip1,head->IP2,(tile?head->IP3:-1),head->TYPVAR,head->NOMVAR,&idxs[Invert?nk-k-1:k],&k2,1);

         if (k2==0) {
            Tcl_AppendResult(Interp,"FSTD_FieldReadLevels: Could not find specific level ",Tcl_GetString(obj),(char*)NULL);
            FSTD_FileUnset(Interp,head->FID);
            return(0);
         }
         tmp[k]=k;
      }
   } else {
      /*Recuperer les indexes de tout les niveaux*/
      cs_fstinl(head->FID->Id,&ni,&nj,&nk,head->DATEV,head->ETIKET,-1,head->IP2,(tile?head->IP3:-1),head->TYPVAR,head->NOMVAR,idxs,&nk,FSTD_NKMAX);

      if (nk<=1) {
         FSTD_FileUnset(Interp,head->FID);
         return(1);
      }

      /*Determiner les niveaux disponibles*/
      k2=0;
      for(k=0;k<nk;k++) {
         ok=cs_fstprm(idxs[k],&idump,&idump,&idump,&idump,&idump,&idump,&idump,
            &idump,&tmp[k2],&idump,&idump,cdump,cdump,cdump,grtyp,&idump,&idump,
            &idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump);

         /*Verifier que l'on garde le meme type de niveau*/
         level=ZRef_IP2Level(tmp[k2],&type);
         type=type==LVL_SIGMA?LVL_ETA:type;

         if (type==Field->Ref->ZRef.Type) {

            /* Check if level is within the specified range if any*/
            if (LevelFrom==LevelTo || (level>=LevelFrom && level<=LevelTo)) {

               /*Verifier que l'on a pas deja ce niveau (niveau en double)*/
               for(i=0;i<k2;i++) {
                  if (tmp[k2]==tmp[i]) break;
               }

               /*Garder ce niveau en metre pour le tri*/
               if (i>=k2) {
                  levels[k2]=ZRef_IP2Meter(tmp[k2]);
                  idxs[k2]=idxs[k];
                  k2++;
               }
            }
         }
      }
      nk=k2;

      if (nk==0) {
         return(1);
      }

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
   }

   k2=Field->Ref->NId;
   FSTD_FieldSubSelect(Field,0);

   /*Augmenter la dimension du tableau*/
   if (!DataDef_Resize(Field->Def,Field->Def->NI,Field->Def->NJ,nk)) {
      Tcl_AppendResult(Interp,"FSTD_FieldReadLevels: Not enough memory to allocate levels",(char*)NULL);
      FSTD_FileUnset(Interp,head->FID);
      return(0);
   }

#ifdef DEBUG
   fprintf(stdout,"(DEBUG) FSTD_FieldReadLevels: found %i levels\n",Field->Def->NK);
#endif
   val=0.0;
   uvw=Data_VectorTableCheck(head->NOMVAR,NULL);

   /*Recuperer le data*/
   for(k=0;k<Field->Def->NK;k++) {
      idx=FSIZE2D(Field->Def)*tmp[k];
      Def_Pointer(Field->Def,0,idx,p);

      /*Recuperer les donnees du champs*/
      c_fst_data_length(TData_Size[Field->Def->Type]);
      if (cs_fstlukt(p,head->FID->Id,idxs[k],&tile,&ni,&nj,&idump)<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldReadLevels: Could not find read field data (c_fstluk failed)",(char*)NULL);
         FSTD_FileUnset(Interp,head->FID);
         return(0);
      }

      /*Recuperer le data seulement*/
      ok=cs_fstprm(idxs[k],&idump,&idump,&idump,&idump,&idump,&idump,&idump,
         &idump,&ip1,&idump,&idump,cdump,cdump,cdump,cdump,&idump,&idump,
         &idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump);

      /*Champs vectoriel ???*/
      if (uvw) {
         if (uvw->VV) {
            Def_Pointer(Field->Def,1,idx,p);
            ok=cs_fstinf(head->FID->Id,&ni,&nj,&idump,head->DATEV,head->ETIKET,ip1,head->IP2,head->IP3,head->TYPVAR,uvw->VV);
            c_fst_data_length(TData_Size[Field->Def->Type]);
            cs_fstlukt(p,head->FID->Id,ok,&tile,&ni,&nj,&idump);
         }
         if (uvw->WW) {
            Def_Pointer(Field->Def,2,idx,p);
            ok=cs_fstinf(head->FID->Id,&ni,&nj,&idump,head->DATEV,head->ETIKET,ip1,head->IP2,head->IP3,head->TYPVAR,uvw->WW);
            c_fst_data_length(TData_Size[Field->Def->Type]);
            cs_fstlukt(p,head->FID->Id,ok,&tile,&ni,&nj,&idump);
            if (uvw->WWFactor!=0.0) {
               for(i=0;i<FSIZE2D(Field->Def);i++) {
                  Def_Get(Field->Def,2,idx+i,val);
                  val*=uvw->WWFactor;
                  Def_Set(Field->Def,2,idx+i,val);
               }
            }
         }
      }

      /*Assigner le niveaux courant*/
      if (ip1==head->IP1) {
         Field->Def->Level=tmp[k];
      }
      levels[tmp[k]]=ZRef_IP2Level(ip1,&type);

      if (ok<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldReadLevels: Something really wrong here (c_fstprm failed)",(char*)NULL);
         FSTD_FileUnset(Interp,head->FID);
         return(0);
      }
   }

   type=type==LVL_SIGMA?LVL_ETA:type;
   ref=Field->Ref;

   if (tile) {
      grtyp[0]='Z';grtyp[1]='#';
   } else {
      grtyp[0]=ref->Grid[0];grtyp[1]=ref->Grid[1];
   }
   
   if (ref->Grid[0]=='W') {
       Field->Ref=GeoRef_WKTSetup(ni,nj,nk,type,levels,grtyp,ref->IG1,ref->IG2,ref->IG3,ref->IG4,ref->String,ref->Transform,ref->InvTransform,NULL);
   } else {
       Field->Ref=GeoRef_RPNSetup(ni,nj,nk,type,levels,grtyp,head->IG1,head->IG2,head->IG3,head->IG4,head->FID->Id);
   }
   Field->Ref->Grid[1]=ref->Grid[1];
   if (ref!=Field->Ref) GeoRef_Destroy(Interp,ref->Name);
   GeoRef_Qualify(Field->Ref);

   if (Field->Ref->NRef>1) {
      Data_Clean(Field,1,0,1);
   } else {
      Data_Clean(Field,1,1,1);
   }

   if (ref->Grid[0]=='U') {
      FSTD_FieldSubBuild(Field);
   }
   FSTD_FieldSubSelect(Field,k2);
   FSTD_FileUnset(Interp,head->FID);

   Data_GetStat(Field);

   return(1);
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

   FSTD_File    *file;
   FSTD_Head    *head=(FSTD_Head*)Field->Head;
   TDataVector  *uvw;
   int          ok=-1,ip1,datyp;
   unsigned long k,idx;
   void         *p;

   /*Verifier l'existence du champs*/
   if (!Field) {
      Tcl_AppendResult(Interp,"FSTD_FieldWrite: Invalid field",(char*)NULL);
      return(TCL_ERROR);
   }

   file=FSTD_FileGet(Interp,Id);
   if (FSTD_FileSet(Interp,file)<0)
      return(TCL_ERROR);

   // Check for DATYP and NBIT
   if (head->DATYP==-1) {
      switch(Field->Def->Type) {
         case TD_Binary:  head->DATYP=0;head->NBITS=1;break;
         case TD_UByte:   head->DATYP=2;head->NBITS=8;break;
         case TD_Byte:    head->DATYP=4;head->NBITS=8;break;
         case TD_UInt16:  head->DATYP=2;head->NBITS=16;break;
         case TD_Int16:   head->DATYP=4;head->NBITS=16;break;
         case TD_UInt32:  head->DATYP=2;head->NBITS=32;break;
         case TD_Int32:   head->DATYP=4;head->NBITS=32;break;
         case TD_UInt64:  head->DATYP=2;head->NBITS=64;break;
         case TD_Int64:   head->DATYP=4;head->NBITS=64;break;
         case TD_Float32: head->DATYP=1;head->NBITS=32;break;
         case TD_Float64: head->DATYP=5;head->NBITS=64;break;
         case TD_Unknown:
         default: return(TCL_ERROR);
      }     
   }
   
   head->FID=file;
   datyp=(NPack>0 && head->DATYP==1)?5:head->DATYP;
   NPack=NPack==0?-head->NBITS:(NPack>0?-NPack:NPack);

   /*Check for compression flag and adjust datyp accordingly*/
   if (Compress) {
      switch (head->DATYP) {
         case 2: datyp=130; break;
         case 4: datyp=132; break;
         case 5: datyp=133; break;
         case 1: datyp=134; break;
      }
//      if (head->NBITS==64 && (head->DATYP=1 || head->DATYP==5)) datyp=801;
   }

   EZLock_RPNField();

   for(k=0;k<Field->Def->NK;k++) {
      idx=k*FSIZE2D(Field->Def);

      /*If IP1 is set, use it otherwise, convert it from levels array*/
      if ((ip1=head->IP1)==-1 || Field->Def->NK>1) {
         ip1=ZRef_Level2IP(Field->Ref->ZRef.Levels[k],Field->Ref->ZRef.Type);
      }
      /*Inscription de l'enregistrement*/
      Def_Pointer(Field->Def,0,idx,p);
      c_fst_data_length(TData_Size[Field->Def->Type]);
      ok=c_fstecr(p,NULL,NPack,file->Id,head->DATEO,head->DEET,head->NPAS,Field->Def->NI,Field->Def->NJ,1,
                  ip1,head->IP2,head->IP3,head->TYPVAR,head->NOMVAR,head->ETIKET,
                  (Field->Ref?(Field->Ref->Grid[1]!='\0'?&Field->Ref->Grid[1]:Field->Ref->Grid):"X"),head->IG1,head->IG2,head->IG3,head->IG4,datyp,Rewrite);

      /*Inscription des champs complementaires*/
      if (Field->Def->Data[1]) {
         if ((uvw=Data_VectorTableCheck(head->NOMVAR,NULL))) {
            /*Inscription du champs complementaire 2D*/
            if (uvw->VV) {
               Def_Pointer(Field->Def,1,idx,p);
               c_fst_data_length(TData_Size[Field->Def->Type]);
               ok=c_fstecr(p,NULL,NPack,file->Id,head->DATEO,head->DEET,head->NPAS,Field->Def->NI,Field->Def->NJ,1,
                           ip1,head->IP2,head->IP3,head->TYPVAR,uvw->VV,head->ETIKET,
                           (Field->Ref?(Field->Ref->Grid[1]!='\0'?&Field->Ref->Grid[1]:Field->Ref->Grid):"X"),head->IG1,head->IG2,head->IG3,head->IG4,datyp,Rewrite);
            }
            /*Inscription du champs complementaire 3D*/
            if (Field->Def->Data[2] && uvw->WW) {
               Def_Pointer(Field->Def,2,idx,p);
               c_fst_data_length(TData_Size[Field->Def->Type]);
               ok=c_fstecr(p,NULL,NPack,file->Id,head->DATEO,head->DEET,head->NPAS,Field->Def->NI,Field->Def->NJ,1,
                           ip1,head->IP2,head->IP3,head->TYPVAR,uvw->WW,head->ETIKET,
                           (Field->Ref?(Field->Ref->Grid[1]!='\0'?&Field->Ref->Grid[1]:Field->Ref->Grid):"X"),head->IG1,head->IG2,head->IG3,head->IG4,datyp,Rewrite);
            }
         }
      }
   }
   EZUnLock_RPNField();

   FSTD_FileUnset(Interp,file);

   if (ok>=0){
      return(TCL_OK);
   } else {
      Tcl_AppendResult(Interp,"FSTD_FieldWrite: Could not write field (c_fstecr failed)",(char*)NULL);
      return(TCL_ERROR);
   }
}

#endif