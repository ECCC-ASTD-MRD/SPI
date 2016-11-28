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

#include "App.h"
#include "tclFSTD.h"
#include "tclGeoRef.h"
#include "Projection.h"
#include "ZRefInterp.h"

TCL_DECLARE_MUTEX(MUTEX_FSTDVI)

int      FSTD_UNTILE=0;
Tcl_Obj *FSTD_HIDELIST=NULL;

/*----------------------------------------------------------------------------
 * Nom      : <FSTD_TypeCheck>
 * Creation : Mars 2009 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Determiner le type (TDef_Type) de donnees RPN.
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
TDef_Type FSTD_TypeCheck(int Type,int Size) {

   switch(Type) {
//      case 0: Type=TD_Binary;                                                                          break;
      case 0: Type=Size>1?(Size>8?(Size>16?(Size>32?TD_UInt64:TD_UInt32):TD_UInt16):TD_UByte):TD_Binary; break;
      case 7: Type=TD_UByte;                                                                             break;
      case 3: 
      case 2: Type=Size<8?TD_UInt32:(Size>8?(Size>16?(Size>32?TD_UInt64:TD_UInt32):TD_UInt16):TD_UByte); break;
      case 4: Type=Size<8?TD_UInt32:(Size>8?(Size>16?(Size>32?TD_Int64:TD_Int32):TD_Int16):TD_Byte);     break;
      case 1:
      case 6:
      case 5:  Type=Size>32?TD_Float64:TD_Float32;                                                       break;
      default: App_Log(ERROR,"%s: Unknown data type (DATYP=%i)\n",__func__,Type);
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

   TRPNHeader *head=NULL;

   if (Data->Head && Data->Free && Data->Set!=FSTD_FieldSet)
      Data->Free(Data);

   if (!Data->Head) {
      head=(TRPNHeader*)malloc(sizeof(TRPNHeader));
   } else {
      head=(TRPNHeader*)Data->Head;
   }

   /*Initialiser les parametres de definition du champs*/
   if (head) {
      head->IG1=0;
      head->IG2=0;
      head->IG3=0;
      head->IG4=0;
      head->File=NULL;
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
   memcpy((TRPNHeader*)To,(TRPNHeader*)From,sizeof(TRPNHeader));
}

int FSTD_FieldSubBuild(TData *Field) {

   unsigned long long dij=0;
   int ni,nj,ig;
   char grtyp[2];
   int i,c;

   if (Field->Def) {

      // Allocate subgrid array ans set index 0 to supergrid
      if (!Field->SDef) {
         Field->SDef=(TDef**)calloc((Field->GRef->NbId+1),sizeof(TDef*));
      }
      if (Field->SDef) {
         Field->SDef[0]=Field->Def;

         // Loop on subgrids
         for(i=1;i<=Field->GRef->NbId;i++) {
            c_ezgprm(Field->GRef->Ids[i],grtyp,&ni,&nj,&ig,&ig,&ig,&ig);

            if (Field->SDef[i])
               Def_Free(Field->SDef[i]);

            // Allocate a container
            if ((Field->SDef[i]=Def_New(ni,nj,Field->Def->NK,-Field->Def->NC,Field->Def->Type))) {

               // Point to subgrid data within global data array
               for(c=0;c<Field->Def->NC;c++) {
                  Field->SDef[i]->Data[c]=&Field->Def->Data[c][dij*TDef_Size[Field->Def->Type]];
               }
               Field->SDef[i]->Idx=dij;
               Field->SDef[i]->NIJ=Field->Def->NI*Field->Def->NJ;
               Field->SDef[i]->Level=Field->Def->Level;
               Field->SDef[i]->Mode=Field->SDef[i]->Data[0];
               
              // Increment after global grid
               dij+=ni*nj;
            }
         }
      } else {
         App_Log(ERROR,"%s: Unable to allocate subgrid array\n",__func__);
         return(0);
      }
   }
   return(1);
}

int FSTD_FieldSubSelect(TData *Field,int N) {

   int ni,nj,ig;
   char grtyp[2];
   
   // If the subgrid index is different from thte current
   if (Field->GRef->Grid[0]=='U' && N!=Field->GRef->NId && N<=Field->GRef->NbId) {
      // Clean positionnal data
      Data_Clean(Field,1,0,1);

      Field->GRef->NId=N;

      // Point to subgrid data within global data array
      Field->Def=Field->SDef[N];

      // Define grid limits
      c_ezgprm(Field->GRef->Ids[N],grtyp,&ni,&nj,&ig,&ig,&ig,&ig);
      Field->GRef->X0=0;    Field->GRef->Y0=0;
      Field->GRef->X1=ni-1; Field->GRef->Y1=nj-1;

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
int FSTD_FieldReadComp(TRPNHeader *Head,float **Ptr,char *Var,int Grid,int Force) {

   int key=0,ni=0,nj=0,nk=0;

   if (Var && (!*Ptr || Force)) {
      if (Grid==1) {
         // Look for corresponding time and if not use any time
         if ((key==cs_fstinf(Head->File->Id,&ni,&nj,&nk,Head->DATEV,"",Head->IG1,Head->IG2,Head->IG3,"",Var))<=0) {
            key=cs_fstinf(Head->File->Id,&ni,&nj,&nk,-1,"",Head->IG1,Head->IG2,Head->IG3,"",Var);
         }
      } else if (Grid==0) {
         key=cs_fstinf(Head->File->Id,&ni,&nj,&nk,Head->DATEV,Head->ETIKET,Head->IP1,Head->IP2,Head->IP3,Head->TYPVAR,Var);
      } else {
         key=cs_fstinf(Head->File->Id,&ni,&nj,&nk,Head->DATEV,Head->ETIKET,-1,Head->IP2,Head->IP3,Head->TYPVAR,Var);
      }

      if (key<0) {
         // Too many warnings so we catch it later
         // App_Log(WARNING,"%s: Could not find component field %s (c_fstinf failed)\n",__func__,Var);
         return(0);
      } else {
         if (!*Ptr) {
            if (!(*Ptr=(float*)malloc(ni*nj*nk*sizeof(float)))) {
               App_Log(ERROR,"%s: Not enough memory to read coordinates fields\n",__func__);
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

   if (Field->ZRef->Levels)
      free(Field->ZRef->Levels);

   Field->ZRef->Levels=NULL;
   Field->ZRef->LevelNb=FSTD_FieldReadComp((TRPNHeader*)Field->Head,&Field->ZRef->Levels,Field->Spec->Extrude,1,0);

   // If we don't find any level definition, use level index
   if (!Field->ZRef->Levels) {
      if (!(Field->ZRef->Levels=(float*)malloc(Field->Def->NJ*sizeof(float)))) {
         App_Log(ERROR,"%s: Not enough memory to read coordinates fields\n",__func__);
      } else {
         for(nj=0;nj<Field->Def->NJ;nj++) {
            Field->ZRef->Levels[nj]=nj+1;
         }
         Field->ZRef->LevelNb=Field->Def->NJ;
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

   TRPNHeader *head=(TRPNHeader*)Field->Head;
   int         key,ni,nj,nk,nijk=0;

   // !! -> NI==3 ### Field->Def->NI<4 || 
   if (!Field->GRef || !(Field->GRef->Type&(GRID_SPARSE|GRID_VARIABLE|GRID_VERTICAL)) || (Field->GRef->NY==1 && Field->GRef->Grid[0]!='Y' && Field->GRef->Grid[1]!='Y' && Field->GRef->Grid[0]!='M'))
      return(0);

   if ((!Field->GRef->AY || !Field->GRef->AX) && head->File) {
      if (FSTD_FileSet(NULL,head->File)<0) {
         return(0);
      }

      switch(Field->GRef->Grid[0]) {
         case 'M':
            if (!Field->GRef->AY) FSTD_FieldReadComp(head,&Field->GRef->AY,"^^",1,0);
            if (!Field->GRef->AX) nijk=FSTD_FieldReadComp(head,&Field->GRef->AX,">>",1,0);

            /* Lire le champs d'indexes*/
            if (!Field->GRef->Idx) {
               key=cs_fstinf(head->File->Id,&ni,&nj,&nk,-1,"",head->IG1,head->IG2,head->IG3,"","##");
               if (key < 0) {
                  App_Log(ERROR,"%s: Could not find index field %s (c_fstinf failed)",__func__,"##");
                  FSTD_FileUnset(NULL,head->File);
                  return(0);
               } else {
                  Field->GRef->NIdx=ni*nj*nk;
                  if (!(Field->GRef->Idx=(unsigned int*)malloc(Field->GRef->NIdx*sizeof(unsigned int)))) {
                     App_Log(ERROR,"%s: Not enough memory to read coordinates fields",__func__);
                     FSTD_FileUnset(NULL,head->File);
                     return(0);
                  }
                  cs_fstluk((float*)Field->GRef->Idx,key,&ni,&nj,&nk);
               }
               GeoRef_BuildIndex(Field->GRef);
            }
            break;

         case 'W':
            if (Field->GRef->Grid[1]=='X' || Field->GRef->Grid[1]=='Y' || Field->GRef->Grid[1]=='Z') {
               if (!Field->GRef->AY) FSTD_FieldReadComp(head,&Field->GRef->AY,"^^",1,0);
               if (!Field->GRef->AX) nijk=FSTD_FieldReadComp(head,&Field->GRef->AX,">>",1,0);
            }
            
            if (Field->GRef->Grid[1]=='Y') {
               if (!Field->GRef->AY) FSTD_FieldReadComp(head,&Field->GRef->AY,"LA",0,0);
               if (!Field->GRef->AX) nijk=FSTD_FieldReadComp(head,&Field->GRef->AX,"LO",0,0);
               if (!Field->GRef->Hgt) FSTD_FieldReadComp(head,&Field->GRef->Hgt,"ZH",0,0);
            }
            break;

         case 'Y':
            if (!Field->GRef->AY) FSTD_FieldReadComp(head,&Field->GRef->AY,"LA",0,0);
            if (!Field->GRef->AY) FSTD_FieldReadComp(head,&Field->GRef->AY,"^^",1,0);
            if (!Field->GRef->AX) nijk=FSTD_FieldReadComp(head,&Field->GRef->AX,"LO",0,0);
            if (!Field->GRef->AX) nijk=FSTD_FieldReadComp(head,&Field->GRef->AX,">>",1,0);
            if (!Field->GRef->Hgt) FSTD_FieldReadComp(head,&Field->GRef->Hgt,"ZH",0,0);
            break;

         case 'X':
            if (!Field->GRef->AY) FSTD_FieldReadComp(head,&Field->GRef->AY,"^^",1,0);
            if (!Field->GRef->AX) nijk=FSTD_FieldReadComp(head,&Field->GRef->AX,">>",1,0);
            GeoRef_BuildIndex(Field->GRef);          
            break;

          case 'V':
            if (!Field->GRef->AY) FSTD_FieldReadComp(head,&Field->GRef->AY,"^^",1,0);
            if (!Field->GRef->AX) nijk=FSTD_FieldReadComp(head,&Field->GRef->AX,">>",1,0);
            FSTD_FieldReadVLevels(Field);
            break;
      }
      FSTD_FileUnset(NULL,head->File);     

      // Need to re-qualify to check AX order
      GeoRef_Qualify(Field->GRef);
   }

   return(Field->GRef->AY && Field->GRef->AX);
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

   TRPNHeader   *head=(TRPNHeader*)Field->Head;
   Coord         coord;
   int           i,j,k;
   unsigned long idx;
   double        z;
   float        *gz=NULL;

   if (!FSTD_FieldReadMesh(Field)) {
      App_Log(WARNING,"%s: Could not find grid definition components\n",__func__);
      return(NULL);
   }

   // Allocate memory for gridpoint positions
   if (!Field->GPos)
      Field->GPos=GeoPos_Find(Field->GRef,Field->ZRef);

   if (!Field->GPos)
      return(NULL);

   if (!Field->GPos->Pos[Level]) {
      Field->GPos->Pos[Level]=(Vect3d*)malloc(FSIZE2D(Field->Def)*sizeof(Vect3d));
      if (!Field->GPos->Pos[Level]) {
         App_Log(ERROR,"%s: Not enough memory to calculate gridpoint location\n",__func__);
         return(NULL);
      }
   }

   if (Field->Spec->Topo && head->File) {
      if (FSTD_FileSet(NULL,head->File)<0) {
         return(NULL);
      }
      RPN_FieldLock();
      idx=c_fstinf(head->File->Id,&i,&j,&k,head->DATEV,head->ETIKET,head->IP1,head->IP2,head->IP3,head->TYPVAR,Field->Spec->Topo);
      if (idx<0) {
         App_Log(WARNING,"%s: Could not load corresponding topo field, trying for any (%s)\n",__func__,Field->Spec->Topo);
         idx=c_fstinf(head->File->Id,&i,&j,&k,-1,"",-1,-1,-1,"",Field->Spec->Topo);
      }
      if (idx<0) {
         App_Log(WARNING,"%s: Could not load corresponding modulator (%s)\n",__func__,Field->Spec->Topo);
      } else {
         if (!gz) gz=(float*)malloc(i*j*k*sizeof(float));
         if (gz)  c_fstluk(gz,idx,&i,&j,&k);
      }
      RPN_FieldUnlock();
      FSTD_FileUnset(NULL,head->File);
   }

   // Precalculer les tableaux de particules dans l'espace
   if (Field->GRef->AY && Field->GRef->AX) {
      z=ZRef_Level2Meter(Field->ZRef->Levels[Level],Field->ZRef->Type);
      for (i=0;i<Field->Def->NI;i++) {
         for (j=0;j<Field->Def->NJ;j++) {

            idx=j*Field->Def->NI+i;
            coord.Elev=0.0;
            if (gz) coord.Elev=gz[idx];

            // Reproject coordinates if needed
            if (Field->GRef->Grid[1]=='Z') {
               Field->GRef->Project(Field->GRef,i,j,&coord.Lat,&coord.Lon,1,1);
            } else if (Field->GRef->Grid[1]=='Y') {
               Field->GRef->Project(Field->GRef,i,j,&coord.Lat,&coord.Lon,1,1);
            } else {
               coord.Lat=Field->GRef->AY[idx];
               coord.Lon=Field->GRef->AX[idx];
            }

            if (Field->GRef->Hgt) {
               coord.Elev+=ZRef_Level2Meter(Field->GRef->Hgt[idx],Field->ZRef->Type);
            } else {
               coord.Elev+=z;
            }
            coord.Elev*=Field->Spec->TopoFactor;

            // Si les positions sont hors domaine, outter space
            if (coord.Lat<-900.0 || coord.Lon<-900.0) {
               coord.Elev=1e32;
            }
            Vect_Init(Field->GPos->Pos[Level][idx],coord.Lon,coord.Lat,coord.Elev);
         }
      }
      Proj->Type->Project(Proj,(GeoVect*)Field->GPos->Pos[Level],NULL,FSIZE2D(Field->Def));
   }

   if (gz)
      free(gz);

   return(Field->GPos->Pos);
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

   TRPNHeader *head=(TRPNHeader*)Field->Head;
   TDef       *def;
   Coord       coord;
   float      *lat=NULL,*lon=NULL,*gz=NULL,flat,flon,fele;
   int         i,j,idx,ni,nj,nk,ip1;
   int         idxi;
   char        tile;

   def=Field->SDef?Field->SDef[0]:Field->Def;

   // Verifier la validite de la grille
   if (!Field->GRef)
      return(NULL);

  // TODO: Verifier la validite de grille non geographique*
//   if (Field->GRef->Grid[0]=='X' && ((Projection*)Proj)->Type->Def!=PROJPLANE)
//      return(NULL);

   if (Field->GPos && Field->GPos->Pos[Level])
      return(Field->GPos->Pos[Level]);

   if (Field->GRef->Type&GRID_SPARSE) {
      FSTD_FieldGetMesh(Field,Proj,Level);
      return(Field->GPos->Pos[Level]);
   }

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

   // Check for auto untile
   tile=(Field->GRef->Grid[1]=='#')&&FSTD_UNTILE?'#':0;

   if (Field->GRef->Grid[0]=='V') {
      FSTD_FieldReadMesh(Field);

      if (!Field->GRef->AY || !Field->GRef->AX) {
         App_Log(ERROR,"%s: Section coordinates not defined",__func__);
         return(NULL);
      }

      FSTD_FileSet(NULL,head->File);

      for (j=0;j<def->NJ;j++) {

         /*Essayer de recuperer le modulateur (GZ)*/
         if (head->File && Field->Spec->Topo) {
            ip1=ZRef_Level2IP(Field->ZRef->Levels[j],Field->ZRef->Type,DEFAULT);
            idx=cs_fstinf(head->File->Id,&ni,&nj,&nk,head->DATEV,head->ETIKET,ip1,head->IP2,head->IP3,head->TYPVAR,Field->Spec->Topo);
            if (idx<0) {
               if (gz) { free(gz); gz=NULL; };
               App_Log(WARNING,"%s:d: Could not load corresponding modulator (%s) (%f(%i)), using constant pressure\n",__func__,Field->Spec->Topo,Field->ZRef->Levels[j],ip1);
            } else {
               if (!gz) gz=(float*)malloc(ni*nj*nk*sizeof(float));
               if (gz)  cs_fstluk(gz,idx,&ni,&nj,&nk);
            }
         }

         coord.Elev=ZRef_Level2Meter(Field->ZRef->Levels[j],Field->ZRef->Type)*Field->Spec->TopoFactor;
         for (i=0;i<def->NI;i++) {
            flat=coord.Lat=Field->GRef->AY[i];
            flon=coord.Lon=CLAMPLON(Field->GRef->AX[i]);
            idx=j*def->NI+i;
            if (gz && Field->GRef->RefFrom && Field->GRef->RefFrom->Ids[0]>-1) {
//               RPN_IntLock();
               c_gdllsval(Field->GRef->RefFrom->Ids[0],&fele,gz,&flat,&flon,1);
//               RPN_IntUnlock();
               coord.Elev=fele*10.0*Field->Spec->TopoFactor;
            }
            Vect_Init(Field->GPos->Pos[Level][idx],Field->GRef->AX[i],Field->GRef->AY[i],coord.Elev);
         }
      }

      if (Proj) {
         ((Projection*)Proj)->Type->Project(((Projection*)Proj),(GeoVect*)Field->GPos->Pos[Level],NULL,FSIZE2D(def));
      }
      FSTD_FileUnset(NULL,head->File);
   } else {
      if (Field->GRef->Ids && Field->GRef->Ids[0]>-1) {
         // Recuperer les coordonnees des points de grille
         lat=(float*)malloc(FSIZE2D(def)*sizeof(float));
         lon=(float*)malloc(FSIZE2D(def)*sizeof(float));

         if (!lat || !lon) {
            App_Log(ERROR,"%s: Not enough memory to process gridpoint location",__func__);
            free(Field->GPos->Pos[Level]);
            return(Field->GPos->Pos[Level]=NULL);
         }
//         RPN_IntLock();
         c_gdll(Field->GRef->Ids[0],lat,lon);
//         RPN_IntUnlock();
      }

      // Essayer de recuperer le GZ
      if (Field->Spec->Topo && head->File && FSTD_FileSet(NULL,head->File)>=0) {

         ip1=ZRef_Level2IP(Field->ZRef->Levels[Level],Field->ZRef->Type,DEFAULT);
         if (Field->Spec->Topo) {
            idx=cs_fstinf(head->File->Id,&ni,&nj,&nk,head->DATEV,head->ETIKET,ip1,head->IP2,head->IP3,head->TYPVAR,Field->Spec->Topo);
            if (idx<0) {
               App_Log(WARNING,"%s: Could not load corresponding topo field, trying for any (%s)\n",__func__,Field->Spec->Topo);
               idx=cs_fstinf(head->File->Id,&ni,&nj,&nk,-1,"",-1,-1,-1,"",Field->Spec->Topo);
            }
            if (!tile && (ni!=def->NI || nj!=def->NJ)) {
               idx=-1;
               }
         } else {
            idx=-1;
         }
         if (idx<0) {
            if (gz) { free(gz); gz=NULL; };
            App_Log(WARNING,"%s: Could not load corresponding (%s) (%f(%i)), using constant pressure\n",__func__,Field->Spec->Topo,Field->ZRef->Levels[Level],ip1);
         } else {
            if (!gz) gz=(float*)malloc(def->NI*def->NJ*nk*sizeof(float));
            if (gz)  cs_fstlukt(gz,head->File->Id,idx,&tile,&ni,&nj,&nk);
         }
         FSTD_FileUnset(NULL,head->File);
      }

      coord.Elev=ZRef_Level2Meter(Field->ZRef->Levels[Level],Field->ZRef->Type)*Field->Spec->TopoFactor;
      // For every gridpoints
      for (j=0;j<def->NJ;j++) {
         for (i=0;i<def->NI;i++) {

            // Figure out table plane indexes
            idxi=j*def->NI+i;

            // Get height from topographic field
            if (gz) {
               coord.Elev=gz[idxi]*Field->Spec->TopoFactor;
               if (Field->Spec->Topo[0]=='G' && Field->Spec->Topo[1]=='Z' ) {
                  coord.Elev*=10.0;
               }
            }

            if (((Projection*)Proj)->Type->Def==PROJPLANE && !Field->GRef->Ids) {
               Vect_Init(Field->GPos->Pos[Level][idxi],i,j,coord.Elev);
            } else if (((Projection*)Proj)->Type->Def==PROJPLANE && ((Projection*)Proj)->Ref && ((Projection*)Proj)->Ref->Ids && ((Projection*)Proj)->Ref->Ids[0]==Field->GRef->Ids[0]) {
               Vect_Init(Field->GPos->Pos[Level][idxi],i,j,coord.Elev);
            } else {
              if (Field->GRef->Ids && Field->GRef->Ids[0]>-1) {
                   // Fix for G grids which seems to have inverted lat on IG2=1
                  coord.Lat=(Field->GRef->Grid[0]=='G' && head->IG2==1)?-lat[idxi]:lat[idxi];
                  coord.Lon=CLAMPLON(lon[idxi]);
               } else {
                  Field->GRef->Project(Field->GRef,i,j,&coord.Lat,&coord.Lon,0,1);
               }
               Vect_Init(Field->GPos->Pos[Level][idxi],coord.Lon,coord.Lat,coord.Elev);
            }
         }
      }

      if (((Projection*)Proj)->Type->Def==PROJPLANE && !Field->GRef->Ids) {
         FSTD_Project(((Projection*)Proj),Field->GPos->Pos[Level],FSIZE2D(def));
      } else if (((Projection*)Proj)->Type->Def==PROJPLANE && ((Projection*)Proj)->Ref && ((Projection*)Proj)->Ref->Ids && ((Projection*)Proj)->Ref->Ids[0]==Field->GRef->Ids[0]) {
         FSTD_Project(((Projection*)Proj),Field->GPos->Pos[Level],FSIZE2D(def));
      } else {
         ((Projection*)Proj)->Type->Project(((Projection*)Proj),(GeoVect*)Field->GPos->Pos[Level],NULL,FSIZE2D(def));
      }

      if (Field->GRef->Ids && Field->GRef->Ids[0]>-1) {
         free(lat);
         free(lon);
      }
   }

   if (gz) free(gz);

   return(Field->GPos->Pos[Level]);
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

   TRPNFile *fid;
   int       i=1;

   if (Field->ZRef->Type==LVL_UNDEF || Field->ZRef->Type==LVL_SIGMA || Field->ZRef->Type==LVL_HYBRID || Field->ZRef->Type==LVL_ETA) {
      if (Field->ZRef->Version==-1) {
         if ((fid=((TRPNHeader*)Field->Head)->File)) {
            i=0;

            if (FSTD_FileSet(NULL,fid)<0)
               return(i);

            i=ZRef_DecodeRPN(Field->ZRef,fid->Id);

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
int FSTD_FieldVertInterpolate(Tcl_Interp *Interp,TData *FieldTo,TData *FieldFrom,TData *ZFieldTo,TData *ZFieldFrom,char **Index) {

   char        *from,*to;
   int          i,ip1;
   TZRefInterp *interp;
   TRPNHeader  *headto=(TRPNHeader*)FieldTo->Head;
   TRPNHeader  *headfrom=(TRPNHeader*)FieldFrom->Head;

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

   if (!Index || Index[0]==(char*)0x1) {

      if (FieldFrom->ZRef->Type==LVL_GALCHEN || FieldFrom->ZRef->Type==LVL_MASL || FieldFrom->ZRef->Type==LVL_MAGL ||
         FieldTo->ZRef->Type==LVL_GALCHEN || FieldTo->ZRef->Type==LVL_MASL ||  FieldTo->ZRef->Type==LVL_MAGL) {
         if (!ZFieldFrom && FieldFrom->ZRef->Type!=LVL_MASL) {
            Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid GZ source field data",(char*)NULL);
            return(TCL_ERROR);
         }
         if (!ZFieldTo && FieldTo->ZRef->Type!=LVL_MASL) {
            Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid GZ destination field data",(char*)NULL);
            return(TCL_ERROR);
         }
      } else {
         if (!ZFieldFrom && FieldFrom->ZRef->Type!=LVL_PRES) {
            Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid P0 source field data",(char*)NULL);
            return(TCL_ERROR);
         }
         if (!ZFieldTo && FieldTo->ZRef->Type!=LVL_PRES) {
            Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid P0 destination field data",(char*)NULL);
            return(TCL_ERROR);
         }
      }

      // Recuperer tout les niveaux disponibles
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

      // Decode RPN specific params
      FSTD_DecodeRPNLevelParams(FieldFrom);

      if (FieldFrom->ZRef->Type==LVL_UNDEF && ZFieldFrom->Def->NK==1) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid vertical dimension for source Z field",(char*)NULL);
         Tcl_MutexUnlock(&MUTEX_FSTDVI);
         return(TCL_ERROR);
      }
      if (FieldTo->ZRef->Type==LVL_UNDEF && ZFieldTo->Def->NK==1) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: Invalid vertical dimension for destination Z field",(char*)NULL);
         Tcl_MutexUnlock(&MUTEX_FSTDVI);
         return(TCL_ERROR);
      }

      FieldFrom->ZRef->P0=ZFieldFrom?(float*)(ZFieldFrom->Def->Data[0]):NULL;
      FieldTo->ZRef->P0=ZFieldTo?(float*)(ZFieldTo->Def->Data[0]):NULL;

      if (!FSTD_DecodeRPNLevelParams(FieldTo)) {
         Tcl_AppendResult(Interp,"FSTD_FieldVertInterpolate: (WARNING) Could not find destination hybrid definition field HY",(char*)NULL);
      }

      // Initialize verticap interpolator
      Tcl_MutexLock(&MUTEX_FSTDVI);
      ZRefInterp_SetOption("INTERP_DEGREE",FieldTo->Spec->InterpDegree);
      ZRefInterp_SetOption("VERBOSE","NO");
      
      if (!(interp=ZRefInterp_Define(FieldTo->ZRef,FieldFrom->ZRef,FieldFrom->Def->NI,FieldFrom->Def->NJ))) {
         Tcl_AppendResult(Interp,"Unable to initialize vertical dataset (ZRefInterp_Define)\n");
         Tcl_MutexUnlock(&MUTEX_FSTDVI);
         return(TCL_ERROR);
      }
   } else {
      Tcl_MutexLock(&MUTEX_FSTDVI);
      interp=(TZRefInterp*)Index[0];
   }
   
   // Inter ET/OU Extrapolation 
   for(i=0;i<3;i++) {
      if (FieldFrom->Def->Data[i]) {
         if (!FieldTo->Def->Data[i]) {
            FieldTo->Def->Data[i]=(char*)calloc(FSIZE3D(FieldTo->Def),TDef_Size[FieldTo->Def->Type]);
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
   
   if (Index) {
      // A variable was provided for the index
      if (Index[0]==(char*)0x1) Index[0]=(char*)interp;
   } else {
      // Free the index
      ZRefInterp_Free(interp);
   }

   FieldFrom->ZRef->P0=NULL;
   FieldTo->ZRef->P0=NULL;

   if (FieldTo->GRef->NbId!=FieldFrom->GRef->NbId) {
      FieldTo->GRef->Ids=(int*)realloc(FieldTo->GRef->Ids,FieldFrom->GRef->NbId*sizeof(int));
   }

   if (FieldTo->GRef->Ids && FieldTo->GRef->Ids[FieldTo->GRef->NId]!=FieldFrom->GRef->Ids[FieldFrom->GRef->NId]) {
      memcpy(FieldTo->GRef->Ids,FieldFrom->GRef->Ids,FieldFrom->GRef->NbId*sizeof(int));
      FieldTo->GRef->NId=FieldFrom->GRef->NId;
      FieldTo->GRef->Grid[0]=FieldFrom->GRef->Grid[0];
      FieldTo->GRef->Project=FieldFrom->GRef->Project;
      FieldTo->GRef->UnProject=FieldFrom->GRef->UnProject;
      FieldTo->GRef->Value=FieldFrom->GRef->Value;
      FieldTo->GRef->Type=FieldFrom->GRef->Type;
   }

   ip1=((TRPNHeader*)FieldTo->Head)->IP1;
   memcpy(headto,headfrom,sizeof(TRPNHeader));
   ((TRPNHeader*)FieldTo->Head)->IP1=ip1;

   if (FieldTo->Stat) {
      Data_StatFree(FieldTo->Stat);
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

   TRPNHeader *headto=(TRPNHeader*)FieldTo->Head;
   TRPNHeader *headfrom=(TRPNHeader*)FieldFrom->Head;

   /*Initialiaser les valeurs de descriptions du champs destination*/
   if (FieldTo->Stat) {
      Data_StatFree(FieldTo->Stat);
      FieldTo->Stat=NULL;
   }

   headto->File=headfrom->File;
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
 *  <Mode>     : Mode d'interpolation (0=NEAREST, 1=LINEAR, 2=CUBIC, autre=INTERPDEGREE du champs FieldTo)
 *
 * Retour:
 *  <TCL_...> : Code d'erreur de TCL.
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int FSTD_FieldGridInterpolate(Tcl_Interp *Interp,TData *FieldTo,TData *FieldFrom,int Mode,float *Index){

   double     val,dir,lat,lon,di,dj,dval;
   int        ezto=1,ezfrom=1,ok=-1,idx,i,j,k,gotidx,msk;
   void      *pf0,*pt0,*pf1,*pt1;
   float     *ip=NULL;
   
   if (!FieldFrom || !FieldFrom->GRef) {
      Tcl_AppendResult(Interp,"FSTD_FieldGridInterpolate: Origin field not valid",(char*)NULL);
      return(TCL_ERROR);
   }
   if (!FieldTo || !FieldTo->GRef) {
      Tcl_AppendResult(Interp,"FSTD_FieldGridInterpolate: Destination field not valid",(char*)NULL);
      return(TCL_ERROR);
   }

   // Verifier la compatibilite entre source et destination
   Def_Compat(FieldTo->Def,FieldFrom->Def);
   ZRef_Free(FieldTo->ZRef);
   FieldTo->ZRef=ZRef_Define(FieldFrom->ZRef->Type,FieldFrom->ZRef->LevelNb,FieldFrom->ZRef->Levels);
   msk=(((TRPNHeader*)FieldFrom->Head)->TYPVAR[0]=='@' && ((TRPNHeader*)FieldFrom->Head)->TYPVAR[1]=='@');

   // Checl for ezscint capability
   if (FieldFrom->Def->Type!=TD_Float32) {
      ezfrom=0;
   }

   if (FieldFrom->GRef->Grid[0]=='R' || FieldFrom->GRef->Grid[0]=='W' || FieldFrom->GRef->Grid[0]=='X' || FieldFrom->GRef->Grid[0]=='M') {
      ezfrom=0;
   }

   if (FieldTo->GRef->Grid[0]=='R' || FieldTo->GRef->Grid[0]=='W' || FieldTo->GRef->Grid[0]=='X' || FieldTo->GRef->Grid[0]=='Y' || FieldTo->GRef->Grid[0]=='M' || FieldTo->GRef->Hgt) {
      ezto=0;
   }

   if (FieldFrom->GRef->Grid[0]!='R' && FieldTo->GRef->Grid[0]!='R') {
      FSTD_FieldSetTo(FieldTo,FieldFrom);
   }
   
   if (Mode==0) {
      c_ezsetopt("INTERP_DEGREE","NEAREST");
   } else if (Mode==1) {
      c_ezsetopt("INTERP_DEGREE","LINEAR");
   } else if (Mode==2) {
      c_ezsetopt("INTERP_DEGREE","CUBIC");
   } else {
      c_ezsetopt("INTERP_DEGREE",(char*)FieldTo->Spec->InterpDegree);
   }
   if (FieldTo->Spec->ExtrapDegree[0]=='V') {
      c_ezsetval("EXTRAP_VALUE",FieldTo->Def->NoData);
   }
   c_ezsetopt("EXTRAP_DEGREE",(char*)FieldTo->Spec->ExtrapDegree);
   
   // Use ezscint
   if (ezto && ezfrom) {
      RPN_IntLock();

      ok=c_ezdefset(FieldTo->GRef->Ids[FieldTo->GRef->NId],FieldFrom->GRef->Ids[FieldFrom->GRef->NId]);

      if (ok<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldGridInterpolate: EZSCINT internal error, could not define gridset",(char*)NULL);
         RPN_IntUnlock();
         return(TCL_ERROR);
      }

      for(k=0;k<FieldTo->Def->NK;k++) {
         // Effectuer l'interpolation selon le type de champs
         if (FieldTo->Def->Data[1]) {
            /*Interpolation vectorielle*/
            Def_Pointer(FieldTo->Def,0,k*FSIZE2D(FieldTo->Def),pt0);
            Def_Pointer(FieldFrom->Def,0,k*FSIZE2D(FieldFrom->Def),pf0);
            Def_Pointer(FieldTo->Def,1,k*FSIZE2D(FieldTo->Def),pt1);
            Def_Pointer(FieldFrom->Def,1,k*FSIZE2D(FieldFrom->Def),pf1);

            // In case of Y grid, get the speed and dir instead of wind components
            // since grid oriented components dont mean much
            if (FieldTo->GRef->Grid[0]=='Y') {
               ok=c_ezwdint(pt0,pt1,pf0,pf1);
            } else {
               ok=c_ezuvint(pt0,pt1,pf0,pf1);
           }
        } else{
            // Interpolation scalaire
            Def_Pointer(FieldTo->Def,0,k*FSIZE2D(FieldTo->Def),pt0);
            Def_Pointer(FieldFrom->Def,0,k*FSIZE2D(FieldFrom->Def),pf0);
            ok=c_ezsint(pt0,pf0);
        }
        FieldTo->ZRef->Levels[k]=FieldFrom->ZRef->Levels[k];
      }
      RPN_IntUnlock();
      if (ok<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldGridInterpolate: EZSCINT internal error, interpolation problem",(char*)NULL);
         return(TCL_ERROR);
      }
  } else { 
      idx=0;  
            
      for(k=0;k<FieldTo->Def->NK;k++) {
         ip=Index;
         gotidx=(Index && Index[0]!=DEF_INDEX_EMPTY);

         for(j=0;j<FieldTo->Def->NJ;j++) {
            for(i=0;i<FieldTo->Def->NI;i++,idx++) {

               if (gotidx) {

                  // Got the index, use coordinates from it
                  di=*(ip++);
                  dj=*(ip++);

//                  if (di>=0.0 && !FIN2D(FieldFrom->Def,di,dj)) {
//                     App_Log(ERROR,"%s: Wrong index, index coordinates (%f,%f)\n",__func__,di,dj);
//                     return(TCL_ERROR);
//                  }
               } else {

                  // No index, project coordinate and store in index if provided
                  FieldTo->GRef->Project(FieldTo->GRef,i,j,&lat,&lon,0,1);
                  ok=FieldFrom->GRef->UnProject(FieldFrom->GRef,&di,&dj,lat,lon,0,1);
                  if (ip) {
                     *(ip++)=di;
                     *(ip++)=dj;
                  }
               }
               if (di>=0.0 && FieldFrom->GRef->Value(FieldFrom->GRef,FieldFrom->Def,Mode==0?'N':'L',0,di,dj,k,&val,&dir)) {
                  if (FieldTo->Def->Data[1]) {
                     // Have to reproject vector
                     dir=DEG2RAD(dir)+GeoRef_GeoDir(FieldTo->GRef,i,j);
                     dval=msk?val!=0.0:-val*sin(dir);
                     Def_Set(FieldTo->Def,0,idx,dval);
                     dval=msk?val!=0.0:-val*cos(dir);
                     Def_Set(FieldTo->Def,1,idx,dval); 
                  } else {
                     Def_Set(FieldTo->Def,0,idx,val);
                  } 
               } else {  
                  Def_Set(FieldTo->Def,0,idx,FieldTo->Def->NoData);
                  if (FieldTo->Def->Data[1]) {
                     Def_Set(FieldTo->Def,1,idx,FieldTo->Def->NoData); 
                  }
               } 
            }
         }
         
         // Mark end of index
//         if (!gotidx && ip) *(ip++)=DEF_INDEX_END;
      }
   }

   // In case of vectorial field, we have to recalculate the module
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
   TRPNHeader *head0=(TRPNHeader*)Field0->Head;
   TRPNHeader *head1=(TRPNHeader*)Field1->Head;

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
      Data_StatFree(field->Stat);
      field->Stat=NULL;
   }

   memcpy(field->Head,head0,sizeof(TRPNHeader));
   field->Spec->Map=Field0->Spec->Map;

   /*Modifier les parametres de dates*/
   head0=(TRPNHeader*)field->Head;
   head0->DATEV=Stamp;
   head0->DATEO=Stamp;
   head0->NPAS=0;
   head0->DEET=0;
   head0->IP2=0;

   if (field->GRef)
      GeoRef_Destroy(Interp,field->GRef->Name);
   field->GRef=GeoRef_Copy(Field0->GRef);

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
   TRPNHeader   *head=(TRPNHeader*)Field->Head;
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

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",TCL_EXACT,&idx)!=TCL_OK) {
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
               if (head->File) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(head->File->CId,-1));
               }
            } else {
               head->File=FSTD_FileGet(Interp,Tcl_GetString(Objv[++i]));
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
               Field->ZRef->Levels[Field->Def->Level]=ZRef_IP2Level(head->IP1,&Field->ZRef->Type);
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
            if (Objc==1 && Field->GRef) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(Field->GRef->NId));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&nidx);
               if (Field->GRef->NbId>1) {
                  if (nidx>Field->GRef->NbId) {
                     Tcl_AppendResult(Interp,"\n   FSTD_FieldDefine: Invalid subgrid index",(char*)NULL);
                     return(TCL_ERROR);
                  } else {
                     FSTD_FieldSubSelect(Field,nidx);
                  }
               }
            }
            break;

         case GRIDID:
            if (Objc==1 && Field->GRef) {
               obj=Tcl_NewListObj(0,NULL);
               for(nidx=0;nidx<Field->GRef->NbId;nidx++) {
                  Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(Field->GRef->Ids[nidx]));
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
            }
            break;

         case PROJECTION:
            if (Objc==1) {
               if (Field->GRef && Field->GRef->String)
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->GRef->String,-1));
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
                  ref=NULL;
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
                  App_Log(WARNING,"%s: Unable to generate the inverse transform matrix\n",__func__);
                  im=NULL;
               } else {
                  im=inv;
               }
               if (!Field->GRef || !Field->GRef->Transform || memcmp(tm,Field->GRef->Transform,6*sizeof(double))!=0) {
                  ref=Field->GRef;
                  if (ref) {
                     Field->GRef=GeoRef_Find(GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,ref->Grid,ref->IG1,ref->IG2,ref->IG3,ref->IG4,ref->String,tm,im,NULL));
                     GeoRef_Destroy(Interp,ref->Name);
                  } else {
                     Field->GRef=GeoRef_Find(GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,NULL,0,0,0,0,NULL,tm,im,NULL));
                  }
                  ref=NULL;
                  Data_Clean(Field,1,1,1);
               }
            }
            break;

         case GEOREF:
            if (Objc==1) {
               if (Field->GRef) {
                  Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->GRef->Name,-1));
                  GeoRef_Incr(Field->GRef);
               }
            } else {
               ref=GeoRef_Get(Tcl_GetString(Objv[++i]));
               if (!ref) {
                  Tcl_AppendResult(Interp,"\n   FSTD_FieldDefine: Georef name unknown: \"",Tcl_GetString(Objv[i]),"\"",(char *)NULL);
                  return TCL_ERROR;
               }
               if (Field->GRef && ref!=Field->GRef) {
                  GeoRef_Destroy(Interp,Field->GRef->Name);
                  Data_Clean(Field,1,1,1);
               }
               Field->GRef=ref;
               ref=NULL;
               GeoRef_Incr(Field->GRef);
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

               if (!GeoRef_Positional(Field->GRef,fieldAX->Def,fieldAY->Def)) {
                  Tcl_AppendResult(Interp,"unable to initialize positional data",(char*)NULL);
                  return(TCL_ERROR);
               }

               if (Field->GRef->Grid[0]=='Z' || Field->GRef->Grid[0]=='Y') {
                  head=(TRPNHeader*)fieldAX->Head;
                  RPN_IntLock();
                  if (!Field->GRef->Ids)
                     Field->GRef->Ids=(int*)malloc(1*sizeof(int));

                  Field->GRef->Ids[Field->GRef->NId]=c_ezgdef_fmem(Field->Def->NI,Field->Def->NJ,Field->GRef->Grid,fieldAX->GRef->Grid,head->IG1,head->IG2,head->IG3,head->IG4,Field->GRef->AX,Field->GRef->AY);
                  RPN_IntUnlock();
               }
               if (Field->Stat) { Data_StatFree(Field->Stat); Field->Stat=NULL; }

               GeoRef_Qualify(Field->GRef);
               Data_Clean(Field,1,1,1);
            }
            break;

         case GRTYP:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewStringObj(Field->GRef->Grid,-1));
            } else {
               grtyp=Tcl_GetString(Objv[++i]);

               if (Objc==2 && (Field->GRef && grtyp[0]==Field->GRef->Grid[0])) {
                  return(TCL_OK);
               }
               Data_Clean(Field,1,1,1);
               ref=Field->GRef;

               if (grtyp[0]=='W' || grtyp[1]=='W') {
                  if (ref) {
                     Field->GRef=GeoRef_Find(GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,grtyp,ref->IG1,ref->IG2,ref->IG3,ref->IG4,ref->String,ref->Transform,ref->InvTransform,NULL));
                  } else {
                     Field->GRef=GeoRef_Find(GeoRef_WKTSetup(Field->Def->NI,Field->Def->NJ,grtyp,0,0,0,0,NULL,NULL,NULL,NULL));
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
                     if (!Field->GRef) {
                         Field->GRef=GeoRef_Reference(GeoRef_Find(GeoRef_RPNSetup(Field->Def->NI,Field->Def->NJ,"A ",0,0,0,0,-1)));
                         //TODO:EER   GeoRef_Put(Interp,NULL,Field->GRef);

                     }
                     Field->ZRef->Levels=(float*)realloc(Field->ZRef->Levels,Field->Def->NJ*sizeof(float));
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
               i++;
               if (Tcl_GetIntFromObj(Interp,Objv[i],&nidx)==TCL_OK) {
                  if (Objc>2) {
                     obj=Objv[++i];
                  }
               } else {
                  obj=Objv[i];
               }
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
      Field->GRef=GeoRef_Find(GeoRef_RPNSetup(Field->Def->NI,Field->Def->NJ,grtyp,head->IG1,head->IG2,head->IG3,head->IG4,head->File?head->File->Id:-1));
      Data_Clean(Field,1,1,1);
      GeoRef_Qualify(Field->GRef);
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

   TRPNHeader *head=(TRPNHeader*)Field->Head;

   if (Field && head) {
//      if (head->File) __sync_sub_and_fetch(&(head->File->NRef),1);
      free(head);
   }
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
TData *FSTD_FieldCreate(Tcl_Interp *Interp,char *Name,int NI,int NJ,int NK,TDef_Type Type){

   TData *field;

   field=Data_Valid(Interp,Name,NI,NJ,NK,1,Type==TD_Binary?TD_Byte:Type);

   if (!field)
     return(NULL);

   FSTD_FieldSet(field);
   field->GRef=GeoRef_Find(GeoRef_RPNSetup(NI,NJ,"X",0,0,0,0,-1));
   field->ZRef=ZRef_Define(LVL_UNDEF,NK,NULL);
   field->GPos=NULL;
   
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

   TRPNFile *file;
   int       ni,nj,nk,*idlst,idnb=0;
   char      buf[32];

   /*Recuperer les index de tout les champs satisfaisant*/
   file=FSTD_FileGet(Interp,Id);
   if (FSTD_FileSet(Interp,file)<0)
      return(TCL_ERROR);

   if (!(idlst=(int*)malloc(Max*sizeof(int)))) {
      Tcl_AppendResult(Interp,"FSTD_FieldFind: unable to allocate find array",(char*)NULL);
      FSTD_FileUnset(Interp,file);
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

   TRPNFile *file;
   TRPNHeader h;
   int       ok,ni,nj,nk;
   char      buf[1024],grtyp[2];

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
int FSTD_FieldList(Tcl_Interp *Interp,TRPNFile *File,int Mode,char *Var){

   TRPNHeader   head;
   Tcl_Obj     *list,*obj,*tmp;
   int          nobj,nb,ni,nj,nk;
   int          yyyy,mm,dd,h,m,s;
   char         buf[1024],grtyp[2];
   double       nhour;

   if((nb=FSTD_FileSet(Interp,File))<0)
      return(TCL_ERROR);

   if (Mode==FSTD_LISTNONE) {
      FSTD_FileUnset(Interp,File);
      return(TCL_OK);
   }

   list=Tcl_NewListObj(0,NULL);
   obj=Tcl_NewObj();

   RPN_FieldLock();
   head.KEY=c_fstinf(File->Id,&ni,&nj,&nk,-1,"",-1,-1,-1,"","");

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
               
               ZRef_IPFormat(strend(buf),head.IP1,False);
               ZRef_IPFormat(strend(buf),head.IP2,True);
               ZRef_IPFormat(strend(buf),head.IP3,True);

               System_StampDecode(head.DATEV,&yyyy,&mm,&dd,&h,&m,&s);
               sprintf(strend(buf)," %-12s %04i%02i%02i%02i%02i %s %i %i %i %i fstdfield",head.ETIKET,yyyy,mm,dd,h,m,File->CId,head.KEY,head.IP1,head.IP2,head.IP3);
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

            case FSTD_LISTLEVEL:
               ZRef_IPFormat(buf,head.IP1,False);
               Tcl_SetStringObj(obj,buf,-1);
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
   RPN_FieldUnlock();
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

   TRPNFile    *file;
   TData       *field=NULL;
   TDataVector *uvw=NULL;
   TDef_Type    dtype;
   TRPNHeader   h;
   int          ok,ni,nj,nk,i,type,idx,datyp,mni,mnj,mnk;
   int          pni,pnj,ig1,ig2,ig3,ig4,*tmpi;
   float        lvl;
   char         nomvar[5],typvar[2],grtyp[3],tile,etik[13],*proj=NULL;
   double       nhour,val=0.0;

   file=FSTD_FileGet(Interp,Id);
   if (FSTD_FileSet(Interp,file)<0)
      return(TCL_ERROR);

   // Rechercher et lire l'information de l'enregistrement specifie
   if (Key==-1) {
      Key=cs_fstinf(file->Id,&ni,&nj,&nk,DateV,Eticket,IP1,IP2,IP3,TypVar,NomVar);
      if (Key<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldRead: Specified field does not exist (c_fstinf failed)",(char*)NULL);
         RPN_FieldUnlock();
         FSTD_FileUnset(Interp,file);
         return(TCL_ERROR);
      }
   }

   grtyp[0]=grtyp[1]=grtyp[2]='\0';

   h.KEY=Key;
   h.File=file;
   h.FID=file->Id;
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
   
   // Remove compression flag (128) and missing value flag (64)
   datyp=h.DATYP;
   if (datyp>128) datyp-=128;
   if (datyp>64)  datyp-=64;

   // have to boost nbit to 32 for nbit=1, not sure why (X32) ????
   if (h.NBITS==32 && datyp==0) datyp=5;
   dtype=FSTD_TypeCheck(datyp,h.NBITS==1?32:h.NBITS);

   // Calculer la date de validitee du champs
   if (h.DATEO!=0) {
      nhour=((double)h.NPAS*h.DEET)/3600.0;
      f77name(incdatr)(&h.DATEV,&h.DATEO,&nhour);
      if (h.DATEV==101010101) h.DATEV=0;
   } else {
      h.DATEV=0;
   }

   // Supprimer les espaces inutiles
   strtrim(h.NOMVAR,' ');
   strtrim(h.TYPVAR,' ');
   strtrim(h.ETIKET,' ');

   // Champs vectoriel ???
   if ((uvw=Data_VectorTableCheck(h.NOMVAR,&idx)) && uvw->VV) {
      field=Data_Valid(Interp,Name,mni,mnj,mnk,(uvw->WW?3:2),dtype);
      if (!field) {
         FSTD_FileUnset(Interp,file);
         return(TCL_ERROR);
      }

      // Recuperer les donnees du champs
      c_fst_data_length(TDef_Size[field->Def->Type]);
      if (cs_fstlukt(field->Def->Data[idx],h.File->Id,h.KEY,&tile,&ni,&nj,&nk)<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not read field data (c_fstluk failed)",(char*)NULL);
         FSTD_FileUnset(Interp,file);
         return(TCL_ERROR);
      }
      strcpy(h.NOMVAR,uvw->UU);

      if (uvw->UU && idx!=0) {
         // Rechercher et lire l'information de l'enregistrement complementaire
         ok=cs_fstinf(h.File->Id,&ni,&nj,&nk,h.DATEV,h.ETIKET,h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->UU);
         if (ok<0) {
            ok=cs_fstinf(h.File->Id,&ni,&nj,&nk,h.DATEV,"",h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->UU);
         }

         if (ok<0 || (!tile && (ni!=mni || nj!=mnj))) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not find first component field ",uvw->UU," (c_fstinf failed)",(char*)NULL);
            FSTD_FileUnset(Interp,file);
            return(TCL_ERROR);
         } else {
            c_fst_data_length(TDef_Size[field->Def->Type]);
            if (cs_fstlukt(field->Def->Data[0],h.File->Id,ok,&tile,&ni,&nj,&nk)<0) {
               Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not read field data (c_fstluk failed)",(char*)NULL);
               FSTD_FileUnset(Interp,file);
               return(TCL_ERROR);
            }
         }
      }
      if (uvw->VV && idx!=1) {
         // Rechercher et lire l'information de l'enregistrement complementaire
         ok=cs_fstinf(h.File->Id,&ni,&nj,&nk,h.DATEV,h.ETIKET,h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->VV);
         if (ok<0) {
            ok=cs_fstinf(h.File->Id,&ni,&nj,&nk,h.DATEV,"",h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->VV);
         }

         if (ok<0 || (!tile && (ni!=mni || nj!=mnj))) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not find second component field ",uvw->VV," (c_fstinf failed)",(char*)NULL);
            FSTD_FileUnset(Interp,file);
            return(TCL_ERROR);
         } else {
            c_fst_data_length(TDef_Size[field->Def->Type]);
            if (cs_fstlukt(field->Def->Data[1],h.File->Id,ok,&tile,&ni,&nj,&nk)<0) {
               Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not read field data (c_fstluk failed)",(char*)NULL);
               FSTD_FileUnset(Interp,file);
               return(TCL_ERROR);
            }
         }
      }
      if (uvw->WW && idx!=2) {
         // Rechercher et lire l'information de l'enregistrement complementaire
         ok=cs_fstinf(h.File->Id,&ni,&nj,&nk,h.DATEV,h.ETIKET,h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->WW);
         if (ok<0) {
            ok=cs_fstinf(h.File->Id,&ni,&nj,&nk,h.DATEV,"",h.IP1,h.IP2,h.IP3,h.TYPVAR,uvw->WW);
         }

         if (ok<0 || (!tile && (ni!=mni || nj!=mnj))) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not find third component field ",uvw->WW," (c_fstinf failed)",(char*)NULL);
            FSTD_FileUnset(Interp,file);
            return(TCL_ERROR);
         } else {
            c_fst_data_length(TDef_Size[field->Def->Type]);
            if (cs_fstlukt(field->Def->Data[2],h.File->Id,ok,&tile,&ni,&nj,&nk)<0) {
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
      // Verifier si le champs existe et est valide
      field=Data_Valid(Interp,Name,mni,mnj,mnk,1,dtype);
      if (!field) {
         FSTD_FileUnset(Interp,file);
         return(TCL_ERROR);
      }

      // Recuperer les donnees du champs 
      c_fst_data_length(TDef_Size[field->Def->Type]);
      if ((ok=cs_fstlukt(field->Def->Data[0],h.File->Id,h.KEY,&tile,&ni,&nj,&nk))<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldRead: Could not read field data (c_fstluk failed)",(char*)NULL);
         FSTD_FileUnset(Interp,file);
         return(TCL_ERROR);
      } 
   }

   // Check for mask (TYPVAR==@@) 
   if (h.TYPVAR[0]!='@' && h.TYPVAR[1]=='@') {
      ok=cs_fstinf(h.File->Id,&ni,&nj,&nk,h.DATEV,h.ETIKET,h.IP1,h.IP2,h.IP3,"@@",h.NOMVAR);
      if (ok>0 && (tile || (ni==mni && nj==mnj && nk==mnk))) {
         if ((field->Def->Mask=(char*)malloc(ni*nj))) {
            if ((tmpi=(int*)malloc(ni*nj*sizeof(int)))) {
               cs_fstlukt(tmpi,h.File->Id,ok,&tile,&ni,&nj,&nk);
               for(i=0;i<ni*nj;i++) {
                  field->Def->Mask[i]=tmpi[i]!=0x0;
               }
               free(tmpi);
            } else {
               free(field->Def->Mask);
               field->Def->Mask=NULL;
               App_Log(WARNING,"%s: Could not allocate memory to read mask",__func__);
            }
         } else {
            App_Log(WARNING,"%s: Could not allocate memory for mask",__func__);
         }
      }
   }

   // Recuperer les type de niveaux et forcer ETA pour SIGMA
   lvl=ZRef_IP2Level(h.IP1,&type);
   type=type==LVL_SIGMA?LVL_ETA:type;

   // Override le type de niveaux pour ZH is ip1=0
   if (h.NOMVAR[0]=='Z' && h.NOMVAR[1]=='H') {
      grtyp[0]='Y';
      if (h.IP1==0){
         type=LVL_MASL;
      }
   }

   // Creer une grille comme definie dans l'enregistrement
   if (grtyp[0]=='W' || grtyp[0]=='Y' || grtyp[0]=='X' || grtyp[0]=='Z') {
      float tmpv[6];
      double mtx[6],inv[6],*tm=NULL,*im=NULL;
      char   t='\0';

      ig1=h.IG1;
      ig2=h.IG2;
      ig3=h.IG3;
      ig4=h.IG4;

      // Here we test for W grids, by getting the subgrid format from the >> field
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

         // This is no W grid so keep previous grtyp
         if (grtyp[0]!='W') {
            grtyp[0]=t;
         }
      }

      // If it is a W grid, look for PROJ and MATRIX fields
      if (grtyp[0]=='W') {

         Key=cs_fstinf(file->Id,&pni,&pnj,&nk,-1,"",ig1,ig2,ig3,"","PROJ");
         if (Key<0) {
            Tcl_AppendResult(Interp,"FSTD_FieldRead: Projection description field does not exist (c_fstinf failed)",(char*)NULL);
         } else {
            c_fst_data_length(1);
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
         field->GRef=GeoRef_Find(GeoRef_WKTSetup(ni,nj,grtyp,h.IG1,h.IG2,h.IG3,h.IG4,proj,tm,im,NULL));
         if (proj) free(proj);
      } 
   }

   if (tile) {
      grtyp[0]='Z';grtyp[1]='#';
   }

   field->ZRef=ZRef_Define(type,nk,&lvl);
   if (grtyp[0]!='W') {
      field->GRef=GeoRef_Find(GeoRef_RPNSetup(ni,nj,grtyp,h.IG1,h.IG2,h.IG3,h.IG4,h.File->Id));
   }

   field->GPos=GeoPos_Find(field->GRef,field->ZRef);
   if (grtyp[0]=='U') {
      FSTD_FieldSubBuild(field);
   }

//TODO   h.File->NRef++;

   FSTD_FieldSet(field);
   GeoRef_Qualify(field->GRef);

   if (field->Spec->Desc)
      free(field->Spec->Desc);

   field->Spec->Desc=strdup(h.NOMVAR);
   memcpy(field->Head,&h,sizeof(TRPNHeader));

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

   TRPNHeader  *head=(TRPNHeader*)Field->Head;
   TDataVector *uvw;
   Tcl_Obj     *obj;
   int          idxs[FSTD_NKMAX],tmp[FSTD_NKMAX],i,k,k2,idx,ok,idump,ni,nj,nk,type,ip1;
   char         cdump[16],grtyp[2],tile;
   char        *p;
   float        levels[FSTD_NKMAX];
   double       level,val;

   if (Field->Def->NK>1 || Field->GRef->Grid[0]=='V')
      return(1);

   if (FSTD_FileSet(Interp,head->File)<0)
      return(0);

   if (LevelFrom>LevelTo) {
      level=LevelTo;
      LevelTo=LevelFrom;
      LevelFrom=level;
   }

   // Check for auto untile
   tile=(Field->GRef->Grid[1]=='#')&&FSTD_UNTILE?'#':0;

   if (List) {
      Tcl_ListObjLength(Interp,List,&nk);
      for(k=0;k<nk;k++) {
         Tcl_ListObjIndex(Interp,List,k,&obj);
         Tcl_GetDoubleFromObj(Interp,obj,&level);
      
         ip1=ZRef_Level2IP(level,Field->ZRef->Type,DEFAULT);
         cs_fstinl(head->File->Id,&ni,&nj,&k2,head->DATEV,head->ETIKET,ip1,head->IP2,(tile?head->IP3:-1),head->TYPVAR,head->NOMVAR,&idxs[Invert?nk-k-1:k],&k2,1);

         if (k2==0) {
            Tcl_AppendResult(Interp,"FSTD_FieldReadLevels: Could not find specific level ",Tcl_GetString(obj),(char*)NULL);
            FSTD_FileUnset(Interp,head->File);
            return(0);
         }
         tmp[k]=k;
      }
   } else {
      // Recuperer les indexes de tout les niveaux
      cs_fstinl(head->File->Id,&ni,&nj,&nk,head->DATEV,head->ETIKET,-1,head->IP2,(tile?head->IP3:-1),head->TYPVAR,head->NOMVAR,idxs,&nk,FSTD_NKMAX);

      if (nk<=1) {
         FSTD_FileUnset(Interp,head->File);
         return(1);
      }

      // Determiner les niveaux disponibles
      k2=0;
      for(k=0;k<nk;k++) {
         ok=cs_fstprm(idxs[k],&idump,&idump,&idump,&idump,&idump,&idump,&idump,
            &idump,&tmp[k2],&idump,&idump,cdump,cdump,cdump,grtyp,&idump,&idump,
            &idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump);

         // Verifier que l'on garde le meme type de niveau
         level=ZRef_IP2Level(tmp[k2],&type);
         type=type==LVL_SIGMA?LVL_ETA:type;

         if (type==Field->ZRef->Type) {

            // Check if level is within the specified range if any
            if (LevelFrom==LevelTo || (level>=LevelFrom && level<=LevelTo)) {

               // Verifier que l'on a pas deja ce niveau (niveau en double)
               for(i=0;i<k2;i++) {
                  if (tmp[k2]==tmp[i]) break;
               }

               // Garder ce niveau en metre pour le tri
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
         FSTD_FileUnset(Interp,head->File);
         return(1);
      }

      // Trier les niveaux
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

   k2=Field->GRef->NId;
   FSTD_FieldSubSelect(Field,0);

   // Augmenter la dimension du tableau
   if (!Def_Resize(Field->Def,Field->Def->NI,Field->Def->NJ,nk)) {
      Tcl_AppendResult(Interp,"FSTD_FieldReadLevels: Not enough memory to allocate levels",(char*)NULL);
      FSTD_FileUnset(Interp,head->File);
      return(0);
   }

   App_Log(DEBUG,"%s: found %i levels\n",__func__,Field->Def->NK);

   val=0.0;
   uvw=Data_VectorTableCheck(head->NOMVAR,NULL);

   // Recuperer le data
   for(k=0;k<Field->Def->NK;k++) {
      idx=FSIZE2D(Field->Def)*tmp[k];
      Def_Pointer(Field->Def,0,idx,p);

      // Recuperer les donnees du champs
      c_fst_data_length(TDef_Size[Field->Def->Type]);
      if (cs_fstlukt(p,head->File->Id,idxs[k],&tile,&ni,&nj,&idump)<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldReadLevels: Could not read field data (c_fstluk failed) for ",head->NOMVAR,(char*)NULL);
         FSTD_FileUnset(Interp,head->File);
         return(0);
      }

      // Recuperer le data seulement
      ok=cs_fstprm(idxs[k],&idump,&idump,&idump,&idump,&idump,&idump,&idump,
         &idump,&ip1,&idump,&idump,cdump,cdump,cdump,cdump,&idump,&idump,
         &idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump,&idump);

      // Champs vectoriel ???
      if (uvw) {
         if (uvw->VV) {
            Def_Pointer(Field->Def,1,idx,p);
            ok=cs_fstinf(head->File->Id,&ni,&nj,&idump,head->DATEV,head->ETIKET,ip1,head->IP2,head->IP3,head->TYPVAR,uvw->VV);
            c_fst_data_length(TDef_Size[Field->Def->Type]);
            if (cs_fstlukt(p,head->File->Id,ok,&tile,&ni,&nj,&idump)<0) {
               Tcl_AppendResult(Interp,"FSTD_FieldReadLevels: Could not read field data (c_fstluk failed) for ",uvw->VV,(char*)NULL);
               FSTD_FileUnset(Interp,head->File);
               return(0);
            }
         }
         if (uvw->WW) {
            Def_Pointer(Field->Def,2,idx,p);
            ok=cs_fstinf(head->File->Id,&ni,&nj,&idump,head->DATEV,head->ETIKET,ip1,head->IP2,head->IP3,head->TYPVAR,uvw->WW);
            c_fst_data_length(TDef_Size[Field->Def->Type]);
            if (cs_fstlukt(p,head->File->Id,ok,&tile,&ni,&nj,&idump)<0) {
               Tcl_AppendResult(Interp,"FSTD_FieldReadLevels: Could not read field data (c_fstluk failed) for ",uvw->WW,(char*)NULL);
               FSTD_FileUnset(Interp,head->File);
               return(0);
            }
            
            if (uvw->WWFactor!=0.0) {
               for(i=0;i<FSIZE2D(Field->Def);i++) {
                  Def_Get(Field->Def,2,idx+i,val);
                  val*=uvw->WWFactor;
                  Def_Set(Field->Def,2,idx+i,val);
               }
            }
         }
      }

      // Assigner le niveaux courant
      if (ip1==head->IP1) {
         Field->Def->Level=tmp[k];
      }
      levels[tmp[k]]=ZRef_IP2Level(ip1,&type);

      if (ok<0) {
         Tcl_AppendResult(Interp,"FSTD_FieldReadLevels: Something really wrong here (c_fstprm failed)",(char*)NULL);
         FSTD_FileUnset(Interp,head->File);
         return(0);
      }
   }

   GeoPos_Free(Field->GPos);
   ZRef_Free(Field->ZRef);
   Field->ZRef=ZRef_Define(type==LVL_SIGMA?LVL_ETA:type,nk,levels);
   Field->GPos=GeoPos_Find(Field->GRef,Field->ZRef);

   FSTD_FileUnset(Interp,head->File);

   Data_Clean(Field,1,1,1);
   Data_GetStat(Field);

   if (Field->GRef->Grid[0]=='U') {
      FSTD_FieldSubBuild(Field);
   }
//   FSTD_FieldSubSelect(Field,k2);
   
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

   TRPNFile     *file;
   TRPNHeader   *head=(TRPNHeader*)Field->Head;
   TDataVector  *uvw;
   int          ok=-1,ip1,datyp,nid;
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

   // Force datadef to master grid
   nid=Field->GRef->NId;
   FSTD_FieldSubSelect(Field,0);

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
         case TD_Float32: head->DATYP=5;head->NBITS=32;break;
         case TD_Float64: head->DATYP=5;head->NBITS=64;break;
         case TD_Unknown:
         default: return(TCL_ERROR);
      }
   }

   head->File=file;
   head->FID=file->Id;
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

   RPN_FieldLock();

   for(k=0;k<Field->Def->NK;k++) {
      idx=k*FSIZE2D(Field->Def);

      /*If IP1 is set, use it otherwise, convert it from levels array*/
      if ((ip1=head->IP1)==-1 || Field->Def->NK>1) {
         ip1=ZRef_Level2IP(Field->ZRef->Levels[k],Field->ZRef->Type,DEFAULT);
      }
      
      /*Inscription de l'enregistrement*/
      Def_Pointer(Field->Def,0,idx,p);
      c_fst_data_length(TDef_Size[Field->Def->Type]);
      ok=c_fstecr(p,NULL,NPack,file->Id,head->DATEO,head->DEET,head->NPAS,Field->Def->NI,Field->Def->NJ,1,
                  ip1,head->IP2,head->IP3,head->TYPVAR,head->NOMVAR,head->ETIKET,
                  (Field->GRef?(Field->GRef->Grid[1]!='\0'?&Field->GRef->Grid[1]:Field->GRef->Grid):"X"),head->IG1,head->IG2,head->IG3,head->IG4,datyp,Rewrite);

      /*Inscription des champs complementaires*/
      if (Field->Def->Data[1]) {
         if ((uvw=Data_VectorTableCheck(head->NOMVAR,NULL))) {
            /*Inscription du champs complementaire 2D*/
            if (uvw->VV) {
               Def_Pointer(Field->Def,1,idx,p);
               c_fst_data_length(TDef_Size[Field->Def->Type]);
               ok=c_fstecr(p,NULL,NPack,file->Id,head->DATEO,head->DEET,head->NPAS,Field->Def->NI,Field->Def->NJ,1,
                           ip1,head->IP2,head->IP3,head->TYPVAR,uvw->VV,head->ETIKET,
                           (Field->GRef?(Field->GRef->Grid[1]!='\0'?&Field->GRef->Grid[1]:Field->GRef->Grid):"X"),head->IG1,head->IG2,head->IG3,head->IG4,datyp,Rewrite);
            }
            /*Inscription du champs complementaire 3D*/
            if (Field->Def->Data[2] && uvw->WW) {
               Def_Pointer(Field->Def,2,idx,p);
               c_fst_data_length(TDef_Size[Field->Def->Type]);
               ok=c_fstecr(p,NULL,NPack,file->Id,head->DATEO,head->DEET,head->NPAS,Field->Def->NI,Field->Def->NJ,1,
                           ip1,head->IP2,head->IP3,head->TYPVAR,uvw->WW,head->ETIKET,
                           (Field->GRef?(Field->GRef->Grid[1]!='\0'?&Field->GRef->Grid[1]:Field->GRef->Grid):"X"),head->IG1,head->IG2,head->IG3,head->IG4,datyp,Rewrite);
            }
         }
      }
   }
   RPN_FieldUnlock();

   FSTD_FileUnset(Interp,file);
   FSTD_FieldSubSelect(Field,nid);

   if (ok>=0){
      return(TCL_OK);
   } else {
      Tcl_AppendResult(Interp,"FSTD_FieldWrite: Could not write field (c_fstecr failed)",(char*)NULL);
      return(TCL_ERROR);
   }
}

int FSTD_FieldTile(Tcl_Interp *Interp,char *Id,TData *Field,int NI,int NJ,int Halo,int NPack,int Rewrite,int Compress) {

   TRPNFile    *file;
   TRPNHeader  *head=(TRPNHeader*)Field->Head;
   TDataVector *uvw;
   int          ok=0,datyp,nid;
   char         pvar[5];

   /*Verifier l'existence du champs*/
   if (!Field) {
      Tcl_AppendResult(Interp,"FSTD_FieldWrite: Invalid field",(char*)NULL);
      return(TCL_ERROR);
   }

   file=FSTD_FileGet(Interp,Id);
   if (FSTD_FileSet(Interp,file)<0)
      return(TCL_ERROR);

   // Force datadef to master grid
   nid=Field->GRef->NId;
   FSTD_FieldSubSelect(Field,0);

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
         case TD_Float32: head->DATYP=5;head->NBITS=32;break;
         case TD_Float64: head->DATYP=5;head->NBITS=64;break;
         case TD_Unknown:
         default: FSTD_FileUnset(Interp,file); return(TCL_ERROR);
      }
   }

   head->File=file;
   head->FID=file->Id;
   datyp=(NPack>0 && head->DATYP==1)?5:head->DATYP;
   NPack=NPack==0?-head->NBITS:(NPack>0?-NPack:NPack);

   // Check for compression flag and adjust datyp accordingly
   if (Compress) {
      switch (head->DATYP) {
         case 2: datyp=130; break;
         case 4: datyp=132; break;
         case 5: datyp=133; break;
         case 1: datyp=134; break;
      }
//      if (head->NBITS==64 && (head->DATYP=1 || head->DATYP==5)) datyp=801;
   }

   ok=RPN_FieldTile(file->Id,Field->Def,(TRPNHeader*)Field->Head,Field->GRef,Field->ZRef,0,NI,NJ,Halo,datyp,NPack,Rewrite,Compress);

   // Inscription des champs complementaires
   if (Field->Def->Data[1]) {
      if ((uvw=Data_VectorTableCheck(head->NOMVAR,NULL))) {
         strncpy(pvar,head->NOMVAR,4);

         // Inscription du champs complementaire 2D
         if (uvw->VV) {
            strncpy(head->NOMVAR,uvw->VV,4);
            ok=RPN_FieldTile(file->Id,Field->Def,(TRPNHeader*)Field->Head,Field->GRef,Field->ZRef,1,NI,NJ,Halo,datyp,NPack,Rewrite,Compress);
         }

         // Inscription du champs complementaire 3D
         if (Field->Def->Data[2] && uvw->WW) {
            strncpy(head->NOMVAR,uvw->WW,4);
            ok=RPN_FieldTile(file->Id,Field->Def,(TRPNHeader*)Field->Head,Field->GRef,Field->ZRef,2,NI,NJ,Halo,datyp,NPack,Rewrite,Compress);
         }
         strncpy(head->NOMVAR,pvar,4);
      }
   }

   FSTD_FileUnset(Interp,file);
   FSTD_FieldSubSelect(Field,nid);

   if (ok){
      return(TCL_OK);
   } else {
      Tcl_AppendResult(Interp,"FSTD_FieldWrite: Could not write field (c_fstecr failed)",(char*)NULL);
      return(TCL_ERROR);
   }
}

#endif
