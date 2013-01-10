/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de divers fichiers de donnees
 * Fichier      : tclDataDef.c
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

#include "tclDataDef.h"

/*----------------------------------------------------------------------------
 * Nom      : <DataDef_Clear>
 * Creation : Fevrier 2003- J.P. Gauthier - CMC/CMOE
 *
 * But      : Reinitialiser la structure de definitions des donnees
 *
 * Parametres :
 *  <Def>     : Structure a reinitialiser
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
void DataDef_Clear(TDataDef *Def){

   int n,i;

   for(n=0;n<DSIZE(Def->Data);n++) {
      for(i=0;i<FSIZE3D(Def);i++) {
         Def_Set(Def,n,i,Def->NoData);
      }
   }
   if (Def->Buffer) {
      free(Def->Buffer);
      Def->Buffer=NULL;
   }
   if (Def->Accum) {
      free(Def->Accum);
      Def->Accum=NULL;
   }
   if (Def->Mask) {
      free(Def->Mask);
      Def->Mask=NULL;
   }

   if (Def->Segments) {
      TList_Clear(Def->Segments,T3DArray_Free);
      Def->Segments=NULL;
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <DataDef_Compat>
 * Creation : Mars 2009- J.P. Gauthier - CMC/CMOE
 *
 * But      : Verifier les dimensiont entre 2 Def et les ajuster en consequence.
 *
 * Parametres :
 *  <DefTo>   : Definition a redimensionner
 *  <DefTFrom>: Definition de laquelle redimensionner
 *
 * Retour:
 *  <Compat>  : Compatibles?
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int DataDef_Compat(TDataDef *DefTo,TDataDef *DefFrom) {

   int ch=1;

   if (DefTo->Mode && DefTo->Mode!=DefTo->Data[0]) {
      free(DefTo->Mode);
   }
   DefTo->Mode=NULL;

   /*Verifier la dimension verticale*/
   if (DefTo->NK!=DefFrom->NK) {
      if (DefTo->Data[1]) {
         free(DefTo->Data[1]);
         DefTo->Data[1]=NULL;
      }
      if (DefTo->Data[2]) {
         free(DefTo->Data[2]);
         DefTo->Data[2]=NULL;
      }
      if (DefTo->Data[1]) {
         free(DefTo->Data[1]);
         DefTo->Data[1]=NULL;
      }
      if (DefTo->Data[0]) {
         free(DefTo->Data[0]);
      }
      DefTo->NK=DefFrom->NK;
      DefTo->Data[0]=(char*)calloc(FSIZE3D(DefTo),TData_Size[DefTo->Type]);
      ch=0;
   }

   /*Verifier la 2ieme composantes*/
   if (DefFrom->Data[1]) {
      if (!DefTo->Data[1]) {
         DefTo->Data[1]=(char*)calloc(FSIZE3D(DefTo),TData_Size[DefTo->Type]);
      }

      /*Verifier la 3ieme composantes*/
      if (DefFrom->Data[2]) {
         if (!DefTo->Data[2]) {
            DefTo->Data[2]=(char*)calloc(FSIZE3D(DefTo),TData_Size[DefTo->Type]);
         }
         DefTo->NC=3;
      } else {
         if (DefTo->Data[2]) {
            free(DefTo->Data[2]);
            DefTo->Data[2]=NULL;
         }
         DefTo->NC=2;
      }
   } else {
     if (DefTo->Data[1]) {
         free(DefTo->Data[1]);
         DefTo->Data[1]=NULL;
      }
      if (DefTo->Data[2]) {
         free(DefTo->Data[2]);
         DefTo->Data[2]=NULL;
      }
      DefTo->NC=1;
   }

   return(ch);
}

/*----------------------------------------------------------------------------
 * Nom      : <DataDef_Copy>
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
TDataDef *DataDef_Copy(TDataDef *Def){

   int       i;
   TDataDef *def;

   if (Def && (def=(TDataDef*)malloc(sizeof(TDataDef)))) {
      def->Container=Def->Container;
      def->CellDim=Def->CellDim;
      def->NI=Def->NI;
      def->NJ=Def->NJ;
      def->NK=Def->NK;
      def->NC=Def->NC;
      def->NoData=Def->NoData;
      def->Type=Def->Type;
      def->Level=Def->Level;
      def->Idx=Def->Idx;
      def->Buffer=NULL;
      def->Segments=NULL;
      def->Accum=NULL;
      def->Mask=NULL;
      def->Pres=NULL;
      def->Height=NULL;
      def->Pick=def->Poly=NULL;
      def->Sample=Def->Sample;

      memcpy(def->Limits,Def->Limits,6*sizeof(int));
      def->CoordLimits[0][0]=Def->CoordLimits[0][0];
      def->CoordLimits[0][1]=Def->CoordLimits[0][1];
      def->CoordLimits[1][0]=Def->CoordLimits[1][0];
      def->CoordLimits[1][1]=Def->CoordLimits[1][1];

      for(i=0;i<4;i++) {
         if (def->Container) {
            def->Data[i]=Def->Data[i];
         } else {
            if (Def->Data[i] && (def->Data[i]=(char*)malloc(FSIZE3D(Def)*TData_Size[Def->Type]))) {
               memcpy(def->Data[i],Def->Data[i],FSIZE3D(Def)*TData_Size[Def->Type]);
            } else {
               def->Data[i]=NULL;
            }
         }
      }
      def->Mode=def->Data[0];
   }
   return(def);
}

TDataDef *DataDef_CopyPromote(TDataDef *Def,TData_Type Type){

   int       i;
   TDataDef *def;

   if (Def && (def=(TDataDef*)malloc(sizeof(TDataDef)))) {
      def->Container=0;
      def->CellDim=Def->CellDim;
      def->NI=Def->NI;
      def->NJ=Def->NJ;
      def->NK=Def->NK;
      def->NC=Def->NC;
      def->NoData=Def->NoData;
      def->Type=Type;
      def->Level=Def->Level;
      def->Idx=Def->Idx;
      def->Buffer=NULL;
      def->Segments=NULL;
      def->Accum=NULL;
      def->Mask=NULL;
      def->Pres=NULL;
      def->Height=NULL;
      def->Pick=def->Poly=NULL;
      def->Sample=Def->Sample;

      memcpy(def->Limits,Def->Limits,6*sizeof(int));
      def->CoordLimits[0][0]=Def->CoordLimits[0][0];
      def->CoordLimits[0][1]=Def->CoordLimits[0][1];
      def->CoordLimits[1][0]=Def->CoordLimits[1][0];
      def->CoordLimits[1][1]=Def->CoordLimits[1][1];

      for(i=0;i<4;i++) {
         if (Def->Data[i]) {
            def->Data[i]=(char*)calloc(FSIZE3D(Def),TData_Size[def->Type]);
         } else {
            def->Data[i]=NULL;
         }
      }
      def->Mode=def->Data[0];
   }

   return(def);
}

/*----------------------------------------------------------------------------
 * Nom      : <DataDef_Free>
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
void DataDef_Free(TDataDef *Def){

   if (Def) {
      if (!Def->Container) {
         if (Def->Mode && Def->Mode!=Def->Data[0]) free(Def->Mode);
         if (Def->Data[0])            free(Def->Data[0]);
         if (Def->Data[1])            free(Def->Data[1]);
         if (Def->Data[2])            free(Def->Data[2]);
         if (Def->Data[3])            free(Def->Data[3]);
      }

      if (Def->Buffer)             free(Def->Buffer);
      if (Def->Accum)              free(Def->Accum);
      if (Def->Mask)               free(Def->Mask);
      if (Def->Pres>(float*)0x1)   free(Def->Pres);
      if (Def->Height>(float*)0x1) free(Def->Height);
      if (Def->Poly)               OGR_G_DestroyGeometry(Def->Poly);
//      if (Def->Pick)       OGR_G_DestroyGeometry(Def->Pick);
      if (Def->Segments)           TList_Clear(Def->Segments,T3DArray_Free);

      free(Def);
   }
}

/*----------------------------------------------------------------------------
 * Nom      : <DataDef_New>
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
TDataDef *DataDef_New(int NI,int NJ,int NK,int Dim,TData_Type Type){

   int       i;
   TDataDef *def;

   def=(TDataDef*)malloc(sizeof(TDataDef));
   if (!def)
     return(NULL);

   def->NI=NI;
   def->NJ=NJ;
   def->NK=NK;
   def->NC=abs(Dim);
   def->Container=Dim<=0;
   def->CellDim=2;
   def->NoData=nan("NaN");
   def->Level=0;
   def->Idx=0;

   def->Limits[0][0]=0;
   def->Limits[1][0]=0;
   def->Limits[2][0]=0;
   def->Limits[0][1]=NI-1;
   def->Limits[1][1]=NJ-1;
   def->Limits[2][1]=NK-1;

   def->CoordLimits[0][0]=-180;
   def->CoordLimits[0][1]=180;
   def->CoordLimits[1][0]=-90;
   def->CoordLimits[1][1]=90;

   def->Sample=1;

   /* Allocate data vector */
   def->Data[0]=def->Data[1]=def->Data[2]=def->Data[3]=NULL;
   def->Type=Type;
   def->Buffer=NULL;
   def->Segments=NULL;
   def->Accum=NULL;
   def->Mask=NULL;
   def->Pres=NULL;
   def->Height=NULL;
   def->Pick=def->Poly=NULL;

   for(i=0;i<Dim;i++) {
      if (!(def->Data[i]=(char*)calloc(FSIZE3D(def),TData_Size[Type]))) {
         DataDef_Free(def);
         return(NULL);
      }
   }
   def->Mode=def->Data[0];

   return(def);
}

/*----------------------------------------------------------------------------
 * Nom      : <DataDef_Resize>
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
TDataDef *DataDef_Resize(TDataDef *Def,int NI,int NJ,int NK){

   int i;

   if (!Def)
     return(NULL);

   if (!Def->Container && (Def->NI!=NI || Def->NJ!=NJ || Def->NK!=NK)) {
      Def->NI=NI;
      Def->NJ=NJ;
      Def->NK=NK;

      Def->Limits[0][0]=0;
      Def->Limits[1][0]=0;
      Def->Limits[2][0]=0;
      Def->Limits[0][1]=NI-1;
      Def->Limits[1][1]=NJ-1;
      Def->Limits[2][1]=NK-1;

      Def->CoordLimits[0][0]=-180;
      Def->CoordLimits[0][1]=180;
      Def->CoordLimits[1][0]=-90;
      Def->CoordLimits[1][1]=90;

      Def->Sample=1;

      if (Def->Mode && Def->Mode!=Def->Data[0]) {
         free(Def->Mode);
         Def->Mode=NULL;
      }

      for(i=0;i<4;i++) {
         if (Def->Data[i]) {
            if (!(Def->Data[i]=(char*)realloc(Def->Data[i],FSIZE3D(Def)*TData_Size[Def->Type]))) {
               DataDef_Free(Def);
               return(NULL);
            }
         }
      }
      Def->Mode=Def->Data[0];

      if (Def->Buffer)             free(Def->Buffer); Def->Buffer=NULL;
      if (Def->Accum)              free(Def->Accum);  Def->Accum=NULL;
      if (Def->Mask)               free(Def->Mask);   Def->Mask=NULL;
      if (Def->Pres>(float*)0x1)   free(Def->Pres);   Def->Pres=NULL;
      if (Def->Height>(float*)0x1) free(Def->Pres);   Def->Height=NULL;
   }
   return(Def);
}

/*----------------------------------------------------------------------------
 * Nom      : <DataDef_Tile>
 * Creation : Novembre 2007- J.P. Gauthier - CMC/CMOE
 *
 * But      : Copier les donnees d'un TDataDef dans un autres (Tiling).
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
int DataDef_Tile(TDataDef *DefTo,TDataDef *DefTile,int X0, int Y0) {

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
