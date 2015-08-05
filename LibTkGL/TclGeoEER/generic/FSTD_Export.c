/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de fichiers standards.
 * Fichier   : FSTD_Export.c
 * Creation  : Mai 2002 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Fonctions d'exportations de champs standards RPN.
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

#include "RPN.h"
#include "tclFSTD.h"
#include "tclGDAL.h"

int HIRLAM_WriteData(TData *Field,FILE *FID,float *Data,double Factor,char *Mode,int I0,int J0,int I1,int J1);
int HIRLAM_WriteHead(FILE *FID,char *Desc,char *Info,double Factor);

/*----------------------------------------------------------------------------
 * Nom      : <HIRLAM_Export>
 * Creation : Decembre 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Exporter un champs en fomat @#^$*&%@$(% HIRLAM.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Field>   : Identificateur du champs
 *  <Desc>    : Description
 *  <Info>    : Information complementaire (Level|Time)
 *  <File>    : Nom du fichier
 *  <Factor>  : Facteur a appliquer aux valeurs
 *  <Mode>    : Format entier (I) ou exponentiel (E)
 *  <Type>    : Exporter les positions des points de grilles(GRID) ,vitesse(SPEED), direction(DIR) ou donnees(DATA)
 *  <I0>      : Index 0 en I
 *  <J0>      : Index 0 en J
 *  <I1>      : Index end en I
 *  <J1>      : Index end en J
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int HIRLAM_Export(Tcl_Interp *Interp,TData *Field,char* Desc,char *Info,char *File,double Factor,char *Mode,char *Type,int I0,int J0,int I1,int J1){

#ifdef HAVE_RMN
   FILE  *fid;
   int    i,j,sz;
   float *spd,*dir,*fx,*fy,*x,*y;

   if (Mode[0]!='E' && Mode[0]!='I') {
       Tcl_AppendResult(Interp,"HIRLAM_Export: Invalid export format",(char*)NULL);
       return(TCL_ERROR);
   }

   if (!(fid=fopen(File,"a"))) {
       Tcl_AppendResult(Interp,"HIRLAM_Export: Could not open output file \"",File,"\"",(char*)NULL);
       return(TCL_ERROR);
   }

   /* Calculer la region */
   I0=I0==-1?0:I0-1;
   J0=J0==-1?0:J0-1;
   I1=I1==-1?Field->Def->NI-1:I1-1;
   J1=J1==-1?Field->Def->NJ-1:J1-1;
   sz=FSIZE2D(Field->Def);

   /* If in grid mode, output grid instead of field values*/

   if (Type[1]=='R') {
      x=fx=(float*)malloc(sz*sizeof(float));
      fy=(float*)malloc(sz*sizeof(float));
      if (!fx || !fy) {
         Tcl_AppendResult(Interp,"HIRLAM_Export: Could not allocate memory for grid processing",(char*)NULL);
         return(TCL_ERROR);
      }
//      RPN_IntLock();
      c_gdll(Field->GRef->Ids[Field->GRef->NId],fy,fx);
//      RPN_IntUnlock();

      /* Output grid longitude*/

      for (i=0;i<sz;i++,x++) {
         CLAMPLON(*x);
      }

      HIRLAM_WriteHead(fid,"longitude (decimal deg.)",Info,Factor);
      HIRLAM_WriteData(Field,fid,fx,Factor,Mode,I0,J0,I1,J1);

      /* Output grid latitude*/
      HIRLAM_WriteHead(fid,"latitude (decimal deg.)",Info,Factor);
      HIRLAM_WriteData(Field,fid,fy,Factor,Mode,I0,J0,I1,J1);

      free(fx);
      free(fy);

   } else if (Type[1]=='P' || Type[1]=='I') {

      if (!Field->Def->Data[1]) {
         Tcl_AppendResult(Interp,"HIRLAM_Export: Field is not vetorial",(char*)NULL);
         return(TCL_ERROR);
      }

      spd=(float*)malloc(sz*sizeof(float));
      dir=(float*)malloc(sz*sizeof(float));
      x=fx=(float*)malloc(sz*sizeof(float));
      y=fy=(float*)malloc(sz*sizeof(float));

      if (!fx || !fy || !dir || !spd) {
         Tcl_AppendResult(Interp,"HIRLAM_Export: Could not allocate memory for data processing",(char*)NULL);
         return(TCL_ERROR);
      }

      for (j=1;j<=Field->Def->NJ;j++) {
         for (i=1;i<=Field->Def->NI;i++) {
           *x=i;
           *y=j;
           x++;y++;
         }
      }
//      RPN_IntLock();
      c_gdxywdval(Field->GRef->Ids[Field->GRef->NId],spd,dir,(float*)(Field->Def->Data[0]),(float*)(Field->Def->Data[1]),fx,fy,sz);
//      RPN_IntUnlock();

      if (Type[1]=='P') {
         HIRLAM_WriteHead(fid,"wind speed (m/s)",Info,Factor);
         HIRLAM_WriteData(Field,fid,spd,Factor/0.515,Mode,I0,J0,I1,J1);
      } else {
         HIRLAM_WriteHead(fid,"wind direction (decimal deg.)",Info,Factor);
         HIRLAM_WriteData(Field,fid,dir,Factor,Mode,I0,J0,I1,J1);
      }
      free(spd);
      free(dir);
      free(fx);
      free(fy);
   } else {
      HIRLAM_WriteHead(fid,Desc,Info,Factor);
      HIRLAM_WriteData(Field,fid,(float*)Field->Def->Data[0],Factor,Mode,I0,J0,I1,J1);
   }

   fclose(fid);
#else
   App_ErrorSet("%s: Need RMNLIB",__func__);
#endif
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <HIRLAM_WriteData>
 * Creation : Decembre 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Ecrire les valeurs en fomat @#^$*&%@$(% HIRLAM.
 *
 * Parametres :
 *  <Field>   : Identificateur du champs
 *  <FID>     : Identificateur du fichier
 *  <Data>    : Matrice de donnees
 *  <Factor>  : Facteur a appliquer aux valeurs
 *  <Mode>    : Format entier (I) ou exponentiel (E)
 *  <I0>      : Index 0 en I
 *  <J0>      : Index 0 en J
 *  <I1>      : Index end en I
 *  <J1>      : Index end en J
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int HIRLAM_WriteData(TData *Field,FILE *FID,float *Data,double Factor,char *Mode,int I0,int J0,int I1,int J1) {

   int  i,j,idx,n=0;
   char buf[16];

   for (j=J0;j<=J1;j++) {
      idx=j*Field->Def->NI;
      for (i=I0;i<=I1;i++) {
         if (Mode[0]=='E') {
            sprintf(buf,"%14.6e",Data[idx+i]);
         } else {
            sprintf(buf," %i",(int)(Data[idx+i]/Factor));
         }
         fputs(buf,FID);

         if (++n==10) {
            fputc('\n',FID);
            n=0;
         }
      }
   }

   if (n)
      fputc('\n',FID);

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <HIRLAM_WriteHead>
 * Creation : Decembre 2003 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Ecrire l'entete en fomat @#^$*&%@$(% HIRLAM.
 *
 * Parametres :
 *  <FID>     : Identificateur du fichier
 *  <Desc>    : Description des donnees
 *  <Info>    : Information complementaire (Level|Time)
 *  <Factor>  : Facteur a appliquer aux valeurs
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int HIRLAM_WriteHead(FILE *FID,char *Desc,char *Info,double Factor) {

   char buf[64];

   fputs(Desc,FID);
   sprintf(buf,"\n%.2e\n",Factor);
   fputs(buf,FID);

   if (strlen(Info)) {
      fputs(Info,FID);
      fputc('\n',FID);
   }

   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <WIX_Export>
 * Creation : Mars 2006 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Exporter un champs en fomat WIX.
 *
 * Parametres :
 *  <Interp>  : Interpreteur TCL
 *  <Fields>  : Identificateurs des champs
 *  <File>    : Nom du fichier
 *  <I0>      : Index 0 en I
 *  <J0>      : Index 0 en J
 *  <I1>      : Index end en I
 *  <J1>      : Index end en J
 *
 * Retour:
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
int WIX_Export(Tcl_Interp *Interp,Tcl_Obj *Fields,char *File,int I0,int J0,int I1,int J1){

   Tcl_Obj  *obj;
   TData    *fld;
   FILE     *fid;
   float     val,dx,dy,la0,lo0;
   short     tmps;
   int       tmpi;
   int       n,nb,yyyy,mm,dd,h,m,s;
   int       i,j,sz;
   int       endian;

   if (!(fid=fopen(File,"w"))) {
       Tcl_AppendResult(Interp,"WIX_Export: Could not open output file \"",File,"\"",(char*)NULL);
       return(TCL_ERROR);
   }

   Tcl_ListObjLength(Interp,Fields,&nb);
   if (nb<=0) {
       Tcl_AppendResult(Interp,"WIX_Export: No fields were specified",(char*)NULL);
       return(TCL_ERROR);
   }

   Tcl_ListObjIndex(Interp,Fields,0,&obj);
   fld=Data_Get(Tcl_GetString(obj));
   if (!fld) {
       Tcl_AppendResult(Interp,"WIX_Export: Invalid field \"",Tcl_GetString(obj),"\"",(char*)NULL);
       return(TCL_ERROR);
   }

   endian=System_ByteOrder();

   /* Calculer la region */
   I0=I0==-1?0:I0-1;
   J0=J0==-1?0:J0-1;
   I1=I1==-1?fld->Def->NI-1:I1-1;
   J1=J1==-1?fld->Def->NJ-1:J1-1;
   sz=FSIZE2D(fld->Def);

#define WIX_NUM_HEADER_RECORDS 6
#define WIX_RECL 8

   tmps=WIX_RECL;                         SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);
   tmps=sz;                               SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);
   tmpi=WIX_NUM_HEADER_RECORDS+nb+sz*nb;  SYS_IFSWAP4(SYS_LITTLE_ENDIAN,endian,tmpi); fwrite(&tmpi,sizeof(int),1,fid);
   tmps=fld->Def->NI;                     SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);
   tmps=fld->Def->NJ;                     SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);

   /*DLat, DLon, Lat0 and Lon0*/
   if (fld->GRef->Grid[0]=='Z') {
      GeoRef_Expand(fld->GRef);
      dx=fld->GRef->AX[1]-fld->GRef->AX[0];
      dy=fld->GRef->AY[1]-fld->GRef->AY[0];
      lo0=fld->GRef->AX[0];
      la0=fld->GRef->AY[0];
   } else {
      f77name(cigaxg)(&fld->GRef->Grid[0],&la0,&lo0,&dx,&dy,&((TRPNHeader*)fld->Head)->IG1,&((TRPNHeader*)fld->Head)->IG2,&((TRPNHeader*)fld->Head)->IG3,&((TRPNHeader*)fld->Head)->IG4);
   }

   lo0-=0.5*dx;
   la0-=0.5*dy;
   CLAMPLON(lo0);

   SYS_IFSWAP4(SYS_LITTLE_ENDIAN,endian,dx);  fwrite(&dx,sizeof(float),1,fid);
   SYS_IFSWAP4(SYS_LITTLE_ENDIAN,endian,dy);  fwrite(&dy,sizeof(float),1,fid);
   SYS_IFSWAP4(SYS_LITTLE_ENDIAN,endian,lo0); fwrite(&lo0,sizeof(float),1,fid);
   SYS_IFSWAP4(SYS_LITTLE_ENDIAN,endian,la0); fwrite(&la0,sizeof(float),1,fid);

   tmps=nb;    SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);
   tmps=0;     SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);

   System_StampDecode(((TRPNHeader*)fld->Head)->DATEV,&yyyy,&mm,&dd,&h,&m,&s);
   tmps=yyyy;  SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);
   tmps=mm;    SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);
   tmps=dd;    SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);
   tmps=h;     SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);
   tmps=m;     SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);
   tmps=s;     SYS_IFSWAP2(SYS_LITTLE_ENDIAN,endian,tmps); fwrite(&tmps,sizeof(short),1,fid);
   tmpi=0;     SYS_IFSWAP4(SYS_LITTLE_ENDIAN,endian,tmpi); fwrite(&tmpi,sizeof(int),1,fid);

   for(n=0;n<nb;n++) {
      tmpi=WIX_NUM_HEADER_RECORDS+nb+sz*n+1;  SYS_IFSWAP4(SYS_LITTLE_ENDIAN,endian,tmpi); fwrite(&tmpi,sizeof(int),1,fid);
      val=n;                                  SYS_IFSWAP4(SYS_LITTLE_ENDIAN,endian,val);  fwrite(&val,sizeof(float),1,fid);
   }

   /*Loop on fields*/
   for(n=0;n<nb;n++) {
      Tcl_ListObjIndex(Interp,Fields,n,&obj);
      fld=Data_Get(Tcl_GetString(obj));
      if (!fld) {
         Tcl_AppendResult(Interp,"WIX_Export: Invalid field \"",Tcl_GetString(obj),"\"",(char*)NULL);
         return TCL_ERROR;
      }

      /*Write field data*/
      for(j=0;j<fld->Def->NJ;j++) {
         for(i=0;i<fld->Def->NI;i++) {
            Def_Get(fld->Def,0,FIDX2D(fld->Def,i,j),val);
            SYS_IFSWAP4(SYS_LITTLE_ENDIAN,endian,val); fwrite(&val,sizeof(float),1,fid);
            if (fld->Def->Data[1]) {
               Def_Get(fld->Def,1,FIDX2D(fld->Def,i,j),val);
               SYS_IFSWAP4(SYS_LITTLE_ENDIAN,endian,val); fwrite(&val,sizeof(float),1,fid);
            }
         }
      }
   }
   fclose(fid);
   return(TCL_OK);
}

#endif