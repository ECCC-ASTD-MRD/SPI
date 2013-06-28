/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture de fichiers d'observations ASCII
 * Fichier      : tclMetObs_ASCII.c
 * Creation     : Octobre 2012 - J.P. Gauthier
 *
 * Description  : Fonction relatives au ASCII.
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
 *
 *=========================================================
 */
#ifdef HAVE_ECBUFR

#include "tclMetObs.h"

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_LoadASCII>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Chargement d'un fichier d'observations en format ASCII
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl
 *   <File>     : Le nom du fichier
 *   <Obs>      : Observation
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_LoadASCII(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

   TMetLoc  *loc=NULL;

   FILE    *stream;
   char    buf[256];
   char    *bytes=NULL;
   int     sz,sk,nb,n,hd,k,sec;
   int     ntok,gntok,nltok;
   char    **tok,**gtok,**ltok;
   int     err=TCL_OK,date,time;
   time_t  *gtime;

   stream=fopen(File,"r");

   if (!stream) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadASCII :  Could not open observation file ",File,(char*)NULL);
      return(TCL_ERROR);
   }

   /*Read the version*/
   fgetskip(buf,256,stream);

   if (!strstr(buf,"Obs 3.1")) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadASCII :  Wrong file version while reading ",File,(char*)NULL);
      fclose(stream);
      return(TCL_ERROR);
   }

   /*Read the title*/
   fgetskip(buf,256,stream);
   if (Obs->Desc) free(Obs->Desc);
   Obs->Desc=strdup(buf);

   /*Read the header*/
   nb=255;
   while(nb==255) {
      fgetskip(buf,256,stream);
      bytes=strcatalloc(bytes,buf);
      nb=strlen(buf);
   }

   /*Split the header tokens into list*/
   Tcl_SplitList(Interp,bytes,&gntok,&gtok);

   /*Allocate big enough buffer to read data*/
   sz=strlen(bytes)*4;
   free(bytes);
   bytes=(char*)malloc(sz);

   gtime=(time_t*)calloc(gntok,sizeof(time_t));

   Obs->NbInfo=0;
   /*Parse header tokens*/
   for(n=0;n<gntok;n++) {
      if (strncmp(gtok[n],"DATA",4)==0) {
         /*Extract date and time*/
         hd=0;sec=0;
         sscanf((char*)(strrchr(gtok[n],(int)'.'))+1,"%8d%02d%02d%02d",&date,&time,&hd,&sec);
         time=time*10000+hd*100+sec;
         gtime[n]=System_DateTime2Seconds(date,time,1);


      } else if (strcmp(gtok[n],"ID")!=0 && strcmp(gtok[n],"NO")!=0 && strcmp(gtok[n],"LAT")!=0 && strcmp(gtok[n],"LON")!=0 && strcmp(gtok[n],"ELEV")!=0 && strcmp(gtok[n],"ELEVTYPE")!=0 && strcmp(gtok[n],"SIZE")!=0) {
         Obs->NbInfo++;
      }
   }
   Obs->Info=(char**)calloc(Obs->NbInfo,sizeof(char**));

   nb=0;
   while (!feof(stream)) {
      fgetskip(bytes,sz,stream);

      if (strlen(bytes)>10) {
         Tcl_SplitList(Interp,bytes,&ntok,&tok);

         if (ntok!=gntok) {
            Tcl_AppendResult(Interp,"\n   MetObs_LoadASCII :  Invalid number of item on following line\n",bytes,(char*)NULL);
            fclose(stream);
            return(TCL_ERROR);
         }

         /*Parse the tokens*/
         hd=0;
         for(n=0;n<ntok;n++) {
            if (strcmp(gtok[n],"LAT")==0) {                  /*Latitude information*/
               loc->Coord.Lat=atof(tok[n]);
            } else if (strcmp(gtok[n],"LON")==0) {           /*Longitude information*/
               loc->Coord.Lon=atof(tok[n]);
            } else if (strcmp(gtok[n],"ELEVTYPE")==0) {      /*Elevation type information*/
               if (isdigit(tok[n][0])) {
                  loc->Level=atoi(tok[n]);
               } else {
                  if (strcmp(tok[n],"MASL")==0) {
                     loc->Level=0;
                  } else if (strcmp(tok[n],"SIGMA")==0) {
                     loc->Level=1;
                  } else if (strcmp(tok[n],"PRESSURE")==0) {
                     loc->Level=2;
                  } else if (strcmp(tok[n],"UNDEFINED")==0) {
                     loc->Level=3;
                  } else if (strcmp(tok[n],"MAGL")==0) {
                     loc->Level=4;
                  } else if (strcmp(tok[n],"HYBRID")==0) {
                     loc->Level=5;
                  } else if (strcmp(tok[n],"THETA")==0) {
                     loc->Level=6;
                  } else if (strcmp(tok[n],"ETA")==0) {
                     loc->Level=7;
                  } else if (strcmp(tok[n],"GALCHEN")==0) {
                     loc->Level=8;
                  } else {
                     Tcl_AppendResult(Interp,"\n   MetObs_LoadASCII : Invalid level type, must be [ MASL SIGMA PRESSURE UNDEFINED MAGL HYBRID THETA ETA GALCHEN ]",(char*)NULL);
                     err=TCL_ERROR;
                  }
               }
            } else if (strcmp(gtok[n],"ELEV")==0) {          /*Elevation information*/
               loc->Coord.Elev=atof(tok[n]);
            } else if (strcmp(gtok[n],"NO")==0) {            /*Number information*/
               loc->No=strdup(tok[n]);
               strrep(loc->No,'(',' ');
               strrep(loc->No,')',' ');
            } else if (strcmp(gtok[n],"ID")==0) {            /*Identificateur*/
               /*Insert station in list if not already done*/
               loc=TMetLoc_Find(Obs,NULL,tok[n],MET_TYPEID);
               if (!loc) {
                  loc=TMetLoc_New(Obs,tok[n],NULL,0.0,0.0,0.0);
               }
            } else if (strncmp(gtok[n],"DATA",4)==0) {       /*Values*/
/*TODO
               data=TMetElem_Insert(loc,0,gtime[n],0x0,nelem,nt*nval);

               if (tok[n][0]=='-' && tok[n][1]=='\0') {
                  ((float*)obs->Def->Data[0])[nb]=-999.0f;
               } else {
                  Tcl_SplitList(Interp,tok[n],&nltok,&ltok);
                  for(k=0;k<nltok;k++) {
                     if (!obs->Def->Data[k]) {
                        obs->Def->Data[k]=(char*)calloc(FSIZE3D(obs->Def),TData_Size[TD_Float32]);
                     }
                     Data_FromString(ltok[k],obs->Def,k,nb);
                  }
                  Tcl_Free((char*)ltok);
               }
*/
            } else {                                         /*Information*/
               if (!Obs->Info[hd])
                  Obs->Info[hd]=strdup(gtok[n]);
               if (!loc->Info)
                  loc->Info=(char**)malloc(Obs->NbInfo*sizeof(char*));
               loc->Info[hd]=strdup(tok[n]);
               hd++;
            }
         }

         nb++;
         Tcl_Free((char*)tok);
      }
   }

   /*Is there any observation in there*/
   if (!nb) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadASCII :  No observation found ",(char*)NULL);
      fclose(stream);
      return(TCL_ERROR);
   }
   fclose(stream);

   Tcl_Free((char*)gtok);
   free(bytes);

   return(err);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_WriteASCII>
 * Creation     : Decembre 2004 J.P. Gauthier
 *
 * But          : Ecriture d'un fichier d'observations.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl (IN)
 *   <File>     : Le nom du fichier (IN)
 *   <List>     : Liset des observations
 *   <Title>    : Tire du set de donnees
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_WriteASCII(Tcl_Interp *Interp,char *File,Tcl_Obj *List,char *Title) {

   return(TCL_OK);
}

#endif
