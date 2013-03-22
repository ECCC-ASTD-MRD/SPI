/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture de fichiers d'observations BURP
 * Fichier      : tclMetObs_BURP.c
 * Creation     : Octobre 2012 - J.P. Gauthier
 *
 * Description  : Fonction relatives au BURP.
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
#ifdef HAVE_RMN

#include "tclMetObs.h"

TCL_DECLARE_MUTEX(MUTEX_BURPFILE)

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_LoadBURP>
 * Creation     : Avril 2007 J.P. Gauthier
 *
 * But          : Chargement d'un fichier d'observations en format BURP.
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
int MetObs_LoadBURP(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

   int       err,sz,handle,code=TCL_OK;
   int      *buf=NULL;
   time_t    time=0,dt;

   EntryTableB  **eb=NULL;

   Tcl_Obj      *obj;
   TMetLoc      *loc;
   TMetElemData *data;

   int      e,sz1=0,sz2=0,c;

   int      hhmm,flag,codtyp,blat,blon,hgt,dx,dy,dlay,yymmdd,oars,runn,nblk,sup=0,nsup=0,xaux=0,nxaux=0,mkr=0;;
   int      blkno,nelem,nval,nt,bfam,bdesc,btyp,nbit,bit0,datyp,bknat,bktyp,bkstp;
   char     stnid[10],previd[10];
   int     *elems=NULL,*tblval=NULL,*codes=NULL;
   float   *tblvalf=NULL;
   char     multi=0;

   Tcl_MutexLock(&MUTEX_BURPFILE);

   if (Obs->FId==-1)
      Obs->FId=cs_fstlockid();

   dt=Obs->Time-Obs->Cache;
   dt=Obs->Time-60;
   dt=0;

   /*Setup the API*/
   c_mrfopc("MSGLVL","FATAL");
   c_mrfopr("MISSING",Obs->NoData);

   /*Open the file*/
   err=c_fnom(Obs->FId,File,"RND",0);
   if (err<0) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Unable to link filename ",File,(char*)NULL);
      Tcl_MutexUnlock(&MUTEX_BURPFILE);
      return(TCL_ERROR);
   }
   err=c_mrfopn(Obs->FId,"READ");
   if (err<0) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Could not open observation file ",File,(char*)NULL);
      c_fclos(Obs->FId);
      cs_fstunlockid(Obs->FId);
      Tcl_MutexUnlock(&MUTEX_BURPFILE);
      return(TCL_ERROR);
   }

   /*Allocate enough words*/
   sz=c_mrfmxl(Obs->FId)*4+1024000;
   buf=(int*)malloc(sz*4);
   if (!buf) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Could not allocate memory",(char*)NULL);
      c_mrfcls(Obs->FId);
      c_fclos(Obs->FId);
      cs_fstunlockid(Obs->FId);
      Tcl_MutexUnlock(&MUTEX_BURPFILE);
      return(TCL_ERROR);
   }

   obj=Tcl_NewStringObj("",0);

  /*Start reading reports*/
   handle=0;
   while((handle=c_mrfloc(Obs->FId,handle,"*********",-1,-1,-1,-1,-1,-1,0))>0) {

     *buf=sz;
      err=c_mrfget(handle,buf);
      if (err<0) {
         Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Allocated buffer too small",(char*)NULL);
         code=TCL_ERROR;
         break;
      }

      strcpy(stnid,"         ");
      err=c_mrbhdr(buf,&hhmm,&flag,stnid,&codtyp,&blat,&blon,&dx,&dy,&hgt,&dlay,&yymmdd,&oars,&runn,&nblk,sup,nsup,xaux,nxaux);
      if (err<0) {
         Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Unable to read message header",(char*)NULL);
         code=TCL_ERROR;
         break;
      }

      /*Skip if it is a resume or a header*/
      if (stnid[0]=='>')
         continue;

      /*Check if station already exists, unless this is a satobs file with multiple location for same id and station name is same as before*/
      strtrim(stnid,' ');
      loc=NULL;
      if (!multi || strcmp(previd,stnid)!=0)
         loc=TMetLoc_FindWithCoord(Obs,NULL,stnid,(blat-9000.0)/100.0,blon/100.0,hgt-400,MET_TYPEID,&multi);

      strcpy(previd,stnid);
      /*Insert station in list if not already done*/
      if (!loc) {
         loc=TMetLoc_New(Obs,stnid,NULL,(blat-9000.0)/100.0,blon/100.0,hgt-400);
         loc->Grid[0]=dx/10.0;
         loc->Grid[1]=dy/10.0;
         loc->CodeType=codtyp;
      }
      if ((time=System_DateTime2Seconds(yymmdd,hhmm*100,1))<0)
         continue;

      Obs->Time0=(Obs->Time0<time && Obs->Time0!=0)?Obs->Time0:time;
      Obs->Time1=Obs->Time1>time?Obs->Time1:time;

      /*Start reading the information blocs*/
      blkno=0;
      while ((blkno=c_mrbloc(buf,-1,-1,-1,blkno))>0) {
         err=c_mrbprm(buf,blkno,&nelem,&nval,&nt,&bfam,&bdesc,&btyp,&nbit,&bit0,&datyp);
         if (err<0) {
            Tcl_AppendResult(Interp,"\n   MetObs_LoadBURP :  Invalid block",(char*)NULL);
            code=TCL_ERROR;
            break;
         }

         bknat=(btyp>>11)&0x0F;
         bktyp=(btyp>>4)%0x7F;
         bkstp=(btyp)&0x0F;

         /*Skip if empty*/
         if ((nelem*nval*nt)==0) {
            fprintf(stdout,"(WARNING) MetObs_LoadBURP: Found empty report\n");
            continue;
         }
         /*Resize temporary buffers if needed*/
         if (nelem>sz1) {
            sz1=nelem;
            elems=(int*)realloc(elems,sz1*sizeof(int));
            codes=(int*)realloc(codes,sz1*sizeof(int));
            eb=(EntryTableB**)realloc(eb,sz1*sizeof(EntryTableB*));
         }

         if (nelem*nval*nt>sz2) {
            sz2=nelem*nval*nt;
            tblval=(int*)realloc(tblval,sz2*sizeof(int));
            tblvalf=(float*)realloc(tblvalf,sz2*sizeof(float));
         }

         /*Extract info*/
         err=c_mrbxtr(buf,blkno,elems,tblval);
         err=c_mrbdcl(elems,codes,nelem);

         /*Test for superobs ..... ta daaaaaaa*/
         if (stnid[0]=='^') {
            if (stnid[1]=='^') {
               fprintf(stdout,"(DEBUG) MetObs_LoadBURP: Found super duper obs\n");
            } else {

            }
         }

         /*If this is a marker bloc*/
         mkr=0;
         if (bknat==0x3 || codes[0]>=200000) {
            mkr=nelem*nval*nt;
         } else {
            err=c_mrbcvt(elems,tblval,tblvalf,nelem,nval,nt,0);
         }

         /*Get the elements code list and cache it within obs object*/
         c=0;
         for(e=0;e<nelem;e++) {
            if (mkr) {
               if (codes[e]>=200000)
                  codes[e]-=200000;
            }
            c=codes[e];

            if (eb[e]=MetObs_BUFRFindTableCode(c)) {
               Tcl_SetIntObj(obj,eb[e]->descriptor);
               if (TclY_ListObjFind(Interp,Obs->Elems,obj)==-1) {
                  Tcl_ListObjAppendElement(Interp,Obs->Elems,Tcl_DuplicateObj(obj));
               }
               c=1;
            } else {
               fprintf(stdout,"(WARNING) MetObs_LoadBURP: Found invalid code (%i)\n",c);
               c=0;
               break;
            }
         }

         /*if the elements where ok, add the dataset*/
         if (c) data=TMetElem_Insert(loc,dt,time,bfam,bktyp,bkstp,nelem,nval,nt,mkr?NULL:tblvalf,mkr?tblval:NULL,eb);
      }
   }

   Tcl_DecrRefCount(obj);
   if (elems)   free(elems);
   if (codes)   free(codes);
   if (eb)      free(eb);
   if (tblval)  free(tblval);
   if (tblvalf) free(tblvalf);
   free(buf);

   /*Close the file*/
   c_mrfcls(Obs->FId);
   c_fclos(Obs->FId);
   cs_fstunlockid(Obs->FId);

   Tcl_MutexUnlock(&MUTEX_BURPFILE);
   return(code);
}

#endif
#endif