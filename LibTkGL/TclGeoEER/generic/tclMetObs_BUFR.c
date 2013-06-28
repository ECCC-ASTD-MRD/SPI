/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture de fichiers d'observations BUFR
 * Fichier      : tclMetObs_BUFR.c
 * Creation     : Octobre 2012 - J.P. Gauthier
 *
 * Description  : Fonction relatives au BUFR.
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
 * Nom          : <MetObs_LoadBUFR>
 * Creation     : Mars 2008 J.P. Gauthier
 *
 * But          : Chargement d'un fichier d'observations en format BUFR.
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
int MetObs_LoadBUFR(Tcl_Interp *Interp,char *File,TMetObs *Obs) {

   Tcl_Obj        *obj=NULL;
   TMetElemData   *data=NULL;

   FILE           *fpBufr;
   BUFR_Tables    *tbls,*master;
   BUFR_Message   *msg;
   BUFR_Dataset   *dts;
   DataSubset     *subset;
   BufrDescriptor *bcv;
   EntryTableB    *eb;
   int             i,j,ne,len,strid=0,t,n;
   char            stnid[256],multi=0;
   double          value,lat,lon,hgt=0.0;
   int             yyyy,mm,dd,hh,mn,ss;

   obj=Tcl_NewStringObj("",0);

   if (!(fpBufr=fopen(File,"r"))) {
      Tcl_AppendResult(Interp,"\n   MetObs_LoadBUFR :  Unable to open file ",File,(char*)NULL);
      return(TCL_ERROR);
   }

   master=MetObs_GetTables();

   while ((bufr_read_message(fpBufr,&msg))>0) {

      /*Decode message*/
      dts=bufr_decode_message(msg,master);
      bufr_free_message(msg);
      if (!dts) {
         Tcl_AppendResult(Interp,"\n   MetObs_LoadBUFR :  Unable to decode message",(char*)NULL);
         return(TCL_ERROR);
      }

      /*Check for local lable update*/
      if (bufr_contains_tables(dts)) {
         if ((tbls=bufr_extract_tables(dts))) {
            bufr_merge_tables(master,tbls);
            bufr_free_tables(tbls);
         }
      }

      for (i=0;i<bufr_count_datasubset(dts);i++) {
         if (!(subset=bufr_get_datasubset(dts,i))) {
            fprintf(stdout,"(WARNING) MetObs_LoadBUFR: Invalid subset");
            continue;
         }

         ne=bufr_datasubset_count_descriptor(subset);
         data=TMetElemData_New(ne,1,1);
         data->Ne=0;

         stnid[0]='-';stnid[1]='\0';
         lat=lon=-999.0;
         yyyy=1970;mm=1;dd=1;hh=mn=ss=0;
         t=n=0;
         
         for (j=0;j<bufr_datasubset_count_descriptor(subset);j++) {
            if (!(bcv=bufr_datasubset_get_descriptor(subset,j))) {
               fprintf(stdout,"(WARNING) MetObs_LoadBUFR: Invalid subset code");
               continue;
            }

            /*Skip Table D && Table C operators*/
            if (bcv->descriptor>=300000 || bcv->descriptor>=100000)
               continue;

            if (bcv->meta) {
//               bufr_print_metadata(buf,bcv->meta);
//               bufr_print_output(buf);
            }

            if (bcv->flags & FLAG_SKIPPED) {
//               printf("#  %.6d ",bcv->descriptor);
            } else {

               /*If this code has a value*/
               if (bcv->value) {
                  if (!(eb=MetObs_BUFRFindTableCode(bcv->descriptor))) {
                     fprintf(stdout,"(WARNING) MetObs_LoadBUFR: Could not find element code (%i) int tables",bcv->descriptor);
                  } else {
                     Tcl_SetIntObj(obj,eb->descriptor);
                     if (TclY_ListObjFind(Interp,Obs->Elems,obj)==-1) {
                        Tcl_ListObjAppendElement(Interp,Obs->Elems,Tcl_DuplicateObj(obj));
                     }
                  }

                  /*Look for needed specific descriptor*/
                  switch(bcv->descriptor) {
                     /*Date related*/
                     case 4001:  yyyy=bufr_descriptor_get_ivalue(bcv); t++; break;
                     case 4002:  mm=bufr_descriptor_get_ivalue(bcv);   t++; break;
                     case 4003:  dd=bufr_descriptor_get_ivalue(bcv);   t++; break;
                     case 4004:  hh=bufr_descriptor_get_ivalue(bcv);   t++; break;
                     case 4005:  mn=bufr_descriptor_get_ivalue(bcv);   t++; break;
                     case 4006:  ss=bufr_descriptor_get_ivalue(bcv);   t++; break;
                     case 5001:  lat=bufr_descriptor_get_dvalue(bcv);  break;

                     /*Location related*/
                     case 6001:
                     case 6002:
                     case 28001:
                     case 28002:
                     case 28003:
                     case 28004:
                     case 6192:
                     case 6193:  lon=bufr_descriptor_get_dvalue(bcv);  break;
                     case 5002:
                     case 27001:
                     case 27002:
                     case 27003:
                     case 27004:
                     case 5192:
                     case 5193:  lat=bufr_descriptor_get_dvalue(bcv);  break;

                     /*Height related*/
                     case 7001:
                     case 7002:  hgt=bufr_descriptor_get_dvalue(bcv);  break;

                     /*Station ID related but prioritize string version*/
                     case 1002:
                     case 1007:  if (!strid) { sprintf(stnid,"%i",bufr_descriptor_get_ivalue(bcv));  strtrim(stnid,' '); }; break;
                     case 1015:
                     case 1018:
                     case 1019:  sprintf(stnid,"%s",bufr_descriptor_get_svalue(bcv,&len)); strtrim(stnid,' '); strid=1; break;

                     /*Time displacement*/
                     case 4011:  yyyy+=bufr_descriptor_get_ivalue(bcv); t++; break;
                     case 4012:  mm+=bufr_descriptor_get_ivalue(bcv);   t++; break;
                     case 4013:  dd+=bufr_descriptor_get_ivalue(bcv);   t++; break;
                     case 4014:  hh+=bufr_descriptor_get_ivalue(bcv);   t++; break;
                     case 4015:  mn+=bufr_descriptor_get_ivalue(bcv);   t++; break;
                     case 4016:  ss+=bufr_descriptor_get_ivalue(bcv);   t++; break;
                     
                     default: t=0; 
                  }
                 
                  // We try to split the message by time slices
                  if (t==1) {
                     if (n) {
                        if (TMetElem_BUFRAdd(Obs,data,lat,lon,hgt,System_DateTime2Seconds(yyyy*10000+mm*100+dd,hh*10000+mn*100+ss,1),stnid,&multi)) {
                           data=TMetElemData_New(ne,1,1);
                           data->Ne=0;
                        }
                     }
                     n++;
                  }
                 
                  // If there are Associated Fields 
                  if (bcv->value->af)  {
//                     BufrAF *af = bcv->value->af;
//                     sprintf( buf, "(0x%llx:%d bits)", af->bits, af->nbits );
                  }

                  value=-999.0;
                  switch(bcv->value->type) {
                     case VALTYPE_INT8:
                     case VALTYPE_INT32:
                        value=bufr_descriptor_get_ivalue(bcv);
                        break;
                     case VALTYPE_INT64:
                        value=bufr_descriptor_get_ivalue(bcv);
                        break;
                     case VALTYPE_FLT32:
                        value = bufr_descriptor_get_fvalue(bcv);
                        if (bufr_is_missing_float(value)) {
                           value=-999.0;
                        }
                        break;
                    case VALTYPE_FLT64:
                        value = bufr_descriptor_get_dvalue(bcv);
                        if (bufr_is_missing_double(value)) {
                           value=-999.0;
                        }
                        break;
                     case VALTYPE_STRING:
/*                        int   len;

                        char *str = bufr_descriptor_get_svalue(bcv,&len);
                        printf("VALUE=%s",str);
*/
                           value=-999.0;
                        break;
                        
                     case VALTYPE_UNDEFINE:
                        fprintf(stdout,"(WARNING) MetObs_LoadBUFR: Found undefined value");
                        value=-999.0;
                        break;
                  }
                  data->Code[data->Ne]=eb;
                  data->Data[data->Ne]=value;
                  data->Ne++;
               }
            }
         }
         /*Insert station in list if not already done*/
         TMetElem_BUFRAdd(Obs,data,lat,lon,hgt,System_DateTime2Seconds(yyyy*10000+mm*100+dd,hh*10000+mn*100+ss,1),stnid,&multi);
         n++;
      }
      bufr_free_dataset(dts);
   }

   Tcl_DecrRefCount(obj);
   fclose(fpBufr);
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObs_WriteBUFR>
 * Creation     : Decembre 2004 J.P. Gauthier
 *
 * But          : Ecriture d'un fichier d'observations.
 *
 * Parametres   :
 *   <Interp>   : L'interpreteur Tcl (IN)
 *   <File>     : Le nom du fichier (IN)
 *   <List>     : Liset des observations
 *   <Compress> : Compression
 *
 * Retour       : Code d'erreur standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int MetObs_WriteBUFR(Tcl_Interp *Interp,char *File,char *Template,Tcl_Obj *List,int Compress) {

   return(TCL_OK);
}

#endif