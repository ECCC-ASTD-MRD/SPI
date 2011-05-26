/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Lecture et traitements de fichiers d'observations
 * Fichier      : tclMetDataset.c
 * Creation     : Mai 2008 - J.P. Gauthier
 *
 * Description  : Fonctions de manipulation des Dataset et Template BUFR par Tcl.
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
#include "tclMetDataset.h"

static Tcl_HashTable MetDatafileTable;
static Tcl_HashTable MetDatasetTable;
static Tcl_HashTable MetDatasubsetTable;
static Tcl_HashTable MetTemplateTable;
static long          MetDatasetNo=0;
static long          MetTemplateNo=0;
static int           MetDatasetInit=0;

static int MetDataset_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int MetDataset_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int MetDataset_FreeHash(Tcl_Interp *Interp,char *Name);

static int MetTemplate_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]);
static int MetTemplate_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]);
static int MetTemplate_FreeHash(Tcl_Interp *Interp,char *Name);

extern BUFR_Tables *MetObs_GetTables(void);

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclMetDataset_Init>
 * Creation     : Avril 2006 J.P. Gauthier
 *
 * But          : Initialisation des commandes Tcl pour utilisation des obseravtions
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
int TclMetDataset_Init(Tcl_Interp *Interp) {

   if (!MetDatasetInit++) {
      Tcl_InitHashTable(&MetDatasetTable,TCL_STRING_KEYS);
      Tcl_InitHashTable(&MetDatasubsetTable,TCL_STRING_KEYS);
      Tcl_InitHashTable(&MetDatafileTable,TCL_STRING_KEYS);
      Tcl_InitHashTable(&MetTemplateTable,TCL_STRING_KEYS);
   }

   Tcl_CreateObjCommand(Interp,"bufrdataset",MetDataset_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);
   Tcl_CreateObjCommand(Interp,"bufrtemplate",MetTemplate_Cmd,(ClientData)NULL,(Tcl_CmdDeleteProc*)NULL);

   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetDataset_Cmd>
 * Creation     : Mam 2008 J.P. Gauthier
 *
 * But          : Effectuer les commandes metdataset du package
 *
 * Parametres    :
 *   <clientData>: Nom de la template
 *   <interp>    : Interpreteur Tcl
 *   <Objc>      : Nombre d'arguments
 *   <Objv>      : Pointeur sur la liste des arguments (word)
 *
 * Retour        : Code de retour standard TCL
 *
 * Remarques     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int MetDataset_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   BUFR_Template *tmp;
   BUFR_Dataset  *set,*sub;
   BUFR_Message  *msg;
   FILE          *file;
   long           fidx=0;
   int            idx,co;

   static CONST char *sopt[] = { "create","free","read","write","define","is","all",NULL };
   enum               opt { CREATE,FREE,READ,WRITE,DEFINE,IS,ALL };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case CREATE:
         if(Objc<2 || Objc>5) {
            Tcl_WrongNumArgs(Interp,1,Objv,"id [template|file] [file index]");
            return(TCL_ERROR);
         }

         if ((set=MetDataset_Get(Tcl_GetString(Objv[2])))) {
            MetDataset_FreeHash(Interp,Tcl_GetString(Objv[2]));
         }
         if ((tmp=MetTemplate_Get(Tcl_GetString(Objv[3])))) {
            if (!(set=bufr_create_dataset(tmp))) {
               Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to create dataset \"",Tcl_GetString(Objv[2]),"\"",(char*)NULL);
               return(TCL_ERROR);
            }
         } else {

            /*Check if the associated file is already openned*/
            if (!(file=MetDatafile_Get(Tcl_GetString(Objv[2])))) {
               /*Open the file or socket*/
               if (!(file=TclY_ChannelOrSocketOpen(Interp,Objv[3],"r"))) {
                  return(TCL_ERROR);
               } else {
                  /*Keep associated file*/
                  MetDatafile_Put(Interp,Tcl_GetString(Objv[2]),file);
               }
            }

            /*Check for index within file*/
            if (Objc==5) {
               Tcl_GetLongFromObj(Interp,Objv[4],&fidx);
               if (fidx)
                  fseek(file,fidx,SEEK_SET);
            }

            /*Read the next message*/
            if (bufr_read_message(file,&msg)<=0) {
               Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
               TclY_HashDel(&MetDatafileTable,Tcl_GetString(Objv[2]));
               fclose(file);
               return(TCL_OK);
            }
            /*Decode the message into the dataset*/
            if (!(set=bufr_decode_message(msg,MetObs_GetTables()))) {
               Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to decode message to dataset",(char*)NULL);
               return(TCL_ERROR);
            }

            bufr_free_message(msg);
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         }
         if (!MetDataset_Put(Interp,Tcl_GetString(Objv[2]),set)) {
           return(TCL_ERROR);
         }
         break;

      case FREE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,1,Objv,"id");
            return(TCL_ERROR);
         }
         MetDataset_FreeHash(Interp,Tcl_GetString(Objv[2]));
         break;

      case READ:
         if (Objc!=4) {
            Tcl_WrongNumArgs(Interp,1,Objv,"id file");
            return(TCL_ERROR);
         }
         if (!(set=MetDataset_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Invalid dataset \"",Tcl_GetString(Objv[2]),"\"",(char*)NULL);
            return(TCL_ERROR);
         }
         if (!(file=fopen(Tcl_GetString(Objv[3]),"r"))) {
            Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to open file \"",Tcl_GetString(Objv[3]),"\"",(char*)NULL);
            return(TCL_ERROR);
         }

         if (bufr_read_dataset_dump(set,file)<=0) {
            if (bufr_read_message(file,&msg)<=0) {
               Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to read message from file \"",Tcl_GetString(Objv[3]),"\"",(char*)NULL);
               return(TCL_ERROR);
            }
            if (!(sub=bufr_decode_message(msg,MetObs_GetTables()))) {
               Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to decode message to dataset",(char*)NULL);
               return(TCL_ERROR);
            }
            if (bufr_merge_dataset(set,bufr_count_datasubset(set)-1,sub,0,bufr_count_datasubset(sub))<0) {
               Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to merge message to dataset",(char*)NULL);
               return(TCL_ERROR);
            }
            bufr_free_dataset(sub);
            bufr_free_message(msg);
            fclose(file);
         }
         break;

      case WRITE:
         if (Objc<5) {
            Tcl_WrongNumArgs(Interp,1,Objv,"id file [BUFR|ASCII] [Compress]");
            return(TCL_ERROR);
         }
         if (!(set=MetDataset_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Invalid dataset \"",Tcl_GetString(Objv[2]),"\"",(char*)NULL);
            return(TCL_ERROR);
         }
         if (strcmp(Tcl_GetString(Objv[4]),"ASCII")==0) {
            if (bufr_dump_dataset(set,Tcl_GetString(Objv[3]))<0) {
               Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to write dataset to file \"",Tcl_GetString(Objv[3]),"\"",(char*)NULL);
               return(TCL_ERROR);
            }
         } else if (strcmp(Tcl_GetString(Objv[4]),"BUFR")==0) {
            co=0;
            if (Objc==6) {
               Tcl_GetBooleanFromObj(Interp,Objv[5],&co);
            }
            if (!(file=fopen(Tcl_GetString(Objv[3]),"w"))) {
               Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to open file for writing\"",Tcl_GetString(Objv[3]),"\"",(char*)NULL);
               return(TCL_ERROR);
            }
            if ((msg=bufr_encode_message(set,co))) {
               bufr_write_message(file,msg);
               bufr_free_message (msg);
            } else {
               Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to encode message to file \"",Tcl_GetString(Objv[3]),"\"",(char*)NULL);
               return(TCL_ERROR);
            }
            fclose(file);
         } else {
            Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Invalid file format, must be BUFR or ASCII",(char*)NULL);
            return(TCL_ERROR);
         }
         break;

      case DEFINE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,1,Objv,"id ?option?");
            return(TCL_ERROR);
         }
         return(MetDataset_Define(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,1,Objv,"id");
            return(TCL_ERROR);
         }
         if (MetDataset_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         TclY_HashAll(Interp,&MetDatasetTable);
         break;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <MetDataset_Define>
 * Creation : Mai 2008 J.P. Gauthier
 *
 * But      : Definition des donnees template
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Liste des arguments
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int MetDataset_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   Tcl_Obj         *obj=NULL,*lst;
   BUFR_Dataset    *set;
   BUFR_Datasubset *sset;
   BUFR_Template   *tmp;
   DataSubset      *subset;
   ListNode        *node;
   BufrDescriptor  *bcv,**pbcd;
   BUFR_Sequence   *bseq;
   BufrDDOp        *ddo=NULL;
   int              i,idx;
   int              s,c,ns,nc,code;
   int              f,x,y;
   int              err,skip;
   char             buf[32];

   static CONST char *sopt[] = {  "-BUFR_EDITION","-BUFR_MASTER_TABLE","-ORIG_CENTER","-ORIG_SUB_CENTER","-UPDATE_SEQUENCE","-DATA_CATEGORY","-INTERN_SUB_CATEGORY","-LOCAL_SUB_CATEGORY",
      "-MASTER_TABLE_VERSION","-LOCAL_TABLE_VERSION","-YEAR","-MONTH","-DAY","-HOUR","-MINUTE","-SECOND","-DATA_FLAG","-subsetnb","-subset","-subsetadd","-subsetadddescriptor","-subsetstart","-subsetend","-template",NULL };
   enum                opt { BUFR_EDITION,BUFR_MASTER_TABLE,ORIG_CENTER,ORIG_SUB_CENTER,UPDATE_SEQUENCE,DATA_CATEGORY,INTERN_SUB_CATEGORY,LOCAL_SUB_CATEGORY,
      MASTER_TABLE_VERSION,LOCAL_TABLE_VERSION,YEAR,MONTH,DAY,HOUR,MINUTE,SECOND,DATA_FLAG,SUBSETNB,SUBSET,SUBSETADD,SUBSETADDDESCRIPTOR,SUBSETSTART,SUBSETEND,TEMPLATE };

   set=MetDataset_Get(Name);
   if (!set) {
      Tcl_AppendResult(Interp,"\n   MetDataset_Define: Dataset id unknown: \"",Name,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }
      switch ((enum opt)idx) {
         case BUFR_EDITION:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(set->tmplte->edition));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&set->tmplte->edition);
            }
            break;

         case BUFR_MASTER_TABLE:
            if (Objc==1) {
               c=BUFR_GET_MASTER_TABLE(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_MASTER_TABLE(set,c);
            }
            break;

         case ORIG_CENTER:
            if (Objc==1) {
               c=BUFR_GET_ORIG_CENTRE(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_ORIG_CENTRE(set,c);
            }
            break;

         case ORIG_SUB_CENTER:
            if (Objc==1) {
               c=BUFR_GET_SUB_CENTRE(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_SUB_CENTRE(set,c);
            }
            break;

         case UPDATE_SEQUENCE:
            if (Objc==1) {
              c=BUFR_GET_UPD_SEQUENCE(set);
              Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_UPD_SEQUENCE(set,c);
            }
            break;

         case DATA_CATEGORY:
            if (Objc==1) {
               c=BUFR_GET_DATA_CATEGORY(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_DATA_CATEGORY(set,c);
            }
            break;

         case INTERN_SUB_CATEGORY:
            if (Objc==1) {
               c=BUFR_GET_INTERN_SUB_CAT(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_INTERN_SUB_CAT(set,c);
            }
            break;

         case LOCAL_SUB_CATEGORY:
            if (Objc==1) {
               c=BUFR_GET_LOCAL_SUB_CAT(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_LOCAL_SUB_CAT(set,c);
            }
            break;

         case MASTER_TABLE_VERSION:
            if (Objc==1) {
               c=BUFR_GET_MSTR_TBL_VRSN(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_MSTR_TBL_VRSN(set,c);
            }
            break;

         case LOCAL_TABLE_VERSION:
            if (Objc==1) {
               c=BUFR_GET_LOCAL_TBL_VRSN(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_LOCAL_TBL_VRSN(set,c);
            }
            break;

         case YEAR:
            if (Objc==1) {
               c=BUFR_GET_YEAR(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_YEAR(set,c);
            }
            break;

         case MONTH:
            if (Objc==1) {
               c=BUFR_GET_MONTH(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               TclY_Get0IntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_MONTH(set,c);
            }
            break;

         case DAY:
            if (Objc==1) {
               c=BUFR_GET_DAY(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               TclY_Get0IntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_DAY(set,c);
            }
            break;

         case HOUR:
            if (Objc==1) {
               c=BUFR_GET_HOUR(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               TclY_Get0IntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_HOUR(set,c);
            }
            break;

         case MINUTE:
            if (Objc==1) {
               c=BUFR_GET_MINUTE(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               TclY_Get0IntFromObj(Interp,Objv[++i],&c);
                BUFR_SET_MINUTE(set,c);
            }
            break;

         case SECOND:
            if (Objc==1) {
               c=BUFR_GET_SECOND(set);
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(c));
            } else {
               TclY_Get0IntFromObj(Interp,Objv[++i],&c);
               BUFR_SET_SECOND(set,c);
            }
            break;

         case DATA_FLAG:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(set->data_flag));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&set->data_flag);
            }
            break;

         case TEMPLATE:
            if (Objc==1) {
               if ((tmp=bufr_get_dataset_template(set))) {
                  if (!(obj=MetTemplate_Put(Interp,NULL,tmp))) {
                     return(TCL_ERROR);
                  }
               }
               Tcl_SetObjResult(Interp,obj);
            } else {
            }
            break;

         case SUBSETNB:
            if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(bufr_count_datasubset(set)));
            } else {
            }
            break;

         case SUBSET:
            if (Objc<1 || Objc>4) {
               Tcl_WrongNumArgs(Interp,1,Objv,"[subset index] [code index] [value]");
               return(TCL_ERROR);
            }
            if (Objc==1) {
                lst=Tcl_NewListObj(0,NULL);
                for(s=0;s<bufr_count_datasubset(set);s++) {
                  subset=bufr_get_datasubset(set,s);
                  for (c=0;c<bufr_datasubset_count_descriptor(subset);c++) {
                     Tcl_ListObjAppendElement(Interp,lst,MetDataset_Code2Obj(Interp,bufr_datasubset_get_descriptor(subset,c)));
                  }
               }
               Tcl_SetObjResult(Interp,lst);

            } else {

               Tcl_GetIntFromObj(Interp,Objv[++i],&ns);
               if (ns<0 || ns>=bufr_count_datasubset(set)) {
                  Tcl_AppendResult(Interp,"Invalid subset index",(char*)NULL);
                  return(TCL_ERROR);
               }
               subset=bufr_get_datasubset(set,ns);
               if (Objc==2) {
                  lst=Tcl_NewListObj(0,NULL);
                  for (c=0;c<bufr_datasubset_count_descriptor(subset);c++) {
                     Tcl_ListObjAppendElement(Interp,lst,MetDataset_Code2Obj(Interp,bufr_datasubset_get_descriptor(subset,c)));
                  }
                  Tcl_SetObjResult(Interp,lst);
               } else {
                  Tcl_GetIntFromObj(Interp,Objv[++i],&nc);
                  if (nc<0 || nc>=bufr_datasubset_count_descriptor(subset)) {
                     Tcl_AppendResult(Interp,"Invalid code index",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  if (Objc==3) {
                     Tcl_SetObjResult(Interp,MetDataset_Code2Obj(Interp,bufr_datasubset_get_descriptor(subset,nc)));
                  } else {
                     return(MetDataset_Obj2Code(Interp,bufr_datasubset_get_descriptor(subset,nc),Objv[++i]));
                  }
               }
            }
            break;

         case SUBSETADD:
            if (Objc!=2) {
               Tcl_WrongNumArgs(Interp,1,Objv,"[code/value list]");
               return(TCL_ERROR);
            } else {
               Tcl_ListObjLength(Interp,Objv[++i],&nc);
               Tcl_ListObjIndex(Interp,Objv[i],0,&obj);

               bseq=bufr_create_sequence(NULL);
               pbcd=(BufrDescriptor**)arr_get(set->tmplte->gabarit,0);
               for (c=0;c<arr_count(set->tmplte->gabarit);c++) {
                  bufr_add_descriptor_to_sequence(bseq,bufr_dupl_descriptor(pbcd[c]));
               }
               ddo=bufr_apply_Tables(NULL,bseq,set->tmplte,NULL,&f);
               if (!ddo) {
                  sprintf(buf,"%i",f);
                  Tcl_AppendResult(Interp,"Error while applying BUFR tables (libECBUFR error code: ",buf,")",(char*)NULL);
                  return(TCL_ERROR);
               }
               node=lst_firstnode(bseq->list);

               for(c=0;c<nc;c++) {
                  Tcl_ListObjIndex(Interp,Objv[i],c,&lst);
                  Tcl_ListObjIndex(Interp,lst,0,&obj);
                  TclY_Get0IntFromObj(Interp,obj,&code);
                  while (node) {
                     bcv=(BufrDescriptor*)node->data;
                     if ((code!=bcv->descriptor)&&(bcv->flags & FLAG_SKIPPED)) {
                        node=lst_nextnode(node);
                     } else {
                        break;
                     }
                  }
                  if (!node) {
                     Tcl_AppendResult(Interp,"Too many code for template",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  if ((code==bcv->descriptor)&&(bcv->flags & FLAG_SKIPPED)) {
                     node=lst_nextnode(node);
                     continue;
                  }
                  if (code!=bcv->descriptor) {
                     sprintf(buf," %06i",bcv->descriptor);
                     Tcl_AppendResult(Interp,"Mismatch between data and template, code ",Tcl_GetString(obj)," should have been ",buf,(char*)NULL);
                     return(TCL_ERROR);
                  }
                  bufr_descriptor_to_fxy(bcv->descriptor,&f,&x,&y);
                  ddo->current=node;

                  MetDataset_Obj2Code(Interp,bcv,lst);

                  if (bcv->flags & FLAG_CLASS31) {
                     err=0;
                     bufr_expand_node_descriptor(bseq->list,lst_prevnode(node),OP_EXPAND_DELAY_REPL|OP_ZDRC_IGNORE,set->tmplte->tables,&skip,&err);
                     if (err) {
                        fprintf(stdout,"(WARNING) MetDataset_Define: invalid node descriptor (err=%i)\n",err);
                        set->data_flag |= BUFR_FLAG_INVALID;
                     }

                     /*See if data present bitmap count matched with data code list*/
                     if ((((BufrDescriptor*)(lst_nextnode(node))->data)->descriptor==31031)&&(ddo->dpbm)) {
                        int   nb31;

                        nb31=bufr_descriptor_get_ivalue(bcv);
                        if (nb31!=ddo->dpbm->nb_codes) {
                           fprintf(stdout,"(WARNING) MetDataset_Define: DP node rcount invalid for 31031 (DPBM 31001=%d NBCODES=%d)\n",nb31,ddo->dpbm->nb_codes);
                        }
                     }
                  }
                  node=lst_nextnode(node);
               }
               bufr_add_datasubset(set,bseq,ddo);
               bufr_free_BufrDDOp(ddo);
            }
            break;

         case SUBSETSTART:
            sset=(BUFR_Datasubset*)malloc(sizeof(BUFR_Datasubset));
            MetDatasubset_Put(Interp,Name,sset);

            sset->BSeq=bufr_create_sequence(NULL);
            pbcd=(BufrDescriptor**)arr_get(set->tmplte->gabarit,0);
            for (c=0;c<arr_count(set->tmplte->gabarit);c++) {
               bufr_add_descriptor_to_sequence(sset->BSeq,bufr_dupl_descriptor(pbcd[c]));
            }
            sset->BDDO=bufr_apply_Tables(NULL,sset->BSeq,set->tmplte,NULL,&f);
            sset->Node=lst_firstnode(sset->BSeq->list);

            if (!sset->BDDO) {
               sprintf(buf,"%i",f);
               Tcl_AppendResult(Interp,"Error while applying BUFR tables (libECBUFR error code: ",buf,")",(char*)NULL);
               return(TCL_ERROR);
            }
            break;

         case SUBSETEND:
            if ((sset=MetDatasubset_Get(Name))) {
               bufr_add_datasubset(set,sset->BSeq,sset->BDDO);
               bufr_free_BufrDDOp(sset->BDDO);
               TclY_HashDel(&MetDatasubsetTable,Name);
               free(sset);
            }
            break;

         case SUBSETADDDESCRIPTOR:
            if (Objc<2) {
               Tcl_WrongNumArgs(Interp,1,Objv,"[code/value list]");
               return(TCL_ERROR);
            } else {
               sset=MetDatasubset_Get(Name);
               if (!sset) {
                  Tcl_AppendResult(Interp,"\n   MetDataset_Define: No new subset initialized",(char*)NULL);
                  return(TCL_ERROR);
               }

               for(++i;i<Objc;i++) {
                  Tcl_ListObjIndex(Interp,Objv[i],0,&obj);
                  TclY_Get0IntFromObj(Interp,obj,&code);

                  while (sset->Node) {
                     bcv=(BufrDescriptor*)sset->Node->data;
                     if ((code!=bcv->descriptor)&&(bcv->flags & FLAG_SKIPPED)) {
                        sset->Node=lst_nextnode(sset->Node);
                     } else {
                        break;
                     }
                  }
                  if (!sset->Node) {
                     Tcl_AppendResult(Interp,"Too many code for template",(char*)NULL);
                     return(TCL_ERROR);
                  }
                  if ((code==bcv->descriptor)&&(bcv->flags & FLAG_SKIPPED)) {
                     sset->Node=lst_nextnode(sset->Node);
                     continue;
                  }
                  if (code!=bcv->descriptor) {
                     sprintf(buf," %06i",bcv->descriptor);
                     Tcl_AppendResult(Interp,"Mismatch between data and template, code ",Tcl_GetString(obj)," should have been ",buf,(char*)NULL);
                     return(TCL_ERROR);
                  }
                  bufr_descriptor_to_fxy(bcv->descriptor,&f,&x,&y);
                  sset->BDDO->current=sset->Node;

                  MetDataset_Obj2Code(Interp,bcv,Objv[i]);

                  if (bcv->flags & FLAG_CLASS31) {
                     err=0;
                     bufr_expand_node_descriptor(sset->BSeq->list,lst_prevnode(sset->Node),OP_EXPAND_DELAY_REPL|OP_ZDRC_IGNORE,set->tmplte->tables,&skip,&err);
                     if (err) {
                        fprintf(stdout,"(WARNING) MetDataset_Define: invalid node descriptor (err=%i)\n",err);
                        set->data_flag |= BUFR_FLAG_INVALID;
                     }

                     /*See if data present bitmap count matched with data code list*/
                     if ((((BufrDescriptor*)(lst_nextnode(sset->Node))->data)->descriptor==31031)&&(sset->BDDO->dpbm)) {
                        int nb31;

                        nb31=bufr_descriptor_get_ivalue(bcv);
                        if (nb31!=sset->BDDO->dpbm->nb_codes) {
                           fprintf(stdout,"(WARNING) MetDataset_Define: DP node rcount invalid for 31031 (DPBM 31001=%d NBCODES=%d)\n",nb31,sset->BDDO->dpbm->nb_codes);
                        }
                     }
                  }
                  sset->Node=lst_nextnode(sset->Node);
               }
            }
            break;
       }
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetDataset_FreeHash>
 * Creation     : Mai 2008 J.P. Gauthier
 *
 * But          : Destruction d'un dataset a partir de son nom.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'observation a detruire
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int MetDataset_FreeHash(Tcl_Interp *Interp,char *Name) {

   BUFR_Dataset *set=NULL;

   if ((set=(BUFR_Dataset*)TclY_HashDel(&MetDatasetTable,Name))) {
      bufr_free_dataset(set);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetDataset_Get>
 * Creation     : Mai 2008 J.P. Gauthier
 *
 * But          : Obtenir un objet dataset en fonction de son nom
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet template a obtenir.
 *
 * Retour       : Une structure BUFRDataset ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
BUFR_Dataset* MetDataset_Get(char *Name) {
   return((BUFR_Dataset*)TclY_HashGet(&MetDatasetTable,Name));
}

FILE* MetDatafile_Get(char *Name) {
   return((FILE*)TclY_HashGet(&MetDatafileTable,Name));
}

BUFR_Datasubset* MetDatasubset_Get(char *Name) {
   return((BUFR_Datasubset*)TclY_HashGet(&MetDatasubsetTable,Name));
}

int MetDatafile_Put(Tcl_Interp *Interp,char *Name,FILE *File) {
   return(TclY_HashSet(Interp,&MetDatafileTable,Name,File));
}

int MetDatasubset_Put(Tcl_Interp *Interp,char *Name,BUFR_Datasubset *SSet) {
   return(TclY_HashSet(Interp,&MetDatasubsetTable,Name,SSet));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetDataset_Put>
 * Creation     : Mai 2008 J.P. Gauthier
 *
 * But          : Creation d'un objet dataset et insertion d'un nouveau nom dans la table.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'observation a creer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Tcl_Obj* MetDataset_Put(Tcl_Interp *Interp,char *Name,BUFR_Dataset *Set) {

   char buf[64];

   if (Set) {
      if (!Name) {
         sprintf(buf,"BUFRDATASET_____%li",MetDatasetNo++);
         Name=buf;
      }
      if (TclY_HashSet(Interp,&MetDatasetTable,Name,Set)==TCL_ERROR) {
         return(NULL);
      }
//      Ref->Name=strdup(Name);

      return(Tcl_NewStringObj(Name,-1));
   } else {
      if (Interp)
         Tcl_AppendResult(Interp,"\n   MetDataset_Put: Invalid dataset: \"",Name, "\"",(char *)NULL);
      return(NULL);
   }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetDataset_Code2Obj>
 * Creation     : Mai 2008 J.P. Gauthier
 *
 * But          : Mettre en liste Tcl les donnees d'un code BUFR
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <BCV>      : Code BUFR
 *
 * Retour       : Code de retour Tcl
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
 */
Tcl_Obj* MetDataset_Value2Obj(BufrValue *V) {

   Tcl_Obj *obj=NULL;
   char    *str;
   float    fval;
   double   dval;
   int      ival;
   long     lval;
   int      len;

   if (V) {

      switch (V->type) {
         case VALTYPE_STRING :
            str=bufr_value_get_string(V,&len);
            if (str) {
               obj=Tcl_NewStringObj(str,-1);
            } else {
               obj=Tcl_NewStringObj("MSNG",-1);
            }
            break;
         case VALTYPE_INT8:
         case VALTYPE_INT32:
            ival=bufr_value_get_int32(V);
            if (ival==-1) {
               obj=Tcl_NewStringObj("MSNG",-1);
            } else {
               obj=Tcl_NewIntObj(ival);
            }
            break;

         case VALTYPE_INT64  :
            lval=bufr_value_get_int64(V);
            if (lval==-1) {
               obj=Tcl_NewStringObj("MSNG",-1);
            } else {
               obj=Tcl_NewLongObj(lval);
            }
            break;

         case VALTYPE_FLT32  :
            fval=bufr_value_get_float(V);
            if (bufr_is_missing_float(fval)) {
               obj=Tcl_NewStringObj("MSNG",-1);
            } else {
               obj=Tcl_NewDoubleObj(lval);
            }
            break;

         case VALTYPE_FLT64  :
            dval=bufr_value_get_double(V);
            if (bufr_is_missing_double(dval)) {
               obj=Tcl_NewStringObj("MSNG",-1);
            } else {
               obj=Tcl_NewDoubleObj(lval);
            }
      }
   }
   return(obj);
}

Tcl_Obj* MetDataset_Code2Obj(Tcl_Interp *Interp,BufrDescriptor *BCV) {

   Tcl_Obj *obj;

   obj=Tcl_NewListObj(0,NULL);
   Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(BCV->descriptor));
   if (!(BCV->flags & FLAG_SKIPPED)) {
      /*if there is a alias descriptor (Data Present Bitmap Operator) */
      if (BCV->s_descriptor) {
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewIntObj(BCV->s_descriptor));
      }
      /*if there are Meta Data defined
      if (BCV->meta) {
         char     buf[256];
         bufr_print_metadata(buf,BCV->meta);
         Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(buf,strlen(buf)));
      }*/
      if (BCV->value) {
         Tcl_ListObjAppendElement(Interp,obj,MetDataset_Value2Obj(BCV->value));
         if (BCV->value->af) {
//            Tcl_ListObjAppendElement(Interp,obj,Tcl_NewByteArrayObj(BCV->value->af->bits,8));
         }

      }
   }
   return(obj);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetDataset_Obj2Code>
 * Creation     : Mai 2008 J.P. Gauthier
 *
 * But          : Mettre en code BUFR les donnees d'une liste Tcl
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <BCV>      : Code BUFR
 *   <Obj>      : Objet liste Tcl { code value [bits] }
 *
 * Retour       : Code de retour Tcl
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
 */
int MetDataset_Obj2Code(Tcl_Interp *Interp,BufrDescriptor *BCV,Tcl_Obj *Obj) {

   Tcl_Obj *obj;
   int      n;
   double   dval;

   Tcl_ListObjLength(Interp,Obj,&n);
   if (!n) {
      Tcl_AppendResult(Interp,"Invalid code/value pair",(char*)NULL);
   }

   if (!BCV->value)
      BCV->value=bufr_mkval_for_descriptor(BCV);

   if (BCV->value && n>1) {
      Tcl_ListObjIndex(Interp,Obj,1,&obj);
      switch(BCV->value->type) {
         case VALTYPE_STRING :
            bufr_descriptor_set_svalue(BCV,Tcl_GetString(obj));
         break;

         case VALTYPE_INT8 :
         case VALTYPE_INT32 :
         case VALTYPE_INT64 :
            if (strcmp(Tcl_GetString(obj),"MSNG")==0) {
               bufr_descriptor_set_ivalue(BCV,-1);
            } else if (Tcl_GetDoubleFromObj(Interp,obj,&dval)==TCL_OK) {
               bufr_descriptor_set_ivalue(BCV,(int)dval);
            }
            break;

         case VALTYPE_FLT64  :
            if (strcmp(Tcl_GetString(obj),"MSNG")==0) {
               bufr_descriptor_set_dvalue(BCV,bufr_get_max_double());
            } else if (Tcl_GetDoubleFromObj(Interp,obj,&dval)==TCL_OK) {
               if (!bufr_is_missing_double(dval)) {
                  bufr_descriptor_set_dvalue(BCV,dval);
               }
            }
            break;

         case VALTYPE_FLT32  :
            if (strcmp(Tcl_GetString(obj),"MSNG")==0) {
               bufr_descriptor_set_fvalue(BCV,bufr_get_max_float());
            } else if (Tcl_GetDoubleFromObj(Interp,obj,&dval)==TCL_OK) {
               if (!bufr_is_missing_float(dval)) {
                  bufr_descriptor_set_fvalue(BCV,dval);
               }
            }
            break;
      }

      /*AF bits*/
      if (n>2 && BCV->value->af) {
         Tcl_ListObjIndex(Interp,Obj,2,&obj);
         Tcl_GetLongFromObj(Interp,obj,&BCV->value->af->bits);
         fprintf(stdout,"---- %li\n",BCV->value->af->bits);
     }

      /*?????*/
      if (BCV->descriptor==223255) {
         bufr_print_debug("223255\n");
      }
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetTemplate_Cmd>
 * Creation     : Mam 2008 J.P. Gauthier
 *
 * But          : Effectuer les commandes mettemplate du package
 *
 * Parametres    :
 *   <clientData>: Nom de la template
 *   <interp>    : Interpreteur Tcl
 *   <Objc>      : Nombre d'arguments
 *   <Objv>      : Pointeur sur la liste des arguments (word)
 *
 * Retour        : Code de retour standard TCL
 *
 * Remarques     :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
BUFR_Template* MetTemplate_CreateFromObj(Tcl_Interp *Interp,Tcl_Obj *Obj,int Edition) {

   BUFR_Template *tmp;
   BufrDescValue *code;
   EntryTableB   *e;
   ValueType     vtype;
   int           vlen,nc,c,v;
   Tcl_Obj      *obj,*lst,*val;
   double        dval;
   int           ival;
   long          lval;

   Tcl_ListObjLength(Interp,Obj,&nc);
   code=(BufrDescValue*)calloc(nc,sizeof(BufrDescValue));

   for(c=0;c<nc;c++) {
      // Extract the code
      Tcl_ListObjIndex(Interp,Obj,c,&lst);
      Tcl_ListObjIndex(Interp,lst,0,&obj);
      TclY_Get0IntFromObj(Interp,obj,&code[c].descriptor);

      // Extract the values
      Tcl_ListObjLength(Interp,Obj,&v);
      code[c].nbval=0;
      if (v>1) {
         Tcl_ListObjIndex(Interp,lst,1,&obj);
         Tcl_ListObjLength(Interp,obj,&code[c].nbval);
      }
      if (!code[c].nbval) {
         code[c].values=NULL;
         continue;
      }
      code[c].values=(BufrValue**)calloc(code[c].nbval,sizeof(BufrValue*));
      for(v=0;v<code[c].nbval;v++) {
         Tcl_ListObjIndex(Interp,obj,1,&val);

         vtype=bufr_datatype_to_valtype(bufr_descriptor_to_datatype(MetObs_GetTables(),e,code[c].descriptor,&vlen),32,0);

         switch(vtype) {
            case VALTYPE_STRING :
               code[c].values[v]=bufr_create_value(vtype);
               bufr_value_set_string(code[c].values[v],Tcl_GetString(val),vlen);
               break;
            case VALTYPE_INT64 :
               code[c].values[v]=bufr_create_value(vtype);
               Tcl_GetLongFromObj(Interp,val,&lval);
               bufr_value_set_int64(code[c].values[v],lval);
               break;
            case VALTYPE_INT32  :
               code[c].values[v]=bufr_create_value(vtype);
               Tcl_GetIntFromObj(Interp,val,&ival);
               bufr_value_set_int32(code[c].values[v],ival);
               break;
            case VALTYPE_FLT64  :
            case VALTYPE_FLT32  :
               if (strcmp(Tcl_GetString(Obj),"MSNG")!= 0) {
                  Tcl_GetDoubleFromObj(Interp,val,&dval);
                  if (!bufr_is_missing_float(dval)) {
                     code[c].values[v]=bufr_create_value(vtype);
                     bufr_value_set_float(code[c].values[v],dval);
                  }
               }
               break;
            default :
               break;
         }
      }
   }

   tmp=bufr_create_template(code,nc,MetObs_GetTables(),Edition);
   return(tmp);
}

static int MetTemplate_Cmd(ClientData clientData,Tcl_Interp *Interp,int Objc,Tcl_Obj *CONST Objv[]) {

   BUFR_Template *tmp,*new;

   int         idx,ed;
   static CONST char *sopt[] = { "create","free","read","write","define","is","all",NULL };
   enum               opt { CREATE,FREE,READ,WRITE,DEFINE,IS,ALL };

   Tcl_ResetResult(Interp);

   if (Objc<2) {
      Tcl_WrongNumArgs(Interp,1,Objv,"command ?arg arg ...?");
      return(TCL_ERROR);
   }

   if (Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx)!=TCL_OK) {
      return(TCL_ERROR);
   }

   switch ((enum opt)idx) {
      case CREATE:
         if(Objc!=4 && Objc!=5)  {
            Tcl_WrongNumArgs(Interp,1,Objv,"template [file|edition list]");
            return(TCL_ERROR);
         }

         if (Objc==5) {
            Tcl_GetIntFromObj(Interp,Objv[3],&ed);
            if (!(tmp=MetTemplate_CreateFromObj(Interp,Objv[4],ed))) {
               Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to create template",(char*)NULL);
               return(TCL_ERROR);
            }
         } else {
            if ((!(tmp=bufr_load_template(Tcl_GetString(Objv[3]),MetObs_GetTables())))) {
               Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to read template file \"",Tcl_GetString(Objv[3]),"\"",(char*)NULL);
               return(TCL_ERROR);
            }
         }
         if (!MetTemplate_Put(Interp,Tcl_GetString(Objv[2]),tmp)) {
             return(TCL_ERROR);
         }
         break;

      case FREE:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,1,Objv,"template");
            return(TCL_ERROR);
         }
         break;

      case READ:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,1,Objv,"template file");
            return(TCL_ERROR);
         }
         if ((!(new=bufr_load_template(Tcl_GetString(Objv[3]),MetObs_GetTables())))) {
            Tcl_AppendResult(Interp,"\n   MetDataset_Cmd :  Unable to read template file \"",Tcl_GetString(Objv[3]),"\"",(char*)NULL);
            return(TCL_ERROR);
         }
         tmp=TclY_HashReplace(Interp,&MetTemplateTable,Tcl_GetString(Objv[2]),(void*)new);
         if (tmp) {
            bufr_free_template(tmp);
         }
         break;

      case WRITE:
         if(Objc!=4) {
            Tcl_WrongNumArgs(Interp,1,Objv,"template file");
            return(TCL_ERROR);
         }
         if (!(tmp=MetTemplate_Get(Tcl_GetString(Objv[2])))) {
            Tcl_AppendResult(Interp,"\n   MetTemplatet_Cmd :  Invalid template \"",Tcl_GetString(Objv[2]),"\"",(char*)NULL);
            return(TCL_ERROR);
         }
         if (bufr_save_template(Tcl_GetString(Objv[3]),tmp)<0) {
            Tcl_AppendResult(Interp,"\n   MetTemplatet_Cmd :  Problem while writing template \"",Tcl_GetString(Objv[2]),"\"",(char*)NULL);
            return(TCL_ERROR);
         }
         break;

      case DEFINE:
         if(Objc<3) {
            Tcl_WrongNumArgs(Interp,1,Objv,"id ?option?");
            return(TCL_ERROR);
         }
         return(MetTemplate_Define(Interp,Tcl_GetString(Objv[2]),Objc-3,Objv+3));
         break;

      case IS:
         if(Objc!=3) {
            Tcl_WrongNumArgs(Interp,1,Objv,"id");
            return(TCL_ERROR);
         }
         if (MetTemplate_Get(Tcl_GetString(Objv[2]))) {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(1));
         } else {
            Tcl_SetObjResult(Interp,Tcl_NewBooleanObj(0));
         }
         break;

      case ALL:
         TclY_HashAll(Interp,&MetTemplateTable);
         break;
   }
   return(TCL_OK);
}

/*----------------------------------------------------------------------------
 * Nom      : <MetTemplate_Define>
 * Creation : Mai 2008 J.P. Gauthier
 *
 * But      : Definition des donnees template
 *
 * Parametres :
 *  <Interp>  : Interpreteur Tcl
 *  <Name>    : Nom de la projection
 *  <Objc>    : Nombre d'arguments
 *  <Objv>    : Liste des arguments
 *
 * Retour     :
 *  <TCL_...> : Code de retour standard TCL
 *
 * Remarques :
 *
 *----------------------------------------------------------------------------
*/
static int MetTemplate_Define(Tcl_Interp *Interp,char *Name,int Objc,Tcl_Obj *CONST Objv[]){

   BUFR_Template *tmp;
   BufrDescValue *code;
   int            i,idx,c,v;
   Tcl_Obj       *obj,*cd,*vl;

   static CONST char *sopt[] = { "-DESCRIPTOR","-BUFR_EDITION",NULL };
   enum                opt { DESCRIPTOR,BUFR_EDITION };

   tmp=MetTemplate_Get(Name);
   if (!tmp) {
      Tcl_AppendResult(Interp,"\n   MetTemplate_Define: Template id unknown: \"",Name,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

   for (i=0;i<Objc;i++) {

      if (Tcl_GetIndexFromObj(Interp,Objv[i],sopt,"option",0,&idx)!=TCL_OK) {
         return(TCL_ERROR);
      }

      switch ((enum opt)idx) {
         case BUFR_EDITION:
            if (Objc!=1 && Objc!=2) {
               Tcl_WrongNumArgs(Interp,1,Objv,"[edition]");
               return(TCL_ERROR);
            } else if (Objc==1) {
               Tcl_SetObjResult(Interp,Tcl_NewIntObj(tmp->edition));
            } else {
               Tcl_GetIntFromObj(Interp,Objv[++i],&tmp->edition);
            }
            break;

         case DESCRIPTOR:
            if (Objc!=1 && Objc!=2 && Objc!=3) {
               Tcl_WrongNumArgs(Interp,1,Objv,"[index] [code/value]");
               return(TCL_ERROR);
            } else if (Objc==1) {
               obj=Tcl_NewListObj(0,NULL);
               for (c=0;c<arr_count(tmp->codets);c++) {
                  code=(BufrDescValue*)arr_get(tmp->codets,c);
                  cd=Tcl_NewListObj(0,NULL);
                  vl=Tcl_NewListObj(0,NULL);
                  for (v=0;v<code->nbval;v++) {
                     Tcl_ListObjAppendElement(Interp,vl,MetDataset_Value2Obj(code->values[v]));
                  }
                  Tcl_ListObjAppendElement(Interp,cd,Tcl_NewIntObj(code->descriptor));
                  Tcl_ListObjAppendElement(Interp,cd,vl);
                  Tcl_ListObjAppendElement(Interp,obj,cd);
               }
               Tcl_SetObjResult(Interp,obj);
            } else if (Objc>1) {
               Tcl_GetIntFromObj(Interp,Objv[++i],&c);
               if (c<0 || c>=arr_count(tmp->codets)) {
                  Tcl_AppendResult(Interp,"\n   MetTemplate_Define : Invalid code index",(char*)NULL);
                  return(TCL_ERROR);
               }
               code=(BufrDescValue *)arr_get(tmp->codets,c);

               if (Objc==2) {
                  cd=Tcl_NewListObj(0,NULL);
                  vl=Tcl_NewListObj(0,NULL);
                  for (v=0;v<code->nbval;v++) {
                     Tcl_ListObjAppendElement(Interp,vl,MetDataset_Value2Obj(code->values[v]));
                  }
                  Tcl_ListObjAppendElement(Interp,cd,Tcl_NewIntObj(code->descriptor));
                  Tcl_ListObjAppendElement(Interp,cd,vl);
                  Tcl_SetObjResult(Interp,cd);
               } else {
               }
            }
            break;
      }
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetTemplate_FreeHash>
 * Creation     : Mai 2008 J.P. Gauthier
 *
 * But          : Destruction d'une template a partir de son nom.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'observation a detruire
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int MetTemplate_FreeHash(Tcl_Interp *Interp,char *Name) {

   BUFR_Template *tmp=NULL;

   if ((tmp=(BUFR_Template*)TclY_HashDel(&MetTemplateTable,Name))) {
      bufr_free_template(tmp);
   }
   return(TCL_OK);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MeTemplate_Get>
 * Creation     : Mai 2008 J.P. Gauthier
 *
 * But          : Obtenir un objet mettemplate en fonction de son nom dans la table de Tcl MetTemplateTable
 *
 * Parametres   :
 *   <Name>     : Nom de l'objet template a obtenir.
 *
 * Retour       : Une structure TTemplate ou un pointeur NULL si rien trouve.
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
BUFR_Template* MetTemplate_Get(char *Name) {
   return((BUFR_Template*)TclY_HashGet(&MetTemplateTable,Name));
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetDataset_Put>
 * Creation     : Mai 2008 J.P. Gauthier
 *
 * But          : Creation d'un objet dataset et insertion d'un nouveau nom dans la table.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Name>     : Nom de l'observation a creer
 *
 * Retour       : Code de retour standard TCL
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
Tcl_Obj* MetTemplate_Put(Tcl_Interp *Interp,char *Name,BUFR_Template *Tmp) {

   char buf[64];

   if (Tmp) {
      if (!Name) {
         sprintf(buf,"BUFRTEMPLATE_____%li",MetTemplateNo++);
         Name=buf;
      }
      if (TclY_HashSet(Interp,&MetTemplateTable,Name,Tmp)==TCL_ERROR) {
         return(NULL);
      }

      return(Tcl_NewStringObj(Name,-1));
   } else {
      if (Interp)
         Tcl_AppendResult(Interp,"\n   MetTemplate_Put: Invalid template: \"",Name, "\"",(char *)NULL);
      return(NULL);
   }
}
