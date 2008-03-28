/*=============================================================================
* Environnement Canada
* Centre Meteorologique Canadian
* 2100 Trans-Canadienne
* Dorval, Quebec
*
* Projet    : Librairie Tcl de fichiers standards.
* Fichier   : GRIB_Field.c
* Creation  : Aout 1998 - J.P. Gauthier - CMC/CMOE
*
* Description: Manipulation de champs standards RPN dans des scripts Tcl.
*
* Remarques :
*
*==============================================================================
*/

#include "tclGRIB.h"
#include "Projection.h"

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FieldSet>
 * Creation : Janvier 2007 - J.P. Gauthier - CMC/CMOE
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
void GRIB_FieldSet(TData *Data){

   GRIB_Head *head;

   if (Data->Head)
      free(Data->Head);

   head=(GRIB_Head*)malloc(sizeof(GRIB_Head));

   /*Initialiser les parametres de definition du champs*/
   Data->Head=head;
   Data->Set=GRIB_FieldSet;
   Data->Free=GRIB_FieldFree;
   Data->Copy=GRIB_HeadCopy;
   Data->Grid=GRIB_Grid;
   Data->ReadCube=NULL;
}

void GRIB_HeadCopy(void *To,void *From) {
   memcpy(To,From,sizeof(GRIB_Head));
}

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_Grid>
 * Creation : Janvier 2007 - J.P. Gauthier - CMC/CMOE
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
Vect3d* GRIB_Grid(TData *Field,void *Proj) {

   GRIB_Head *head=(GRIB_Head*)Field->Head;
   Coord      coord;
   float      flat,flon,fele;
   int        i,j,k,idx,ni,nj,nk,ip1;
   int        idxi,idxk;

   /*Verifier la validite de la grille*/
   if (!Field->Ref)
      return(NULL);

   if (Field->Ref->Pos)
      return(Field->Ref->Pos);

   if (Field->Ref->Grid[0]=='V') {
      /*Localiser les point de grille dans l'espace*/
      Field->Ref->Pos=(Vect3d*)malloc(FSIZE2D(Field->Def)*sizeof(Vect3d));
      if (!Field->Ref->Pos) {
         fprintf(stderr,"(ERROR) FSTD_Grid: Not enough memory to calculate gridpoint location");
         return(NULL);
      }

      for (j=0;j<Field->Def->NJ;j++) {

         for (i=0;i<Field->Def->NI;i++) {
            flat=coord.lat=Field->Ref->Lat[i];
            flon=coord.lon=CLAMPLON(Field->Ref->Lon[i]);
            idx=j*Field->Def->NI+i;
            coord.elev=Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[j]);

            if (Proj) {
               ((Projection*)Proj)->Type->Project(((Projection*)Proj)->Params,&coord,&Field->Ref->Pos[idx],1);
            } else {
               Vect_Init(Field->Ref->Pos[idx],Field->Ref->Lat[i],Field->Ref->Lon[i],coord.elev);
            }
         }
      }
   } else {

      /*Localiser les point de grille dans l'espace*/
      Field->Ref->Pos=(Vect3d*)malloc(FSIZE3D(Field->Def)*sizeof(Vect3d));
      if (!Field->Ref->Pos) {
         fprintf(stderr,"(ERROR) GRIB_Grid: Not enough memory to calculate gridpoint location");
         return(NULL);
      }

      for (k=0;k<Field->Def->NK;k++) {

         /*For every gridpoints*/
         for (j=0;j<Field->Def->NJ;j++) {
            for (i=0;i<Field->Def->NI;i++) {

               /*Figure out table plane indexes*/
               idxi=j*Field->Def->NI+i;
               idxk=k*Field->Def->NI*Field->Def->NJ+idxi;

               Field->Ref->Project(Field->Ref,i,j,&coord.lat,&coord.lon,0,1);
               coord.elev=Data_Level2Meter(Field->Ref->LevelType,Field->Ref->Levels[k]);
               Vect_Init(Field->Ref->Pos[idxk],coord.lon,coord.lat,coord.elev);
            }
         }
      }
      if (((Projection*)Proj)->Type->Def==PROJPLANE && ((Projection*)Proj)->Params->Ref && ((Projection*)Proj)->Params->Ref->Id==Field->Ref->Id) {
         FSTD_Project(((Projection*)Proj),Field->Ref->Pos,NULL,FSIZE3D(Field->Def));
      } else {
         ((Projection*)Proj)->Type->Project(((Projection*)Proj)->Params,Field->Ref->Pos,NULL,FSIZE3D(Field->Def));
      }
   }
   return(Field->Ref->Pos);
}
/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FieldFree>
 * Creation : Janvier 2007 - J.P. Gauthier - CMC/CMOE
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
void GRIB_FieldFree(TData *Data){

   GRIB_Head *head=(GRIB_Head*)Data->Head;

   if (Data) {
      if (head) {
//         grib_handle_delete(head->Handle);
         free(head);
      }
   }
 }

/*----------------------------------------------------------------------------
 * Nom      : <GRIB_FieldRead>
 * Creation : Janvier 2007 - J.P. Gauthier - CMC/CMOE
 *
 * But      : Lit et stocke un enregistrement en memoire de meme qu'initialise
 *            les valeurs de configuraiton par defaut.
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
int GRIB_FieldRead(Tcl_Interp *Interp,char *Name,char *File,int Key) {

   TData         *field=NULL;
   GRIB_File     *file=NULL;
   GRIB_Head      head;
   grib_keys_iterator *iter;

   int         err=0;
   long        lval,i,ni,nj,nk=1,date,time;
   size_t      len;
   double      dval;
   char        sval[512];
   double      mtx[6],inv[6],*data;

   /*Get the file*/
   file=GRIB_FileGet(File);
   if (!file) {
      Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Invalid file \"",File,"\"",(char*)NULL);
      return(TCL_ERROR);
   }

  /*Move to right offset within the file*/
   if (Key>file->Size) {
      Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Invalid field index",(char*)NULL);
      return(TCL_ERROR);
   }
   fseek(file->Handle,Key,SEEK_SET);

   /*Get GRIB handle to message*/
   head.Handle=grib_handle_new_from_file(0,file->Handle,&err);
   if (err!=GRIB_SUCCESS) {
      Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Could not get field handle",(char*)NULL);
      return(TCL_ERROR);
   }

   /*Figue out valid time*/
   grib_get_long(head.Handle,"date",&date);
   grib_get_long(head.Handle,"time",&time);
   head.Valid=System_DateTime2Seconds(date,time*100,1);

   /*Get message info*/
   err=grib_get_long(head.Handle,"numberOfPointsAlongAParallel",&ni);
   err=grib_get_long(head.Handle,"numberOfPointsAlongAMeridian",&nj);
//   err=grib_get_long(head.Handle,"numberOfVerticalCoordinateValues",&nk);

  /*Verifier si le champs existe et est valide*/
   field=Data_Valid(Interp,Name,ni,nj,nk,1,TD_Float32);
   if (!field) {
      return(TCL_ERROR);
   }

   len=FSIZE3D(field->Def);
   data=malloc(len*sizeof(double));
   if ((grib_get_double_array(head.Handle,"values",data,&len))!=GRIB_SUCCESS) {
      Tcl_AppendResult(Interp,"\n   GRIB_FieldRead: Could not load data",(char*)NULL);
      return(TCL_ERROR);
   }

   for(i=0;i<FSIZE3D(field->Def);i++) {
      Def_Set(field->Def,0,i,data[i]);
   }
   free(data);

   GRIB_FieldSet(field);

   err=grib_get_long(head.Handle,"gridDefinition",&i);
   mtx[2]=mtx[4]=0.0;
   err=grib_get_double(head.Handle,"latitudeOfFirstGridPointInDegrees",&mtx[3]);
   err=grib_get_double(head.Handle,"longitudeOfFirstGridPointInDegrees",&mtx[0]);
   err=grib_get_double(head.Handle,"iDirectionIncrementInDegrees",&mtx[1]);
   err=grib_get_double(head.Handle,"jDirectionIncrementInDegrees",&mtx[5]);
   GDALInvGeoTransform(mtx,inv);
   field->Ref=GeoRef_WKTSetup(ni,nj,nk,LVL_MASL,NULL,NULL,mtx,inv,NULL);
   GeoRef_Qualify(field->Ref);

   grib_get_string(head.Handle,"centre",sval,&len);
   fprintf(stderr,"------ %s\n",sval);

   grib_get_string(head.Handle,"short_name",sval,&len);
   fprintf(stderr,"------ %s\n",sval);

   grib_get_string(head.Handle,"long_name",sval,&len);
   fprintf(stderr,"------ %s\n",sval);

/*
   fprintf(stderr,"\n\n\n-------------------\n");
   iter=grib_keys_iterator_new(head.Handle,0,NULL);
   while(grib_keys_iterator_next(iter)){
      switch(grib_keys_iterator_get_native_type(iter)) {
         case GRIB_TYPE_LONG:
            grib_keys_iterator_get_long(iter,&lval,&len);
            fprintf(stderr,"%s=%ld\n",grib_keys_iterator_get_name(iter),lval);
            break;
         case GRIB_TYPE_DOUBLE:
            grib_keys_iterator_get_double(iter,&dval,&len);
            fprintf(stderr,"%s=%e\n",grib_keys_iterator_get_name(iter),dval);
            break;
         case GRIB_TYPE_STRING:
            grib_get_string(head.Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"string ----------- <<<<<<<<< %s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
         case GRIB_TYPE_BYTES:
            grib_get_string(head.Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"bytes ----------- >>>>>>>> %s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
         case GRIB_TYPE_SECTION:
            grib_get_string(head.Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"%s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
         case GRIB_TYPE_LABEL:
            grib_keys_iterator_get_string(iter,sval,&len);
            grib_get_string(head.Handle,grib_keys_iterator_get_name(iter),sval,&len);
            sval[len]='\0';
            fprintf(stderr,"label ---------- %s=%s %i\n",grib_keys_iterator_get_name(iter),sval,len);
            break;
      }
   }
   grib_keys_iterator_delete(iter);
*/

   return(TCL_OK);
}
