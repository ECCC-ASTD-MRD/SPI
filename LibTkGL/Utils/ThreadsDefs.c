#include "ThreadsDefs.h"

void Thread_SingleWait(sem_t *Sem,int Count){

   while (Count--) {
#ifdef _THREAD_DEBUG_
      printf("(_THREAD_DEBUG_) Thread_SingleWait: %i\n",Count);
#endif
      Thread_SemaphoreIgnore(Sem);
   }
}

void Thread_SingleSend(sem_t *Sem){

   sem_post(Sem);

#ifdef _THREAD_DEBUG_
      printf("(_THREAD_DEBUG_) Thread_SingleSend: Sending semaphore\n");
#endif
}

void Thread_SemaphoreIgnore(sem_t *Sem){

   while (sem_wait(Sem) != 0) {}
#ifdef _THREAD_DEBUG_
      printf("(_THREAD_DEBUG_) Thread_SemaphoreIgnore: Ignoring\n");
#endif
}

typedef struct Thread_RasterInfo {
   int X0,Y0,X1,Y1;
   ViewportItem *VP;
   Projection *Proj;
} Thread_RasterInfo;

sem_t Sem;

void VP_ThreadRasterDraw(Thread_RasterInfo *Info){
   Coord coord;
   Vector pix;
   int deltacolor,fieldcolor;
   int x,y,i;
   double value;

   extern int FSTD_FieldGetValue(void *Field,float Lat,float Lon,double *Value);
   extern double Geo_InterpolateTopo(double Lat,double Lon,int *Idx);

   for (y=Info->Y0;y<Info->Y1;y++) {
      for (x=Info->X0;x<Info->X1;x++) {
         pix.x=x;
         pix.y=y;

         /*Obtenir le coordonnees lat-lon*/
         if (Info->Proj->Type->ProjCoord(&coord,pix,Info->Proj->Params)) {

            /*Determiner la topographie*/
            if (Info->VP->Topography || Info->VP->Texture) {
               coord.elev=Geo_InterpolateTopo(coord.lat,coord.lon,&deltacolor);
            } else {
               coord.elev=0.0;
               deltacolor=0;
            }

            /*Determiner les champs*/
            if (Info->VP->NbField) {
               fieldcolor=-1;
               for (i=0;i<=NBFIELDMAX;i++) {
                  if (Info->VP->FieldPtr[i]) {
                     if (FSTD_FieldGetValue(Info->VP->FieldPtr[i],coord.lat,coord.lon,&value)) {
                       fieldcolor=FSTD_Value2Color(Info->VP->FieldPtr[i],value,Info->VP->Colormap->NbPixels-1);
		     }
	          }
	       }
               deltacolor=fieldcolor<0?deltacolor:fieldcolor;
	    }

            deltacolor=deltacolor<=0?Info->VP->Colormap->BackGround->pixel:Info->VP->Colormap->Cells[deltacolor].pixel;
            
            /*Determiner l'emplacement du pixel*/
            if (coord.elev<=0) {
               XPutPixel(Info->VP->RasterImg,x,y,deltacolor);
            } else {
               coord.elev*=Info->Proj->Params->HeightFactor;
               Info->Proj->Type->ProjPoint(coord,&pix);
               Info->Proj->Type->ProjScreen(pix,&pix,Info->Proj->Params);

               /*Si il y a quelque chose, afficher*/
               if (ABS(pix.x-x)<1 && ABS(pix.y-y)<1) {
                  XPutPixel(Info->VP->RasterImg,x,y,deltacolor);
               } else {
                  XPutLine(Info->VP->RasterImg,pix.x,pix.y,x,y,deltacolor);
	       }
	    }
         } else {
            XPutPixel(Info->VP->RasterImg,x,y,Info->VP->Colormap->BackGround->pixel);
	 }
      }
   }
   Thread_SingleSend(&Sem);
   pthread_exit(NULL);
}

#define THREAD_NB_RASTER 1

VP_ThreadRasterTrace(ViewportItem *VP,Projection *Proj) {

   pthread_t threadid;
   pthread_attr_t attr;
   struct sched_param attrparam;
   int h,i;

   Thread_RasterInfo T[THREAD_NB_RASTER];

   if (!VP->Colormap) {
      fprintf(stderr,"VP_ThreadRasterTrace: Warning, Colormap needed to display raster data\n");
      return;
   }

   if (VP->Topography || VP->Texture || VP->NbField) {

      /*Initialisation des threads*/
      pthread_attr_init(&attr);
      pthread_attr_setdetachstate(&attr,PTHREAD_CREATE_JOINABLE);
      pthread_attr_setscope(&attr,PTHREAD_SCOPE_SYSTEM);

      /*Mettre la priorite au maximum permis*/
      pthread_attr_getschedparam(&attr,&attrparam);
      attrparam.sched_priority=sched_get_priority_max(SCHED_RR);
      pthread_attr_setschedparam(&attr,&attrparam);
   
      /*Niveaux de concurrence pour les resources*/ 
      pthread_setconcurrency(1);
  
      sem_init(&Sem,NULL,0);

      for(i=0;i<THREAD_NB_RASTER;i++){
         h=VP->Height/THREAD_NB_RASTER;
         T[i].X0=0;
         T[i].Y0=h*i;
         T[i].X1=VP->Width;
         T[i].Y1=h*(i+1); 
         T[i].VP=VP;
         T[i].Proj=Proj;
         pthread_create(&threadid,&attr,(void*)VP_ThreadRasterDraw,&T[i]);
      }

      Thread_SingleWait(&Sem,THREAD_NB_RASTER);

      XSetFunction(VP->display,VP->drawgc,GXxor);
      XPutImage(VP->display,VP->Pix,VP->drawgc,VP->RasterImg,0,0,0,0,VP->Width,VP->Height);
      XSetFunction(VP->display,VP->drawgc,GXcopy);
   }
}

