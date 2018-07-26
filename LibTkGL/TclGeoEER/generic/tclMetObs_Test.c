#include "tclMetObs.h"
#include "tclMetObs_Test.h"

#define LOC_SHOW_PREFIX       "   "
#define ELEM_SHOW_PREFIX      LOC_SHOW_PREFIX  "    "
#define ELEM_DATA_SHOW_PREFIX ELEM_SHOW_PREFIX "    "
void MetObs_ShowObs(TMetObs *obs)
{
   printf("This TMetObs has Name : %s\n", obs->Model->Name);
   int nb_loc=0;
   for(TMetLoc *curr = obs->Loc; curr != NULL; curr = curr->Next){
      /*
       * Looking for TMetLoc's that have more than one TMetElem
       */
      if(MetObs_CountElems(curr) > 1){
         MetObs_ShowLoc(curr);
         nb_loc++;
         if(nb_loc > 50){
            break;
         }
      }
   }
}

void MetObs_ShowLoc(TMetLoc *loc)
{
   Coord c = loc->Coord;
   printf("TMetLoc at %p\n", loc);

   printf(LOC_SHOW_PREFIX "Id=%s\n", loc->Id);
   printf(LOC_SHOW_PREFIX "No=%s\n", loc->No);
   printf(LOC_SHOW_PREFIX "Coord=(%f, %f, %f),\n", c.Lat, c.Lon, c.Elev);
   printf(LOC_SHOW_PREFIX "Next=%p\n", loc->Next);

   if(loc->Elems == NULL){
      printf(LOC_SHOW_PREFIX "no Elems\n");
   } else for(TMetElem *curr = loc->Elems; curr != NULL; curr = curr->Next){
      MetObs_ShowElem(curr);
   }
}
void MetObs_ShowElem(TMetElem *elem)
{
   printf(LOC_SHOW_PREFIX "Elem at %p\n", elem);

   printf(ELEM_SHOW_PREFIX "Time=%ld\n", elem->Time);
   printf(ELEM_SHOW_PREFIX "NData=%d\n", elem->NData);
   printf(ELEM_SHOW_PREFIX "EData=%p\n", elem->EData);
   for(int i = 0; i < elem->NData ; ++i){
      MetObs_ShowElemData(elem->EData[i]);
   }
   putchar('\n');
}
void MetObs_ShowElemData(TMetElemData *ed)
{
   printf(ELEM_SHOW_PREFIX "TMetElemData at %p\n", ed);

   printf(ELEM_DATA_SHOW_PREFIX "Time=%ld\n", ed->Time);
   printf(ELEM_DATA_SHOW_PREFIX "Nv,Nt,Ne=%d,%d,%d\n", ed->Nv, ed->Nt, ed->Ne);
   printf(ELEM_DATA_SHOW_PREFIX "Family=%d\n", ed->Family);
   printf(ELEM_DATA_SHOW_PREFIX "Data=");
   for(int i = 0; i < ed->Nv*ed->Nt*ed->Ne ; i++){
      printf("(%f, %d),", ed->Data[i], ed->Code[i]->descriptor);
   } putchar('\n');
}

void MetObsTest_FillTestObs(TMetObs *obs)
{
   TMetLoc_New(obs, "LocID1", "LocNo1", 3.1415, 2.71828, 20.0);
   TMetLoc_New(obs, "LocID2", "LocNo2", 3.1415, 2.71828, 20.0);
   float data[] = { 1.1, 1.2, 1.3, 1.4 };
   TMetElem_Merge(obs->Loc,
         100, 101, // time_t Min, Time
         3, 4, 9,  // Fam, Type, SType
         4, 4, 4,  // Ne, Nv, Nt
         data,     // float *data
         NULL,     // int *Marker
         NULL      // EntryTableB **Codes
   );
}

int MetObs_CountLoc(TMetObs *Obs)
{
   int nb = 0;
   for(TMetLoc *loc = Obs->Loc; loc; loc = loc->Next)
      nb++;
   return nb;
}

int MetObs_CountElems(TMetLoc *Loc)
{
   int nb = 0;
   for(TMetElem *el = Loc->Elems; el; el = el->Next)
      nb++;
   return nb;
}
