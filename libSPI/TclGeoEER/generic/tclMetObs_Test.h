#ifdef HAVE_ECBUFR

#ifndef _TCL_MET_OBS_SHOW_H_
void MetObs_ShowLoc(TMetLoc *loc);
void MetObs_ShowElem(TMetElem *elem);
void MetObs_ShowElemData(TMetElemData *elem_data);
void MetObs_ShowObs(TMetObs *obs);
int MetObs_CountLoc(TMetObs *obs);
int MetObs_CountElems(TMetLoc *loc);
void MetObsTest_FillTestObs(TMetObs *obs);
#endif // _TCL_MET_OBS_SHOW_H_

#endif