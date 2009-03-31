/*=============================================================================
 * Environnement Canada
 * Centre Meteorologique Canadian
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet    : Librairie Tcl de manipulation de type de donnees.
 * Fichier   : Data_Calc.h
 * Creation  : Aout 2000 - J.P. Gauthier - CMC/CMOE
 *
 * Description: Fonctions de base de la calculatrice de donnees.
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

#ifndef _DATA_CALC_H_
#define _DATA_CALC_H_

#include "tclFSTD.h"
#include "tclGDAL.h"
#include "tclObs.h"
#include "tclVector.h"
#include "Data_Matrix.h"

#define T_VAL  0
#define T_FLD  1
#define T_OBS  2
#define T_VEC  3
#define T_BAND 4

int  Calc_Parse(Tcl_Interp* Interp,int Except,char* Champ,TData_Type Type,char* Expr);
int  Calc_Validate(Tcl_Interp* Interp);
int  yyparse(void);

#endif
