/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Package Tk permettant l'utilisation d'un canvas item comme device R
 * Fichier      : tclData.c
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * Description  : Fonctions de conversion entre variables tcl et
 *                variables R ainsi que hook tcl pour exécuter du
 *                code R.
 *
 * Remarques    :
 *      - La gestion des erreurs est pour le moment presque iniexistante.
 *        L'utilisation de tryEval aide beaucoup à réduire les erreurs,
 *        mais le remplacement de hook pourrait éventuellement être
 *        considéré, bien que loin d'être suggéré
 *        (voir https://stat.ethz.ch/pipermail/r-help/2008-August/171493.html)
 *      - Plusieurs situations potentiellement problématiques avec le
 *        embedding de R ne sont pas non plus prises en compte.
 *        Voir https://cran.r-project.org/doc/manuals/r-release/R-exts.html#Embedding-R-under-Unix_002dalikes
 *        pour plus de détails.
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
 *=========================================================
 */

// Tcl includes
#include <tcl.h>

// TclRDevice include
#include "tkCanvRDevice.h"

// Tcl defines

#define TCL_ASRT(x) if( (x)!=TCL_OK ) return(TCL_ERROR)

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Tclrdevice_Init>
 * Creation     : Décembre 2018 - E. Legault-Ouellet
 *
 * But          : Initialise la librairie. Cette fonction est appellée par Tcl via la commande "load".
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *
 * Retour       : TCL_OK en cas de succès, TCL_ERROR sinon
 *
 * Remarques    : Un message d'erreur est laissé dans l'interpréteur en cas d'erreur.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
int Tclrdevice_Init(Tcl_Interp *Interp) {
    // Since we link with the stubs lib, we need to init the API
    if( Tcl_InitStubs(Interp,"8.5",0) == NULL )
        return TCL_ERROR;

    // Provide the package
    TCL_ASRT( Tcl_PkgProvide(Interp,PACKAGE_NAME,PACKAGE_VERSION) );

    // Install our home made graphical device if running under tk
    if( Tcl_GetVar(Interp,"tk_version",TCL_GLOBAL_ONLY) ) {
        RDeviceItem_Register();
    }

    return TCL_OK;
}
