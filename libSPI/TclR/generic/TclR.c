/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Package Tcl pour interfacer avec la librairie R
 * Fichier      : TclR.c
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

// Normal includes
#include <stdio.h>

// R includes
#include <R.h>
#include <Rinternals.h>
#include <R_ext/Parse.h>
#include <Rdefines.h>
#include <Rinterface.h>
#include <Rembedded.h>

// Tcl includes
#include <tcl.h>

// To load FSTD functions
#include <dlfcn.h>

// Interface defines

typedef enum TclR_TTrace {TCLR_TRACE_NONE,TCLR_TRACE_ALL,TCLR_TRACE_LAST} TclR_TTrace;

typedef struct TclR_Context {
    int             ProtectCnt;
    TclR_TTrace     TraceCmd;
    TclR_TTrace     TraceRes;
    Tcl_Channel     Chan;
    int             (*EXT_DataStat)(Tcl_Interp*,char*,char*,int,int*,int*);
    int             (*EXT_DataCopy)(Tcl_Interp*,char*,const char*,void*,int,int*);
} TclR_Context;

// Tcl defines

#define TCL_ASRT(x) if( (x)!=TCL_OK ) return(TCL_ERROR)

typedef enum TclY_TType     { TCLY_BOOLEAN,TCLY_INT,TCLY_DOUBLE,TCLY_STRING,TCLY_LIST,TCLY_DICT,TCLY_BIGNUM,TCLY_UNKNOWN } TclY_TType;
typedef enum TclR_TListType { TCLR_LIST_BOOLEAN,TCLR_LIST_INT,TCLR_LIST_DOUBLE,TCLR_LIST_COMPLEX,TCLR_LIST_STRING,TCLR_LIST_MIXED,TCLR_LIST_UNKNOWN } TclR_TListType;

// R defines

#define R_PROTECT(x)        {PROTECT(x); Context->ProtectCnt++;}
#define R_UNPROTECT         {if( Context->ProtectCnt>0) {UNPROTECT(1); Context->ProtectCnt--;}}
#define R_UNPROTECT_ALL     {if( Context->ProtectCnt>0 ) UNPROTECT(Context->ProtectCnt); Context->ProtectCnt=0;}

#define IsNaInt(val)        ((val)==R_NaInt)
#define IsNaReal(val)       R_IsNA(val)

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_RInit>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Initialise le module R
 *
 * Parametres   :
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclR_RInit() {
    //static char *argv[] = {"TclR","--no-save","--no-environ","--no-restore","--no-init-file","--quiet","--slave"};
    static char *argv[] = {"TclR","--vanilla","--quiet","--slave"};

    // This doesn't override a value if one is already given. This is a last resort effort : R won't start without a R_HOME variable
    setenv("R_HOME",R_DEFAULT_RHOME,0);

    // Disable signal handlers
    R_SignalHandlers = 0;

    Rf_initEmbeddedR(sizeof(argv)/sizeof(*argv),argv);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_RRelease>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Libère le module R
 *
 * Parametres   :
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclR_RRelease() {
    Rf_endEmbeddedR(0);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_RTypeName>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Retourne un string représentant le type de variable R
 *
 * Parametres   :
 *   <RObj>     : Variable R à dont on veut le type
 *
 * Retour       : Un string représentant le type de variable R
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static const char* TclR_RTypeName(SEXP RObj) {
    // This info is taken verbatim from the Rinternal.h "defines" section and its comments
    switch( TYPEOF(RObj) ) {
        case NILSXP:
            // nil = NULL
            return "NILSXP (NULL)";
        case SYMSXP:
            // symbols
            return "SYMSXP (symbols)";
        case LISTSXP:
            // lists of dotted pairs
            return "LISTSXP (lists of dotted pairs)";
        case CLOSXP:
            // closures
            return "CLOSXP (closures)";
        case ENVSXP:
            // environments
            return "ENVSXP (environments)";
        case PROMSXP:
            // promises: [un]evaluated closure arguments
            return "PROMSXP (promises: [un]evaluated closure arguments)";
        case LANGSXP:
            // language constructs (special lists)
            return "LANGSXP (language constructs (special lists))";
        case SPECIALSXP:
            // special forms
            return "SPECIALSXP (special forms)";
        case BUILTINSXP:
            // builtin non-special forms
            return "BUILTINSXP (builtin non-special forms)";
        case CHARSXP:
            // "scalar" string type (internal only)
            return "CHARSXP (\"scalar\" string type (internal only))";
        case LGLSXP:
            // logical vectors
            return "LGLSXP (logical vectors)";
        case INTSXP:
            // integer vectors
            return "INTSXP (integer vectors)";
        case REALSXP:
            // real variables
            return "REALSXP (real variables)";
        case CPLXSXP:
            // complex variables
            return "CPLXSXP (complex variables)";
        case STRSXP:
            // string vectors
            return "STRSXP (string vectors)";
        case DOTSXP:
            // dot-dot-dot object
            return "DOTSXP (dot-dot-dot object)";
        case ANYSXP:
            // make "any" args work. Used in specifying types for symbol registration to mean anything is okay
            return "ANYSXP (Make \"any\" args work)";
        case VECSXP:
            // generic vectors
            return "VECSXP (generic vectors)";
        case EXPRSXP:
            // expressions vectors
            return "EXPRSXP (expressions vectors)";
        case BCODESXP:
            // byte code
            return "BCODESXP (byte code)";
        case EXTPTRSXP:
            // external pointer
            return "EXTPTRSXP (external pointer)";
        case WEAKREFSXP:
            // weak reference
            return "WEAKREFSXP (weak reference)";
        case RAWSXP:
            // raw bytes
            return "RAWSXP (raw bytes)";
        case S4SXP:
            // S4, non-vector
            return "S4SXP (S4, non-vector)";
        case NEWSXP:
            // fresh node created in new page (used for detecting PROTECT issues in memory.c)
            return "NEWSXP (fresh node created in new page)";
        case FREESXP:
            // node released by GC (used for detecting PROTECT issues in memory.c)
            return "FREESXP (node released by GC)";
        case FUNSXP:
            // Closure or Builtin or Special
            return "FUNSXP (Closure or Builtin or Special)";
        default:
            return NULL;
    }
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_RObjFromName>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Retourne la variable R associée au nom donné
 *
 * Parametres   :
 *   <Context>  : Contexte
 *   <Name>     : Nom de la variable R
 *
 * Retour       : Variable R
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static SEXP TclR_RObjFromName(TclR_Context *Context,const char *Name) {
    SEXP rname;

    R_PROTECT( rname=Rf_findVar(Rf_install(Name),R_GlobalEnv) );

    return rname;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_RPrint>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Affiche le contenu d'une variable R
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Context>  : Contexte
 *   <RVar>     : Variable R à afficher
 *
 * Retour       : TCL_OK en cas de succès, TCL_ERROR sinon.
 *
 * Remarques    : Un message d'erreur est laissé dans l'interpréteur en cas d'erreur.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
#define CHAN_PRINTF(...) \
    if( Tcl_WriteObj(Context->Chan,Tcl_ObjPrintf(__VA_ARGS__))==-1 ) { \
        Tcl_AppendResult(Interp,"An error occured while writing to the channel",NULL); \
        return TCL_ERROR; \
    }
static int TclR_RPrint(Tcl_Interp *Interp,TclR_Context *Context,SEXP RVar) {
    R_len_t len;

    if( !Context->Chan ) {
        Tcl_AppendResult(Interp,"A channel openned for writing is needed to print\n",NULL);
        return TCL_ERROR;
    }

    switch( TYPEOF(RVar) ) {
        case NILSXP:
            CHAN_PRINTF("(NULL)\n");
            break;
        case CHARSXP:
            // "scalar" string type (internal only)
            CHAN_PRINTF("%s\n",CHAR(RVar));
            break;
        case LGLSXP:
            {
                // logical vectors
                int *ptr = LOGICAL(RVar);
                len = LENGTH(RVar);

                if( len-- ) {
                    if( IsNaInt(*ptr) ) {
                        CHAN_PRINTF("NA");
                    } else {
                        CHAN_PRINTF("%d",*ptr);
                    }
                    ptr++;
                }
                while( len-- > 0 ) {
                    if( IsNaInt(*ptr) ) {
                        CHAN_PRINTF(" NA");
                    } else {
                        CHAN_PRINTF(" %d",*ptr);
                    }
                    ptr++;
                }
                CHAN_PRINTF("\n");
            }
            break;
        case INTSXP:
            {
                // integer vectors
                int *ptr = INTEGER(RVar);
                len = LENGTH(RVar);

                if( len-- ) {
                    if( IsNaInt(*ptr) ) {
                        CHAN_PRINTF("NA");
                    } else {
                        CHAN_PRINTF("%d",*ptr);
                    }
                    ptr++;
                }
                while( len-- > 0 ) {
                    if( IsNaInt(*ptr) ) {
                        CHAN_PRINTF(" NA");
                    } else {
                        CHAN_PRINTF(" %d",*ptr);
                    }
                    ptr++;
                }
                CHAN_PRINTF("\n");
            }
            break;
        case REALSXP:
            {
                // real variables
                double *ptr = REAL(RVar);
                len = LENGTH(RVar);

                if( len-- ) {
                    if( IsNaReal(*ptr) ) {
                        CHAN_PRINTF("NA");
                    } else if( isnan(*ptr) ) {
                        CHAN_PRINTF("NaN");
                    } else {
                        CHAN_PRINTF("%f",*ptr);
                    }
                    ptr++;
                }
                while( len-- > 0 ) {
                    if( IsNaReal(*ptr) ) {
                        CHAN_PRINTF(" NA");
                    } else if( isnan(*ptr) ) {
                        CHAN_PRINTF(" NaN");
                    } else {
                        CHAN_PRINTF(" %f",*ptr);
                    }
                    ptr++;
                }
                CHAN_PRINTF("\n");
            }
            break;
        case STRSXP:
            {
                // string vectors
                len = LENGTH(RVar);
                SEXP *ptr = STRING_PTR(RVar);

                if( len ) {
                    CHAN_PRINTF("'%s'",CHAR(*ptr++));
                    --len;
                }
                while(len--)
                    CHAN_PRINTF(" '%s'",CHAR(*ptr++));
                CHAN_PRINTF("\n");
            }
            break;
        case VECSXP:
            {
                // generic vectors (Could be anything, we'll mostly let recursion sort it out)
                SEXP        rattr,rnames=R_NilValue;
                R_len_t     i;

                len=LENGTH(RVar);

                // Add all the attributes (if available)
                for(rattr=ATTRIB(RVar),rnames=R_NilValue; rattr!=R_NilValue; rattr=CDR(rattr)) {
                    // Get the attribute name
                    if( TAG(rattr) == R_NamesSymbol ) {
                        // The names attribute is special : we'll use it later for keys
                        rnames = CAR(rattr);
                    } else {
                        // Print the attribute
                        CHAN_PRINTF("(Attrib) %s : ",CHAR(PRINTNAME(TAG(rattr))));
                        TCL_ASRT( TclR_RPrint(Interp,Context,CAR(rattr)) );
                    }
                }

                // Print all the elements
                if( rnames!=R_NilValue && Rf_isString(rnames) && len==LENGTH(rnames) ) {
                    // Print what we have with column headers
                    for(i=0; i<len; ++i) {
                        CHAN_PRINTF("%s : ",CHAR(STRING_ELT(rnames,i)));
                        TCL_ASRT( TclR_RPrint(Interp,Context,VECTOR_ELT(RVar,i)) );
                    }
                } else {
                    // Just print what we have without column headers
                    for(i=0; i<len; ++i) {
                        TCL_ASRT( TclR_RPrint(Interp,Context,VECTOR_ELT(RVar,i)) );
                    }
                }
            }
            break;
        case LISTSXP:
            {
                while( RVar != R_NilValue ) {
                    CHAN_PRINTF("%s : ",CHAR(PRINTNAME(TAG(RVar))));
                    TCL_ASRT( TclR_RPrint(Interp,Context,CAR(RVar)) );
                    RVar = CDR(RVar);
                }
            }
            break;
        default:
            // Either unimplemented, unsupported or with no possible tcl equivalent
            CHAN_PRINTF("Object of type <%s>\n",TclR_RTypeName(RVar));
            break;
    }

    return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_R2Tcl>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Transforme une variable R en une variable Tcl
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Context>  : Contexte
 *   <RVar>     : Variable R à transformer
 *
 * Retour       : Variable Tcl en cas de succès, NULL en cas d'erreur
 *
 * Remarques    : Un message d'erreur est laissé dans l'interpréteur en cas d'erreur.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
#define R2TCL(RVar,TclVar,InternalType,TclFct,RFct,NaFct) { \
    len = LENGTH(RVar); \
    if( len == 1 ) { \
        InternalType val = RFct(RVar)[0]; \
        TclVar = NaFct(val)?Tcl_NewObj():TclFct(val); \
    } else { \
        InternalType *ptr = RFct(RVar); \
        TclVar = Tcl_NewListObj(0,NULL); \
        while(len--) { \
            Tcl_ListObjAppendElement(Interp,TclVar,NaFct(*ptr)?Tcl_NewObj():TclFct(*ptr)); \
            ptr++; \
        } \
    } \
}
static Tcl_Obj* TclR_R2Tcl(Tcl_Interp *Interp,TclR_Context *Context,SEXP RVar) {
    Tcl_Obj     *obj=NULL,*elem,*name;
    R_len_t     len,i;
    SEXP        rattr,rnames;
    const char  *str;

    // Convert the value to a tcl one
    switch( TYPEOF(RVar) ) {
        case NILSXP:
            obj = Tcl_NewObj();
            break;
        case LISTSXP:
            // lists of dotted pairs (Could be anything, let recursion sort it out)
            obj = Tcl_NewDictObj();
            if( obj ) {
                // This is a linked list. CAR gets the element out of the current node, CDR gets the next node and the last element is NULL
                while( RVar != R_NilValue ) {
                    // Get the element
                    if( !(elem=TclR_R2Tcl(Interp,Context,CAR(RVar))) ) {
                        // An error message should already be on the stack
                        Tcl_DecrRefCount(obj);
                        return NULL;
                    }

                    // Get the name
                    if( !(name=Tcl_NewStringObj(CHAR(PRINTNAME(TAG(RVar))),-1)) ) {
                        Tcl_AppendResult(Interp,"Could not get the key from names attribute",NULL);
                        Tcl_DecrRefCount(obj);
                        Tcl_DecrRefCount(elem);
                        return NULL;
                    }

                    // Add the key/value pair to the dictionary
                    if( Tcl_DictObjPut(Interp,obj,name,elem) != TCL_OK ) {
                        Tcl_AppendResult(Interp,"Could not add key/value pair to dict",NULL);
                        Tcl_DecrRefCount(obj);
                        Tcl_DecrRefCount(elem);
                        Tcl_DecrRefCount(name);
                        return NULL;
                    }

                    // Get the next node in the linked list
                    RVar = CDR(RVar);
                }
            }
            break;
        case CHARSXP:
            // "scalar" string type (internal only)
            obj = Tcl_NewStringObj(CHAR(RVar),-1);
            break;
        case LGLSXP:
            // logical vectors
            R2TCL(RVar,obj,int,Tcl_NewBooleanObj,LOGICAL,IsNaInt);
            break;
        case INTSXP:
            // integer vectors
            R2TCL(RVar,obj,int,Tcl_NewIntObj,INTEGER,IsNaInt);
            break;
        case REALSXP:
            // real variables
            R2TCL(RVar,obj,double,Tcl_NewDoubleObj,REAL,IsNaReal);
            break;
        case STRSXP:
            // string vectors
            len = LENGTH(RVar);
            if( len == 1 ) {
                obj = Tcl_NewStringObj(CHAR(STRING_PTR(RVar)[0]),-1);
            } else {
                SEXP *ptr = STRING_PTR(RVar);
                obj = Tcl_NewListObj(0,NULL);
                while(len--)
                    Tcl_ListObjAppendElement(Interp,obj,Tcl_NewStringObj(CHAR(*ptr++),-1));
            }
            break;
        case VECSXP:
            // generic vectors (Could be anything, we'll let recursion sort it out)
            len = LENGTH(RVar);

            obj = Tcl_NewDictObj();
            if( !obj ) break;

            // Add all the attributes (if available)
            for(rattr=ATTRIB(RVar),rnames=R_NilValue; rattr!=R_NilValue; rattr=CDR(rattr)) {
                // Get the attribute name
                str = CHAR(PRINTNAME(TAG(rattr)));
                if( !strcmp(str,"names") ) {
                    // The names attribute is special : we'll use it later for keys
                    rnames = CAR(rattr);
                } else {
                    // Get the element
                    if( !(elem=TclR_R2Tcl(Interp,Context,CAR(rattr))) ) {
                        // An error message should already be on the stack
                        Tcl_DecrRefCount(obj);
                        return NULL;
                    }

                    // Get the name to use as key and prepend "attr." in front of it
                    if( !(name=Tcl_ObjPrintf("attr.%s",str)) ) {
                        Tcl_AppendResult(Interp,"Could not get the key from names attribute",NULL);
                        Tcl_DecrRefCount(obj);
                        Tcl_DecrRefCount(elem);
                        return NULL;
                    }

                    // Add the key/value pair to the dictionary
                    if( Tcl_DictObjPut(Interp,obj,name,elem) != TCL_OK ) {
                        Tcl_AppendResult(Interp,"Could not add key/value pair to dict",NULL);
                        Tcl_DecrRefCount(obj);
                        Tcl_DecrRefCount(elem);
                        Tcl_DecrRefCount(name);
                        return NULL;
                    }
                }
            }

            // Get the column names
            if( rnames==R_NilValue || !Rf_isString(rnames) || len!=LENGTH(rnames) ) {
                // We can't have a dictionary without keys
                Tcl_AppendResult(Interp,"Could not get \"names\" attribute",NULL);
                Tcl_DecrRefCount(obj);
                return NULL;
            }

            if( obj ) {
                for(i=0; i<len; ++i) {
                    // Get the element
                    elem = TclR_R2Tcl(Interp,Context,VECTOR_ELT(RVar,i));
                    if( !elem ) {
                        // An error message should already be on the stack
                        Tcl_DecrRefCount(obj);
                        return NULL;
                    }

                    // Get the name
                    if( !(name=Tcl_NewStringObj(CHAR(STRING_ELT(rnames,i)),-1)) ) {
                        Tcl_AppendResult(Interp,"Could not get the key from names attribute",NULL);
                        Tcl_DecrRefCount(obj);
                        return NULL;
                    }

                    // Add that value to the list
                    if( Tcl_DictObjPut(Interp,obj,name,elem) != TCL_OK ) {
                        Tcl_AppendResult(Interp,"Could not add key/value pair to dict",NULL);
                        Tcl_DecrRefCount(obj);
                        Tcl_DecrRefCount(name);
                        Tcl_DecrRefCount(elem);
                        return NULL;
                    }
                }
            }
            break;
        case RAWSXP:
            // raw bytes
            obj = Tcl_NewByteArrayObj(RAW(RVar),LENGTH(RVar));
            break;
        case CPLXSXP:
        default:
            // Either unimplemented, unsupported or with no possible tcl equivalent
            Tcl_AppendResult(Interp,"Invalid type to convert to Tcl :",TclR_RTypeName(RVar),NULL);
            obj = NULL;
    }

    // Return the object
    return obj;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclY_GetObjectType>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Retourne le type d'un objet Tcl (dans sa forme actuelle)
 *
 * Parametres   :
 *   <Obj>      : Object Tcl dont on veut le type
 *
 * Retour       : Le type de l'objet Tcl
 *
 * Remarques    : Seul les types suivants sont supportés pour l'instant : list,int,double,dict,boolean,bignum,string.
 *                Tous les autres types sont considérés comme 'unknown'.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static TclY_TType TclY_GetObjType(Tcl_Obj *Obj) {
    const char *type = Obj->typePtr ? Obj->typePtr->name : NULL;

    //printf("type : %s\n",type);
    //Tcl_Obj *tmp = Tcl_NewObj();
    //Tcl_AppendAllObjTypes(NULL,tmp);
    //printf("obj types : \"%s\"\n",Tcl_GetString(tmp));
    // boolean double end-offset regexp list cmdName bytecode procbody bytearray int dict {array search} string

    if( !type ) {
        return TCLY_STRING;
    } else if( !strcmp(type,"list") ) {
        return TCLY_LIST;
    } else if( !strcmp(type,"int") ) {
        return TCLY_INT;
    } else if( !strcmp(type,"double") ) {
        return TCLY_DOUBLE;
    } else if( !strcmp(type,"dict") ) {
        return TCLY_DICT;
    } else if( !strcmp(type,"booleanString") ) {
        return TCLY_BOOLEAN;
    } else if( !strcmp(type,"bignum") ) {
        return TCLY_BIGNUM;
    }

    return TCLY_UNKNOWN;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_GetListObjType>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Retourne le type de liste en essayant le plus possible de retourner un type homogène parmis
 *                INT, DOUBLE, BOOLEAN ou STRING. Un type MIXED est retourné si des listes ou dictionnaires
 *                explicites sont rencontrés parmis les éléments.
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Lst>      : Liste dont ont veut le type.
 *
 * Retour       : Le type de liste ou TCLR_LIST_UNKNOWN en cas d'erreur
 *
 * Remarques    : Cette fonction se sert de l'API Tcl en essayant de convertir les éléments en le type testé
 *                pour déterminer si cet élément est bien de ce type. Se faisant, la structure interne (et le type)
 *                des variables testés pourraient être modifiés, de façon analogue à ce que fait la commande "string is".
 *
 *---------------------------------------------------------------------------------------------------------------
*/
#define TCL_IS_BOOLEAN(x)   (Tcl_GetBooleanFromObj(NULL,x,&ival)==TCL_OK)
#define TCL_IS_DOUBLE(x)    (Tcl_GetDoubleFromObj(NULL,x,&dval)==TCL_OK)
#define TCL_IS_INT(x)       (Tcl_GetIntFromObj(NULL,x,&ival)==TCL_OK)
static TclR_TListType TclR_GetListObjType(Tcl_Interp *Interp,Tcl_Obj *Lst) {
    TclR_TListType  ltype=TCLR_LIST_UNKNOWN;
    Tcl_Obj         **elems;
    double          dval;
    int             len,i,ival;

    if( Tcl_ListObjGetElements(Interp,Lst,&len,&elems) != TCL_OK )
        return TCLR_LIST_UNKNOWN;

    for(i=0; i<len; ++i) {
        switch( TclY_GetObjType(elems[i]) ) {
            case TCLY_LIST:
            case TCLY_DICT:
                return TCLR_LIST_MIXED;
            default:
                if( elems[i]->length ) {
                    // Check with the list type we have so far what are our options
                    switch( ltype ) {
                        case TCLR_LIST_UNKNOWN:
                            // We have an uninitialized list type; go from more restrictive to less restrictive types
                            if( TCL_IS_INT(elems[i]) )          ltype = TCLR_LIST_INT;
                            else if( TCL_IS_DOUBLE(elems[i]) )  ltype = TCLR_LIST_DOUBLE;
                            else if( TCL_IS_BOOLEAN(elems[i]) ) ltype = TCLR_LIST_BOOLEAN;
                            else                                return TCLR_LIST_STRING;

                            break;
                        case TCLR_LIST_INT:
                            if( TCL_IS_INT(elems[i]) ) {
                                break;
                            } else if( TCL_IS_DOUBLE(elems[i]) ) {
                                // Since every int can be a double, any previously encountered int will now be considered as doubles
                                ltype = TCLR_LIST_DOUBLE;
                                break;
                            }
                            return TCLR_LIST_STRING;
                        case TCLR_LIST_DOUBLE:
                            if( TCL_IS_DOUBLE(elems[i]) ) {
                                break;
                            }
                            return TCLR_LIST_STRING;
                        case TCLR_LIST_BOOLEAN:
                            // This basically checks for true/false strings
                            if( TCL_IS_BOOLEAN(elems[i]) ) {
                                break;
                            }
                            return TCLR_LIST_STRING;
                        case TCLR_LIST_COMPLEX:
                        default:
                            // Any other type is considered a string since it may not have an equivalent on the R side
                            return TCLR_LIST_STRING;
                    }
                }
                break;
        }
    }

    return ltype!=TCLR_LIST_UNKNOWN ? ltype : TCLR_LIST_STRING;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_Tcl2R>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Transforme une variable Tcl en une variable R
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Context>  : Contexte
 *   <TclVar>   : Variable Tcl à transformer
 *   <RName>    : Nom de la variable R résultante
 *
 * Retour       : Variable R en cas de succès, NULL en cas d'erreur
 *
 * Remarques    : Un message d'erreur est laissé dans l'interpréteur en cas d'erreur.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static SEXP TclR_Tcl2R(Tcl_Interp *Interp,TclR_Context *Context,Tcl_Obj *TclVar,const char *RName) {
    SEXP        rvar;
    int         len,slen,i,*iptr;
    double      *dptr;
    const char  *str;
    TclY_TType  type = TclY_GetObjType(TclVar);
    Tcl_Obj     **tclvals = NULL;

    // Check if we can transform some types into more friendlier ones
    switch( type ) {
        case TCLY_LIST:
            {
                // Get the length
                if( Tcl_ListObjGetElements(Interp,TclVar,&len,&tclvals) != TCL_OK ) {
                    // A message will be in the Interp already
                    return NULL;
                }

                if( !len )
                    break;

                // Get a more specific type if possible
                switch( TclR_GetListObjType(Interp,TclVar) ) {
                    case TCLR_LIST_UNKNOWN:
                        // A message will be in the Interp already
                        return NULL;
                    case TCLR_LIST_INT:
                        type = TCLY_INT;
                        break;
                    case TCLR_LIST_DOUBLE:
                        type = TCLY_DOUBLE;
                        break;
                    case TCLR_LIST_BOOLEAN:
                        type = TCLY_BOOLEAN;
                        break;
                    case TCLR_LIST_MIXED:
                        type = TCLY_LIST;
                        break;
                    case TCLR_LIST_COMPLEX:
                    default:
                        type = TCLY_STRING;
                        break;
                }
            }
            break;
        case TCLY_DICT:
            len = 1;
            break;
        default:
            len = 1;
            tclvals = &TclVar;
            break;
    }

    if( len && strcmp(Tcl_GetString(TclVar),"") ) {
        switch( type ) {
            case TCLY_INT:
                R_PROTECT( rvar=allocVector(INTSXP,len) );
                iptr = INTEGER(rvar);
                while( len-- ) {
                    if( !(*tclvals)->length ) {
                        *iptr++ = R_NaInt;
                        tclvals++;
                    } else if( Tcl_GetIntFromObj(Interp,*tclvals++,iptr++) != TCL_OK ) {
                        // A message will be in the Interp already
                        R_UNPROTECT_ALL;
                        return NULL;
                    }
                }
                break;
            case TCLY_DOUBLE:
                R_PROTECT( rvar=allocVector(REALSXP,len) );
                dptr = REAL(rvar);
                while( len-- ) {
                    if( !(*tclvals)->length ) {
                        *dptr++ = R_NaReal;
                        tclvals++;
                    } else if( Tcl_GetDoubleFromObj(Interp,*tclvals++,dptr++) != TCL_OK ) {
                        // A message will be in the Interp already
                        R_UNPROTECT_ALL;
                        return NULL;
                    }
                }
                break;
            case TCLY_BOOLEAN:
                R_PROTECT( rvar=allocVector(LGLSXP,len) );
                iptr = LOGICAL(rvar);
                while( len-- ) {
                    if( !(*tclvals)->length ) {
                        *iptr++ = R_NaInt;
                        tclvals++;
                    } else if( Tcl_GetBooleanFromObj(Interp,*tclvals++,iptr++) != TCL_OK ) {
                        // A message will be in the Interp already
                        R_UNPROTECT_ALL;
                        return NULL;
                    }
                }
                break;
            case TCLY_LIST:
                {
                    SEXP rtmp;

                    R_PROTECT( rvar=allocVector(VECSXP,len) );
                    for(i=0; i<len; ++i) {
                        if( !(rtmp=TclR_Tcl2R(Interp,Context,*tclvals++,NULL)) ) {
                            // A message will be in the Interp already
                            R_UNPROTECT_ALL;
                            return NULL;
                        }
                        PROTECT(rtmp);
                        SET_VECTOR_ELT(rvar,i,rtmp);
                    }
                }
                break;
            case TCLY_DICT:
                {
                    Tcl_DictSearch  search;
                    Tcl_Obj         *tkey,*tval;
                    int             done,nattr=0;
                    const char      **attrnames=NULL;
                    SEXP            rtmp,rnames,*attr=NULL;

                    // This will begin the iteration process and put a lock on the dict
                    if( Tcl_DictObjFirst(Interp,TclVar,&search,&tkey,&tval,&done) != TCL_OK ) {
                        // A message will be in the Interp already
                        return NULL;
                    }

                    // Get the length. Since there is a lock in place, the number of items should not change
                    if( Tcl_DictObjSize(Interp,TclVar,&len) != TCL_OK ) {
                        // A message will be in the Interp already
                        return NULL;
                    }

                    if( !len ) {
                        rvar = R_NilValue;
                        break;
                    }

                    // Create the R var and the names attribute vector
                    R_PROTECT( rvar=allocVector(VECSXP,len) );
                    R_PROTECT( rnames=allocVector(STRSXP,len) );

                    // Loop through all values in the dict
                    for(i=0,nattr=0; !done; Tcl_DictObjNext(&search,&tkey,&tval,&done)) {
                        // Transform the dict value into an R one
                        if( !(rtmp=TclR_Tcl2R(Interp,Context,tval,NULL)) ) {
                            // A message will be in the Interp already
                            R_UNPROTECT_ALL;
                            return NULL;
                        }
                        PROTECT(rtmp);

                        // Check if we have an attribute
                        str = Tcl_GetString(tkey);
                        if( !strncmp(str,"attr.",5) && strlen(str)>5 ) {
                            // Since resizing the R vectors with SET_LENGTH removes all attributes, we need to save them for later
                            ++nattr;
                            if( !(attr=realloc(attr,nattr*sizeof(*attr))) || !(attrnames=realloc(attrnames,nattr*sizeof(*attrnames))) ) {
                                Tcl_AppendResult(Interp,"Could not allocate memory",NULL);
                                UNPROTECT(1);
                                R_UNPROTECT_ALL;
                                return NULL;
                            }
                            attr[nattr-1] = rtmp;
                            attrnames[nattr-1] = str+5;
                            // We have an attribute : add it to the list of attributes
                            //setAttrib(rvar,Rf_install(str+5),rtmp);
                        } else {
                            // We have a normal value : add it to the vector and its name to the names attribute
                            SET_VECTOR_ELT(rvar,i,rtmp);
                            SET_STRING_ELT(rnames,i,Rf_mkChar(str));
                            ++i;
                            UNPROTECT(1); // rtmp
                        }
                    }

                    // Adjust the length to account for the attributes
                    if( i != len ) {
                        SET_LENGTH(rvar,i);
                        SET_LENGTH(rnames,i);
                    }

                    // Add all attributes (must be done after SET_LENGTH which removes all attributes)
                    for(i=0; i<nattr; ++i) {
                        setAttrib(rvar,Rf_install(attrnames[i]),attr[i]);
                    }
                    UNPROTECT(nattr);
                    free(attr);
                    free(attrnames);

                    // Add the names to the main vector
                    setAttrib(rvar,R_NamesSymbol,rnames);

                    // Release the lock on the dict
                    Tcl_DictObjDone(&search);
                }
                break;
            default:
                // We consider everything else as string
                R_PROTECT( rvar=allocVector(STRSXP,len) );
                for(i=0; i<len; ++i) {
                    if( !(str=Tcl_GetStringFromObj(*tclvals++,&slen)) ) {
                        Tcl_AppendResult(Interp,"Null string encountered",NULL);
                        R_UNPROTECT_ALL;
                        return NULL;
                    }
                    SET_STRING_ELT(rvar,i,Rf_mkCharLen(str,slen));
                }
                break;
        }
    } else {
        // We have an empty list or an empty string
        rvar = R_NilValue;
    }

    // Assign the value to the right name in the R environment
    if( RName ) {
        Rf_defineVar(Rf_install(RName),rvar,R_GlobalEnv);
    }

    R_UNPROTECT_ALL;
    return rvar;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_TclLsts2RDF>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Transforme une liste Tcl du type {HEADER {ROW1 ROW2 ...}} en une variable R de type data.frame.
 *
 *                Le HEADER et toutes les rangées doivent avoir le même nombre d'item. Les items des rangées peuvent
 *                ne pas être homogènes, mais les rangées doivent être homogènes entre elles (si la première rangée
 *                a des items de type {STRING INT DOUBLE}, alors toutes les rangées doivent suivre ce pattern).
 *                Il est possible de préciser ce pattern pour éviter de mauvaises surprises (int au lieu de double, par ex).
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Context>  : Contexte
 *   <TclVar>   : Variable Tcl à transformer
 *   <RName>    : Nom de la variable R résultante
 *   <Pattern>  : Pattern que suivent les rangées. Dois être sous la forme d'une liste contenant les items suivant,
 *                possiblement précédés d'un nombre multiplicateur : STRING, DOUBLE, INT, BOOLEAN.
 *                Ex : {INT INT DOUBLE} et {2 INT DOUBLE} représentent tous deux le même pattern.
 *
 * Retour       : TCL_OK en cas de succès, TCL_ERROR sinon.
 *
 * Remarques    : Un message d'erreur est laissé dans l'interpréteur en cas d'erreur.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
#define ENDOK           {status=TCL_OK; goto end;}
#define ENDERR(...)     {Tcl_AppendResult(Interp,__VA_ARGS__,NULL); status=TCL_ERROR; goto end;}
#define CHKTCL(x,...)   if( (x) != TCL_OK ) ENDERR(__VA_ARGS__)
#define CHKMEM(v,x)     if( !(v=(x)) ) ENDERR("Could not allocate memory for "#v)
#define FREEMEM(x)      if( (x) ) { free(x); x=NULL; }
#define FREETCL(x)      if( (x) ) { Tcl_DecrRefCount(x); x=NULL; }
static int TclR_TclLst2RDF(Tcl_Interp *Interp,TclR_Context *Context,Tcl_Obj *TclVar,const char *RName,Tcl_Obj *Pattern) {
    enum e_ttype {TT_INT,TT_DOUBLE,TT_BOOLEAN,TT_STRING} *ttypes=NULL;
    SEXP        rdf,rtmp,*rcontent=NULL;
    Tcl_Obj     **members,**content;
    int         nrows,ncols,n,i,j,status=TCL_OK;
    void        **rptrs=NULL;
    char        *buf=NULL;


    static const char *help = "The tcl value should be a list of the form {HEADER {ROW1 ROW2 ...}} where HEADER and ROWx are lists with the same number of items. Furthermore, all the ROWx lists should follow the same pattern.";

    // Get the top list element
    CHKTCL( Tcl_ListObjGetElements(Interp,TclVar,&nrows,&members),"The tcl value should be a list.\n",help );

    // Make sure we have exactly two elements
    if( nrows != 2 ) {
        ENDERR("The tcl list should have two elements.\n",help);
    }

    // Get the Header elements
    CHKTCL( Tcl_ListObjGetElements(Interp,members[0],&ncols,&content),"The first element of the tcl list should be a list of column headers.\n",help );

    // Handle the case when ncols is 0
    if( !ncols ) {
        Rf_defineVar(Rf_install(RName),R_NilValue,R_GlobalEnv);
        ENDOK;
    }

    // Create the base container
    R_PROTECT( rdf=allocVector(VECSXP,ncols) );

    // Convert the header as an R string vector and add it to the base container
    R_PROTECT( rtmp=allocVector(STRSXP,ncols) );
    for(i=0; i<ncols; ++i) {
        SET_STRING_ELT(rtmp,i,Rf_mkChar(Tcl_GetString(content[i])));
    }
    setAttrib(rdf,R_NamesSymbol,rtmp);

    // Get the number of rows
    CHKTCL( Tcl_ListObjGetElements(Interp,members[1],&nrows,&content),"The second element of the tcl list should be a list of column rows.\n",help );

    // Handle the case when nrows is 0
    if( !nrows ) {
        Rf_defineVar(Rf_install(RName),rdf,R_GlobalEnv);
        ENDOK;
    }

    // Allocate the array memory
    CHKMEM( rcontent,malloc(ncols*sizeof(*rcontent)) );
    CHKMEM( ttypes,malloc(ncols*sizeof(*ttypes)) );
    CHKMEM( rptrs,malloc(ncols*sizeof(*rptrs)) );
    CHKMEM( buf,malloc(((int)ceil(log10(nrows))+2)*sizeof(*buf)) );

    // Check for a pattern
    if( Pattern ) {
        Tcl_Obj         **pat;
        int             mult,npat;
        enum e_ttype    ttype;
        SEXPTYPE        rtype;

        // If we have a pattern, use it to determine what kind of vector to create
        CHKTCL( Tcl_ListObjGetElements(Interp,Pattern,&npat,&pat),"The pattern should be a list" );

        // Loop over the pattern
        for(i=0,j=0,mult=0; i<npat; ++i) {
            // Get the rtype
            if( !mult && i<npat-1 && Tcl_GetIntFromObj(NULL,pat[i],&mult)==TCL_OK && mult>0 ) {
                // We have a multiplicator
                continue;
            } else if( !strcmp(Tcl_GetString(pat[i]),"STRING") ) {
                rtype = STRSXP;
                ttype = TT_STRING;
            } else if( !strcmp(Tcl_GetString(pat[i]),"INT") ) {
                rtype = INTSXP;
                ttype = TT_INT;
            } else if( !strcmp(Tcl_GetString(pat[i]),"DOUBLE") ) {
                rtype = REALSXP;
                ttype = TT_DOUBLE;
            } else if( !strcmp(Tcl_GetString(pat[i]),"BOOLEAN") ) {
                rtype = LGLSXP;
                ttype = TT_BOOLEAN;
            } else {
                ENDERR("The pattern should be of the form {?mult? ITEM ?mult? ITEM ...} where ITEM can be any of STRING, INT, DOUBLE, BOOLEAN");
            }

            // If no mult was given, we go for it once
            if( !mult ) mult=1;

            // Make sure we won't write outside of our table
            if( j+mult > ncols ) {
                ENDERR("The pattern describes more rows than what was given");
            }

            // Create the r vectors
            for(; mult; --mult,++j) {
                R_PROTECT( rcontent[j]=allocVector(rtype,nrows) );
                ttypes[j] = ttype;
                switch( ttype ) {
                    case TT_INT:
                        rptrs[j] = INTEGER(rcontent[j]);
                        break;
                    case TT_DOUBLE:
                        rptrs[j] = REAL(rcontent[j]);
                        break;
                    case TT_BOOLEAN:
                        rptrs[j] = LOGICAL(rcontent[j]);
                        break;
                    case TT_STRING:
                        rptrs[j] = NULL;
                        break;
                }
            }
        }

        // Make sure we are not missing vectors
        if( j != ncols ) {
            ENDERR("The pattern should describe all rows");
        }
    } else {
        // We don't have a pattern : we'll have to decide using our error-prone method
        double  dval;
        int     ival;

        // Get the first row's members
        CHKTCL( Tcl_ListObjGetElements(Interp,content[0],&n,&members),"All the rows should be lists\n",help);

        // Make sure we have the right number of columns
        if( n != ncols ) {
            ENDERR("All the rows should have the same number of elements as the header\n",help);
        }

        // Check for a suitable type for each value of the first row
        for(i=0; i<ncols; ++i) {
            if( Tcl_GetIntFromObj(NULL,members[i],&ival) == TCL_OK ) {
                R_PROTECT( rcontent[i]=allocVector(INTSXP,nrows) );
                ttypes[i]   = TT_INT;
                rptrs[i]    = INTEGER(rcontent[i]);
            } else if( Tcl_GetDoubleFromObj(NULL,members[i],&dval) == TCL_OK ) {
                R_PROTECT( rcontent[i]=allocVector(REALSXP,nrows) );
                ttypes[i]   = TT_DOUBLE;
                rptrs[i]    = REAL(rcontent[i]);
            } else if( Tcl_GetBooleanFromObj(NULL,members[i],&ival) == TCL_OK ) {
                R_PROTECT( rcontent[i]=allocVector(LGLSXP,nrows) );
                ttypes[i]   = TT_BOOLEAN;
                rptrs[i]    = LOGICAL(rcontent[i]);
            } else {
                R_PROTECT( rcontent[i]=allocVector(STRSXP,nrows) );
                ttypes[i]   = TT_STRING;
                rptrs[i]    = NULL;
            }
        }
    }

    // Fill the R vectors
    for(i=0; i<nrows; ++i) {
        // Get the row's members
        CHKTCL( Tcl_ListObjGetElements(Interp,content[i],&n,&members),"All the rows should be lists\n",help );

        // Make sure it has the right number of columns
        if( n != ncols ) {
            ENDERR("All the rows should have the same number of elements as the header\n",help);
        }

        // Add every element in the row to each R vector
        for(j=0; j<ncols; ++j) {
            switch( ttypes[j] ) {
                case TT_INT:
                    if( members[j]->length ) {
                        CHKTCL( Tcl_GetIntFromObj(Interp,members[j],(int*)rptrs[j]),Pattern?"The value doesn't respect the pattern":"The pattern might have been wrongly inferred" );
                    } else {
                        *(int*)rptrs[j] = R_NaInt;
                    }
                    rptrs[j] = (int*)rptrs[j]+1;
                    break;
                case TT_DOUBLE:
                    if( members[j]->length ) {
                        CHKTCL( Tcl_GetDoubleFromObj(Interp,members[j],(double*)rptrs[j]),Pattern?"The value doesn't respect the pattern":"The pattern might have been wrongly inferred" );
                    } else {
                        *(double*)rptrs[j] = R_NaReal;
                    }
                    rptrs[j] = (double*)rptrs[j]+1;
                    break;
                case TT_BOOLEAN:
                    if( members[j]->length ) {
                        CHKTCL( Tcl_GetBooleanFromObj(Interp,members[j],(int*)rptrs[j]),Pattern?"The value doesn't respect the pattern":"The pattern might have been wrongly inferred" );
                    } else {
                        *(int*)rptrs[j] = R_NaInt;
                    }
                    rptrs[j] = (int*)rptrs[j]+1;
                    break;
                case TT_STRING:
                    SET_STRING_ELT(rcontent[j],i,Rf_mkChar(Tcl_GetString(members[j])));
                    break;
            }
        }
    }

    // Attach those data vectors to the data.frame
    for(j=0; j<ncols; ++j) {
        SET_VECTOR_ELT(rdf,j,rcontent[j]);
    }

    // Add the row.names
    R_PROTECT( rtmp=allocVector(STRSXP,nrows) );
    for(i=0; i<nrows; ++i) {
        sprintf(buf,"%d",i+1);
        SET_STRING_ELT(rtmp,i,Rf_mkChar(buf));
    }
    Rf_setAttrib(rdf,R_RowNamesSymbol,rtmp);

    // Declare the class as data.frame so that R knows it's a data.frame
    R_PROTECT( rtmp=allocVector(STRSXP,1) );
    SET_STRING_ELT(rtmp,0,Rf_mkChar("data.frame"));
    Rf_setAttrib(rdf,R_ClassSymbol,rtmp);

    // Set the variable in the R environment
    Rf_defineVar(Rf_install(RName),rdf,R_GlobalEnv);

end:
    R_UNPROTECT_ALL;
    free(rcontent);
    free(ttypes);
    free(rptrs);
    free(buf);
    return status;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_RDF2TclLst>
 * Creation     : Septembre 2015 - E. Legault-Ouellet
 *
 * But          : Transforme un data.frame R en une liste Tcl du type {HEADER {ROW1 ROW2 ...}}
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Context>  : Contexte
 *   <RVar>     : Variable R contenant le data.frame à transformer
 *
 * Retour       : L'objet Tcl en cas de succès, NULL sinon.
 *
 * Remarques    : Un message d'erreur est laissé dans l'interpréteur en cas d'erreur.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static Tcl_Obj* TclR_RDF2TclLst(Tcl_Interp *Interp,TclR_Context *Context,SEXP RVar) {
    Tcl_Obj     *tres=NULL,*tobj=NULL,**trows=NULL,**data=NULL,**ptr,**decr=NULL;
    SEXP        rattr,robj;
    const char  *str;
    int         nbdecr=0,cnt,col,row,nbrows=0,nbcols=0,i,status=TCL_OK;

    // Make sure we have a generic vector (the container type of a data.frame)
    if( TYPEOF(RVar) != VECSXP )
        ENDERR("RVar is not a data.frame",NULL);

    // Allocate memory based on the number of columns
    nbcols = LENGTH(RVar);
    CHKMEM(tres,Tcl_NewListObj(0,NULL));
    if( !nbcols )
        ENDOK;
    CHKMEM(decr,malloc((nbcols+1)*sizeof(Tcl_Obj*)));

    // Loop on the attributes to find the attributes we need and that must be present in a data.frame
    for(i=2,rattr=ATTRIB(RVar); rattr!=R_NilValue&&i; rattr=CDR(rattr)) {
        // Get the attribute name
        str = CHAR(PRINTNAME(TAG(rattr)));

        if( !strcmp(str,"names") ) {
            robj = CAR(rattr);
            if( robj==R_NilValue || !Rf_isString(robj) || nbcols!=LENGTH(robj) )
                ENDERR("Invalid \"names\" attribute");

            tobj = TclR_R2Tcl(Interp,Context,robj);
            if( !tobj )
                ENDERR("Could not get the \"names\" attribute");

            // Add the HEADER part
            CHKTCL(Tcl_ListObjAppendElement(Interp,tres,tobj),"Could not add HEADER to resulting list");
            tobj = NULL;
            --i;
        } else if( !strcmp(str,"class") ) {
            // Make sure the class is "data.frame"
            robj = CAR(rattr);
            if( robj==R_NilValue || !Rf_isString(robj) || LENGTH(robj)!=1 || strcmp(CHAR(STRING_ELT(robj,0)),"data.frame") )
                ENDERR("Invalid \"class\" attribute : is this a data.frame?");
            --i;
        }
    }

    // Make sure we are not missing a mandatory attribute
    if( i )
        ENDERR("At least one of the mandatory attributes is missing : is this a data.frame?");

    // Get the number of rows
    if( !(robj=VECTOR_ELT(RVar,0)) )
        ENDERR("Could not get the number of rows from the data.frame. This should NOT happen...");
    if( !(nbrows=LENGTH(robj)) ) {
        // If we don't have any rows, we might as well stop here
        CHKMEM(tobj,Tcl_NewListObj(0,NULL));
        CHKTCL(Tcl_ListObjAppendElement(Interp,tres,tobj),"Could not add empty ROWS to resulting list");
        ENDOK;
    }

    // Allocate the pivot table memory
    CHKMEM(data,malloc(nbrows*nbcols*sizeof(Tcl_Obj*)));

    // Fill the pivot table
    for(col=0; col<nbcols; ++col) {
        // Get the column
        robj = VECTOR_ELT(RVar,col);
        if( LENGTH(robj) != nbrows )
            ENDERR("Invalid data.frame : not all columns have the same length.");

        // Convert into tcl objects
        if( !(tobj=TclR_R2Tcl(Interp,Context,robj)) )
            ENDERR("could not transform column into tcl objects");

        // Add the objects to our data table
        if( nbrows > 1 ) {
            CHKTCL(Tcl_ListObjGetElements(Interp,tobj,&cnt,&ptr),"Could not get element from tcl obj");
            if( cnt != nbrows )
                ENDERR("The number of the items is not equal to the number of R items. This should NEVER happen...");

            for(row=0,i=col; row<nbrows; i+=nbcols,++row)
                data[i] = ptr[row];

            // Add the list to our items to free
            // Note that this is needed because lists increment the count on the object they contain
            // (and any items in this list will be added in the resulting list)
            decr[nbdecr++] = tobj;
        } else {
            data[col] = tobj;
        }
        tobj = NULL;
    }

    // Add the pivot table's data into our resulting tcl list
    CHKMEM(trows,calloc(nbrows,sizeof(Tcl_Obj*)));
    for(i=0,ptr=data; i<nbrows; ++i,ptr+=nbcols) {
        CHKMEM(trows[i],Tcl_NewListObj(nbcols,ptr));
    }
    CHKMEM(tobj,Tcl_NewListObj(nbrows,trows));
    trows[0] = NULL;
    CHKTCL(Tcl_ListObjAppendElement(Interp,tres,tobj),"Could not add ROWS to resulting list");
    tobj = NULL;

end:
    // Let go of the temporary tcl objects
    // It is worth noting that, since all data from the pivot table come from data contained in lists
    // referenced in "decr", this effectively cleans everything from a tcl standpoint
    for(i=0; i<nbdecr; ++i) {
        Tcl_DecrRefCount(decr[i]);
    }

    // Free the result memory only if things went south
    if( status != TCL_OK ) {
        FREETCL( tres );
        FREETCL( tobj );

        for(i=0; i<nbrows&&trows[i]; ++i)
            FREETCL(trows[i]);
    }

    // Free the allocated memory
    FREEMEM( decr );
    FREEMEM( data );
    FREEMEM( trows );

    return tres;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_FSTD2R>
 * Creation     : Novembre 2015 - E. Legault-Ouellet
 *
 * But          : Transforme les données d'un champ FSTD en une "matrix" R
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Context>  : Contexte
 *   <FID>      : Field ID dont les données doivent être passés au monde R
 *   <RVar>     : Nom de la variable R résultante
 *
 * Retour       : TCL_OK en cas de succès, TCL_ERROR sinon.
 *
 * Remarques    : Un message d'erreur est laissé dans l'interpréteur en cas d'erreur.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int TclR_FSTD2R(Tcl_Interp *Interp,TclR_Context *Context,const char *FID,const char *RName) {
    int     i,j,ni,nj,n,*iptr,status=TCL_OK;
    char    buf[sizeof(i)*3+4]; // Since log10(2^8)=2.41, any binary number of x bytes can be represented with 3x decimal characters. +4 is for the "[,]" and the null characters
    SEXP    rvar,rdim,rdimnames,rrownames,rcolnames,rclass;

    char    typebuf[19]; // The longest name we support is "unsigned long long"

    // Check if we have access to the library functions
    if( !Context->EXT_DataStat && !(Context->EXT_DataStat=dlsym(NULL,"EXT_DataStat")) )
        ENDERR("Symbol EXT_DataStat is not loaded. Is the TclGeoEER library loaded?");
    if( !Context->EXT_DataCopy && !(Context->EXT_DataCopy = dlsym(NULL,"EXT_DataCopy")) )
        ENDERR("Symbol EXT_DataCopy is not loaded. Is the TclGeoEER library loaded?");

    // Get the type and size of the buffer we need to allocate
    if( Context->EXT_DataStat(Interp,(char*)FID,typebuf,19,&ni,&nj) != TCL_OK ) {
        ENDERR("\nCould not stat the field");
    }
    n = ni*nj;

    if( n == 0 ) {
        Rf_defineVar(Rf_install(RName),R_NilValue,R_GlobalEnv);
        ENDOK;
    } else if( n < 0 ) {
        ENDERR("Negative number of values : there is something wrong with that field");
    }

    // Allocate the memory for the data and copy it
    if( !strcmp(typebuf,"float")
            || !strcmp(typebuf,"double")
            || !strcmp(typebuf,"unsigned long long")
            || !strcmp(typebuf,"long long")
            || !strcmp(typebuf,"unsigned int") ) {
        R_PROTECT( rvar=allocVector(REALSXP,n) );
        if( Context->EXT_DataCopy(Interp,(char*)FID,"double",REAL(rvar),n,NULL) != TCL_OK ) {
            ENDERR("\nCould not copy data");
        }
    } else if( !strcmp(typebuf,"char")
            || !strcmp(typebuf,"short")
            || !strcmp(typebuf,"int")
            || !strcmp(typebuf,"unsigned char")
            || !strcmp(typebuf,"unsigned short") ) {
        R_PROTECT( rvar=allocVector(INTSXP,n) );
        if( Context->EXT_DataCopy(Interp,(char*)FID,"int",INTEGER(rvar),n,NULL) != TCL_OK ) {
            ENDERR("\nCould not copy data");
        }
    } else {
        ENDERR("Unsupported type : [",typebuf,"]");
    }

    // Allocate the rest of the R memory
    R_PROTECT( rdim=allocVector(INTSXP,2) );
    R_PROTECT( rdimnames=allocVector(VECSXP,2) );
    R_PROTECT( rrownames=allocVector(STRSXP,ni) );
    R_PROTECT( rcolnames=allocVector(STRSXP,nj) );
    R_PROTECT( rclass=allocVector(STRSXP,1) );

    // Dimensions (nrow,ncol)
    iptr = INTEGER(rdim);
    *iptr++ = ni;
    *iptr = nj;

    // Row names
    for(i=0;i<ni;++i) {
        sprintf(buf,"[%d,]",i+1);
        SET_STRING_ELT(rrownames,i,Rf_mkChar(buf));
    }
    SET_VECTOR_ELT(rdimnames,0,rrownames);

    // Column names
    for(j=0;j<nj;++j) {
        sprintf(buf,"[,%d]",j+1);
        SET_STRING_ELT(rcolnames,j,Rf_mkChar(buf));
    }
    SET_VECTOR_ELT(rdimnames,1,rcolnames);

    // Class
    SET_STRING_ELT(rclass,0,Rf_mkChar("matrix"));

    // Set the attributes
    Rf_setAttrib(rvar,R_DimSymbol,rdim);
    Rf_setAttrib(rvar,R_DimNamesSymbol,rdimnames);
    Rf_setAttrib(rvar,R_ClassSymbol,rclass);

    // Set the variable in the R environment
    Rf_defineVar(Rf_install(RName),rvar,R_GlobalEnv);

end:
    R_UNPROTECT_ALL;
    return status;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_RExec>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Exécute une commande R
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Context>  : Contexte
 *   <RCmd>     : Commande R à exécuter
 *   <Res2TclFct: La fonction à utiliser pour transformer le résultat sous forme Tcl. Mettre à NULL pour ne pas
 *                retourner de résultat.
 *
 * Retour       : TCL_OK en cas de succès, TCL_ERROR sinon.
 *
 * Remarques    : Un message d'erreur est laissé dans l'interpréteur en cas d'erreur.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int TclR_RExec(Tcl_Interp *Interp,TclR_Context *Context,const char* RCmd,Tcl_Obj* (*Res2TclFct)(Tcl_Interp*,TclR_Context*,SEXP)) {
    SEXP rcmd,rexpr,res=R_NilValue;
    ParseStatus pstatus;
    R_len_t i;
    int err;

    // Put the string in an R vector string
    R_PROTECT( rcmd=allocVector(STRSXP,1) );
    SET_STRING_ELT(rcmd,0,mkChar(RCmd));

    // Parse the string
    // -1 is the max number of expr to parse; The null value indicates that this doesn't come from a source file (a filename would then have been given)
    R_PROTECT( rexpr=R_ParseVector(rcmd,-1,&pstatus,R_NilValue) );
    if( pstatus != PARSE_OK ) {
        // Release our protected variables
        R_UNPROTECT_ALL;

        // Return a (hopefully) meaningful error
        switch( pstatus ) {
            case PARSE_INCOMPLETE:
                Tcl_AppendResult(Interp,"Error while parsing the R expression : expression is incomplete\nExpression was : ",RCmd,NULL);
                return TCL_ERROR;
            case PARSE_ERROR:
                Tcl_AppendResult(Interp,"Error while parsing the R expression : syntax error\nExpression was : ",RCmd,NULL);
                return TCL_ERROR;
            case PARSE_NULL:
                Tcl_AppendResult(Interp,"Error while parsing the R expression : parsing returned PARSE_NULL\nExpression was : ",RCmd,NULL);
                return TCL_ERROR;
            case PARSE_EOF:
                Tcl_AppendResult(Interp,"Error while parsing the R expression : parsing returned PARSE_EOF\nExpression was : ",RCmd,NULL);
                return TCL_ERROR;
            default:
                Tcl_AppendResult(Interp,"Error while parsing the R expression\nExpression was : ",RCmd,NULL);
                return TCL_ERROR;
        }
    }

    // Print the command if necessary
    if( Context->TraceCmd==TCLR_TRACE_ALL && Context->Chan ) {
        if( Tcl_WriteChars(Context->Chan,"=== CMD ===\n",-1)==-1 || Tcl_WriteChars(Context->Chan,RCmd,-1)==-1 || Tcl_WriteChars(Context->Chan,"\n",-1)==-1) {
            Tcl_AppendResult(Interp,"An error occured while writing to the channel",NULL);
            R_UNPROTECT_ALL;
            return TCL_ERROR;
        }
    }

    // Execute the statements
    for(i=0; i<length(rexpr); ++i) {
        // Unprotect the last "res"
        if( i ) R_UNPROTECT;

        // Execute the statement and protect the new "res"
        R_PROTECT( res=R_tryEval(VECTOR_ELT(rexpr,i),R_GlobalEnv,&err) );
        if( err ) {
            Tcl_AppendResult(Interp,"Error while executing expression\nExpression was : ",RCmd,NULL);
            R_UNPROTECT_ALL;
            return TCL_ERROR;
        }

        // Print the intermediate result if necessary
        if( Context->TraceRes==TCLR_TRACE_ALL && Context->Chan && res!=R_NilValue ) {
            if( Tcl_WriteChars(Context->Chan,"=== RES ===\n",-1)==-1 ) {
                Tcl_AppendResult(Interp,"An error occured while writing to the channel",NULL);
                R_UNPROTECT_ALL;
                return TCL_ERROR;
            }
            if( TclR_RPrint(Interp,Context,res) != TCL_OK ) {
                R_UNPROTECT_ALL;
                return TCL_ERROR;
            }
        }
    }

    // Print the last statement result if necessary
    if( Context->TraceRes==TCLR_TRACE_LAST && Context->Chan && res!=R_NilValue ) {
        if( Tcl_WriteChars(Context->Chan,"=== RES ===\n",-1)==-1 ) {
            Tcl_AppendResult(Interp,"An error occured while writing to the channel",NULL);
            R_UNPROTECT_ALL;
            return TCL_ERROR;
        }
        if( TclR_RPrint(Interp,Context,res) != TCL_OK ) {
            R_UNPROTECT_ALL;
            return TCL_ERROR;
        }
    }

    // Return the result in the interpreter if necessary
    if( Res2TclFct && res!=R_NilValue ) {
        Tcl_Obj* tobj = Res2TclFct(Interp,Context,res);
        if( !tobj ) {
            R_UNPROTECT_ALL;
            return TCL_ERROR;
        }
        Tcl_SetObjResult(Interp,tobj);
    }

    R_UNPROTECT_ALL;
    return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_Configure>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Configurer le contexte de l'interface
 *
 * Parametres   :
 *   <Interp>   : Interpreteur Tcl
 *   <Context>  : Contexte à configurer
 *   <Option>   : Option à changer/retourner
 *   <Value>    : Nouvelle valeur de l'option ou NULL pour une query
 *
 * Retour       : TCL_OK en cas de succès, TCL_ERROR sinon.
 *
 * Remarques    : Un message d'erreur est laissé dans l'interpréteur en cas d'erreur, le résultat en cas de query.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int TclR_Configure(Tcl_Interp *Interp,TclR_Context *Context,Tcl_Obj *Option,Tcl_Obj *Value) {
    const char  *str;
    int         idx;

    // List of available options
    static const char *sopt[] = {"-tracechannel","-resulttraces","-commandtraces"};
    enum                opt {TRACECHANNEL,RESULTTRACES,COMMANDTRACES};

    // List of available resulttraces and commandtraces options (must match the TclR_TTrace enum)
    static const char *srestraces[] = {"PRINT_NONE","PRINT_ALL","PRINT_LAST"};
    static const char *scmdtraces[] = {"PRINT_NONE","PRINT_ALL"};

    // Get the subcommand index
    TCL_ASRT( Tcl_GetIndexFromObj(Interp,Option,sopt,"option",0,&idx) );

    switch( idx ) {
        case TRACECHANNEL:
            if( Value ) {
                int perm;
                Tcl_Channel chan = Tcl_GetChannel(Interp,Tcl_GetString(Value),&perm);

                // Make sure we have a valid and writable channel
                if( !chan ) return TCL_ERROR;
                if( !(perm&TCL_WRITABLE) ) {
                    Tcl_AppendResult(Interp,"Channel is not writable",NULL);
                    return TCL_ERROR;
                }

                // Set the context
                Context->Chan = chan;
            } else {
                // Return the channel name
                if( Context->Chan && (str=Tcl_GetChannelName(Context->Chan)) ) {
                    if( !strcmp(str,"serial1") )
                        Tcl_SetObjResult(Interp,Tcl_NewStringObj("stdout",-1));
                    else if( !strcmp(str,"serial2") )
                        Tcl_SetObjResult(Interp,Tcl_NewStringObj("stderr",-1));
                    else
                        Tcl_SetObjResult(Interp,Tcl_NewStringObj(str,-1));
                }
            }
            break;
        case RESULTTRACES:
            if( Value ) {
                TCL_ASRT( Tcl_GetIndexFromObj(Interp,Value,srestraces,"value",0,&idx) );
                Context->TraceRes = idx;
            } else {
                Tcl_SetObjResult(Interp,Tcl_NewStringObj(srestraces[Context->TraceRes],-1));
            }
            break;
        case COMMANDTRACES:
            if( Value ) {
                TCL_ASRT( Tcl_GetIndexFromObj(Interp,Value,scmdtraces,"value",0,&idx) );
                Context->TraceCmd = idx;
            } else {
                Tcl_SetObjResult(Interp,Tcl_NewStringObj(scmdtraces[Context->TraceCmd],-1));
            }
            break;
        default:
            Tcl_AppendResult(Interp,"Unimplemented command",NULL);
            return TCL_ERROR;
    }

    return TCL_OK;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_Cmd>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Entry point pour la gestion de la commande-objet "R"
 *
 * Parametres   :
 *   <CData>    : Donnée client (word)
 *   <Interp>   : Interpreteur Tcl
 *   <Objc>     : Nombre d'arguments donnés à la commande
 *   <Objv>     : Arguments donnés à la commande
 *
 * Retour       : TCL_OK en cas de succès, TCL_ERROR sinon
 *
 * Remarques    : Un message d'erreur est laissé dans l'interpréteur en cas d'erreur.
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static int TclR_Cmd(ClientData CData,Tcl_Interp *Interp,int Objc,Tcl_Obj *const Objv[]) {
    TclR_Context    *context = (TclR_Context*)CData;
    const char      *str;
    Tcl_Obj         *tobj;
    int             idx,status=TCL_OK;

    // The list of available subcommands
    static const char *sopt[] = {"exec","type","print","tcl2r","r2tcl","tcllst2rdf","rdf2tcllst","fstd2r","waitondevices","configure"};
    enum                opt {EXEC,TYPE,PRINT,TCL2R,R2TCL,TCLLST2RDF,RDF2TCLLST,FSTD2R,WAIT_ON_DEVICES,CONFIGURE};

    Tcl_ResetResult(Interp);

    // Make sure we have a subcommand
    if( Objc < 2 ) {
        Tcl_WrongNumArgs(Interp,1,Objv,"subcommand ?arg arg ...?");
        return TCL_ERROR;
    }

    // Get the subcommand index
    TCL_ASRT( Tcl_GetIndexFromObj(Interp,Objv[1],sopt,"command",0,&idx) );

    // Process the subcommand
    switch( (enum opt)idx ) {
        case EXEC:
            // Make sure we have a command to execute
            if( Objc<3 || Objc>5 ) {
                Tcl_WrongNumArgs(Interp,2,Objv,"?-get ?[r2tcl|rdf2tcllst]?? r_command");
                status = TCL_ERROR;
                break;
            }
            // Get the R command
            if( Objc <= 4 ) {
                TCL_ASRT( TclR_RExec(Interp,context,Tcl_GetString(Objv[Objc-1]),Objc==4?TclR_R2Tcl:NULL) );
            } else {
                str = Tcl_GetString(Objv[Objc-2]);
                if( !strcmp(str,"r2tcl") ) {
                    TCL_ASRT( TclR_RExec(Interp,context,Tcl_GetString(Objv[Objc-1]),TclR_R2Tcl) );
                } else if( !strcmp(str,"rdf2tcllst") ) {
                    TCL_ASRT( TclR_RExec(Interp,context,Tcl_GetString(Objv[Objc-1]),TclR_RDF2TclLst) );
                } else {
                    Tcl_AppendResult(Interp,"Not a valid transformation command for the -get option. Should be either r2tcl or rdf2tcllst.",NULL);
                    status = TCL_ERROR;
                    break;
                }
            }
            break;
        case TYPE:
            // Make sure we have the name of a variable to get the type from
            if( Objc != 3 ) {
                Tcl_WrongNumArgs(Interp,2,Objv,"rvar");
                status = TCL_ERROR;
                break;
            }
            str = Tcl_GetString(Objv[2]);
            str = TclR_RTypeName(TclR_RObjFromName(context,str));
            Tcl_SetObjResult(Interp,Tcl_NewStringObj(str,-1));
            break;
        case PRINT:
            // Make sure we have the name of a variable to print
            if( Objc != 3 ) {
                Tcl_WrongNumArgs(Interp,2,Objv,"rvar");
                status = TCL_ERROR;
                break;
            }
            str = Tcl_GetString(Objv[2]);
            TCL_ASRT( TclR_RPrint(Interp,context,TclR_RObjFromName(context,str)) );
            break;
        case TCL2R:
            // Make sure we have a tclvar and a name to transform to an rvar
            if( Objc != 4 ) {
                Tcl_WrongNumArgs(Interp,2,Objv,"tclvar rname");
                status = TCL_ERROR;
                break;
            }
            str = Tcl_GetString(Objv[3]);
            if( !TclR_Tcl2R(Interp,context,Objv[2],str) ) {
                status = TCL_ERROR;
            }
            break;
        case R2TCL:
            // Make sure we have the name of a variable to tcl-ify
            if( Objc != 3 ) {
                Tcl_WrongNumArgs(Interp,2,Objv,"rvar");
                status = TCL_ERROR;
                break;
            }
            str = Tcl_GetString(Objv[2]);
            tobj = TclR_R2Tcl(Interp,context,TclR_RObjFromName(context,str));
            if( !tobj ) {
                status = TCL_ERROR;
                break;
            }
            Tcl_SetObjResult(Interp,tobj);
            break;
        case TCLLST2RDF:
            // Make sure we have a tcl variable and the name of an R data.frame
            if( Objc<4 || Objc>5 ) {
                Tcl_WrongNumArgs(Interp,2,Objv,"tcllist rdfname ?pattern?");
                status = TCL_ERROR;
                break;
            }
            str = Tcl_GetString(Objv[3]);
            TCL_ASRT( TclR_TclLst2RDF(Interp,context,Objv[2],str,Objc==5?Objv[4]:NULL) );
            break;
        case RDF2TCLLST:
            // Make sure we have the name of a variable to tcl-ify
            if( Objc != 3 ) {
                Tcl_WrongNumArgs(Interp,2,Objv,"rvar");
                status = TCL_ERROR;
                break;
            }
            str = Tcl_GetString(Objv[2]);
            tobj = TclR_RDF2TclLst(Interp,context,TclR_RObjFromName(context,str));
            if( !tobj ) {
                status = TCL_ERROR;
                break;
            }
            Tcl_SetObjResult(Interp,tobj);
            break;
        case FSTD2R:
            // Make sure we have a field ID and the name of the resulting R variable
            if( Objc!=3 && Objc!=4 ) {
                Tcl_WrongNumArgs(Interp,2,Objv,"fieldID ?rname?");
                status = TCL_ERROR;
                break;
            }
            str = Tcl_GetString(Objv[2]);
            TCL_ASRT( TclR_FSTD2R(Interp,context,str,Objc==4?Tcl_GetString(Objv[3]):str) );
            break;
        case WAIT_ON_DEVICES:
            TCL_ASRT( TclR_RExec(Interp,context,"while( length(dev.list()) > 0 ) { Sys.sleep(0.25) }",0) );
            break;
        case CONFIGURE:
            if( Objc == 3 ) {
                // We have one thing to return
                TCL_ASRT( TclR_Configure(Interp,context,Objv[2],NULL) );
            } else if( Objc<3 || Objc&1 ) {
                // We should have either one thing to return or one or more things to configure
                Tcl_WrongNumArgs(Interp,2,Objv,"option ?value? ?[option value]? ?...?");
                status = TCL_ERROR;
            } else {
                // We have one or more things to configure
                int i;

                for(i=2; i<Objc; i+=2) {
                    TCL_ASRT( TclR_Configure(Interp,context,Objv[i],Objv[i+1]) );
                }
            }
            break;
        default:
            Tcl_AppendResult(Interp,"Unimplemented command",NULL);
            status = TCL_ERROR;
            break;
    }

    return status;
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <TclR_CmdDelete>
 * Creation     : Août 2015 - E. Legault-Ouellet
 *
 * But          : Fonction appellée lorsque la librairie est libérée
 *
 * Parametres   :
 *   <CData>    : Donnée client
 *
 * Retour       :
 *
 * Remarques    :
 *
 *---------------------------------------------------------------------------------------------------------------
*/
static void TclR_CmdDelete(ClientData CData) {
    TclR_RRelease();
    free((TclR_Context*)CData);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <Tclr_Init>
 * Creation     : Août 2015 - E. Legault-Ouellet
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
int Tclr_Init(Tcl_Interp *Interp) {
    TclR_Context *context;

    // Since we link with the stubs lib, we need to init the API
    if( Tcl_InitStubs(Interp,"8.5",0) == NULL )
        return TCL_ERROR;

    // Provide the package
    TCL_ASRT( Tcl_PkgProvide(Interp,PACKAGE_NAME,PACKAGE_VERSION) );

    // Initialize the context
    if( !(context=malloc(sizeof(*context))) ) {
        Tcl_AppendResult(Interp,"Could not allocate context memory",NULL);
        return TCL_ERROR;
    }
    *context = (TclR_Context){0,TCLR_TRACE_NONE,TCLR_TRACE_NONE,Tcl_GetChannel(Interp,"stdout",NULL),NULL,NULL};

    // Create the internal command used to access this extension
    Tcl_CreateObjCommand(Interp,"R",TclR_Cmd,(ClientData)context,TclR_CmdDelete);

    // Initialize the R lib
    TclR_RInit();

    return TCL_OK;
}

