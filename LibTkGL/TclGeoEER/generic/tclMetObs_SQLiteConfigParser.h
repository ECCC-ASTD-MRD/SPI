#ifndef _METOBSSQLITE_GETQUERIES_H
/*--------------------------------------------------------------------------------------------------------------
 * Nom          : <MetObsSQLite_GetQueries>
 * Creation     : Juillet 2018 Philippe Carphin
 *
 * But          : Obtenir des données textuelles dans un fichier de config avec
 *                micro-langage basé sur des blocs de type
 *
 *                #BEGIN_XY [clé]
 *                ...
 *                #END XY
 *
 * Parametres   :
 *  <Filename>  : Fichier de contenant les définitions de requêtes pour les
 *                différents schémas connus.
 *  <Key>       : Chaine de caractères désignant un schéma particulier de base
 *                de données SQLite.  Dénoté tans le fichier de config par
 *
 *                            #BEGIN_SCHEMA une_clé
 *
 * <ObsQueryOut>:
 *<ElemQueryOut>: Les contenus des blocs de suivants dans le fichier de config.
 *
 *                   #BEGIN_SCHEMA key
 *                   #BEGIN_QUERY obs
 *                   <ObsQueryOut>
 *                   #END_QUERY
 *                   #BEGIN_QUERY elem
 *                   <ElemQueryOut>
 *                   #END_QUERY
 *                   #END_SCHEMA
 *
 * <db_filename>: Nom du fichier à ouvrir
 *
 * Retour       : Code d'erreur TCL
 *
 * Remarques : Référez vous à la documentation pour la syntaxe du fichier de
 *             configuration. Portez attention aux messages d'erreur fournis par
 *             l'implémentation.
 *
 *             Cette partie du programme ne valide pas les information extraites
 *             du fichier de configuration.
 *--------------------------------------------------------------------------------------------------------------*/
int MetObsSQLite_GetQueries(const char *filename, const char *key, char **obs_query_out, char **elem_query_out);
#endif
