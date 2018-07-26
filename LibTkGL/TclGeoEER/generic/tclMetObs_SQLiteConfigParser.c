/*=========================================================
 * Environnement Canada
 * Centre Meteorologique Canadien
 * 2100 Trans-Canadienne
 * Dorval, Quebec
 *
 * Projet       : Chargemetn de fichier SQLite
 * Fichier      : tclMetObs_SQLiteConfigParser
 * Creation     : 2018-07 - Philippe Carphin

 * Description : La ce fichier regarde dans un fichier de
 *               configurations pour obtenir des requêtes
 *               SQL marquées par une clé. Ceci permet aux
 *               utilisateurs de choisir la requête que
 *               SPI va utiliser pour parcourir la BD.
 *
 *               Voir documentation pour les contraintes
 *               que ces requêtes doivent respecter pour
 *               être utilisables.
 *
 * Remarques : Ce module ne fait aucune validation des
 *             requêtes.
 *
 * License : This library is free software; you can
 *    redistribute it and/or modify it under the terms of
 *    the GNU Lesser General Public License as published
 *    by the Free Software Foundation, version 2.1 of the
 *    License.
 *
 *    This library is distributed in the hope that it will
 *    be useful, but WITHOUT ANY WARRANTY; without even
 *    the implied warranty of MERCHANTABILITY or FITNESS
 *    FOR A PARTICULAR PURPOSE. See the GNU Lesser General
 *    Public License for more details.
 *
 *    You should have received a copy of the GNU Lesser
 *    General Public License along with this library; if
 *    not, write to the Free Software Foundation, Inc., 59
 *    Temple Place - Suite 330,
 *
 *=========================================================
 */
#include <stdlib.h>
#include <string.h>
#include <stdio.h>


/********************************************************************************
 * Name        : Basic string manipulation functions for the parser
 * Description : Used to handle whitespace, comments checking prefixes
 *******************************************************************************/
int starts_with(const char *line, const char *prefix);
#define IS_WHITE(x) ((x) == ' ' || (x) == '\t' || (x) == '\n');
int is_white(const char *line);
int ignore_line(const char *line);

/*
 * Parser state and response
 */
struct SpiKeyValue {
   FILE *f;
   int   line;
   const char *key;
   char *obs_query;
   char *elem_query;
};

/********************************************************************************
 * Name        : cpfgets(char *s, int n, struct SpiKeyValue *this)
 * Description : Wrapper for doing fgets and incrementing the current line in
 *               the parser state for error reporting.
 * Arguments   : s : out param for the string read from the filename
 *               n : max number of chars to read
 *            this : Parser state
 * Return      : Pointer to string read from file
 * Remarks     : 
 *******************************************************************************/
static char *cpfgets(char *s, int n, struct SpiKeyValue *this)
{
   this->line++;
   return fgets(s, n, this->f);
}

/*--------------------------------------------------------------------------------------------------------------
 * Nom          : MetObsSQLite_GetQueries(const char *Filename, const char *Key, char **ObsQueryOut, char **ElemQueryOut)
 *
 * Creation     : 2018-07 par Philippe Carphin
 *
 * But : Interface function for SPI schema interaction files. Finds the correct
 *       observation and element queries needed for SPI to load a database whose
 *       schema is designated by the paramter key.
 *
 * Parametres   :
 *   <Filename> : The path to the config file to be parsed
 *   <Key>      : The key to look for in the file
 *<ObsQueryOut> : Out parameter for the observation query
 *<ElemQueryOut>: Out parameter for the element query
 *
 * Retour       : Code d'erreur standard (0 succès, -1 échec)
 *
 * Remarques    : 
 *
 *--------------------------------------------------------------------------------------------------------------*/

static int ParseFile(struct SpiKeyValue *this);
int MetObsSQLite_GetQueries(const char *Filename, const char *Key, char **ObsQueryOut, char **ElemQueryOut)
{
   FILE *f;
   if((f = fopen(Filename, "r")) == NULL){
      fprintf(stderr, "ERROR: Could not open config file  %s\n", Filename);
      return -1;
   }

   struct SpiKeyValue skv = {f,0, Key, NULL, NULL};

   if(ParseFile(&skv)){
      return -1;
   }

   *ObsQueryOut = strdup(skv.obs_query);
   *ElemQueryOut = strdup(skv.elem_query);

   return 0;
}

/********************************************************************************
 * Name        : ParseFile(struct SpiKeyValue *this)

 * Description : Drives the parsing of schemas across the loaded file. Ignores
 *               all lines until a line is found starting with '#BEGIN_SCHEMA
 *               xyz'. It extracts the key and if it matches, the function hands
 *               control to ParseSchema. If the key doens't match, then it
 *               continues till the next '#BEGIN_SCHEMA abc'.
 * Arguments   : this (the parser state)
 * Return      : Standard error codes
 * Remarks     : The only thing we are allowed to see in this state is
 * #BEGIN_SCHEMA, anything else is an error.
 *******************************************************************************/
static int ParseSchema(struct SpiKeyValue *this);
static int SkipSchema(struct SpiKeyValue *this);
static int ParseFile(struct SpiKeyValue *this)
{
   char line[1024];
   char buff[1024];

   while(cpfgets(line, sizeof(line), this)){
      if(ignore_line(line))
         continue;

      if(!starts_with(line, "#BEGIN_SCHEMA ")){
         fprintf(stderr, "PARSE_FILE : UNEXPECTED TOKEN AT LINE %d\n", this->line);
         return -1;
      }

      if(sscanf(line, "#BEGIN_SCHEMA %20s", buff) != 1){
         fprintf(stderr, "COULD NOT EXTRACT SCHEMA NAME FROM LINE %d\n", this->line);
         return -1;
      }

      if(strcmp(buff, this->key) != 0){
         if(SkipSchema(this))
            return -1;
      } else {
         return ParseSchema(this);
      }
   }

   fprintf(stderr, "KEY '%s' NOT FOUND IN FILE\n", this->key);
   return -1;
}

/********************************************************************************
 * Name        : SkipSchema(struct SpiKeyValue *this)
 * Description : Advance the parser past the next #END_SCHEMA token
 * Arguments   : this (the parser state)
 * Return      : Standard error codes
 * Remarks : Ignores everything except #END_SCHEMA which signals that we're
 *           done, and #BEGIN_SCHEMA which is an error.
 *******************************************************************************/
static int SkipSchema(struct SpiKeyValue *this)
{
   char line[1024];
   while(cpfgets(line, sizeof(line), this)){
      if(starts_with(line, "#END_SCHEMA")){
         return 0;
      } else if(starts_with(line, "#BEGIN_SCHEMA")){
         fprintf(stderr, "UNEXPECTED '#BEGIN_SCHEMA' on line %d\n", this->line);
         return -1;
      }
   }
   fprintf(stderr, "UNEXPECTED END OF FILE WHILE LOOKING FOR '#END_SCHEMA'\n");
   return -1;
}

/********************************************************************************
 * Name        : ParseSchema(struct SpiKeyValue *this)
 * Description : Parse the inside of a #BEGIN_SCHEMA ... #END_SCHEMA
 * Arguments   : this (the parser state)
 * Return      : standard error codes
 * Remarks     : The only token allowed is #BEGIN_QUERY, which causes a change
 *               to the ParseQuery state and #END_SCHEMA which causes an exit
 *               from this state.
 *******************************************************************************/
static int ParseQuery(struct SpiKeyValue *this, const char *query_name);
static int ParseSchema(struct SpiKeyValue *this)
{
   char line[1024];
   char query_name[1024];

   while(cpfgets(line, sizeof(line), this)){
      if(ignore_line(line))
         continue;

      if(starts_with(line, "#END_SCHEMA")){
         return 0;
      }

      if(!starts_with(line, "#BEGIN_QUERY")){
         fprintf(stderr, "UNEXPECTED TOKEN WHILE PARSING SCHEMA AT LINE %d\n", this->line);
         return -1;
      }

      if(sscanf(line, "#BEGIN_QUERY %20s", query_name) != 1){
         fprintf(stderr, "Could not get query name from line %d\n", this->line);
         return -1;
      }

      if(ParseQuery(this, query_name)){
         return -1;
      }
   }
   fprintf(stderr, "UNEXPECTED END-OF-FILE WHILE PARSING SCHEMA");
   return -1;
}

/********************************************************************************
 * Name        : ParseQuery(struct SpiKeyValue *this, const char *query_name)
 * Description : Parse a query (basically accumulates all the text up to
 *               #END_QUERY)
 * Arguments   : this (parser state)
 *               query_name (name parsed by ParseSchema when it saw the
 *                          #BEGIN_QUERY line)
 *                          It is used to decide where to put the query text in
 *                          the parser's response fields
 * Return      : standard error codes
 * Remarks     : #END
 *******************************************************************************/
static int ParseQuery(struct SpiKeyValue *this, const char *query_name)
{
   char line[1024];
   char query_str[4096] = {0};
   int query_start = this->line;
   while(cpfgets(line, sizeof(line), this)){
      if(ignore_line(line))
         continue;

      if(starts_with(line, "#END_QUERY")){
         goto done;
      }

      if(starts_with(line, "#BEGIN") || starts_with(line, "#END")){
         fprintf(stderr, "Parsing Query : UNEXPECTED TOKEN at line %d\n", this->line);
         return -1;
      }

      size_t space_left = sizeof(query_str) - strlen(query_str) - 1;
      if(space_left < strlen(line)){
         fprintf(stderr, "Your query is too long for %s()'s buffer of 4096 chars\n", __func__);
      }
      strncat(query_str, line, space_left);;
   }

   fprintf(stderr, "UNEXPECTED END OF FILE");
   return -1;

done:
   if(strcmp("obs", query_name) == 0)
      this->obs_query = strdup(query_str);
   else if (strcmp("elem", query_name) == 0)
      this->elem_query = strdup(query_str);
   else {
      fprintf(stderr,"UNRECOGNIZED QUERY NAME '%s' at line %d\n", query_name, query_start);
      return -1;
   }
   return 0;
}


/********************************************************************************
 * Check if prefix is a prefix of line
 ********************************************************************************/
int starts_with(const char *line, const char *prefix){
   return strncmp(line, prefix, strlen(prefix)) == 0;
}

/********************************************************************************
 * check if a line is only whitespace
 ********************************************************************************/
int is_white(const char *line)
{
   const char *s = line;
   char c;
   while((c = *(s++)) != 0){
      if(!((c) == ' ' || (c) == '\t' || (c) == '\n')){
         return 0;
      }
   }
   return 1;
}

/********************************************************************************
 * Check if a line is to be ignored
 ********************************************************************************/
int ignore_line(const char *line)
{
   return starts_with(line, "//") || is_white(line);
}

#ifdef _SELF_TEST
/*
 * This file can be compiled and run without any external dependencies.  If it
 * is compiled with -D_SELF_TEST, then it can be run as an executable for
 * testing in isolation.  Something like:
 *
 *     gcc -o ../MetObsTest/parser ./tclMetObs_SQLiteConfigParser.c ;
 *      ../MetObsTest/parser ../MetObsTest/spi_queries.txt
 *
 */
static int get_values(const char *filename, const char *key, char **obs_query_out, char **elem_query_out){
   return MetObsSQLite_GetQueries(filename, key, obs_query_out, elem_query_out);
}
int main(int argc, char *argv[])
{
   char *first_val;
   char *second_val;

   const char *infile = (argc > 1 ? argv[1] : "spi_queries.txt");

   char *key = "key1";
   /*
    * This test should be successful
    */
   if(get_values(infile, key, &first_val, &second_val)){
      fprintf(stderr, "MAIN: Could not get values for key %s\n", key);
      return -1;
   }
   fprintf(stderr, "========== Values for %s ==============\n", key);
   fprintf(stderr, "========== obs query ================\n%s", first_val);
   fprintf(stderr, "========== elem query ===============\n%s", second_val);
   fprintf(stderr, "========================================\n\n");

   /*
    * This test should fail because the key is not in the file
    */
   key = "key2";
   if(get_values(infile , "key2", &first_val, &second_val)){
      fprintf(stderr,
            "MAIN: Could not get values for key (As expected because "
            "there is no schema named 'key2' %s\n",
            "key2");;
   }

   key = "notkey2";
   if(get_values(infile , key, &first_val, &second_val)){
      fprintf(stderr, "MAIN: Could not get values for key %s\n", key);
      return -1;
   }
   fprintf(stderr, "\n========== Values for %s ==============\n", key);
   fprintf(stderr, "========== obs query ================\n%s", first_val);
   fprintf(stderr, "========== elem query ===============\n%s", second_val);
   fprintf(stderr, "========================================\n\n");

   key = "acars";
   if(get_values(infile , key, &first_val, &second_val)){
      fprintf(stderr, "MAIN: Could not get values for key %s\n", key);
      return -1;
   }
   fprintf(stderr, "\n========== Values for %s ==============\n", key);
   fprintf(stderr, "========== obs query ================\n%s", first_val);
   fprintf(stderr, "========== elem query ===============\n%s", second_val);
   fprintf(stderr, "========================================\n\n");

   key = "other";
   if(get_values(infile , key, &first_val, &second_val)){
      fprintf(stderr, "MAIN: Could not get values for key %s\n", key);
      return -1;
   }
   fprintf(stderr, "\n========== Values for %s ==============\n", key);
   fprintf(stderr, "========== obs query ================\n%s", first_val);
   fprintf(stderr, "========== elem query ===============\n%s", second_val);
   fprintf(stderr, "========================================\n\n");
   return 0;
}
#endif
