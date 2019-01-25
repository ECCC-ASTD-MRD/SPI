/*
 * A cool file with some debugging macros to print the values of variables
 * without having to write a debug message.
 *
 * Should be included in any substancial project where some listings might be
 * useful.
 */

#ifdef PHIL_DEBUG
#define DBG_PRINT(...) do{fprintf(stderr,"\33[2K%s():%d ",__func__, __LINE__);fprintf(stderr,__VA_ARGS__);}while(0)
#else
#define DBG_PRINT(...) do{}while(0)
#endif

#define IVAL(var)      DBG_PRINT("Value of integer " #var " is %d\n", var)

#define SVAL(var)      DBG_PRINT("Value of string " #var " is %s\n", var)

#define VARVALUE(var,type) DBG_PRINT("Value of variable " #var " is " #type "\n", var)

#define FUNCBEGIN      DBG_PRINT("BEGIN\n");

#define FUNCEND        DBG_PRINT("END\n");
