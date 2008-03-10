# Source in configuration

include ../Makefile.conf

# Makefile Specific definitions

INCLUDES    = $(INCLUDE)
CFLAGS       = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(LIBS) $(PROTO) $(DEFINES)

# Start of Makefile

OBJ         = Vector.o sunpos.o tr.o gpc.o

all  : $(OBJ)

clean:
	rm *~ *.o
