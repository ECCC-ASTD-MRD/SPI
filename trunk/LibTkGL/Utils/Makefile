ARCH        = $(shell uname)

# Source in configuration

include ../Makefile.$(ARCH)

# Makefile Specific definitions

INCLUDES    = $(INCLUDE)
CFLAGS       = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(LIBS) $(PROTO) $(DEFINES)

# Start of Makefile

OBJ         = Vector.o sunpos.o tr.o gpc.o

all  : $(OBJ)

clean:
	rm *~ *.o
