ARCH        = $(shell uname)
VERSION     = 1.0

# Source in configuration

include ../Makefile.$(ARCH)

# Makefile Specific definitions

INCLUDES   = $(INCLUDE)
CFLAGS     = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(PROTO) $(DEFINES)
LDFLAGS    = $(LDDEBUGFLAGS)
LIBS       = -lc
OBJS       = tclSystem.o

# Start of Makefile

all  : $(OBJS)

link: 
	$(LD) -o libTclSystem-$(VERSION).so $(OBJS) $(LDFLAGS) $(LIBS)
	cp libTclSystem-$(VERSION).so $(LIB_PATH)
	rm -f libTclSystem.so $(LIB_PATH)/libTclSystem.so
	ln -s libTclSystem-$(VERSION).so libTclSystem.so
	ln -s libTclSystem-$(VERSION).so $(LIB_PATH)/libTclSystem.so

install: all link

clean:
	-rm -f *.o *~ *.lo *.la *.so
