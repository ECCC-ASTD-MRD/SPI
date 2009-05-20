ARCH        = $(shell uname)
VERSION     = 1.0

# Source in configuration

include ../Makefile.$(ARCH)

# Makefile Specific definitions

INCLUDES   = $(INCLUDE)
DEFINES    = -DTCL_THREADS -DVERSION=\"$(VERSION)\"
CFLAGS     = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(LIBS) $(PROTO) $(DEFINES)
LIBHM      = -lc
OBJS       = tclSystem.o

# Start of Makefile

all  : $(OBJS)

link: 
	$(LD) -o libTclSystem-$(VERSION).so $(OBJS) $(LIBHM)
	cp libTclSystem-$(VERSION).so $(INSTALL_PATH)
	rm -f libTclSystem.so $(INSTALL_PATH)/libTclSystem.so
	ln -s libTclSystem-$(VERSION).so libTclSystem.so
	ln -s libTclSystem-$(VERSION).so $(INSTALL_PATH)/libTclSystem.so

install: all link

clean:
	-rm -f *.o *~ *.lo *.la *.so
