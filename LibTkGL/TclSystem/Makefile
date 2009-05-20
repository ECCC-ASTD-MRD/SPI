ARCH        = $(shell uname)

# Source in configuration

include ../Makefile.$(ARCH)

# Makefile Specific definitions

INCLUDES   = $(INCLUDE)
CFLAGS     = $(CDEBUGFLAGS) $(CCOPTIONS) $(INCLUDES) $(LIBS) $(PROTO) $(DEFINES)
LIBHM      = -lc
OBJS       = tclSystem.o

# Start of Makefile

all  : $(OBJS)

link: 
	$(LD) -o libTclSystem-$(LIB_VER).so $(OBJS) $(LIBHM)
	cp libTclSystem-$(LIB_VER).so $(INSTALL_PATH)
	rm -f libTclSystem.so $(INSTALL_PATH)/libTclSystem.so
	ln -s libTclSystem-$(LIB_VER).so libTclSystem.so
	ln -s libTclSystem-$(LIB_VER).so $(INSTALL_PATH)/libTclSystem.so

install: all link

clean:
	-rm -f *.o *~ *.lo *.la *.so
