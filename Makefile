GNUMAKE=@`sh -c 'if make --version | grep "^GNU Make"; then echo gmake; else echo make; fi' 2>/dev/null`
TARGETMAKEFILE=	./Makefile.tinymt32

all:
	$(GNUMAKE) -f $(TARGETMAKEFILE) $*

.DEFAULT:
	$(GNUMAKE) -f $(TARGETMAKEFILE) $*
