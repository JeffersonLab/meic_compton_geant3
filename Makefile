#RHEL5
CERN_ROOT = /apps/cernlib/i386_rhel5/2005/
CERN_LEVEL = 2005
#64 bit CentOs
#CERN_ROOT = /usr/lib64/cernlib/2006
#CERN_LEVEL = 2006


INTER = interactive

#OBJINTER = $(CERNDIR)gxint$(GEANTVER).o
#OBJINTER = gxint$(GEANTVER).o gxcs.o
OBJINTER = gxcs.o

OBJ = uglast.o uginit.o ugmate.o ugeom.o ugstmed.o ugsvolu.o ugffgo.o \
	gukine.o ugsvert.o ugskine.o gustep.o histo_init.o \
	do_compton.o compton.o gufld.o gudigi.o genbeam.o genhalo.o \
	define_cave.o define_vacuum.o define_lattice.o define_detector.o \
	trig.o granor.o mt19937.o gauss1.o

SOURCES = compton.F control.in granor.F define_cave.F define_detector.F \
	define_lattice.F define_vacuum.F do_compton.F fort.4 \
	genbeam.F genhalo.F gudigi.F gufld.F gukine.F gustep.F \
	gxcs.F histo_init.F main.F mt19937.F gauss1.F\
	Makefile materials.database README \
	params.inc testcompton.f trig.F trig.inc ugeom.F \
	ugeom.inc ugffgo.F uginit.F uglast.F ugmate.F \
	ugskine.F ugstmed.F ugsvert.F ugsvolu.F user.inc

CERNDIR = $(CERN_ROOT)/lib
GEANTVER = 321
CERNLIBS = -L$(CERNDIR) \
	-lgeant$(GEANTVER) \
	-lpawlib \
	-lgraflib \
	-lgrafX11 \
	-lmathlib \
	-lpacklib 


CARDIR = $(CERN_ROOT)/src/car/
CAR = includes.car \
	$(CARDIR)geant$(GEANTVER).car \
	$(CARDIR)gcorr$(GEANTVER).car
PATCHYFILE = ./patchy.input

MYOS := $(subst -,,$(shell uname))


ifeq ($(MYOS),HPUX)
  LIBS    = -L/usr/lib/X11R5 -lX11 -lm -ldld
  FC = fort77
  FFLAGS  = +ppu +U77 -C -w -O +e +es -K # -Wl,-ylnblnk
endif

ifeq ($(MYOS),OSF1)
  LIBS    = -lX11		# OSF1
  FFLAGS  = -O -extend_source	# OSF1
endif

ifeq ($(MYOS),solaris)
  FC = f77
  LIBS    = -lX11 -lm -ldl -lnsl -lsocket	# solaris
#  FFLAGS  = -O -extend_source	# solaris
  FFLAGS  = -O
endif

ifeq ($(MYOS),Linux)
 ifeq ($(COMPILER),Absoft)
  FABSFLAGS=-O -V -W -f -s -N1 -B108 -B100 -N90 -N22
  EXTRAFLAGS=-DABSOFTFORTRAN
  FFLAGS= $(INCLUDES) $(FABSFLAGS) $(EXTRAFLAGS)
  LIBS = -L$(CERN_ROOT)/lib -lV77 -lU77 -lg2c -lc -lm -L/usr/X11R6/lib -lX11 -lcrypt -lXt -lXext -lXp -ldl -lnsl
  FC  = $(ABSOFT)/bin/f77
  F77 =$(ABSOFT)/bin/f77
 else
  FC = gfortran
  LIBS    = -L/usr/lib -lXpm -lXm -lXt -lSM -lICE -lXext -lX11 -lXp -ldl
  FFLAGS = -g -I. -I$(CERN_ROOT)/include -DCERNLIB_TYPE -DCERNLIB_MOTIF -D_FILE_OFFSET_BITS=64
  LDXOPTS = -Wl,-uuginit_,-uuglast_,-ugustep_,-ugukine_,-ugudigi,-ugxcs_,-export-dynamic
 endif
endif

all: compton++ compton

compton++: $(OBJINTER) $(OBJ) Makefile
	$(FC) -o $@ gxint321.f $(OBJINTER) $(OBJ) -L$(CERN_ROOT)/lib `cernlib -v $(CERN_LEVEL) geant321 pawlib graflib/Motif packlib mathlib`


compton: main.F $(OBJ) Makefile
	$(FC) $(FFLAGS) -o $@ main.F $(OBJ) $(CERNLIBS) $(LIBS)

tar: $(SOURCES)
	tar zcf compton_geant.tgz $(SOURCES)

clean:
	$(RM) $(OBJINTER) $(BATCH) $(OBJBATCH) $(OBJ)
	$(RM) paw.metafile last.kumac last.kumacold

ugeom.o: params.inc
gukine.o: params.inc
*.o: user.inc

ifeq (tpw,tkw)
gcking.inc gcsets.inc gcbank.inc gclist.inc gcvolu.inc gckine.inc gctrak.inc: $(CAR) $(PATCHYFILE)
	$(PATCHYFILE) $@ | ypatchy - $@ :GO

$(PATCHYFILE): Makefile
	$(RM) -f $(PATCHYFILE)
	echo '#! /bin/csh'					>$(PATCHYFILE)
	echo 'set name = i`echo $$1 | awk -F. '"'"'{print $$1}'"'"'`' \
								>>$(PATCHYFILE)
	echo 'echo +OPT,MAPASM.' 				>>$(PATCHYFILE)
	echo 'echo +USE,$${name},DECS,GCDES.'			>>$(PATCHYFILE)
	echo 'echo +USE,GCORR,T=EXE.'				>>$(PATCHYFILE)
	echo 'echo +EXE.'					>>$(PATCHYFILE)
	echo '"echo +PAM,11,R=*GEANT,T=C,A.$(CARDIR)geant$(GEANTVER).car"' \
								>>$(PATCHYFILE)
	echo 'echo +PAM,11,         T=C,A.$(CARDIR)gcorr$(GEANTVER).car' \
								>>$(PATCHYFILE)
	echo 'echo +PAM,11,         T=C,A.$(CARDIR)geant$(GEANTVER).car' \
								>>$(PATCHYFILE)
	echo 'echo +PAM,11,         T=C,A.includes.car'		>>$(PATCHYFILE)
	echo 'echo +QUIT.'					>>$(PATCHYFILE)
	chmod +x $(PATCHYFILE)
endif
