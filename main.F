      implicit none
      real q,h
      integer ispace,hspace
      parameter (ispace=2000000)
      parameter (hspace=500000)
      common /gcbank/ q(ispace)
      common /pawc/ h(hspace)
      real secmax
      parameter (secmax=300000.)
      integer istat,icycle

C---- Initialization of HBOOK, ZEBRA, clock
      call GZEBRA(ispace)
      call HLIMIT(-hspace)
      call TIMEST(secmax)

C---- Initialization of GEANT
      call HPLINT(0)
      call UGINIT

C---- Simulation
      call GRUN

C---- Termination ----
      CALL UGLAST
      END
