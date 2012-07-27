
      SUBROUTINE GRANOR(A,B)
C.
C.    ******************************************************************
C.    *                                                                *
C.    *       To generate 2 numbers A and B following a NORMAL         *
C.    *       distribution (mean=0 sigma=1.)                           *
C.    *         Copy of the CERN Library routine RANNOR                *
C.    *                                                                *
C.    *    ==>Called by : <USER>, many GEANT routines                  *
C.    *       Author    F.Carminati *********                          *
C.    *                                                                *
C.    ******************************************************************
C.

      Real*8 RNDM1,RNMD2,grnd
*
c      CALL GRNDM(RNDM,2)
c      WRITE(90,*)RNDM(1),RNDM(2)
      RNDM1 = grnd()
      RNDM2 = grnd()
      Y=RNDM1
      Z=RNDM2
      X=6.283185*Z
      A1=SQRT (-2.0*LOG(Y))
      A=A1*SIN (X)
      B=A1*COS (X)
c      WRITE(90,*)A,B
      RETURN
      END
