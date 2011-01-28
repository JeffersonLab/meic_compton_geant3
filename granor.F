
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

      DIMENSION RNDM(2)
*
      CALL GRNDM(RNDM,2)
c      WRITE(90,*)RNDM(1),RNDM(2)
      Y=RNDM(1)
      Z=RNDM(2)
      X=6.283185*Z
      A1=SQRT (-2.0*LOG(Y))
      A=A1*SIN (X)
      B=A1*COS (X)
c      WRITE(90,*)A,B
      RETURN
      END
