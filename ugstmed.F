      subroutine ugstmed (itmed, natmed, nmat, isvol, ifield, fieldm,
     1     tmaxfd, stemax, deemax, epsil, stmin, ubuf, nwbuf)

c This routine dynamically assigns a media number, itmed, which is returned.
c This gets around the problem of what numbers I have defined already!
c
c PW 20-Oct-93
c  - original code

      implicit none

      integer itmed		! Tracking medium number (returned)
      character*(*) natmed	! Tracking medium name
      integer nmat		! material number
      integer isvol		! is this a sensitive volume
      integer ifield		! is there a magnetic field in this volume
      real fieldm		! max field value in kilogauss
      real tmaxfd		! max angle due to field in 1 step (deg)
      real stemax		! maximum step size (cm)
      real deemax		! max fraction energy loss in 1 step
      real epsil		! tracking precision in cm
      real stmin		! min step due to energy loss or mult scat (cm)
      real ubuf(1)		! user buffer
      integer nwbuf		! number of elements in the user buffer

      integer itmed_value /10/  ! Initial value 

      itmed = itmed_value
      itmed_value = itmed_value + 1

      call gstmed (itmed, natmed, nmat, isvol, ifield, fieldm,
     1     tmaxfd, stemax, deemax, epsil, stmin, ubuf, nwbuf)

      end
