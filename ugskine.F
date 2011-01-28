      subroutine ugskine(plab,ipart,vertex_num,ubuf,nubuf,track_num)

c Simple interface to gsvert which checks for error conditions on return
c
c PW 20-Oct-93
c  - original code

      implicit none
      
      real*4 plab(3)		! Momentum components
      integer*4 ipart		! Particle number (see gspart)
      integer*4 vertex_num	! vertex number origin of this track
      integer*4 nubuf		! Number of user floating point numbers
      real*4 ubuf(1)		! User array of floating point numbers
      integer*4 track_num	! track number returned

      call gskine (plab, ipart, vertex_num, ubuf, nubuf, track_num)

      if (track_num .eq. 0) then
         write (*,*) ' ERROR: gskine returned an error!'
         write (*,*) '        plab ', plab
         write (*,*) '        ipart ', ipart, ' vertex_num ', vertex_num
         write (*,*) '        nubuf ', nubuf, ' track_num ', track_num
         if (nubuf .gt. 0) then
	    write (*,*)'        ubuf(i) ', ubuf(1)
         endif
      endif

      end

