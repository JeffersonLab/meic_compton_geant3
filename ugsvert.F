      subroutine ugsvert (vertex, ntbeam, nttarg, ubuf, nubuf,
     c     vertex_num)

c Simple interface to gsvert which checks for error conditions on return
c
c PW 20-Oct-93
c  - original code

      implicit none

      real*4 vertex(3)          ! Vertex position
      integer*4 ntbeam          ! beam track number origin of the vertex
      integer*4 nttarg          ! target track number origin of the vertex
      integer*4 nubuf		! Number of user floating point numbers
      real*4 ubuf(1)		! User array of floating point numbers
      integer*4 vertex_num	! Vertex number returned

      call gsvert (vertex, ntbeam, nttarg, ubuf, nubuf, vertex_num) 

      if (vertex_num .eq. 0) then
         write (6,*) ' ERROR: gsvert returned an error!'
         write (6,*) '        vertex ', vertex
         write (6,*) '        ntbeam ', ntbeam, ' nttarg ', nttarg
         write (6,*)
     c        '        nubuf  ', nubuf, ' vertex_num ', vertex_num
         if (nubuf .gt. 0) then
	    write (6,*)'        ubuf(1) ', ubuf(1)
         endif
      endif

      end

