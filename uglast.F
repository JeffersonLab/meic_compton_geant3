	subroutine uglast

c routine to clean up after myself
c
c PW 20-Oct-93
c  - original code

	implicit none

#include "geant321/gclist.inc"

	include "user.inc"

	integer*4 ierr
	integer iostat
	logical single_pass/.FALSE./		! flag to say only once

	if (single_pass) return			! already done this
	single_pass = .TRUE.			! already been done

	write (*,*) 
	write (*,*) 

	write (*,*) ' Total number of triggers is ', total_triggers

	if (total_triggers .ne. 0) then
	  total_luminosity = total_luminosity / total_triggers
	endif
	write (*,*) ' Total Compton luminosity is ',
     1		total_luminosity * 1e-24,' /b/s'

	call hrout (0, IOSTAT, ' ')
	call hrend ('HISTOS')
	close (90)

	write (*,*) 
	write (*,*) 


	call glast	! Tell zebra to clean it's act up

	if ((nget .ne. 0) .or. (nsave .ne. 0)) then
	  call gclose (0, ierr)
	endif

	call igend	! close higz/gks file

	end
