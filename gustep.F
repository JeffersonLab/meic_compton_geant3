	subroutine gustep

c This routine is the user routine called for each step of a particles track.
c
c PW 20-Oct-93
c  - original code
c
c Richard Jones June 19, 2003
c  - simplified to use standard Geant debug features
c  - stores all secondaries
c  - all hits store only energy loss, save only accumulated sums

	implicit none

#include "geant321/gcflag.inc"
#include "geant321/gclist.inc"
#include "geant321/gcvolu.inc"
#include "geant321/gckine.inc"
#include "geant321/gctrak.inc"
#include "geant321/gcking.inc"
#include "geant321/gcsets.inc"

	character*4 cnames(1)
	equivalence (names(1),cnames(1))

	include 'params.inc'	! parameter definitions
	include 'user.inc'	! user common blocks

	integer ihit		! current hit number

	if (ngkine .gt. 0) then
	  call gsking(0)				! store secondaries
	endif

	call gsxyz

	if ((iset .ne. 0) .and. (idet .ne. 0)) then	! sensitive volume
	  if (destep .ne. 0) then			! some energy deposited
c	     print*,'indet'
	    call gschit (iset, idet, itra, numbv, destep, 1, ihit)
	  endif
	endif

	if (cnames(nlevel).eq.'STOP') then
	  istop = 1
	endif

	if (idebug.ne.0) then
	  call gdebug
	endif

	end
