	subroutine uginit

c User initialization routine for GEANT
c
c Pat Welch 15-Nov-93
c  - original code

	implicit none

#include "geant321/gclist.inc"

	integer*4 ident, ierr

	call ginit	! Initialize GEANT variables

c This statement (gffgo) reads in the data stream from stdin.

	call ugffgo	! read in the data cards through my routine first
	call gzinit	! Initialize data structures
	call gdinit	! initialize graphics

	if (nget .gt. 0) then		! If a file to read in from
	  call grfile (1, 'mygeom.dat', 'I')
	else
	  call gpart	! generate particle database
	  call gmate	! generate materials database
	  call ugmate	! extend the material database with my junk
	  call ugeom	! define my geometry
	endif

	call gphysi	! Compute cross-section and energy loss tables

	if (nsave .gt. 0) then		! save to permanent db
	  call grfile (2, 'mysave.dat', 'NO')
	endif

	call gbhsta	! initialize standard histos
	call histo_init	! initialize my histograms

	end
