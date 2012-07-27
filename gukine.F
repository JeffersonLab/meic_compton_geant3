	subroutine gukine

c This routine is called to initialize the kinematics for each event
c This particular event generator is designed for energy analysis using
c laser backscattering.
c
c Pat Welch 15-Nov-93
c  - original code
c
c Richard Jones June 19, 2003
c  - got rid of "fortran structure" references in favour of standard f77
c  - replaced trig intrinsics based on degrees (sind,cosd,tand..) with
c    local versions because g77 does not support them
c  - all random number generation standardized to internal Geant facilities.
c  - moved beam vertex generator out into separate file genbeam.F
c  - introduced beam halo generator genhalo.F
c  - generalized from do_compton to all tracks that the user() words 
c    pass the kind of process being simulated, cross section and luminosity
c    weight factors for the track, and the polarization asymmetry of the
c    generating process.

	implicit none

#include "geant321/gclist.inc"

	include 'user.inc'
	include 'params.inc'

	real vertex(3), p(3)
	integer vertex_num, track_num, type
	integer i
	real*8 random, grnd				! random number
	real user(4)

	real pressure				! pressure in vacuum line (torr)
	data pressure/1e-9/

	integer iucomp

c Generate background processes:
c	-beam-gas interactions (mainly bremsstrahlung)
c	-beam halo interactions on beam pipe
c	-synchrotron radiation in dipoles
c The to make the bremsstrahlung generation efficient, the pressure in
c the beam pipe is inflated to STP if event_type_brem is turned on.
c This makes little difference to the Compton backscatter simulation
c or beam halo background, but completely swamps synchrotron radiation.
c To study synchrotron radiation you have to turn off bremsstrahlung.

	if (event_type_brem .ne. 0) then
	  user(1) = 1			! indicates beamline bremsstrahlung
	  user(2) = 1			! cross section scale factor
	  user(3) = beam_flux * pressure / 760.	! luminosity scale factor
	  user(4) = 0			! zero asymmetry
	  call genbeam (vertex, p)
	  call ugsvert (vertex, 0, 0, 0, 0, vertex_num)
	  call ugskine (p, electron_id, vertex_num, user, 4, track_num)
	elseif (event_type_sync .ne. 0) then
	  user(1) = 4			! indicates synchrotron radiation
	  user(2) = 1			! cross section scale factor
	  user(3) = beam_flux		! luminosity scale factor
	  user(4) = 0			! zero asymmetry
	  call genbeam (vertex, p)
	  call ugsvert (vertex, 0, 0, 0, 0, vertex_num)
	  call ugskine (p, electron_id, vertex_num, user, 4, track_num)
	endif
	if (event_type_halo .ne. 0) then
	  user(1) = 3			! indicates halo simulation
	  user(2) = 1			! cross section scale factor
	  user(3) = halo_fraction * beam_flux	! luminosity scale factor
	  user(4) = 0			! zero asymmetry
	  call genhalo (vertex, p)
	  call ugsvert (vertex, 0, 0, 0, 0, vertex_num)
	  call ugskine (p, electron_id, vertex_num, user, 4, track_num)
	endif

c Now take a look at producing backscattered photons

	if (event_type_back .ne. 0) then
          user(1) = 2
          type = user(1)
	  call genbeam (vertex, p)
	  call do_compton (vertex, p, type)
	endif

c        if (event_type_halo .ne. 0) then
c           user(1) = 3
c           type = user(1)
c           call genhalo(vertex, p)
c           call do_compton (vertex, p, type)
c        endif 

c Now take care of source lines

	if (event_type_source .ne. 0) then
	  if (sources_quantity .gt. 0) then
	    vertex(1) = 0
	    vertex(2) = 0
	    vertex(3) = 0
	    call ugsvert (vertex, 0, 0, 0, 0, vertex_num)! where the vertex is
	    random = grnd()
	    i = int(1. + random * sources_quantity)
	    p(1) = 0
	    p(2) = 0
	    p(3) = sources_lines(i) / 1000.
	    user(1) = 5
	    user(2) = 1		! not sure how to normalize this
	    user(3) = 1
	    user(4) = 0
	    call ugskine (p, gamma_id, vertex_num, user, 4, track_num)
	  endif
	endif

c Take care of printing stuff out

	if (iucomp ('VERT', lprin, nprin) .ne. 0) call gpvert(0) ! Print vertex
	if (iucomp ('KINE', lprin, nprin) .ne. 0) call gpkine(0) ! Print track

	total_triggers = total_triggers + 1

	end
