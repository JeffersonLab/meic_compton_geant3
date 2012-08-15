	subroutine genbeam (vertex, p)

c This routine is called to generate the coordinates and momentum of an
c electron according to the specifications in the BEAM data card.
c Outputs:
c	vertex(3): coordinates of beam particle at chicane entrance (cm)
c	p(3): incident momentum of beam particle at chicane entrance (GeV/c)
c
c Richard Jones June 19, 2003
c  - original code by Pat Welch extracted from gukine.F
c  - got rid of "fortran structure" references in favour of standard f77
c  - replaced trig intrinsics based on degrees (sind,cosd,tand..) with
c    local versions because g77 does not support them
c  - replaced all custom random generators with Geant versions
c


	implicit none

#include "geant321/gclist.inc"

	include 'user.inc'
	include 'params.inc'

	real vertex(3), p(3)
	real random1, random2, grnd		! random numbers
	real*8 thetax, thetay			! beam divergence in radians
	real*8 px, py, momentum			! electron momentum

c First define the electron vertex at the beginning of the chicane

c	random1 = grnd()
c        random2 = grnd()
        call granor (random1, random2)
	vertex(1) = random1 * beam_sigmax
	vertex(2) = chicane_drop + random2 * beam_sigmay
	vertex(3) = -chicane_length / 2. + 0.8


c Now define the beam electron momentum

	call granor (random1, random2)
	thetax = random1 * beam_thetax
	thetay = random2 * beam_thetay
	call granor (random1, random2)
        momentum = sqrt((beam_E + beam_dE * random1)**2
     &                  - mass_electron**2)
	px = momentum * sin(thetax)
	py = momentum * sin(thetay)
	p(1) = px
	p(2) = py
	p(3) = momentum - sqrt(px**2 + py**2)

	end
