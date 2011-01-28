	subroutine genhalo (vertex, p)

c This routine is called to generate the coordinates and momentum of an
c electron according to the specifications in the HALO data card.
c Outputs:
c	vertex(3): coordinates of beam particle at chicane entrance (cm)
c	p(3): incident momentum of beam particle at chicane entrance (GeV/c)
c
c Richard Jones June 20, 2003
c  - original code
c  - The model for the halo is a 1/r**4 distribution for the spatial part
c    and 1/theta**4 for the angular part.  These are expected to be more
c    or less correct on general grounds.  The 1/theta**4 is the distribution
c    for electron scattering (Compton, Mott, bremsstrahlung) and this leads
c    to a spatial distribution that is 1/r**4 for a point target.  For an
c    extended target the spatial part is softer than that for a point target,
c    but absorption in the beam pipe and magnetic analysis will tend to
c    counteract this effect by truncating the tails for distant sources.
c    Until we have measurements of the halo, this model is probably the
c    best we can do.

	implicit none

#include "geant321/gclist.inc"

	include 'user.inc'
	include 'params.inc'

	real vertex(3), p(3)
	real randoms(4)				! random numbers
	real rcut,tcut
	parameter (rcut = 5, tcut = 1)
	real theta,phi,momentum
	real G,Gclip,r

c First define the electron vertex at the beginning of the chicane

	call grndm (randoms, 2)
	Gclip = 1 / (1 + (rcut * beam_sigmax / halo_radius)**2)
	G = randoms(1) * Gclip
	r = sqrt(1/G - 1)
	phi = randoms(2) * 2*Pi
	vertex(1) = r * rcut * cos(phi) * beam_sigmax
	vertex(2) = r * rcut * sin(phi) * beam_sigmay + chicane_drop
	vertex(3) = -chicane_length / 2. + 0.8

c Now define the beam electron momentum

	call grndm (randoms, 3)
	G = randoms(1)
	theta = sqrt(1/G - 1) * halo_changle
	phi = randoms(2) * 2*Pi
	momentum = beam_E + beam_dE * randoms(3)
	p(1) = momentum * sin(theta) * cos(phi)
	p(2) = momentum * sin(theta) * sin(phi)
	p(3) = momentum * cos(theta)

	end
