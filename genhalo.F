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
C Dave Gaskell May 2012
C       Modify to generate a "flat" distribution between some minimum and 
c       maximum radius. This change was made to more quickly simulate 
c       the interaction of beam halo on small apertures in the Compton


	implicit none

#include "geant321/gclist.inc"

	include 'user.inc'
	include 'params.inc'

	real vertex(3), p(3)
	real*8 grnd, rand1, rand2, rand3, rand4, rand5			! random numbers
	real rcut,tcut
	parameter (rcut = 5, tcut = 1)
	real theta,phi,momentum
	real G,Gclip,r

c First define the electron vertex at the beginning of the chicane

	rand1 = grnd()
        rand2 = grnd()
        rand3 = grnd()
        rand4 = grnd()
        rand5 = grnd()
C DG: Old, RJ approach
c	Gclip = 1 / (1 + (rcut * beam_sigmax / halo_radius)**2)
c	G = randoms(1) * Gclip
c	r = sqrt(1/G - 1)
c	phi = randoms(2) * 2*Pi
c	vertex(1) = r * rcut * cos(phi) * beam_sigmax
c	vertex(2) = r * rcut * sin(phi) * beam_sigmay + chicane_drop
c	vertex(3) = -chicane_length / 2. + 0.8

C DG: new approach
C change to assume flat distribution between rmin and rmax
	r = halo_radius_min + rand1 *(halo_radius-halo_radius_min)
	phi = rand2 * 2*Pi
	vertex(1) = r * cos(phi)
	vertex(2) = r * sin(phi) 
     >              + chicane_drop
	vertex(3) = -chicane_length / 2. - 1.0
c Now define the beam electron momentum

c	call grndm (randoms, 3)
	G = rand3
	theta = sqrt(1/G - 1) * halo_changle
	phi = rand4 * 2*Pi
	momentum = beam_E + beam_dE * rand5
	p(1) = momentum * sin(theta) * cos(phi)
	p(2) = momentum * sin(theta) * sin(phi)
	p(3) = momentum * cos(theta)

	end
