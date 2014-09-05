	subroutine genhalo (vertex, p, weight)

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
c
C June 2014 - modify to use same form as PEP-II CDR 
C SLAC-R-418, page 113.


	implicit none

#include "geant321/gclist.inc"

	include 'user.inc'
	include 'params.inc'

	real vertex(3), p(3)
	real*8 grnd, rand1, rand2, rand3, rand4, rand5,rand6			! random numbers
	real*8 gauss1
	real*8 nsigma_x,nsigma_y,thetax,thetay,px,py
	real rcut,tcut
	parameter (rcut = 5, tcut = 1)
	real theta,phi,momentum
	real G,Gclip,r
	real weight
	real*8 xmult,ymult

c First define the electron vertex at the beginning of the chicane

c        rand1 = grnd()
c        rand2 = grnd()
c        rand3 = grnd()
c        rand4 = grnd()
c        rand5 = grnd()

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
c	r = halo_radius_min + rand1 *(halo_radius-halo_radius_min)
c	phi = rand2 * 2*Pi
c	vertex(1) = r * cos(phi)
c	vertex(2) = r * sin(phi) 
c     >              + chicane_drop
c	vertex(3) = -chicane_length / 2. - 1.0
C DG: PEP-II approach
C hardwire these for now
C multiplier for 2nd gaussian
	xmult=3.0
	ymult=3.0
C truncate the generation to the edge of a 2.5 cm radius pipe
	nsigma_x = 2.5/(xmult*beam_sigmax) 
	nsigma_y = 2.5/(ymult*beam_sigmay) 
c	call granor(rand1,rand2)
	rand1=gauss1(nsigma_x)
	rand2=gauss1(nsigma_y)

	vertex(1)=rand1*beam_sigmax*xmult
	vertex(2)=rand2*beam_sigmay*ymult+chicane_drop
	vertex(3)=-chicane_length / 2. - 1.0

	weight =  1.0/(xmult*ymult)

** Messing around with weighting...*************
cC Generate flat in x,y - weight by Gaussian
cC DG: PEP-II approach
cC hardwire these for now
cC multiplier for 2nd gaussian
c	xmult=3.0
c	ymult=3.0
cC truncate the generation to the edge of a 2.5 cm radius pipe
c	nsigma_x = 2.5/(xmult*beam_sigmax) 
c	nsigma_y = 2.5/(ymult*beam_sigmay) 
cc	call granor(rand1,rand2)
c	rand1=grnd()
c	rand2=grnd()
c	rand3=grnd()
c	rand4=grnd()
c
cc	vertex(1) = 2.0*(0.5-rand1)*2.5 !+/- 2.5 cm
cc	vertex(2) = 2.0*(0.5-rand2)*2.5
c
cc try to make this more efficient
cC generate from 4*sigma to ~ Infinity
cC the normalization is then reduced by a factor 1-0.999936656
cC note - this is chosen by hand for a 1 cm aperture
cC to make the halo generation yield results faster
c	vertex(1)=rand1*(nsigma_x-4.0)*beam_sigmax+4.0*beam_sigmax
c	if((rand3).lt.0.5) then
c	   vertex(1)=-vertex(1)
c	endif
c
c	vertex(2)=rand1*(nsigma_y-4.0)*beam_sigmay+4.0*beam_sigmay
c	if((rand4).lt.0.5) then
c	   vertex(2)=-vertex(2)
c	endif
c
c
c	vertex(3)=-chicane_length / 2. - 1.0
c
cc	weight =  (2.0*2.5)**2/
cc     >          (2.0*pi*(xmult*beam_sigmax)*(ymult*beam_sigmay))*
cc     >            exp(-vertex(1)**2/2./(xmult*beam_sigmax)**2)*
cc     >            exp(-vertex(2)**2/2./(ymult*beam_sigmay)**2)
c
c	weight =  (2.0*(2.5-4.0*beam_sigmax)*2.0*(2.5-4.0*beam_sigmay))/
c     >          (2.0*pi*(xmult*beam_sigmax)*(ymult*beam_sigmay)*
c     >          (1.0-0.999936656))*
c     >            exp(-vertex(1)**2/2./(xmult*beam_sigmax)**2)*
c     >            exp(-vertex(2)**2/2./(ymult*beam_sigmay)**2)
c
c	vertex(2)=vertex(2)+chicane_drop
**********************************************************************


c Now define the beam electron momentum

c	call grndm (randoms, 3)
c	G = rand3
c	theta = sqrt(1/G - 1) * halo_changle
c	phi = rand4 * 2*Pi
c	momentum = beam_E + beam_dE * rand5
c	p(1) = momentum * sin(theta) * cos(phi)
c	p(2) = momentum * sin(theta) * sin(phi)
c	p(3) = momentum * cos(theta)

	call granor(rand3,rand4)
	thetax = rand3 * beam_thetax
	thetay = rand4 *beam_thetay
	
	call granor(rand5,rand6)
	momentum = beam_E + beam_dE*rand5

	px=momentum * sin(thetax)
	py=momentum * sin(thetay)
	p(1)=px
	p(2)=py
	p(3)= momentum - sqrt(px**2 +py**2)

	

	end
