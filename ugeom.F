	subroutine ugeom

c This routine initializes the geometry for the Compton simulation.
c
c DJG 20-Mar-2003
c  - original code
c
c Richard Jones June 19, 2003
c  - added rotation matrix for laser beam (not used at present)

	implicit none

	include 'user.inc'
	include 'params.inc'

	real thx,phx,thy,phy,thz,phz

C DJG Define rotation matrices

	thx = 90.
	phx = 0.
	thy = 90.+dipole_bend/2.
	phy = 270.
	thz = 180.-dipole_bend/2.
	phz = 90.
	call gsrotm(1,thx,phx,thy,phy,thz,phz)	! dipole 1

	thx = 90.
	phx = 0.
	thy = 90.-dipole_bend/2.
	phy = 90.
	thz = dipole_bend/2.
	phz = 270.0
	call gsrotm(2,thx,phx,thy,phy,thz,phz)	! dipole 2

	thx = 90.
	phx = 0.
	thy = 90.+dipole_bend/2.
	phy = 90.
	thz = dipole_bend/2.
	phz = 90.0
	call gsrotm(3,thx,phx,thy,phy,thz,phz) ! dipole 3

	thx = 90.
	phx = 0.
	thy = 90.-dipole_bend/2.
	phy = 270.
	thz = 180.-dipole_bend/2.
	phz = 270.
	call gsrotm(4,thx,phx,thy,phy,thz,phz) ! dipole 4

	thx = 90.
	phx = 0.
	thy = 90.-dipole_bend
	phy = 90.
	thz = dipole_bend
	phz = 270.0
	call gsrotm(5,thx,phx,thy,phy,thz,phz)	! pipe d1-d2

	thx = 90.
	phx = 0.
	thy = 90.+dipole_bend
	phy = 90.
	thz = dipole_bend
	phz = 90.0
	call gsrotm(6,thx,phx,thy,phy,thz,phz)	! pipe d3-d4

	thx = 90.+laser_theta*180/Pi
	phx = laser_phi*180/Pi
	thy = 90.
	phy = 90.+laser_phi*180/Pi
	thz = laser_theta*180/Pi
	phz = laser_phi*180/Pi
	call gsrotm(7,thx,phx,thy,phy,thz,phz)	! laser beam

	thx = 90.
	phx = 0.
	thy = 90.+dipole_bend
	phy = 90.
	thz = dipole_bend
	phz = 90.0
	call gsrotm(8,thx,phx,thy,phy,thz,phz)  ! Electron detector alignment with scattered electron

	call define_CAVE('CAVE')		! define mother volume
	call define_vacuum('CAVE')		! def vacuum can
	call define_lattice('CAVE')		! define magnetic lattice
	call define_detector('CAVE')		! def det, foil, and coll.

	call ggclos	! Tell GEANT I'm done, and it can straighten things up

	end
