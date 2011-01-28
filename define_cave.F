      subroutine define_cave (name)

c This routine sets up the geometry of the outer volume (CAVE) containing
c the Compton chicane and detectors.
c Inputs:
c	name: 4-character name of outer volume
c
c PW 20-Oct-93
c  - original code
c
c Richard Jones June 19, 2003
c  - got rid of "fortran structure" references in favour of standard f77
c  - replaced trig intrinsics based on degrees (sind,cosd,tand..) with
c    local versions because g77 does not support them
c  - use hpge_distance for gamma detector, because detector_distance is
c    the recoil electron detector coordinate.

      implicit none

      include 'user.inc'
      include 'params.inc'
      include 'ugeom.inc'

      character*4 name          ! name of the mother volume
      real pars(9)
      integer volume_number
      integer material_number

c Define various materials I will need

      call ugstmed (material_number, name // '$', VACUUM_MATE,
     $     0, 0, FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0)

c I need to define the mother of all volumes, i.e. the cave

      pars(1) = 100
      pars(2) = 100
      pars(3) = chicane_length / 2. + hpge_distance + 
     $     detector_thickness  + 10.
      call ugsvolu(name,'BOX ',material_number,pars,3,volume_number)
      call gsatt(name, 'COLO', YELLOW)	! Change color of the Cave
      call gsatt(name, 'SEEN', 0) ! Cave is not visible

      end
