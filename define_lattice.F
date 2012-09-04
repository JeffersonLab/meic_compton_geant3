      subroutine define_lattice (mother)

c This routine defines and places the dipoles for the Compton chicane.
c Inputs:
c	mother: 4-character name of outer volume in which magnets are placed
c
c PW 20-Oct-93
c  - original code
c
c DJG 16-Mar-2003
c  - commented out the quad in the middle of the straight section
c  - added dipoles for the chicane
c
c Richard Jones June 19, 2003
c  - got rid of "fortran structure" references in favour of standard f77
c  - replaced trig intrinsics based on degrees (sind,cosd,tand..) with
c    local versions because g77 does not support them
c  - Most of the geometry has now been rewritten.  The magnets are now
c    boxes (instead of tubes) composed of iron and having rectangular
c    slots through them for the beam to pass through.
c  - A problem appeared in the tracking of low-energy electrons in the
c    dipole gap in that they would spiral forever and waste time until
c    Geant would give up with a warning, "10000 steps, track abandoned".
c    To correct this, the media in strong magnetic fields are told to
c    stop tracking electrons below 1 MeV in kinetic energy.

      implicit none

      include 'user.inc'
      include 'params.inc'
      include 'ugeom.inc'
      
      character*(*) mother      ! volume to imbed this one in
      integer volume_num, magnet_mate, beamline_mate
      real pars(9)

c Now define the chicane

C Note: field in ugstmed is the maximum field in the dipole. For the fieldmap,
C best to set this to twice the "nominal" value to avoid problems.

      if (dipole_length .ne. 0.) then
         if (event_type_brem .ne. 0) then
            call ugstmed (beamline_mate, 'dipole vacuum$', MYVAC_MATE,
     $           0, 1, 2.0*dipole_field, TMAXFD, STEMAX, DEEMAX, EPSIL, 
     $           STMIN, 0, 0)
         else
            call ugstmed (beamline_mate, 'dipole vacuum$', VACUUM_MATE,
     $           0, 1, 2.0*dipole_field, TMAXFD, STEMAX, DEEMAX, EPSIL, 
     $           STMIN, 0, 0)
         endif
         call ugstmed (magnet_mate, 'dipole yoke$', IRON_MATE,
     $        0, 1, 2.0*dipole_field, TMAXFD, STEMAX, DEEMAX, EPSIL, 
     $        STMIN, 0, 0)

c Stop electrons below 1 MeV, otherwise they curl up in the field
c and circle forever in the vacuum, using up cpu time and doing nothing

         call gstpar (beamline_mate, 'CUTELE', 1e-3)
         call gstpar (magnet_mate, 'CUTELE', 1e-3)

c Now define the dipole magnets

c-- Old H-shape dipole
c         pars(1) = 20.
c         pars(2) = 40.
c         pars(3) = dipole_length / 2.
c         call ugsvolu('DIPO','BOX  ',magnet_mate,pars,3,volume_num)
c         pars(1) = 1.
c         pars(2) = 8.
c         call ugsvolu('DIPI','BOX  ',beamline_mate,pars,3,volume_num)
c         pars(2) = 2.5
c         call gspos ('DIPI',1,'DIPO',0.,pars(2),0.,0,'ONLY')
c         call gsatt ('DIPO', 'COLO', BLUE) ! Change color of the dipole
c         call gsatt ('DIPO', 'SEEN', 1)	! dipole is visible

         ! from drawing file EXP-245-7982-007
         pars(1) = 22.           !  a guess
         pars(2) = dipole_height/2.0

         pars(3) = dipole_length / 2.
         if(dipole_map.gt.0.1) then
            pars(3) = 156.0/2.0
         endif
         call ugsvolu('DIPO','BOX  ',magnet_mate,pars,3,volume_num)
         pars(1) = 3./2.      
         call ugsvolu('DIPI','BOX  ',beamline_mate,pars,3,volume_num)
         call gspos ('DIPI',1,'DIPO',0.,0.,0.,0,'ONLY')
         call gsatt ('DIPO', 'COLO', BLUE) ! Change color of the dipole
         call gsatt ('DIPO', 'SEEN', 1)	! dipole is visible


C DJG: First dipole
         pars(2) = chicane_drop - dipole_h/2.0 ! offset to account for sagitta
         pars(3) = - chicane_mid/2. - dipole_length 
     $             - chicane_drift - dipole_length/2.
c         call gspos ('DIPO', 1, mother, 0., pars(2), pars(3), 1, 'ONLY')
         call gspos ('DIPO', 1, mother, 0., pars(2), pars(3), 0, 'ONLY')

C DJG: Second dipole
         pars(2) = 0.0+dipole_h/2.0
         pars(3) = -chicane_mid/2. - dipole_length/2.
c         call gspos ('DIPO', 2, mother, 0., pars(2), pars(3), 2, 'ONLY')
         call gspos ('DIPO', 2, mother, 0., pars(2), pars(3), 0, 'ONLY')

C DJG: Third dipole
         pars(2) = 0.0+dipole_h/2.0
         pars(3) = chicane_mid/2. + dipole_length/2.
c         call gspos ('DIPO', 3, mother, 0., pars(2), pars(3), 3, 'MANY')
         call gspos ('DIPO', 3, mother, 0., pars(2), pars(3), 0, 'ONLY')


C DJG: Fourth dipole
         pars(2) = chicane_drop-dipole_h/2.0
         pars(3) = chicane_mid/2. + dipole_length 
     $             + chicane_drift + dipole_length/2.
c         call gspos ('DIPO', 4, mother, 0., pars(2), pars(3), 4, 'ONLY')
         call gspos ('DIPO', 4, mother, 0., pars(2), pars(3), 0, 'ONLY')
      endif

      end
