      subroutine define_vacuum (mother)

c This routine sets up the vacuum pipes connecting the dipoles in the
c Compton chicane.
c Inputs:
c	mother: 4-character name of outer volume
c
c PW 20-Oct-93
c  - original code
c
c Richard Jones June 19, 2003
c  - got rid of "fortran structure" references in favour of standard f77
c  - replaced trig intrinsics based on degrees (sind,cosd,tand..) with
c    local versions because g77 does not support them
c  - Small overlaps are allowed between the magnets and the vacuum pipes
c    connecting to them.  This prevents air gaps between them.
c  - The third dipole needs an exit port through the yoke to let the
c    backscatter gammas pass through to the detector.  This port is allowed
c    overlap the third dipole, and the third dipole is assigned a 'MANY'
c    attribute so that the port ('ONLY') has a higher priority.  This
c    effectively drills a hole through the yoke for the exit port.
c  - A small disk (not shown on drawings but seen by particles) is placed
c    over the end of the beam pipe at the exit from the chicane.  This
c    serves as a beam stop (see gustep.F) and prevents the simulation
c    from wasting time tracking beam particles through the air downstream
c    of the chicane.
c
c Doug Storey Jan 23 2007

      implicit none

      include 'user.inc'
      include 'params.inc'
      include 'ugeom.inc'

      character*(*) mother      ! name of volume to put this in
      integer pipe_mate, pipe_vac, volume_a, volume_b
      integer beamline_mate, port_mate, foil_mate
      real port_length
      parameter (port_length=150.)
      real pars(11)

      if (event_type_brem .ne. 0) then
         call ugstmed (pipe_vac, 'beamline vacuum$', MYVAC_MATE,
     $        0, 0, FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0)
      else
         call ugstmed (pipe_vac, 'beamline vacuum$', VACUUM_MATE,
     $        0, 0, FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0)
      endif

      call ugstmed (pipe_mate, 'beam pipe$', ALUMINUM_MATE,
     $     0, 0, FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0)
      call ugstmed (port_mate, 'exit pipe$', ALUMINUM_MATE,
     $     0, 1, dipole_field, TMAXFD, STEMAX, DEEMAX, EPSIL, 
     $     STMIN, 0, 0)
      call ugstmed (foil_mate, 'exit foil$', int(foil_material + 0.5),
     1     0, 0, FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, 
     2     STMIN, 0, 0)

c vacuum pipe in straight section

      pars(1) =  0.
      pars(2) =  3
      pars(3) = (chicane_mid + 1.4)/2. ! small overlap to prevent air leaks
      call ugsvolu ('PIPE', 'TUBE', pipe_mate, pars, 3, volume_a)

      pars(1)=0
      pars(2)=3.0
      pars(3)=0.6
      call ugsvolu ('UPPL', 'TUBE', pipe_mate, pars, 3, volume_a)   !aperture
      call ugsvolu ('DNPL', 'TUBE', pipe_mate, pars, 3, volume_a)

      pars(1)=0
      pars(2)=2.5
      pars(3) = (chicane_mid + 1.4)/2.   ! small overlap to prevent air leaks
      call ugsvolu ('PIVE', 'TUBE', pipe_vac, pars, 3, volume_a)
      call gspos ('PIPE', 1, mother, 0., 0., 0., 0, 'MANY')
      call gspos ('PIVE', 1, 'PIPE', 0., 0., 0., 0, 'ONLY')

      pars(3) =-62.2
      call gspos ('UPPL', 1, 'PIVE', 0., 0., pars(3), 0, 'ONLY')    ! place aperture in vacuum in pipe
      pars(3) =62.2
      call gspos ('DNPL', 1, 'PIVE', 0., 0., pars(3), 0, 'ONLY')


      pars(1) = 0.5
      pars(2) = 2.0    ! half lengths
      pars(3) = 0.7
      call ugsvolu('UHOL', 'BOX ',pipe_vac, pars, 3, volume_a)       ! openings
      call ugsvolu('DHOL', 'BOX ',pipe_vac, pars, 3, volume_a)

      call gspos ('UHOL', 1, 'UPPL', 0., 0., 0., 0, 'ONLY')          ! place opening in aperture
      call gspos ('DHOL', 1, 'DNPL', 0., 0., 0., 0, 'ONLY')
 

      call gsatt ('PIPE', 'COLO', BLACK) ! Change color of the Pipe
      call gsatt ('PIPE', 'SEEN', 1) ! Pipe is visible

      call gsatt ('UPPL', 'COLO', GREEN)
      call gsatt ('UPPL', 'SEEN', 1)

      call gsatt ('UHOL', 'COLO', BLACK) 
      call gsatt ('UHOL', 'SEEN', 1)

      call gsatt ('DHOL', 'COLO', BLACK) 
      call gsatt ('DHOL', 'SEEN', 1)

      call gsatt ('DNPL', 'COLO', GREEN) 
      call gsatt ('DNPL', 'SEEN', 1)


c vacuum pipe in upstream leg

      pars(1) =  0.
      pars(2) =  3.0
      pars(3) = (chicane_drift + 6.4)/2. !small overlap prevents air leaks
      call ugsvolu ('PIP2', 'TUBE', pipe_mate, pars, 3, volume_b)
      pars(2) =  2.5
      call ugsvolu ('PIV2', 'TUBE', pipe_vac, pars, 3, volume_b)


      call gspos ('PIV2', 1, 'PIP2', 0., 0., 0., 0, 'ONLY')
      pars(2) = chicane_drop/2.
      pars(3) = -chicane_length/2. + chicane_drift/2. + dipole_length
      call gspos ('PIP2', 1, mother, 0., pars(2), pars(3), 5, 'ONLY')
      call gsatt ('PIP2', 'COLO', BLACK) ! Change color of the Pipe
      call gsatt ('PIP2', 'SEEN', 1) ! Pipe is visible

c vacuum pipe in downstream leg (enlarged to contain recoil tracks)
c      pars(1) = (chicane_drift + 8.)/2. ! small overlap
      pars(1) = (chicane_drift + dipole_length/2.)/2. ! small overlap
      pars(2) = 10.3
      pars(3) = 90.
      pars(4) = 3.5
      pars(5) = 3.0
      pars(6) = 3.0
      pars(7) = 0.
      pars(8) = 6.0
      pars(9) = 3.0
      pars(10) = 3.0
      pars(11) = 0.
      call ugsvolu ('PIP3', 'TRAP', pipe_mate, pars, 11, volume_b)
      pars(4) = 3.0
      pars(5) = 2.5
      pars(6) = 2.5
      pars(8) = 5.5
      pars(9) = 2.5
      pars(10) = 2.5
      call ugsvolu ('PIV3', 'TRAP', pipe_vac, pars, 11, volume_b)
c     call gspos ('PIV3', 1, 'PIP3', 0., 0., 0., 0, 'ONLY')
      call gspos ('PIV3', 1, 'PIP3', 0., 0., 0., 0, 'MANY')
      call gsatt ('PIP3', 'COLO', BLACK) ! Change color of the Pipe
      call gsatt ('PIP3', 'SEEN', 1) ! Pipe is visible
      pars(1) = 0.
      pars(2) = chicane_drop/2. + 1.0
      pars(3) = chicane_length/2. - chicane_drift/2. - dipole_length
c     call gspos ('PIP3', 1, mother, pars(1), pars(2), pars(3), 0, 'ONLY')
      call gspos('PIP3',1,mother,pars(1),pars(2),pars(3),0,'MANY')
      
c add exit port for backscatter gammas

      pars(1) = 0.
      pars(2) = 3.5
      pars(3) = port_length / 2.
      call ugsvolu('PORT', 'TUBE', port_mate, pars, 3, volume_a)
      pars(2) = 3.0
      call ugsvolu('PORV', 'TUBE', pipe_vac, pars, 3, volume_a)
      call gspos ('PORV', 1, 'PORT', 0., 0., 0., 0, 'ONLY')
      pars(3) = foil_thickness / 2.
      call ugsvolu ('FOIL', 'TUBE', foil_mate, pars, 3, volume_a)
      pars(3) = port_length / 2. - pars(3)
      call gspos ('FOIL', 1, 'PORV', 0., 0., pars(3), 0, 'ONLY')
      pars(3) = pars(3) + chicane_mid/2 + dipole_length*3/4
      call gspos ('PORT', 1, mother, 0., 0., pars(3), 0, 'ONLY')
      call gsatt ('PORT', 'COLO', BLACK) ! Change color of the port
      call gsatt ('PORT', 'SEEN', 1) ! port is visible
      call gsatt ('FOIL', 'COLO', RED) ! Change color of the foil
      call gsatt ('FOIL', 'SEEN', 1) ! foil is visible

c add virtual beam stop for electron beam (prevents spurious showers)

      pars(1) = 0.
      pars(2) = 10.0
      pars(3) = 0.5
      call ugsvolu('STOP', 'TUBE', pipe_vac, pars, 3, volume_a)
      pars(2) = chicane_drop
      pars(3) = chicane_length / 2. + 5.
      call gspos ('STOP', 1, mother, 0., pars(2), pars(3), 0, 'ONLY')
      call gsatt ('STOP', 'COLO', BLACK) ! Change color of the stop
      call gsatt ('STOP', 'SEEN', 0) ! stop  is invisible
      
      end
