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
      integer flange_mate
      real port_length,ltube3,ecylrad
      parameter (port_length=350.)
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
      call ugstmed (flange_mate, 'beampipe flange$', IRON_MATE,
     1     0, 0, FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, 
     2     STMIN, 0, 0)

c vacuum pipe in straight section

      pars(1) =  0.0
      pars(2) =  3.0
      if(dipole_map.gt.0.1) then
         pars(3) = (chicane_mid - (156.0-dipole_length) + 0.01)/2.0
      else
         pars(3) = (chicane_mid + 0.01)/2. ! small overlap to prevent air leaks
      endif
      call ugsvolu ('PIPE', 'TUBE', pipe_mate, pars, 3, volume_a)

      pars(1)=0
      pars(2)=3.0
      pars(3)=0.635
      call ugsvolu ('UPPL', 'TUBE', pipe_mate, pars, 3, volume_a)   !aperture
      call ugsvolu ('DNPL', 'TUBE', pipe_mate, pars, 3, volume_a)

      pars(1)=0.0
      pars(2)=2.5
      if(dipole_map.gt.0.1) then
         pars(3) = (chicane_mid - (156.0-dipole_length) + 0.01)/2.0
      else
         pars(3) = (chicane_mid + 0.01)/2. ! small overlap to prevent air leaks
      endif
      call ugsvolu ('PIVE', 'TUBE', pipe_vac, pars, 3, volume_a)
      call gspos ('PIPE', 1, mother, 0., 0., 0., 0, 'MANY')
c      call gspos ('PIVE', 1, 'PIPE', 0., 0., 0., 0, 'ONLY')
      call gspos ('PIVE', 1, 'PIPE', 0., 0., 0., 0, 'MANY')


      pars(3) =-62.2
      call gspos ('UPPL', 1, 'PIVE', 0., 0., pars(3), 0, 'ONLY')    ! place aperture in vacuum in pipe
      pars(3) =62.2
      call gspos ('DNPL', 1, 'PIVE', 0., 0., pars(3), 0, 'ONLY')


      pars(1) = 0.5
      pars(2) = 2.0    ! half lengths
      pars(3) = 0.635
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
      pars(2) =  3. ! 3 inch pipe
      if(dipole_map.gt.0.1) then
         pars(3) = (chicane_drift -(156.0-dipole_length) + 6.4)/2.
      else
         pars(3) = (chicane_drift + 6.4)/2. !small overlap prevents air leaks
      endif
      call ugsvolu ('PIP2', 'TUBE', pipe_mate, pars, 3, volume_b)
      pars(2) =  3.5
      call ugsvolu ('PIV2', 'TUBE', pipe_vac, pars, 3, volume_b)


      call gspos ('PIV2', 1, 'PIP2', 0., 0., 0., 0, 'ONLY')
      pars(2) = chicane_drop/2.
      pars(3) = -chicane_length/2. + chicane_drift/2. + dipole_length
c      call gspos ('PIP2', 1, mother, 0., pars(2), pars(3), 5, 'ONLY')
C 'ONLY' above collides with dipole 'ONLY'
      call gspos ('PIP2', 1, mother, 0., pars(2), pars(3), 5, 'MANY')
      call gsatt ('PIP2', 'COLO', BLACK) ! Change color of the Pipe
      call gsatt ('PIP2', 'SEEN', 1) ! Pipe is visible

c detector cylinder

      pars(1) =  0.
c      pars(2) = (0.096*chicane_drift) ! Radius of detector cylinder
      if(dipole_map.gt.0.1) then
         ecylrad =(detector_posoffset-(156.0-dipole_length)/2.0+0.01)        ! Radius of edet can
      else
         ecylrad = detector_posoffset+0.01 ! Radius of edet can
      endif
      pars(2)=ecylrad
      pars(3) = 10.0 ! Detector height      
      call ugsvolu ('DetVac', 'TUBE', pipe_vac, pars, 3, volume_b)
      pars(2) = 3*chicane_drop/4.+2.0
      pars(3) = chicane_length/2. - dipole_length - detector_posoffset
      call gspos ('DetVac', 1, mother, 0., pars(2), pars(3), 9, 'MANY')
      call gsatt ('DetVac', 'COLO', BLACK) ! Change color of the Pipe
      call gsatt ('DetVac', 'SEEN', 1) ! Pipe is visible

c vacuum pipe in downstream leg - just make the pipe bigger

      pars(1) =  0.
      pars(2) =  6.35 ! 5 inch pipe!!
      if(dipole_map.gt.0.1) then
         ltube3=chicane_drift-2.0*ecylrad-(156.0-dipole_length)
      else
         ltube3 = chicane_drift-2.0*ecylrad
      endif
      pars(3)=(ltube3+6.4)/2.0
      call ugsvolu ('PIP3', 'TUBE', pipe_mate, pars, 3, volume_b)
      pars(2) =  6.85 
      call ugsvolu ('PIV3', 'TUBE', pipe_vac, pars, 3, volume_b)
      call gspos ('PIV3', 1, 'PIP3', 0., 0., 0., 0, 'MANY')
      if(dipole_map.gt.0.1) then
         pars(3) = chicane_mid/2.0+dipole_length+ltube3/2.0 + 
     >       (156.0-dipole_length)/2.0
      else
         pars(3) = chicane_mid/2.0+dipole_length+ltube3/2.0
      endif
      if(dipole_map.gt.0.1) then
         pars(2) = chicane_drop/2.0 * 
     >        (ltube3/chicane_drift) + 3.5*1.75
      else
         pars(2) = chicane_drop/2.0 * 
     >        (ltube3/chicane_drift) + 5.5
      endif
      call gspos ('PIP3', 1, mother, 0., pars(2), pars(3), 6, 'MANY')
      call gsatt ('PIP3', 'COLO', BLACK) ! Change color of the Pipe
      call gsatt ('PIP3', 'SEEN', 1) ! Pipe is visible

c block stray photons from photon detector

c      pars(1) =  0.
c      pars(2) = (0.3*chicane_drift) ! Radius of detector sheild
c      pars(3) = 1. ! Detector hight      
c      call ugsvolu ('Sheild', 'TUBE', pipe_mate, pars, 3, volume_b)
c      pars(2) = chicane_drop/4.-1.5
c      pars(3) = chicane_length/2. - 17*chicane_drift/125 - dipole_length
c      call gspos ('Sheild', 1, mother, 0., pars(2), pars(3), 9, 'MANY')
c      call gsatt ('Sheild', 'COLO', BLUE) ! Change color of the Pipe
c      call gsatt ('Sheild', 'SEEN', 1) ! Pipe is visible
   
      


C DG - this is not what we did in reality
cc vacuum pipe in downstream leg (enlarged to contain recoil tracks)
cc      pars(1) = (chicane_drift + 8.)/2. ! small overlap
c      pars(1) = (chicane_drift + dipole_length/2.)/2. ! small overlap
c      pars(2) = 10.3
c      pars(3) = 90.
c      pars(4) = 3.5
c      pars(5) = 3.0
c      pars(6) = 3.0
c      pars(7) = 0.
c      pars(8) = 6.0
c      pars(9) = 3.0
c      pars(10) = 3.0
c      pars(11) = 0.
c      call ugsvolu ('PIP3', 'TRAP', pipe_mate, pars, 11, volume_b)
c      pars(4) = 3.0
c      pars(5) = 2.5
c      pars(6) = 2.5
c      pars(8) = 5.5
c      pars(9) = 2.5
c      pars(10) = 2.5
c      call ugsvolu ('PIV3', 'TRAP', pipe_vac, pars, 11, volume_b)
cc     call gspos ('PIV3', 1, 'PIP3', 0., 0., 0., 0, 'ONLY')
c      call gspos ('PIV3', 1, 'PIP3', 0., 0., 0., 0, 'MANY')
c      call gsatt ('PIP3', 'COLO', BLACK) ! Change color of the Pipe
c      call gsatt ('PIP3', 'SEEN', 1) ! Pipe is visible
c      pars(1) = 0.
c      pars(2) = chicane_drop/2. + 1.0
c      pars(3) = chicane_length/2. - chicane_drift/2. - dipole_length
cc     call gspos ('PIP3', 1, mother, pars(1), pars(2), pars(3), 0, 'ONLY')
c      call gspos('PIP3',1,mother,pars(1),pars(2),pars(3),0,'MANY')
      
c add exit port for backscatter gammas

      pars(1) = 0.
      pars(2) = 2.5
      pars(3) = port_length / 2.
      call ugsvolu('PORT', 'TUBE', port_mate, pars, 3, volume_a)
      pars(2) = 2.0
      call ugsvolu('PORV', 'TUBE', pipe_vac, pars, 3, volume_a)
      call gspos ('PORV', 1, 'PORT', 0., 0., 0., 0, 'ONLY')
      pars(3) = foil_thickness / 2.
      call ugsvolu ('FOIL', 'TUBE', foil_mate, pars, 3, volume_a)
      pars(3) = port_length / 2. - pars(3)
      call gspos ('FOIL', 1, 'PORV', 0., 0., pars(3), 0, 'ONLY')
      if(dipole_map.gt.0.1) then
         pars(3) = pars(3) + chicane_mid/2 + 156.0*0.9
      else
         pars(3) = pars(3) + chicane_mid/2 + dipole_length - 1.0
      endif
      call gspos ('PORT', 1, mother, 0., 0., pars(3), 0, 'MANY')
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
