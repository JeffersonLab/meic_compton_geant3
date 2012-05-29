      subroutine ugffgo

c User routine to define new cards to ffread then read the input data stream
c
c PW 20-Oct-93
c  - original code
c
c Richard Jones June 19, 2003
c  - got rid of "fortran structure" references in favour of standard f77
c  - replaced trig intrinsics based on degrees (sind,cosd,tand..) with
c    local versions because g77 does not support them
c  - changed definition of beam_flux to standard usage (in #/s) in
c    agreement with laser_flux.  The previous definition had included the
c    factor (2/c) from the expression for the luminosity into beam_flux.
c  - changed DETEctor card to refer to recoil electron detector, before
c    it was redundant with the HPGErmanium card
c  - added the HALO card to specify the characteristics of the beam halo
c    that scrapes on the beam pipe and produces background

      implicit none

#include "geant321/gcphys.inc"

      include 'params.inc'
      include 'user.inc'
c
c Generic initializations
c
      total_luminosity = 0.
      total_triggers   = 0

c Define the beam input card

      call ffkey('BEAM', beam, 11, 'REAL') ! define beam card

      beam_E         = 4.       ! GeV
      beam_dE        = 0.01     ! fraction
      beam_I         = 10.      ! microamps
      beam_in_betax  = 10.      ! m
      beam_in_betay  = 10.      ! m
      beam_quad_betax= 10.      ! m
      beam_quad_betay= 10.      ! m
      beam_out_betax = 10.      ! m
      beam_out_betay = 10.      ! m
      beam_emittance = 1e-9     ! m-radians
      beam_polarization = 0.8   ! beam polarization

c Define the halo input card

      call ffkey('HALO', halo, 4, 'REAL') ! define halo card

      halo_fraction  = 1e-7     ! fraction
      halo_radius_min = 0.0     ! cm
      halo_radius    = 1.       ! cm
      halo_changle   = 0.6      ! mr

c Define the chicane input card

      call ffkey('CHICANE', chicane, 4, 'REAL')	! define chicane card
      
      chicane_length     = 10.6 ! m
      chicane_mid        = 2.0  ! m
      chicane_drift      = 2.3  ! m
      chicane_drop	 = 57.0 ! cm

c Define the laser beam input card

      call ffkey('LASER', laser, 11, 'REAL') ! define laser card
      laser_lambda    = 0.514   ! micrometer
      laser_delta     = 1e-4    ! %
      laser_power     = 1.      ! watts
      laser_sigma     = 5.0     ! mm
      laser_emittance = 1.      ! m-radians
      laser_offset_x  = 0.      ! cm
      laser_offset_y  = 0.      ! cm
      laser_offset_z  = 0.      ! cm
      laser_theta     = 35.28   ! mr crossing angle
      laser_phi       = 0.      ! mr crossing angle
      laser_pol       = 1.0     ! photon polarization

c Define detector card (for recoil electron)

      call ffkey('DETECTOR', detector, 12, 'REAL')
      detector_width		= 2.1      ! width of active area (cm)
      detector_height		= 2.1      ! height of active area (cm)
      detector_thickness	= 500.     ! thickness of detector (um)
      detector_stripwidth	= 180.     ! strip pitch (um)
      detector_stripspace	= 20.0     ! space between strips (um)
      detector_stripnumber	= 96       ! number of strips per plane
      detector_planespace	= 1.0      ! space between planes (cm)
      detector_planenumber	= 4        ! number of planes
      detector_planeoffset	= 100      ! vertical shift between planes (um)
      detector_offset		= 5.0      ! offset from beam center (cm)
      detector_posoffset	= 10.16    ! position in front of 4th dipole (cm)
      detector_material         = DIAMOND_MATE

c Define backscatter card

      call ffkey('BACKSCATTER', compton, 3, 'REAL')
      compton_width  = 1.
      compton_number = 1.

c Define the dipole input card

      call ffkey('DIPOLE', dipole, 3, 'REAL') ! define dipole card

      dipole_length = 0.     ! cm
      dipole_height = 0.     ! cm
      dipole_current= 100.0  ! A
c      dipole_rho    = 1.    ! m

	
c Define the foil input card

      call ffkey('FOIL', foil, 2, 'REAL') ! define foil card
      foil_thickness = 0.

c Define the HyperPure Germanium input card

      call ffkey('HPGERMANIUM', hpge, 11, 'REAL') ! define gamma detector
      hpge_material = GERMANIUM_MATE ! by default made of Ge
      hpge_type     = 1         ! a single big chunk
      hpge_distance = 3.	! distance from crossing point (m)
      hpge_offset_x = 0.	! displacement in x from axis (cm)
      hpge_offset_y = 0.	! displacement in y from axis (cm)
      hpge_diameter = 10.	! diameter of active crystal (cm)
      hpge_depth    = 30.	! thickness of active crystal (cm)
      hpge_dead_depth = 0.0	! thickness of dead layer (cm)
      hpge_cover_mate = 9	! material ID for the cover
      hpge_cover_depth = 0.01	! thickness of entrance cover (cm)
      hpge_cover_side = 0.5	! thickness of side covers (cm)

c Define the radioactive source card

      call ffkey ('SOURCES', sources, 20, 'REAL') ! define sources card
      sources_quantity = 0.     ! # of entries
        
c Define the generator  input card

      call ffkey('TYPE', event_type, 5, 'INTEGER') ! define event type card
      event_type_brem   = 0
      event_type_back   = 1
      event_type_halo   = 0
      event_type_sync   = 0
      event_type_source = 0

c Define the Compton suppression shield input card

      call ffkey ('SUPPRESSION', suppression, 3, 'REAL')
      suppression_flag = 0       ! by default suppression off
      suppression_type = 1       ! by default use VU detector
      suppression_threshold = 10 ! default to 10 keV

c Go read in the user data set now

c     write (*,*) ' Enter data stream for gffgo or read # for ',
c     1	'a different LUN'

      call ffset('LINP',4)
      call gffgo		! Actually read the user data cards

      isync = event_type_sync

      beam_dE         = beam_E * beam_dE / 100.	! sigma of beam_E
      beam_in_betax   = beam_in_betax * 100.    ! m->cm
      beam_in_betay   = beam_in_betay * 100.    ! m->cm
      beam_quad_betax = beam_quad_betax * 100.  ! m->cm
      beam_quad_betay = beam_quad_betay * 100.  ! m->cm
      beam_out_betax  = beam_out_betax * 100.   ! m->cm
      beam_out_betay  = beam_out_betay * 100.   ! m->cm
      beam_emittance  = beam_emittance * 100.   ! m->cm
      beam_sigmax     = sqrt((beam_in_betax + beam_quad_betax +
     $     beam_out_betax) / 3. * beam_emittance)
      beam_sigmay     = sqrt((beam_in_betay + beam_quad_betay +
     $     beam_out_betay) / 3. * beam_emittance)
      beam_thetax     = sqrt(3. * beam_emittance /
     $     (beam_in_betax + beam_quad_betax + beam_out_betax))
      beam_thetay     = sqrt(3. * beam_emittance /
     $     (beam_in_betay + beam_quad_betay + beam_out_betay))
      beam_flux       = ((beam_I * 1e-6) / 1.6e-19)

      halo_changle    = halo_changle/1e3      ! mr->r
      
      chicane_length  = chicane_length * 100. ! m->cm
      chicane_mid     = chicane_mid * 100.    ! m->cm
      chicane_drift   = chicane_drift * 100.  ! m->cm
      
      laser_energy = 1239.84244 / (laser_lambda * 1000.) ! eV
      laser_dE     = laser_energy * laser_delta / 100.   ! eV
      laser_theta  = Pi - laser_theta / 1000.            ! mr->R
      laser_phi    = laser_phi / 1000.                   ! mr->R
      laser_sigma  = laser_sigma  * 0.1                  ! mm->cm
      laser_emittance = laser_emittance * 100            ! m->cm
      laser_beta   = (laser_sigma**2)/laser_emittance
      laser_flux   = laser_power / (laser_energy * 1.6d-19)
      laser_dtheta = sqrt(laser_emittance / laser_beta)
      laser_Rxx = cos(laser_phi)
      laser_Rxy = 0.
      laser_Rxz = -sin(laser_phi)
      laser_Ryx =  sin(laser_phi) * sin(laser_theta)
      laser_Ryy =                   cos(laser_theta)
      laser_Ryz =  cos(laser_phi) * sin(laser_theta)
      laser_Rzx =  sin(laser_phi) * cos(laser_theta)
      laser_Rzy =                  -sin(laser_theta)
      laser_Rzz =  cos(laser_phi) * cos(laser_theta)
      
      foil_thickness = foil_thickness / 1e4                ! microns->cm
      
      hpge_distance        = hpge_distance * 100.          ! m->cm
      detector_thickness   = detector_thickness / 1e4      ! microns->cm
      detector_stripwidth  = detector_stripwidth / 1e4     ! microns->cm
      detector_stripspace  = detector_stripspace / 1e4     ! microns->cm
      detector_planeoffset = detector_planeoffset / 1e4    ! microns->cm
      detector_offset      = detector_offset/10.0          ! mm-->cm
      
c      dipole_field = 3.335641 * beam_E / dipole_rho   ! Tesla
  
c      dipole_field = (beam_E/0.3)*chicane_drop/100.0
c     $        /((dipole_length+chicane_drift)*dipole_length*1E-4)    ! Tesla
C get field from current (use field map): good to 0.1% from 60 to 110 A
      dipole_field = 52.335*dipole_current + 65.660  !Gauss
C     Note that this field is extracted from a map that gives
C     Leff =124.58 cm. We want same Bdl but assuming 125 cm
C     So B_MC = B_map*Lmap/Lideal = B_map * 124.58/125.0      
      dipole_field = dipole_field * (124.58/125.0) *1.0E-4
      dipole_rho   = beam_E/0.3/dipole_field          ! in m
      dipole_field = dipole_field * 10.               ! T -> kG
      if(dipole_rho.gt.0.0) then
         dipole_bend = asin(dipole_length/100.0/dipole_rho)
         dipole_h    = 100.0*dipole_rho*(1.0-cos(dipole_bend)) ! cm
         dipole_bend = dipole_bend*180./Pi
      endif
      write(98,*) 'no more cheesy poofs',dipole_bend,dipole_field
      
      suppression_threshold = suppression_threshold * 1e-6 ! keV -> GeV
      
      write(98,*) ' Dipole h ', dipole_h, ' cm'
      write(98,*) ' Dipole bending angle ', dipole_bend, ' deg'
      write(98,*) ' Dipole rho ', dipole_rho, ' m'
      write(98,*) ' Beam energy ', beam_E, ' GeV'
      write(98,*) ' Dipole current ', dipole_current, ' A'
      write(98,*) ' Dipole field ', dipole_field, ' kG'
      
c     seed = 1122334455			! some initial seed
      seed = 1104353453
      
      end
