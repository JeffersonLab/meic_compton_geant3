c -modified by RTJ
c  Replaced the use of fortran "record" statement with standard fortran-77,
c  and changed column alignment to conform to the fortran source form.
c  Why do people write code that can only be compiled on VMS (or Absoft)?
c -June 7, 2003

*	structure /beam/
*	  real E		! beam energy in GeV                  
*	  real dE		! spread in beam energy in GeV        
*	  real I		! electron current in microamps       
*	  real in_betax		! beta_x in cm                        
*	  real in_betay		! beta_y in cm                        
*	  real quad_betax	! beta_x in cm                        
*	  real quad_betay	! beta_y in cm                        
*	  real out_betax	! beta_x in cm                        
*	  real out_betay	! beta_y in cm                        
*	  real emittance	! emittance in cm-radians             
*	  real polarization	! electron beam polarization          
*				
*	  real sigmax		! average sigmax                      
*	  real sigmay		! average sigmay                      
*	  real thetax		! angular spread in x                 
*	  real thetay		! angular spread in y                 
*	  real flux		! # of electrons / sec / cm^2         
*	end structure		! beam                                

*	structure /halo/
*	  real fraction	! fraction of beam in halo
*	  real radius		! size of halo in cm
*	  real changle 	! spread of halo in radians
*	end structure		! halo

*	structure /chicane/
*	  real length
*	  real mid
*	  real drift
*	  real drop
*	end structure

*	structure /laser/
*	  real lambda		! wavelength in microns
*	  real delta		! fractional width of line
*	  real power		! input power in Watts
*	  real emittance	! emittance of laser cm-radians
*	  real sigma 		! sigma at minimum waist
*	  real offset_x	! offset in x from origin to waist
*	  real offset_y	! offset in y from origin to waist
*	  real offset_z	! offset in z from origin to waist
*	  real theta		! rotation in degrees from z
*	  real phi		! azimuthal rotation in degrees
*

*	  real energy		! energy in eV
*	  real dE		! sigma of energy spread
*	  real flux		! # of photons in beam
*	  real beta		! beta of laser beam
*	  real dtheta		! width in theta of beam spread
*	  real Rxx		! rotation matrix
*	  real Rxy
*	  real Rxz
*	  real Ryx
*	  real Ryy
*	  real Ryz
*	  real Rzx
*	  real Rzy
*	  real Rzz
*	end structure		! laser

*	structure /detector/	! recoil electron detector
*	  real width		! length of active area (cm)
*	  real height		! height of active area (cm)
*	  real thickness	! thickness of detector (um)
*	  real stripwidth	! strip pitch (um)
*	  real offset		! offset from beam center (mm)
*	end structure		! detector

*	structure /hpge/	! hyper pure Germainum detector
*	  real distance		! distance from crossing point (m)
*	  real offset_x		! displacement from axis in x (cm)
*	  real offset_y		! displacement from axis in y (cm)
*	  real diameter		! diameter of active crystal in cm
*	  real depth		! thickness of active crystal in cm
*	  real dead_depth	! thickness of dead layer in cm
*	  real cover_mate	! material ID for the cover
*	  real cover_depth	! thickness of entrance cover in cm
*	  real cover_side	! thickness of side covers in cm
*	  real material		! material the crystal is made of
*	  real type		! type of detector to use
*	end structure

*	structure /compton/
*	  real width		! width in z to generate
*	  real number		! number of photons to generate per electron
*	end structure		! compton

*	structure /dipole/	! dipole information
*	  real length		! length of the dipole segment in cm
*	  real rho		! radius of curvature in m
*
*	  real field		! strength of the dipole field in kG
*	  real bend             ! dipole bending angle
*	end structure		! dipole

*	structure /foil/	! foil information
*	  real thickness	! thickness of foil in cm
*	  real material		! type of material
*	end structure		! foil

*	structure /sources/	! radioactive source information
*	  real quantity		! # of sources
*	  real lines(20)	! radioactive lines
*	end structure

*	structure /event_type/
*	  integer brem		! enable Bremsstrahlung
*	  integer back		! enable Compton backscattering
*	  integer halo		! enable beam halo generation
*	  integer sync		! enable synchrotron generation
*	  integer source	! enable radioactive source generation
*	end structure		! event_type

*	structure /suppression/
*	  real flag		! on/off flag for suppression shield
*	  real type		! detector type (1-> VU)
*	  real threshold	! suppression threshold
*	end structure		! suppression

*	record /beam/ beam
*	structure /beam/
 	  real beam_E		
 	  real beam_dE		
 	  real beam_I
 	  real beam_in_betax		
 	  real beam_in_betay		
 	  real beam_quad_betax	
 	  real beam_quad_betay	
 	  real beam_out_betax	
 	  real beam_out_betay	
 	  real beam_emittance	
 	  real beam_polarization	
 	  real beam_sigmax		
 	  real beam_sigmay		
 	  real beam_thetax		
 	  real beam_thetay		
 	  real beam_flux		
*	end structure		
	common /beam/
     +   beam_E, beam_dE, beam_I, beam_in_betax, beam_in_betay,
     +   beam_quad_betax, beam_quad_betay, beam_out_betax,
     +   beam_out_betay, beam_emittance, beam_polarization,
     +   beam_sigmax, beam_sigmay, beam_thetax, beam_thetay,
     +   beam_flux
	equivalence (beam,beam_E)
        real beam

*	record /halo/ halo
*	structure /halo/
 	  real halo_fraction
          real halo_radius_min
 	  real halo_radius
 	  real halo_changle
*	end structure
	common /halo/
     +	 halo_fraction, halo_radius_min, halo_radius, halo_changle
	equivalence (halo,halo_fraction)
	real halo

*	record /chicane/ chicane
*	structure /chicane/
 	  real chicane_length
 	  real chicane_mid
 	  real chicane_drift
 	  real chicane_drop
*	end structure
	common /chicane/
     +   chicane_length, chicane_mid, chicane_drift, chicane_drop
	equivalence (chicane,chicane_length)
        real chicane

*	record /laser/ laser
*	structure /laser/
 	  real laser_lambda		! wavelength in microns
 	  real laser_delta		! fractional width of line
 	  real laser_power		! input power in Watts
 	  real laser_emittance		! emittance of laser cm-radians
 	  real laser_sigma 		! sigma at minimum waist
 	  real laser_offset_x		! offset in x from origin to waist
 	  real laser_offset_y		! offset in y from origin to waist
 	  real laser_offset_z		! offset in z from origin to waist
 	  real laser_theta		! rotation in degrees from z
 	  real laser_phi		! azimuthal rotation in degrees
          real laser_pol             ! photon polarization
 	  real laser_energy		! energy in eV
 	  real laser_dE			! sigma of energy spread
 	  real laser_flux		! # of photons in beam
 	  real laser_beta		! beta of laser beam
 	  real laser_dtheta		! width in theta of beam spread
 	  real laser_Rxx		! rotation matrix
 	  real laser_Rxy
 	  real laser_Rxz
 	  real laser_Ryx
 	  real laser_Ryy
 	  real laser_Ryz
 	  real laser_Rzx
 	  real laser_Rzy
 	  real laser_Rzz
*	end structure			! laser
	common /laser/
     +   laser_lambda, laser_delta, laser_power, laser_emittance,
     +   laser_sigma, laser_offset_x, laser_offset_y, laser_offset_z,
     +   laser_theta, laser_phi, laser_pol, laser_energy, laser_dE, 
     +   laser_flux, laser_beta, laser_dtheta, laser_Rxx, laser_Rxy, 
     +   laser_Rxz, laser_Ryx, laser_Ryy, laser_Ryz, laser_Rzx, 
     +   laser_Rzy, laser_Rzz
	equivalence (laser,laser_lambda)
        real laser

*	record /detector/ detector
*	structure /detector/
	  real detector_width		! length of active area (cm)
	  real detector_height	        ! height of active area (cm)
	  real detector_thickness	! thickness of detector (um)
	  real detector_stripwidth	! strip pitch (um)
	  real detector_stripspace	! space between strips (um)
	  real detector_stripnumber	! number of strips per plane
	  real detector_planespace	! space between planes (cm)
	  real detector_planenumber	! number of planes
	  real detector_planeoffset	! vertical shift between planes (um)
 	  real detector_offset	        ! offset from beam center (mm)
 	  real detector_posoffset	! position offset from the entrance of the 4th dipole (cm)
          real detector_material        ! material of the electron detector
	  real detector_rotation        ! rotation of detector to align with beam (degrees)
          real detector_yroll           ! rotation of detector about the y axis (degrees)
          real detector_zroll           ! rotation of detector about the z axis (degrees)
*	end structure
	common /detector/
     +   detector_width, detector_height, 
     +   detector_thickness, detector_stripwidth, 
     +   detector_stripspace, detector_stripnumber, 
     +   detector_planespace, detector_planenumber, 
     +   detector_planeoffset,
     +   detector_offset, detector_posoffset,
     +   detector_material, detector_rotation
	equivalence (detector,detector_width)
        real detector

*	record /compton/ compton
*	structure /compton/
 	  real compton_width		! width in z to generate
 	  real compton_number		! number of photons to generate per electron
*	end structure			! compton
	common /compton/
     +   compton_width, compton_number
	equivalence (compton,compton_width)
        real compton

*	record /dipole/ dipole
*	structure /dipole/		! dipole information
 	  real dipole_length            ! length of the dipole segment in cm
 	  real dipole_height	        ! height of the dipole segment in cm
          real dipole_current           !dipole current in A
 	  real dipole_rho		! radius of curvature in m
 	  real dipole_field		! strength of the dipole field in kG
 	  real dipole_bend		! dipole bending angle
 	  real dipole_h			! beam height change as travel through dipole
*	end structure			! dipole
	common /dipole/
     1   dipole_length, dipole_height, dipole_current, dipole_rho, 
     2   dipole_field,  dipole_bend, dipole_h
	equivalence (dipole,dipole_length)
        real dipole

*	record /foil/ foil
*	structure /foil/		! foil information
 	  real foil_thickness		! thickness of foil in cm
 	  real foil_material		! type of material
*	end structure			! foil
	common /foil/
     +   foil_thickness, foil_material
	equivalence (foil,foil_thickness)
        real foil

*	record /hpge/ hpge
*	structure /hpge/		! hyper pure Germainum detector
 	  real hpge_distance		! distance from crossing point (m)
 	  real hpge_offset_x		! displacement from axis in x (cm)
 	  real hpge_offset_y		! displacement from axis in y (cm)
 	  real hpge_diameter		! diameter of active crystal (cm)
 	  real hpge_depth		! thickness of active crystal (cm)
 	  real hpge_dead_depth	        ! thickness of dead layer (cm)
 	  real hpge_cover_mate	        ! material ID for the cover
 	  real hpge_cover_depth	        ! thickness of entrance cover (cm)
 	  real hpge_cover_side	        ! thickness of side covers (cm)
 	  real hpge_material		! material the crystal is made of
 	  real hpge_type		! type of detector to use
*	end structure
	common /hpge/
     +   hpge_distance, hpge_offset_x, hpge_offset_y, hpge_diameter,
     +   hpge_depth, hpge_dead_depth, hpge_cover_mate,
     +   hpge_cover_depth, hpge_cover_side, hpge_material, hpge_type
	equivalence (hpge,hpge_distance)
        real hpge

*	record /sources/ sources
*	structure /sources/		! radioactive source information
 	  real sources_quantity		! # of sources
 	  real sources_lines(20)	! radioactive lines
*	end structure
	common /sources/
     +   sources_quantity, sources_lines
	equivalence (sources,sources_quantity)
        real sources

*	record /seeds/ seeds
*	structure /seeds/
 	  integer seeds_1		! on/off flag for suppression shield
 	  integer seeds_2		! detector type (1-> VU)
 	  integer seeds_Iseq		! suppression threshold
*	end structure			! suppression
	common /seeds/
     +   seeds_1, seeds_2, seeds_Iseq
	equivalence (seeds,seeds_1)
        real seeds	 


*	record /suppression/ suppression
*	structure /suppression/
 	  real suppression_flag		! on/off flag for suppression shield
 	  real suppression_type		! detector type (1-> VU)
 	  real suppression_threshold	! suppression threshold
*	end structure			! suppression
	common /suppression/
     +   suppression_flag, suppression_type, suppression_threshold
	equivalence (suppression,suppression_flag)
        real suppression

	real*8 total_luminosity
	real event_weight		! weight for this event

*	record /event_type/ event_type
*	structure /event_type/
 	  integer event_type_brem	! enable Bremsstrahlung
 	  integer event_type_back	! enable Compton backscattering
 	  integer event_type_halo	! enable beam halo generation
 	  integer event_type_sync	! enable synchrotron generation
 	  integer event_type_source	! enable radioactive source generation
*	end structure			! event_type
	common /event_type/
     +   event_type_brem, event_type_back, event_type_halo,
     +	 event_type_sync, event_type_source
	equivalence (event_type,event_type_brem)
	integer event_type

*	integer seeds_1, seeds_2, seeds_Iseq
	integer total_triggers
	common /user/ total_luminosity, 
*    1	event_weight, beam, chicane, laser, detector, compton, dipole,
*    2	foil, hpge, sources, suppression, event_type, halo
*    3	seeds_1, seeds_2, seeds_Iseq,
     4	total_triggers

        integer nht,nht_pl1,nht_pl2,nht_pl3,nht_pl4
     	
        real Egam_dep,nstrip_pl1,nstrip_pl2,
     1        nstrip_pl3,nstrip_pl4,Ehit,Ee_dep,Eparticle,
     2        wXSect,wLumin,apower,vertx,vertp,
     3	      gam_rho,E_recoil,E_beam,egmax,
     4	      Ehit_pl1, Ehit_pl2, Ehit_pl3, Ehit_pl4
        common /ntuple/Egam_dep,nht,nht_pl1,nht_pl2,
     1           nht_pl3,nht_pl4,
     2		 nstrip_pl1(92),nstrip_pl2(92),
     3           nstrip_pl3(92),nstrip_pl4(92),
     4		 Ehit(400),Ehit_pl1(92),Ehit_pl2(92),
     5		 Ehit_pl3(92),Ehit_pl4(92),
     6           Ee_dep,vertx(3),vertp(3),Eparticle(100),
     7           wXSect,wLumin,apower,gam_rho,
     8           E_recoil,E_beam,egmax
	character*700 chform
	parameter (chform='Egam_dep:R,nht[0,400]:I,' //
     1 		    'nht_pl1[0,92]:I,nht_pl2[0,92]:I,' //
     2		    'nht_pl3[0,92]:I,nht_pl4[0,92]:I,' //
     3		    'nstrip_pl1(nht_pl1):R,nstrip_pl2(nht_pl2):R,' //
     4   	    'nstrip_pl3(nht_pl3):R,nstrip_pl4(nht_pl4):R,' //
     4              'Ehit(nht):R,' //
     5              'Ehit_pl1(nht_pl1):R,Ehit_pl2(nht_pl2):R,' //
     6		    'Ehit_pl3(nht_pl3):R,Ehit_pl4(nht_pl4):R,' //
     7              'Ee_dep:R,vertx(3):R,vertp(3):R,' //
     8              'Eparticle(100):R,wXSect:R,wLumin:R,' //
     9              'apower:R,gam_rho:R,E_recoil:R,E_beam:R,egmax:R')
            
