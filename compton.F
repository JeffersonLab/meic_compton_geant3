      subroutine compton_xsect (E_beam, E_photon, theta_photon, 
     $     theta_gamma,
     $     E_gamma, theta_cm, cross_section, pol_xsect)

c This routine will calculate the scattered energy in Compton
c scattering, and polarizations.
c
c INPUTS:
c	E_beam electron beam energy in GeV
c	E_photon incident photon energy in eV
c	theta_photon incident photon angle wrt to the electron beam
c	theta_cm outgoing photon angle in the cm
c
c OUTPUTS:
c	theta_gamma outgoing photon angle wrt to the electron beam
c	E_gamma outgoing photon energy in GeV
c	cross_section differential cross section in fm^2 / MeV
c
c TPW 26-Oct-93
c  - original code
c
c Richard Jones June 20, 2003
c  - introduced private degree-based trig functions because g77 does
c    not support the degree-based intrinsics (sind,cosd,tand...)

      implicit none
	
      include 'trig.inc'

      real max_xsect

      real*8 E_beam             ! Incident electron energy in GeV
      real*8 E_photon           ! Incident photon energy in eV
      real*8 theta_photon       ! Incident photon angle in deg
      real*8 theta_gamma        ! outgoing photon angle in deg
      real*8 E_gamma            ! outgoing photon energy in GeV
      real*8 theta_cm           ! outgoing photon angle in deg in cm
      real*8 cross_section      ! differential xsect in fm^2/MeV
      real*8 pol_xsect		! polarized diff xsect in fm^2/MeV
      
      real*8 Eb			! beam energy in eV
      real*8 pb			! beam momentum in eV/c
      real*8 mass               ! beam mass in eV
      real*8 r_e                ! classical electron radius in fm
      real*8 gamma              ! Lorentz gamma factor
      real*8 beta               ! Lorentz beta factor
      real*8 E_max              ! maximum possible gamma energy
      real*8 rho                ! fraction of maximum gamma energy
      real*8 a			! a factor in various equations
      real*8 kappa              ! term in cross section calc
      real*8 Pi			! you got it
      real*8 P_par              ! lab momentum
      real*8 P_per              ! lab momentum
      real*8 E_cm               ! cm energy of photon
      real*8 P_par_cm           ! cm perp momentum
      real*8 P_per_cm           ! cm parallel momentum
      real*8 P_cm               ! cm momentum
      real*8 E_scat             ! cm scattered energy of photon
      real*8 P_scat_per		! cm scattered perp momentum
      real*8 P_scat_par		! cm scattered parallel momentum
      real*8 cos_theta_o        ! cosine of angle for polarization
      integer i			! index variable
      
      common/dave/max_xsect
      
      Pi = 3.141592653589793238
      
      mass = 0.51099006d6       ! electron mass in eV
      r_e  = 2.818              ! classical electron radius in fm
      
      Eb = E_beam * 1d9		! convert to eV
      pb = sqrt(Eb**2 - mass**2) ! beam momentum
      
      gamma = Eb / mass		! Lorentz gamma factor
      beta  = sqrt(1. - 1. / gamma**2) ! Lorentz beta factor

            
      P_par    = E_photon * dcosd(theta_photon)
      P_per    = E_photon * dsind(theta_photon)

c     Boost to the electron's frame
      E_cm     = gamma * (E_photon - P_par * beta)
      P_par_cm = gamma * (P_par - E_photon * beta)
      P_per_cm = P_per
      P_cm     = sqrt(P_per_cm**2 + P_par_cm**2)
      
c     Now scatter in the cm
      
      E_scat = (P_per_cm**2 + P_par_cm**2 - 
     $     E_cm**2 - 2.*mass*E_cm) /
     $     (2. * (dsind(theta_cm) * P_per_cm +
     $     dcosd(theta_cm) * P_par_cm - mass - E_cm))
      P_scat_par = E_scat * dcosd(theta_cm)
      P_scat_per = E_scat * dsind(theta_cm)
      
c     Now boost back to the lab frame
      
      E_gamma = gamma * (E_scat + P_scat_par * beta)
      P_par = gamma * (P_scat_par + E_scat * beta)
      P_per = P_scat_per
      
c     Now calculate the lab angle
      
      theta_gamma = dacosd(P_par / E_gamma)
      
      a = 1. / (1. + 4. * gamma * E_photon / mass) ! page 6 of our prop
      
      E_max = E_photon * (Eb - pb * dcosd(theta_photon)) /
     $     (Eb + E_photon - pb * cosd(0.) + 
     $     E_photon * dcosd(theta_photon))


      rho = E_gamma / E_max
      
      cross_section = (2. * Pi * r_e**2 * a / (E_max * 1.d-6)) *
     $     ((rho * (1. - a))**2 / (1. - rho * (1. - a)) +
     $     1. +
     $     ((1. - rho * (1. + a)) / (1. - rho * (1. - a)))**2)
      pol_xsect = 2. * Pi * r_e**2 * a / (E_max * 1d-6) *
     $     (1. - rho * (1. + a)) *
     $     (1. - (1. / (1. - rho * (1. - a)))**2)
      max_xsect= (2. * Pi * r_e**2 * a / (E_max * 1.d-6)) *
     $     ((1. * (1. - a))**2 / (1. - 1. * (1. - a)) +
     $     1. +
     $     ((1. - 1. * (1. + a)) / (1. - 1. * (1. - a)))**2)
      
      
      E_gamma = E_gamma / 1d9   ! convert from eV to GeV
      
      end
      
