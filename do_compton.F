      subroutine do_compton (vertex, p, type)

c  This routine will generate a backscatter gamma for this electron.
c  Outputs:
c	vertex(1..3): 3-vector position of electron at entry to chicane (!)
c	p(1..3): 3-momentum of electron at entry to chicane
c  Note that these coordinates do NOT refer to the electron at the
c  laser-beam crossing point!  They have to be propagated there.
c
c PW 20-Oct-93
c  - original code
c
c Richard Jones June 19, 2003
c  - got rid of "fortran structures" in favour of standard f77 code
c  - replaced intrinsic degree-based trigs (sind,cosd,tand..) with private
c    versions because g77 does not support them
c  - corrected luminosity calculation for crossing-angle effects
c  - calculated luminosity in /b/s
c  - added weight factors for cross section and luminosity
c  - stored left-right asymmetry in kinematics bank
c  - generated tracks for both backscattered gamma and recoil electron
c  - got rid of some factors that were calculated but not used

      implicit none

      include 'params.inc'
      include 'user.inc'
      include 'trig.inc'

      integer type
      real vertex(3), p(3)

      real*8 x0, y0, z0, momentum, sintheta, costheta
      real*8 cosphi, sinphi, dz, x, y, z, l
      real*8 lx, ly, lz, sigma, r2, photon_flux, area
      real*8 sigmax, sigmay, Px, Py, Pgamma, electron_flux
      real*8 Px_gen, Py_gen
      real*8 theta_cm, Ephoton,Ebeam
      real*8 theta_photon, thetax_photon, thetay_photon
      real*8 theta_gamma, E_gamma, xsect, pol_xsect
c      real*8 E_recoil
      real*8 ex, ey, ez, theta, phi
      real*8 ux, uy, uz
      real*8 qx, qy, qz, edotq
      real*8 gx, gy, gz
      real*8 phi_photon, phi_gamma, luminosity
      real*8 Pbeam, Egamma_max, rootS, eta, kstar
      real*8 recoilMom, thetae
      real*8 random, randoms(10)  ! random numbers
      real user(8)
      real random1, random2
      real*8 grnd, rand1, rand2, rand3

      real newvertex(3), newp(3), recoilp(3)
      integer track_num, vertex_num, i, npoints


      x0 = vertex(1)
      y0 = vertex(2) - chicane_drop
      z0 = 0.0

      momentum = sqrt(p(1)**2 + p(2)**2 + p(3)**2)
      sintheta = sqrt(p(1)**2 + p(2)**2) / momentum
      costheta = p(3) / momentum
      cosphi   = p(1) / (momentum * sintheta)
      sinphi   = p(2) / (momentum * sintheta)
      theta    = dasind(sintheta)
      phi      = datan2d(sinphi, cosphi)
      ex = p(1) / momentum
      ey = p(2) / momentum
      ez = p(3) / momentum
      
      dz = 2. * compton_width / compton_number   ! =2.0
      npoints = compton_number
      do i = 1, npoints
c
c--- define interaction point
c 
c         call grndm(random, 1)
         rand1 = grnd()
         z = (compton_width * (2.*rand1-1.))
         l  = (z - z0) / costheta
         x  = x0 + l * cosphi * sintheta
         y  = y0 + l * sinphi * sintheta
         
c     
c--- First get the laser flux and origin
c     
         lx = x * laser_Rxx + y * laser_Rxy + z*laser_Rxz+laser_offset_x
         ly = x * laser_Ryx + y * laser_Ryy + z*laser_Ryz+laser_offset_y
         lz = x * laser_Rzx + y * laser_Rzy + z*laser_Rzz+laser_offset_z
         sigma = laser_sigma * sqrt(1. + (lz / laser_beta)**2)
         r2 = lx**2 + ly**2
     
         ! Note that this is really Px * Py
         Pgamma = 1. / (sigma**2 * 2. * Pi) * exp(-r2 / (2. * sigma**2))
         area = Pi * sigma**2
         photon_flux = Pgamma * laser_flux
c     write (*,'(a,5(1pg13.5))') ' l ', sigma, r2, Px, laser_flux, photon_flux

c
c--- Now get the electron beam flux
c
         sigmax = beam_sigmax * sqrt(1. + (z / beam_quad_betax)**2)   ! eq.57
         sigmay = beam_sigmay * sqrt(1. + (z / beam_quad_betay)**2)
         Px = 1. / (sigmax * sqrt(2. * Pi)) * exp(-x**2 /(2.*sigmax**2)) ! eq.56
         Py = 1. / (sigmay * sqrt(2. * Pi)) * exp(-y**2 /(2.*sigmay**2))

         Px_gen = 1. / (beam_sigmax * sqrt(2. * Pi)) * 
     $        exp(-x**2 / (2. * beam_sigmax**2))
         Py_gen = 1. / (beam_sigmay * sqrt(2. * Pi)) * 
     $        exp(-y**2 / (2. * beam_sigmay**2))
         electron_flux = (Px/Px_gen) * (Py/Py_gen) * beam_flux * dz

c     write (*,'(a,9(1pg12.4))') ' e ', sigmax, sigmay, x, y, Px, Py, beam_flux, electron_flux

cpat         Ebeam = momentum
         Ebeam = sqrt(momentum**2 + mass_electron**2)
         call granor(random1, random2)
         Ephoton = laser_energy + laser_dE * random1
         call granor(random1, random2)
         thetax_photon = random1 * laser_dtheta
         thetay_photon = random2 * laser_dtheta
         theta_photon = sqrt(thetax_photon**2 + thetay_photon**2)
         phi_photon   = datan2(thetay_photon,thetax_photon)
         ux = sin(theta_photon) * cos(phi_photon)
         uy = sin(theta_photon) * sin(phi_photon)
         uz = cos(theta_photon)
c         write(90,*)'acos(uz)= ',1000.0*theta_photon
c         write(91,*)1000.0*theta_photon
         qx = ux * laser_Rxx + uy * laser_Ryx + uz * laser_Rzx
         qy = ux * laser_Rxy + uy * laser_Ryy + uz * laser_Rzy
         qz = ux * laser_Rxz + uy * laser_Ryz + uz * laser_Rzz
c         write(90,*)'(ex,qx)= ',ex,qx
c         write(90,*)'(ey,qy)= ',ey,qy
c         write(90,*)'(ez,qz)= ',ez,qz
         edotq = qx * ex + qy * ey + qz * ez

c RTJ: here we switch to degrees for call to compton_xsect()

         if (abs(edotq) .gt. 1.) then
	    if (edotq .gt. 0) then
               theta_photon = 0.
	    else
               theta_photon = 180.
	    endif
         else
	    theta_photon = dacosd(edotq)
         endif
c         write(90,*)'edotq= ',abs(edotq)
c         write(90,*)'theta_photon= ',theta_photon*Pi/180.0
c         write(90,*)'alpha_c= ',1000.0*(Pi-theta_photon*Pi/180.0)
c          call grndm(randoms, 3)
          rand2 = grnd()
          rand3 = grnd()
c         write(6,*) rand1,rand2
          theta_cm = dacosd((2. * rand2) - 1.) ! cm angle (deg)
         call compton_xsect (Ebeam, Ephoton, theta_photon, theta_gamma,
     $        E_gamma, theta_cm, xsect, pol_xsect)
c DJG Actually apply the beam and laser polarizations (duh)
         pol_xsect = pol_xsect*beam_polarization*laser_pol
         
         newvertex(1) = x
         newvertex(2) = y
         newvertex(3) = z
         call ugsvert(newvertex, 0, 0, 0, 0, vertex_num)
         phi_gamma   = 360. * rand3
         gx = dsind(theta_gamma) * dcosd(phi_gamma) + ex
         gy = dsind(theta_gamma) * dsind(phi_gamma) + ey
         gz = sqrt(1.0d0 - gx**2 - gy**2)
         newp(1) = gx * E_gamma
         newp(2) = gy * E_gamma
         newp(3) = gz * E_gamma
         recoilp(1) = p(1) - newp(1)
         recoilp(2) = p(2) - newp(2) 
         recoilp(3) = p(3) - newp(3)
c         recoilp(3) = 1.165 ! for testing electron detector position
         E_recoil   = sqrt(recoilp(1)**2 + recoilp(2)**2 + recoilp(3)**2 
     &                + mass_electron**2)
C     RTJ: luminosity = electron_flux * photon_flux * 2/c  (in /cm^2/s)

         luminosity       = electron_flux * photon_flux * (1-edotq)/c
         total_luminosity = total_luminosity + luminosity

C     RTJ: xsect is returned in fm^2/MeV whereas we are generating uniformly
c     in d(cos(thetastar)).  First convert xsect to barns/MeV and then
c     multiply by 2*d(kf)/d(cos(thetastar)) for correct normalization.
c     It turns out that the conversion factor is equal to the energy
c     of the gamma for 180 degree scattering.

         Pbeam = sqrt(Ebeam**2 - mass_electron**2)
         rootS = sqrt(mass_electron**2 + 2*(Ebeam+Pbeam)*Ephoton/1.e9)
         kstar = 2*(Ephoton/1.e9)*(Ebeam+Pbeam)/(2*rootS)
         eta   = (Pbeam - Ephoton/1.e9)/rootS
         Egamma_max = 2*eta*kstar * 1000       ! in MeV

         user(1) = type
         if (type.eq.2) then      ! backscattering
            user(2) = (xsect / 1.e2) * Egamma_max ! cross section in barns
            user(3) = luminosity / 1e24           ! luminosity in /barns/s
            user(4) = pol_xsect/xsect             ! asymmetry

         elseif (type.eq.3) then  ! beam halo
            user(2) = (xsect / 1.e2) * Egamma_max
            user(3) = halo_fraction * luminosity / 1e24  !halo_fraction * beam_flux
            user(4) = pol_xsect/xsect
         endif
         user(5) = E_gamma*1000/Egamma_max
         user(6) = Egamma_max*1000             ! in keV
         user(7) = E_recoil
         user(8) = Ebeam

c         write(66,*)'UNCERTAINTY CHECK: ',
c     &       beam_polarization,laser_pol,user(3),user(4),user(2),user(7)
c         write(6,*)'initial conditions: ',Ebeam,Ephoton
c         write(6,*)'Final: ',E_recoil,E_gamma*1000,Egamma_max,recoilp(3)
c         IF (E_recoil.ge.1.118.and.E_recoil.le.1.152) THEN
c             write(6,*)'should fire the electron detector !!!' 
c         ENDIF

         if(user(2).lt.0.) then
            write(6,*) 'Error: Negative cross section!'
         endif

         call ugskine(newp, gamma_id, vertex_num, user, 8, track_num)
         call ugskine(recoilp, electron_id, vertex_num,user,8,track_num)
      enddo
      end
	
