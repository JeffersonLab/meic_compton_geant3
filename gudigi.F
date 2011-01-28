      subroutine gudigi

c This routine extracts the hits information from the Geant structures
c and saves the results in the form of hbook ntuples.
c
c PW 20-Oct-93
c  - original code
c
c Richard Jones June 19, 2003
c  - mostly rewritten, the old code was based on canned histograms and
c    relied on a confusing array of duplicate histograms with different
c    normalizations.
c  - The output from the simulation now appears in a series of ntuples
c    that correspond 1-1 to the flags in the input control TYPE card.
c  - One ntuple is produced in the output for each TYPE flag set.
c  - An active ntuple receives one entry for each event for which there
c    was at least one detector that saw a signal.
c  - The simulations for different ntuples employ different optimization
c    schemes to enhance the statistics for that process.  Each entry is
c    provided with weights that can be used to convert it to a normalized
c    rate in Hz.
c  - If the cross section for the production was distorted in the
c    generation phase, the factor wXSect is provided to correct it.
c  - The weight factor wLumin/N converts counts into physical rate (/s),
c    where N is the number of events in the simulation.
c  - For example, the following paw command,
c
c            paw> ntuple/plot 3.Egamma wXSect*xLumin/1e6
c
c    can be run on a simulation of 1,000,000 halo events to produce a
c    plot of the rate as a function of energy in the gamma counter.
c
c Patricia Solvignon March 4, 2010
c - add new variables to take into account the 4 planes of the electron 
c   detector
 
      implicit none

#include "geant321/gclist.inc"
#include "geant321/gcnum.inc"

      include 'user.inc'
      include 'params.inc'

      integer MAX_HITS
      parameter (MAX_HITS=1024)

      integer ilook             ! flag returned from glook
      integer nhits             ! # of hits found in gfhits
      integer mhits             ! # of hits found in shield
      real Esum(MAX_HITS)     ! Energy sum from gfhits
      integer numbv(MAX_HITS) ! volume number for this hit
      integer tracks(MAX_HITS)  ! track number for this hit
      integer numvs(10)         ! volume numbers
      data numvs/10*0/
      integer i,j,it,id         ! index variables
      integer particle_type     ! primary particle type
      integer vertex_number     ! which vertex
      real vertex(3),pvertex(3) ! vertex parameters
      real user(8)              ! user parameters
      integer num_user          ! # of user parameters
      real shield_energy        ! energy deposited in the shield
      real total_energy         ! energy deposited in detectors
      real mass(100)
      
      logical hexist
      external hexist

      integer NSHIELDS,NDETECTS,NPDETECTS,NEDETECTS
      parameter (NSHIELDS = 3)
      parameter (NDETECTS = 2)
      parameter (NPDETECTS = 1)
      parameter (NEDETECTS = 4)
      character*4 shield_names(NSHIELDS) /'DETF', 'DETS', 'DETC'/
c      character*4 detect_names(NDETECTS)/'DETI', 'EDES'/
      character*4 detect_names(NDETECTS)/'DETI', 'ELED'/
      character*4 pdetect_names(NPDETECTS)/'DETI'/
      character*4 edetect_names(NEDETECTS)/'EDE1', 'EDE2', 
     &                                     'EDE3', 'EDE4'/

c  Variables added by Doug in order to calculate the strip hit
      real Eprime, pp, p, me, B, e, thetab, thetap  !,egmax
      me = 0.000511             ! GeV
c      B = 0.67480605            ! T
c      B = 0.53947            ! T
       B = dipole_field/10.    ! T
      e = 1.6022d-19            ! C
c      thetab = 0.171415632      ! rad
cpat      thetab = 0.174532925      ! rad
       thetab = dipole_bend*Pi/180   ! rad


      call glook ('HITS', lprin, nprin, ilook)
      if (ilook .gt. 0) then
         call gphits ('*', '*')
      endif

c First get the energy deposited in the suppression shield

c      shield_energy = 0.
c      do j = 1, NSHIELDS
c         call gfhits ('SDET', shield_names(j), 1, 1, 10, 0,   
c     $        numvs, tracks, numbv, Esum, mhits)
c         do i = 1, mhits
c	    shield_energy = shield_energy + Esum(i)
c         enddo
c      enddo


c Now get the energy deposited in the energy measuring detectors
      
      do id = 1, 5
         nht          = 0
         Egam_dep     = 0.
         Ee_dep       = 0.
         total_energy = 0.
         do i=1,100
            Ehit(i)   = 0.0
         enddo
c         write(99,*) '#### START PROCESS A NEW EVENT ####'
         do it = 1, ntrack
	    call gfkine(it, vertex, pvertex, particle_type,
     $                  vertex_number, user, num_user)
	    if (user(1) .eq. id) then
               call gfhits('EDET', detect_names(1), 1, 1, MAX_HITS, 
     $                     it, numvs, tracks, numbv, Esum, nhits)
               do i = 1, nhits
                  Egam_dep     = Esum(i)
                  total_energy = total_energy + Esum(i)
c                  write(6,*) 'detected hits in photon detector',
c     $                        i,nhits,Esum(i),Egam_dep,it
               enddo
               !do j = 2, NDETECTS
               call gfhits('EDET',detect_names(2),1,1, MAX_HITS,
     $                     it, numvs, tracks, numbv, Esum, nhits)
               do i = 1, nhits
                  nht          = nht + 1                     ! hit number
                  plane(nht)   = numbv(i)                    ! volume number = plane number of each hit 
                  Ehit(nht)    = Esum(i)                     ! energy deposit per hit
                  Ee_dep       = Ee_dep + Esum(i)            ! total energy deposited in electron detector
                  total_energy = total_energy + Esum(i)      ! total energy deposited = Egamma+Eedeposit
c                  write(6,*) 'detected electron',ntrack,i,nhits,nht,
c     $                       numbv(i),Esum(i),
c     $                       plane(nht),Ehit(nht),
c     $                       Ee_dep
               enddo
               !enddo

               if (particle_type.eq.1) then 
                  mass(particle_type) = 0.0
               elseif (particle_type.eq.3) then 
                  mass(particle_type) = mass_electron
               endif
               Eparticle(particle_type) = sqrt(pvertex(1)**2 
     &                         + pvertex(2)**2 + pvertex(3)**2 
     &                         + mass(particle_type)**2)

c               write(6,*) '--- For track ',it,' of ',ntrack 
c               write(6,*) 'position: ', vertex(1), vertex(2), vertex(3)
c               write(6,*) 'momentum: ',pvertex(1),pvertex(2),pvertex(3)
c               write(6,*) 'energy: ',
c     $                  sqrt(pvertex(1)**2+pvertex(2)**2+pvertex(3)**2)
c               write(6,*) 'detected photon energy=',
c     $                     Egam_dep*1E3,' MeV'
c               write(6,*) 'scattered electron total energy= ',
c     $                     Eparticle(3),' GeV'
c               write(6,*) 'plane 1 energy deposit = ',
c     $                     Ehit(1)*1E3,' MeV'
c               write(6,*) 'plane 2 energy deposit = ',
c     $                     Ehit(2)*1E3,' MeV'
c               write(6,*) 'plane 3 energy deposit = ',
c     $                     Ehit(3)*1E3,' MeV'
c               write(6,*) 'plane 4 energy deposit = ',
c     $                     Ehit(4)*1E3,' MeV'
c               write(6,*) 'all planes energy deposit = ',
c     $                     Ee_dep*1E3,' MeV'
c               write(6,*) '---'

c   Draw track for every event

c	      if (nstrips.ne.0) then
c	        call gdraw('CAVE',90.,0.,0.,24.,15.,0.045,0.045)
c	        call gdxyz(0)
c		call gdraw('CAVE',90.,0.,0.,54.,1.5,0.10,0.10)
c		call gdxyz(0)
c		call gdraw('CAVE',90.,0.,0.,10.,15.,0.018,0.018)
c		call gdxyz(0)
c	     endif



c   Calculate theoretical strip hit (added by Doug)
               ereal    = user(5)
               egmax    = user(6)

c               Eprime = beam_E + laser_energy*1.0d-9 - 5*ereal*0.04808 ! GeV
               Eprime  = beam_E + laser_energy*1.0d-9 - ereal*egmax/1000.0 ! GeV
               pp      = sqrt(Eprime**2 - me**2)*1.6022d-10/3.d8 ! kgm/s
               p       = sqrt(beam_E**2 - me**2)*1.6022d-10/3.d8 ! kgm/s
               thetap  = asin(p/pp*sin(thetab)) ! rad

               h       = pp/e/B*(1-cos(thetap)) - p/e/B*(1-cos(thetab))
     $                   + 2.10*(tan(thetap) - tan(thetab))

               calc_strip = (h - (detector_offset-0.11591)/100.)/
     $                      detector_stripwidth/1e-2 

               vertx(1) = vertex(1)
               vertx(2) = vertex(2)
               vertx(3) = vertex(3)
               vertp(1) = pvertex(1)
               vertp(2) = pvertex(2)
               vertp(3) = pvertex(3)
               wXSect   = user(2)
               wLumin   = user(3)
               apower   = user(4)
               E_recoil = user(7)
               Einc     = user(8)
	    endif
         enddo
         if (hexist(id) .and. total_energy .ge. 1e-6) then
c            write(6,*)'GO AHEAD !', total_energy,Eparticle(1),
c     $                              Eparticle(3)
	    call hfnt(id)  ! fill ntuple
         endif
      enddo

      end




