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
      integer nstrip            ! # of strips
      real Esum(MAX_HITS)     ! Energy sum from gfhits
      integer numbv(MAX_HITS) ! volume number for this hit
      integer tracks(MAX_HITS)  ! track number for this hit
      integer numvs(400)         ! volume numbers
      data numvs/400*0/
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

       Integer nstr
       REAL stefpl(384)    
       REAL*8 grnd,rand1,rand2,rand3,rand4

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




c Effiencies of each strip

          do i=1,384
             stefpl(i) = 1
          enddo
c
c          stefpl(1) = 0.738784
c          stefpl(2) = 0.90108
c          stefpl(3) = 0.92897
c          stefpl(4) = 0.906751
c          stefpl(5) = 0.788948
c          stefpl(6) = 0.979106
c          stefpl(7) = 0.479929
c          stefpl(8) = 0.89811
c          stefpl(9) = 0.902879
c          stefpl(10) = 0.825672
c          stefpl(11) = 0.0
c          stefpl(12) = 0.789597
c          stefpl(13) = 0.997649
c          stefpl(14) = 0.36071
c          stefpl(15) = 0.8736
c          stefpl(16) = 0.818515
c          stefpl(17) = 0.872622
c          stefpl(18) = 0.674188
c          stefpl(19) = 0.236137
c          stefpl(20) = 0.694874
c          stefpl(21) = 0.657126
c          stefpl(22) = 0.99154
c          stefpl(23) = 0.868207
c          stefpl(24) = 0.729059
c          stefpl(25) = 0.811685
c          stefpl(26) = 0.806318
c          stefpl(27) = 0.84706
c          stefpl(28) = 0.684571 
c          stefpl(29) = 0.802109
c          stefpl(30) = 0.788766
c          stefpl(31) = 0.866718
c          stefpl(32) = 0.990439
c          stefpl(33) = 0.615052
c          stefpl(34) = 0.352437
c          stefpl(35) = 0.624556
c          stefpl(36) = 0.812179
c          stefpl(37) = 0.187273 
c          stefpl(38) = 0.838017
c          stefpl(39) = 0.929875
c          stefpl(40) = 0.050505
c          stefpl(41) = 0.926972
c          stefpl(42) = 0.284655
c          stefpl(43) = 0.892544
c          stefpl(44) = 0.99623
c          stefpl(45) = 0.976308
c          stefpl(46) = 0.681935
c          stefpl(47) = 0.667589
c          stefpl(48) = 0.743127
c          stefpl(49) = 0.671888
c          stefpl(50) = 0.977432
c          stefpl(51) = 0.823006
c          stefpl(52) = 0.650815
c          stefpl(53) = 0.995517
c          stefpl(54) = 0.053342
c          stefpl(55) = 0.629024
c          stefpl(56) = 0.94284
c          stefpl(57) = 0.983961
c          stefpl(58) = 0.893357
c          stefpl(59) = 0.635873
c          stefpl(60) = 0.765348
c          stefpl(61) = 0.73453
c          stefpl(62) = 0.989518
c          stefpl(63) = 0.835677
c          stefpl(64) = 0.763209
c          stefpl(65) = 0.89631
c          stefpl(66) = 0.84605
c          stefpl(67) = 0.833868
c          stefpl(68) = 0.809937
c          stefpl(69) = 0.830316
c          stefpl(70) = 0.941887
c          stefpl(71) = 0.649549
c          stefpl(72) = 0.9801961
c          stefpl(73) = 0.620372
c          stefpl(74) = 0.73013
c          stefpl(75) = 0.658884
c          stefpl(76) = 0.670023
c          stefpl(77) = 0.6728556
c          stefpl(78) = 0.938624
c          stefpl(79) = 0.917229
c          stefpl(80) = 0.965011
c          stefpl(81) = 0.766583
c          stefpl(82) = 0.757121
c          stefpl(83) = 0.740502
c          stefpl(84) = 0.902815
c          stefpl(85) = 0.649652
c          stefpl(86) = 0.905020
c          stefpl(87) = 0.807634
c          stefpl(88) = 0.668078
c          stefpl(89) = 0.702527
c          stefpl(90) = 0.799731
c          stefpl(91) = 0.812888
c          stefpl(92) = 0.672309
c          stefpl(93) = 0.688432
c          stefpl(94) = 0.886715
c          stefpl(95) = 0.881429
c          stefpl(96) = 0.84684
c         
c       do i=40,96
c           stefpl(i-39+96) =  stefpl(i)
c       enddo
c
c       do i=1,39
c           stefpl(i+96-39+96) =  stefpl(i)
c       enddo
c
c       do i=161,192
c           stefpl(i-64+96) =  stefpl(i)
c       enddo
c
c       do i=97,160
c           stefpl(i+96-64+96) =  stefpl(i)
c       enddo
c
c       do i=219,288
c           stefpl(i-26+96) =  stefpl(i)
c       enddo
c
c       do i=193,218
c           stefpl(i+96-26+96) =  stefpl(i)
c       enddo


c


      call glook ('HITS', lprin, nprin, ilook)
      if (ilook .gt. 0) then
         call gphits ('*', '*')
      endif

c Get the energy deposited in the energy measuring detectors
      nstrip = int(detector_stripnumber)      

      do id = 1, 5   ! loop over process type (i.e., Brem., Compton BS, Halo, Sync. or Source lines)
         nht          = 0
         nht_pl1      = 0.
         nht_pl2      = 0.
         nht_pl3      = 0.
         nht_pl4      = 0.
	 Egam_dep     = 0.
         Ee_dep       = 0.
         total_energy = 0.
         do i=1,4*nstrip
            Ehit(i)   = 0.0
         enddo
	 do i=1,nstrip ! note that the index here is actually hit #
            nstrip_pl1(i) = 0.0
            nstrip_pl2(i) = 0.0
            nstrip_pl3(i) = 0.0
            nstrip_pl4(i) = 0.0
	    Ehit_pl1(i)   = 0.0
	    Ehit_pl2(i)   = 0.0
	    Ehit_pl3(i)   = 0.0
	    Ehit_pl4(i)   = 0.0	
         enddo

c         write(99,*) '#### START PROCESS A NEW EVENT ####'
         do it = 1, ntrack
	    call gfkine(it, vertex, pvertex, particle_type,
     $                  vertex_number, user, num_user)
	    if (user(1) .eq. id) then                    ! check process type
C              Check for hits in photon detector
               call gfhits('EDET', detect_names(1), 1, 1, MAX_HITS, 
     $                     it, numvs, tracks, numbv, Esum, nhits)
c                  if(nhits.ge.1) then
c                   write(6,*) 'detected hits in photon detector',
c     $                  nhits,it,MAX_HITS
c                  endif
               do i = 1, nhits
                  Egam_dep     =  Egam_dep + Esum(i) !total energy deposited in photon det.
                  total_energy = total_energy + Esum(i)
c                  if(nhits.ge.1) then
c                   write(6,*) 'energy deposited in this hit',
c     $                  i,Esum(i)
c                  endif
               enddo
C              Check for hits in electron detector
               call gfhits('EDET',detect_names(2),1,1, MAX_HITS,
     $                   it, numvs, tracks, numbv, Esum, nhits)
               do i = 1, nhits !loop over hits
                  nht          = nht + 1 ! hit number
                  if(numbv(i).le.nstrip .and. 
     $                 numbv(i).ge.5) then ! strips 1-4 are inactive      
                     rand1 = grnd()
                     nstr =  numbv(i)
                     if(rand1.lt.stefpl(nstr)) then ! check efficiency
                        nht_pl1 = nht_pl1 + 1 ! volume number = strip number of each hit
                        nstrip_pl1(nht_pl1) = numbv(i)  
                        Ehit_pl1(nht_pl1) = Esum(i) 
                     endif
		  elseif(numbv(i).le.(2*nstrip) .and. 
     $                    numbv(i).ge.(nstrip+5)) then
                     rand2 = grnd()
                     nstr =  numbv(i)
                     if(rand2.lt.stefpl(nstr)) then
                        nht_pl2 = nht_pl2 + 1
                        nstrip_pl2(nht_pl2) = numbv(i) - nstrip
                        Ehit_pl2(nht_pl2) = Esum(i) 
                     endif
		  elseif(numbv(i).le.(3*nstrip) .and. 
     $                    numbv(i).ge.((2*nstrip)+5)) then 
                     rand3 = grnd()
                     nstr =  numbv(i)
                     if(rand3.lt.stefpl(nstr)) then
                        nht_pl3 = nht_pl3 + 1
                        nstrip_pl3(nht_pl3) = numbv(i) - (2*nstrip)
                        Ehit_pl3(nht_pl3) = Esum(i) 
                     endif
		  elseif(numbv(i).le.(4*nstrip) .and. 
     $                    numbv(i).ge.((3*nstrip)+5)) then
                     rand4 = grnd()
                     nstr =  numbv(i)
                     if(rand4.lt.stefpl(nstr)) then
                        nht_pl4 = nht_pl4 + 1
                        nstrip_pl4(nht_pl4) = numbv(i) - (3*nstrip)
                        Ehit_pl4(nht_pl4) = Esum(i) 
                     endif
		  endif
		  		  
		  Ehit(nht)    = Esum(i)                     ! energy deposit for this hit
                  Ee_dep       = Ee_dep + Esum(i)            ! total energy deposited in electron detector
                  total_energy = total_energy + Esum(i)      ! total energy deposited = Egamma+Eedeposit
c                  write(6,*) 'detected electron',ntrack,i,nhits,nht,
c     $                       numbv(i),Esum(i),
c     $                       plane(nht),Ehit(nht),
c     $                       Ee_dep
               enddo
               


               if (particle_type.eq.1) then  ! photon
                  mass(particle_type) = 0.0
               elseif (particle_type.eq.3) then  ! electron
                  mass(particle_type) = mass_electron
               endif
               if(particle_type.lt.4) then
                  Eparticle(particle_type) = sqrt(pvertex(1)**2 
     &                 + pvertex(2)**2 + pvertex(3)**2 
     &                 + mass(particle_type)**2)
               endif

c               write(6,*) 'particle type, Eparticle',particle_type,
c     >                    Eparticle(particle_type)

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
               vertx(1) = vertex(1)
               vertx(2) = vertex(2)
               vertx(3) = vertex(3)
               vertp(1) = pvertex(1)
               vertp(2) = pvertex(2)
               vertp(3) = pvertex(3)
               wXSect   = user(2)
               wLumin   = user(3)
               apower   = user(4)
               gam_rho  = user(5)
               egmax    = user(6)
               E_recoil = user(7)
               E_beam   = user(8)
	    endif  ! end check on process type
         enddo     ! end loop over tracks

c         if (hexist(id) .and. total_energy .ge. 1e-6) then
C DG Get rid of the energy cut for now. Almost every event passes, and it might
C DG introduce bias.
         if (hexist(id)) then
c            write(6,*)'GO AHEAD !', total_energy,Eparticle(1),
c     $                              Eparticle(3)
	    call hfnt(id)  ! fill ntuple
         endif
      enddo   ! end loop over process types

      end




