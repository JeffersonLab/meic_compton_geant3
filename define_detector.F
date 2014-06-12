      subroutine define_DETEctor(mother)

c This routine defines the geometry and materials of the backscatter
c gamma and recoil electron detectors.
c Inputs:
c	name: 4-character name of outer volume
c
c PW 20-Oct-93
c  - original code
c
c Richard Jones June 20, 2003
c  - got rid of "fortran structure" references in favour of standard f77
c  - replaced trig intrinsics based on degrees (sind,cosd,tand..) with
c    local versions because g77 does not support them
c  - changed mixed-case names to upper-case for geant++ support
c  - This routine has been modified many times since the original version.
c    Some remnants of an early design based on a large crystal of hyper-
c    pure germanium still remain (hence the name hpge) surrounded by an
c    anti-coincidence shield, but most of that is no longer functional
c  - Added an electron detector in the form of silicon microstrips placed
c    downstream of the third dipole.
c
c Doug Storey - Jan 2006
c  Added option of moving electron detector to before dipole 4
c
c Patricia Solvignon - Nov 2009
c  - adding option to use the electron detector from QWEAK final design
c    QWEAK = .FALSE. --> older version of the electron detector 
c

      implicit none

      include 'user.inc'
      include 'params.inc'
      include 'ugeom.inc'

      character*(*) mother      ! volume to imbed this in
      integer foil_mate, col_mate, det_cover_mate, det_mate, volume_num
      integer det_dead_mate, recoil_vac,recoil_mate, edet_mate
      integer side_mate, BaF_mate
      integer iset, idet
      integer material          ! material number
      character*4 names(1)
      integer nbits(1)
      real orig(1),fact(1)
      data names/'Esum'/
      data nbits/32/
      data orig/0/
      data fact/1e8/
      real pars(9)
      integer edetpos, istrip,nstrip,ngap,iplane,nplane, stripID
      real thetab,le
      real detector_position,sensitive_height,sensitive_thick,vshift
      logical QWEAK
      character*4 EDETPL,DETNAME,SPACE
      character*1 ch_plane(4)
      character*2 ch_space(96)
      DATA ch_plane /'1','2','3','4'/
      DATA ch_space /'01','02','03','04','05','06','07','08','09',
     1          '10','11','12','13','14','15','16','17','18','19',
     2          '20','21','22','23','24','25','26','27','28','29',
     3          '30','31','32','33','34','35','36','37','38','39',
     4          '40','41','42','43','44','45','46','47','48','49',
     5          '50','51','52','53','54','55','56','57','58','59',
     6          '60','61','62','63','64','65','66','67','68','69',
     7          '70','71','72','73','74','75','76','77','78','79',
     8          '80','81','82','83','84','85','86','87','88','89',
     9          '90','91','92','93','94','95','96' /

c      thetab = 9.821392245*Pi/180 
      thetab = dipole_bend * Pi/180.0
      write(98,*)'in define_detector.F, thetab= ',dipole_bend


c Now define the foil

c	if (foil_thickness .gt. 0.) then
c	  call ugstmed (foil_mate, 'Foil$', int(foil_material + 0.5),
c	1		0, 0, FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, 
c	2		STMIN, 0, 0)
c	  pars(1) = 0.
c	  pars(2) = 1.
c	  pars(3) = foil_thickness / 2.
c	  call ugsvolu ('FOIL', 'TUBE', foil_mate, pars, 3, volume_num)
c	  pars(3) = (chicane_length + foil_thickness) / 2. + dipole_length
c	  call gspos ('FOIL', 1,mother, 0., 0., pars(3), 0, 'ONLY')
c	  call gsatt ('FOIL', 'COLO', RED)	! Change color of the foil
c	  call gsatt ('FOIL', 'SEEN', 1)	! foil is visible
c	endif

c--------------------------------Photon detector------------------------------
c Now define the DETEctor

      call ugstmed(det_cover_mate,'DETEctor$',int(hpge_cover_mate+0.5),
     $     0,0,FIELDM,TMAXFD,STEMAX,DEEMAX,EPSIL,STMIN,0,0)
C Tube for GSO
c      pars(1) =  0.0            ! inner radius
c      pars(2) =  hpge_diameter / 2. + hpge_cover_depth ! Outer radius
c      pars(3) =  (hpge_depth + hpge_dead_depth) / 2. + hpge_cover_depth
c      call ugsvolu('DETE','TUBE',det_cover_mate,pars,3,volume_num)
C Box for lead-tungstate
c      pars(1) =  hpge_diameter/2. + hpge_cover_depth !x half-width
c      pars(2) =  hpge_diameter / 2. + hpge_cover_depth ! y half-width
c      pars(3) =  (hpge_depth + hpge_dead_depth) / 2. + hpge_cover_depth
c      call ugsvolu('DETE','BOX ',det_cover_mate,pars,3,volume_num)
c      pars(1) = hpge_offset_x
c      pars(2) = hpge_offset_y
c      pars(3) = pars(3) + hpge_distance

C Change to slab in front of detecctor
      pars(1) =  hpge_diameter/2. + 1.0 !x half-width
      pars(2) =  hpge_diameter / 2. + 1.0 ! y half-width
      pars(3) =  hpge_cover_depth/2.
      call ugsvolu('DETE','BOX ',det_cover_mate,pars,3,volume_num)
      pars(1) = hpge_offset_x
      pars(2) = hpge_offset_y
      pars(3) = hpge_distance - pars(3) - 0.5

      call gspos('DETE',1,mother,pars(1),pars(2),pars(3),0,'ONLY')
      call gsatt('DETE', 'COLO', BLUE) ! Change color of the DETEctor
      call gsatt('DETE', 'SEEN', 1) ! DETEctor is visible


c Now fill up the DETEctor with sensitive material

      material =  int(hpge_material)
      call ugstmed (det_mate, 'DETEctor$', material,
     $     1, 0, FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, STMIN, 0, 0)
C Tube for GSO
c      pars(1) =  0.0
c      pars(2) =  hpge_diameter / 2.
c      pars(3) =  hpge_depth / 2.
c      call ugsvolu('DETI', 'TUBE', det_mate, pars, 3, volume_num)
C Box for lead-tungstate
      pars(1) =  hpge_diameter / 2.
      pars(2) =  hpge_diameter / 2.
      pars(3) =  hpge_depth / 2.
      call ugsvolu('DETI', 'BOX ', det_mate, pars, 3, volume_num)
c      call gspos('DETI', 1,'DETE', 0., 0., hpge_cover_depth / 2.,
c     $             0, 'ONLY')
      pars(1) = hpge_offset_x
      pars(2) = hpge_offset_y
      pars(3) = hpge_distance + pars(3)
      call gspos('DETI', 1,mother, pars(1), pars(2), pars(3),
     $             0, 'ONLY')
      call gsatt ('DETI', 'COLO', GREEN) ! Change color of the DETEctor
      call gsatt ('DETI', 'SEEN', 1) ! DETEctor is visible
      if (hpge_type .lt. 4.5) then
         call gsdet('EDET','DETI',1,'DETI',32,1,10,10,iset,idet)
         call gsdeth('EDET', 'DETI', 1, names, nbits, orig, fact)
      endif


c--------------------------------Electron detector------------------------------
c  add electron detector at exit from dipole 4
c     edetpos 1 = after dipole 4
c             2 = before dipole 4
c  thetab --> angle between main electron beam and scattered electron
c  detector_position: position of the detector with respect to the entrance of the 4th dipole
c  detector_offset: position of the lower edge of the detector above the main electron beam

      QWEAK   = .TRUE.
      edetpos = 2
      if (edetpos .eq. 1) detector_position =  1.0*detector_posoffset
      if (edetpos .eq. 2) detector_position = -1.0*detector_posoffset

      if (QWEAK) THEN
         ! first build a virtual box (vacuum) to contain all the planes
         sensitive_thick = detector_planenumber*detector_thickness 
     >                   + (detector_planenumber-1)*detector_planespace
         material = 31  ! --> my_vac
         call ugstmed (recoil_vac, 'recoil vac$', material,
     1                 0, 0, FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, 
     2                 STMIN, 0, 0)
         pars(1) = 0.5 * detector_width +0.1
         pars(2) = 0.5 * detector_height +0.1
         pars(3) = 0.5 * sensitive_thick +0.1

         call ugsvolu('EBOX', 'BOX ', recoil_vac, pars, 3, volume_num)
         le      = chicane_drift + detector_position 
C Position center of box, not 1st plane
         pars(1) = 0.
         pars(2) = dipole_h + le*tan(thetab) 
     >             - detector_planeoffset 
     >             + detector_offset
     >             +  0.5 * detector_height
         pars(2) = pars(2) !* cos(thetab)
         pars(3) = 0.5*chicane_mid + dipole_length
     >             + chicane_drift + detector_position
c     >             + 0.5 * sensitive_thick 
c         write(98,*)'ydet= ',pars(2),' cm'
c         write(98,*)'zdet= ',pars(3),' cm'
         call gspos ('EBOX',1,mother,pars(1),pars(2),pars(3),8,'MANY')
         call gsatt ('EBOX', 'COLO', BLUE) ! Change color of the detector
         call gsatt ('EBOX', 'SEEN', 1)     ! recoil detector is visible

         ! make 4 planes of 96 (or other number) sensitive strips
         nplane = int(detector_planenumber)
         nstrip = int(detector_stripnumber)

         material =  int(detector_material)
         call ugstmed (recoil_mate, 'recoil detector$', material,
     1                 1, 0, FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, 
     2                 STMIN, 0, 0)
         pars(1) = 0.5 * detector_width
         pars(2) = 0.5 * detector_stripwidth ! * 96/nstrip
         pars(3) = 0.5 * detector_thickness 
         call ugsvolu('ELED','BOX ',recoil_mate, pars, 3,volume_num)
         
         do iplane=1,nplane
            if (iplane .eq. 2 .or. iplane .eq. 4) then
               vshift = detector_planeoffset
            else
               vshift = 0.0
            endif
            istrip = 0
            do istrip=1,nstrip           
                   stripID = nstrip*(iplane-1.0) + istrip
  	   	   pars(1) = 0.0
                   pars(2) = -0.5*detector_height 
     >                + (istrip-0.5) * detector_stripwidth ! * 96/nstrip
     >                + (istrip-1.0) * detector_stripspace ! * 96/nstrip
     >  	      + vshift

                   pars(3) = (iplane-2.5)*detector_planespace
c                   pars(3) = -0.5*sensitive_thick+0.5*detector_thickness
c     >                + (iplane-1)*detector_thickness
c     >                + (iplane-1)*detector_planespace
                   call gspos ('ELED',stripID,'EBOX',pars(1),pars(2),
     >                pars(3),0,'ONLY')
            enddo
	 enddo

         call gsatt('ELED', 'COLO', GREEN) ! Change color of the detector
         call gsatt('ELED', 'SEEN', 1)     ! recoil detector is visible
         call gsdet('EDET', 'ELED', 1, 'ELED', 
     1                 32, 1, 10, 10, iset, idet)
         call gsdeth('EDET', 'ELED', 1, names, nbits, orig, fact)

c make gaps filled with diamond but insensitive

         pars(1) = 0.5 * detector_width
         pars(2) = 0.5 * detector_stripspace ! * 96/nstrip
         pars(3) = 0.5 * detector_thickness 
         call ugsvolu('ELES','BOX ',recoil_mate, pars, 3,volume_num)
         
         do iplane=1,nplane
            if (iplane .eq. 2 .or. iplane .eq. 4) then
               vshift = detector_planeoffset
            else
               vshift = 0.0
            endif
            istrip = 0
            ngap = nstrip-1
            do istrip=1,nstrip           
                   stripID = nstrip*(iplane-1.0) + istrip
  	   	   pars(1) = 0.0
                   pars(2) = -0.5*detector_height 
c     >                + (istrip-0.5) * detector_stripspace ! * 96/nstrip
     >                + (istrip-1.5) * detector_stripspace ! * 96/nstrip
     >                + (istrip-1.0) * detector_stripwidth ! * 96/nstrip
     >  	      + vshift
c                   pars(3) = -0.5*sensitive_thick+0.5*detector_thickness
c     >                + (iplane-1)*detector_thickness
c     >                + (iplane-1)*detector_planespace
                   pars(3) = (iplane-2.5)*detector_planespace
                   call gspos ('ELES',stripID,'EBOX',pars(1),pars(2),
     >                pars(3),0,'ONLY')
            enddo
	 enddo

         call gsatt('ELES', 'COLO', GREEN) ! Change color of the detector
         call gsatt('ELES', 'SEEN', 1)     ! recoil detector is visible
         call gsdet('EDET', 'ELES', 1, 'ELES', 
     1                 32, 1, 10, 10, iset, idet)
         call gsdeth('EDET', 'ELES', 1, names, nbits, orig, fact)
        


c xxxxxxxxxxxxxxxxxxxxxx         


      else   ! original code
         call ugstmed (recoil_mate, 'recoil detector$', SILICON_MATE,
     1                 1, 0, FIELDM, TMAXFD, STEMAX, DEEMAX, EPSIL, 
     2                 STMIN, 0, 0)
         pars(1) = detector_width / 2.
         pars(2) = detector_height / 2.
         pars(3) = detector_thickness / 2.
         call ugsvolu('EDET', 'BOX ', recoil_mate, pars, 3, volume_num)
         if (edetpos .eq. 1) then
            pars(1) = 0.
            pars(2) = pars(2) + chicane_drop + detector_offset-.4
            pars(3) = pars(3) + chicane_length/2 + 0.5
            call gspos('EDET',1,mother,pars(1),pars(2),pars(3),0,'ONLY')
         elseif (edetpos .eq. 2) then
            pars(1) = 0.
c            pars(2) = pars(2) + detector_offset + 210.*tan(thetab) + 
c     $                100.*(1-cos(thetab))/sin(thetab)
            pars(2) = pars(2) +(195.0-20.0)*tan(thetab) + 
     >           100.0*beam_E*3.3356/(dipole_field/10.0)*(1-cos(thetab))
            pars(3) = pars(3) + chicane_length/2 - dipole_length - 20. ! 20 cm upstrea of 4th dipole
            call gsrotm(10,90,0,90+9.821392245,90,9.821392245,90)
c            call gspos('EDET',1,mother,pars(1),pars(2),pars(3),6,'ONLY')
            call gspos('EDET',1,mother,pars(1),pars(2),pars(3),0,'ONLY')
         endif
         call gsdvt('EDES', 'EDET', detector_stripwidth, 2, recoil_mate,
     1               int((detector_height+0.001)/detector_stripwidth))
         call gsatt('EDET', 'COLO', GREEN) ! Change color of the detector
         call gsatt('EDET', 'SEEN', 1)     ! recoil detector is visible
         call gsdet('EDET', 'EDES', 1, 'EDES', 
     1                32, 1, 10, 10, iset, idet)
         call gsdeth('EDET', 'EDES', 1, names, nbits, orig, fact)

      endif
     
      end
