	subroutine gufld (location, field)

c This routine returns the magnetic field as a 3-vector evaluated at a
c given point
c Inputs:
c	location(3): coordinates of point in MRS (cm)
c Outputs:
c	field(3): components of field (kG)
c
c PW 20-Oct-93
c  - original code
c
c Richard Jones June 7, 2003
c  - replaced the use of fortran "record" statement with standard fortran-77,
c    and changed column alignment to conform to the fortran source form.
c  - replaced trig intrinsics based on degrees (sind,cosd,tand..) with
c    local versions because g77 does not support them
c
c Patricia Solvignon April 8, 2010
c  - add option to use field map instead of a single value for the dipole field


	implicit none

	include 'user.inc'

*	structure /point/
*	real x, y, z
*	end structure

*	record /point/ location, field

        integer k
	real location(3),field(3)
        real posdip(3)
        real xmap_limit,zmap_limit
        real xphy_limit,yphy_limit
        real xdip(4),ydip(4),zdip(4)
        logical fieldmap       ! true  --> use field mad values
                               ! false --> use one value for the field everywhere in the dipole
c
c-----------------
c
        fieldmap=.false.
c        fieldmap=.true.
        
	field(1) = 0				! initially no field
	field(2) = 0
	field(3) = 0

        xmap_limit = 11.0
        zmap_limit = 78.0 

        xphy_limit = 22.0/2.0
        yphy_limit = dipole_height

        xdip(1)  = 0.0
        xdip(2)  = 0.0
        xdip(3)  = 0.0
        xdip(4)  = 0.0

        ydip(1)  = chicane_drop
        ydip(2)  = 0.0
        ydip(3)  = 0.0
        ydip(4)  = chicane_drop

        zdip(1)  = -chicane_length/2.0 + dipole_length/2.0
        zdip(2)  = -chicane_mid/2.0 - dipole_length/2.0
        zdip(3)  = chicane_mid/2.0 + dipole_length/2.0
        zdip(4)  = chicane_length/2.0 - dipole_length/2.0


        if (fieldmap) then
           open(unit=62,file='dipole_fieldmap.dat',status='old')
           posdip(1) = location(1)
           posdip(2) = location(2)

           k=0
 100	   k=k+1
           if (location(3).ge.(zdip(k)-zmap_limit).and.
     &         location(3).le.(zdip(k)+zmap_limit)) then
               if (posdip(1).lt.(xdip(k)-xphy_limit).or.
     &             posdip(1).gt.(xdip(k)+xphy_limit).or.
     &             posdip(2).lt.(ydip(k)-yphy_limit).or.
     &             posdip(2).gt.(ydip(k)+yphy_limit)) then

                  field(1) = 0.0
               else      
                  posdip(3) = location(3) - zdip(k)
                 if (posdip(3).gt.0.0) posdip(3) = -posdip(3)
                 call get_field(posdip,field)
                 field = (dipole_current/110.0)*field
                 if (k.eq.2.or.k.eq.3) field(1) = -1.0*field(1)
               endif
           else
              if (k.lt.4) then
                 goto 100
              else 
                 field(1) = 0.0
                 goto 101
              endif
           endif

 101	   continue

           goto 11
        else
           goto 10
        endif


 10	continue
        if ((location(3).lt.-chicane_length/2.0 + dipole_length+0.00001)
     1                                 .or.
     2     (location(3).gt.chicane_length/2.0 - dipole_length-0.00001))
     3  then
	   field(1) = dipole_field
	else
	   field(1) = -dipole_field
        endif

 11	continue

c	write(6,*) 'Field:', location(3), field(1)

	end

c----------
        subroutine get_field(posdip,field)
        implicit none
        integer i,j,ix,jz,ifound
        real posdip(3),field(3)
        real f(45,157)
        real A,B,C,D
        real field_tmp(157)
        character line*357
        real xdipole(45),zdipole(157)
        data xdipole/-11.0,-10.5,-10.0, -9.5, -9.0, -8.5, -8.0, -7.5,
     &                -7.0, -6.5, -6.0, -5.5, -5.0, -4.5, -4.0, -3.5, 
     &                -3.0, -2.5, -2.0, -1.5, -1.0, -0.5,  0.0,  0.5,  
     &                 1.0,  1.5,  2.0,  2.5,  3.0,  3.5,  4.0,  4.5,  
     &                 5.0,  5.5,  6.0,  6.5,  7.0,  7.5,  8.0,  8.5,  
     &                 9.0,  9.5, 10.0, 10.5, 11.0/
        data zdipole/-78.0,-77.5,-77.0,-76.5,-76.0,-75.5,-75.0,-74.5,
     &               -74.0,-73.5,-73.0,-72.5,-72.0,-71.5,-71.0,-70.5,
     &               -70.0,-69.5,-69.0,-68.5,-68.0,-67.5,-67.0,-66.5,
     &               -66.0,-65.5,-65.0,-64.5,-64.0,-63.5,-63.0,-62.5,
     &               -62.0,-61.5,-61.0,-60.5,-60.0,-59.5,-59.0,-58.5,
     &               -58.0,-57.5,-57.0,-56.5,-56.0,-55.5,-55.0,-54.5,
     &               -54.0,-53.5,-53.0,-52.5,-52.0,-51.5,-51.0,-50.5,
     &               -50.0,-49.5,-49.0,-48.5,-48.0,-47.5,-47.0,-46.5,
     &               -46.0,-45.5,-45.0,-44.5,-44.0,-43.5,-43.0,-42.5,
     &               -42.0,-41.5,-41.0,-40.5,-40.0,-39.5,-39.0,-38.5,
     &               -38.0,-37.5,-37.0,-36.5,-36.0,-35.5,-35.0,-34.5,
     &               -34.0,-33.5,-33.0,-32.5,-32.0,-31.5,-31.0,-30.5,
     &               -30.0,-29.5,-29.0,-28.5,-28.0,-27.5,-27.0,-26.5,
     &               -26.0,-25.5,-25.0,-24.5,-24.0,-23.5,-23.0,-22.5,
     &               -22.0,-21.5,-21.0,-20.5,-20.0,-19.5,-19.0,-18.5,
     &               -18.0,-17.5,-17.0,-16.5,-16.0,-15.5,-15.0,-14.5,
     &               -14.0,-13.5,-13.0,-12.5,-12.0,-11.5,-11.0,-10.5,
     &               -10.0, -9.5, -9.0, -8.5, -8.0, -7.5, -7.0, -6.5,
     &                -6.0, -5.5, -5.0, -4.5, -4.0, -3.5, -3.0, -2.5,
     &                -2.0, -1.5, -1.0, -0.5,  0.0/


        open(unit=62,file='dipole_fieldmap.dat',status='old')

c       Find corresponding lines in dipole_fieldmap.dat

        do i=1,45
           if (posdip(1).le.xdipole(i).and.
     &         posdip(1).gt.xdipole(i-1)) then
              ix = i
              goto 901              
           endif
        enddo

 901	continue

        do j=1,157
           read(62,*)f(1,j), f(2,j), f(3,j), f(4,j), f(5,j), f(6,j),
     &               f(7,j), f(8,j), f(9,j),f(10,j),f(11,j),f(12,j),
     &               f(13,j),f(14,j),f(15,j),f(16,j),f(17,j),f(18,j),
     &               f(19,j),f(20,j),f(21,j),f(22,j),f(23,j),f(24,j),
     &               f(25,j),f(26,j),f(27,j),f(28,j),f(29,j),f(30,j),
     &               f(31,j),f(32,j),f(33,j),f(34,j),f(35,j),f(36,j),
     &               f(37,j),f(38,j),f(39,j),f(40,j),f(41,j),f(42,j),
     &               f(43,j),f(44,j),f(45,j)
           if (posdip(3).le.zdipole(j).and.
     &         posdip(3).gt.zdipole(j-1)) then 
              jz = j
              goto 902
           endif
        enddo

 902	continue

        ! interpolate wrt x  (simple linear interpolation)
        A = (xdipole(ix) - posdip(1))  / (xdipole(ix)-xdipole(ix-1))
        B = (posdip(1) - xdipole(ix-1))/ (xdipole(ix)-xdipole(ix-1))
        field_tmp(jz-1) = A*f(ix-1,jz-1) + B*f(ix,jz-1)
        field_tmp(jz)   = A*f(ix-1,jz)   + B*f(ix,jz)

        ! interpolate wrt z  (simple linear interpolation)
        C = (zdipole(jz) - posdip(3))  / (zdipole(jz)-zdipole(jz-1))
        D = (posdip(3) - zdipole(jz-1))/ (zdipole(jz)-zdipole(jz-1))
        field(1) = C*field_tmp(jz-1) + D*field_tmp(jz)

        field(1) = field(1)*1E-3
        field(2) = 0.0
        field(3) = 0.0

        goto 1000 ! jump the screen printing of the following control values 
        write(6,*)'************************************************'
        write(6,*)'interp in x:', 
     &              xdipole(ix-1),'<',posdip(1),'<',xdipole(ix)
        write(6,*)'field at (jz-1):', 
     &              f(ix-1,jz-1),field_tmp(jz-1),f(ix,jz-1)
        write(6,*)'field at (jz):',   
     &              f(ix-1,jz), field_tmp(jz), f(ix,jz)
        write(6,*)'interp coeff A & B:',A,B
        write(6,*)'-------'
        write(6,*)'interp in z:', 
     &              zdipole(jz-1),'<',posdip(3),'<',zdipole(jz)
        write(6,*)'field at:',  
     &              field_tmp(jz-1),field(1)*1E3,field_tmp(jz)
        write(6,*)'interp coeff C & D:',C,D

 1000	continue
        close(62)
        return
        end
