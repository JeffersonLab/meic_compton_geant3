c testcompton.f - suite of utility functions related to Compton scattering
c  -Richard Jones
c  -June 7, 2003
c
c This package is intended to be called interactively from within paw
c to produce plots and calculate statistical figure-of-merit factors.
c
c Sample paw session:
c
c   paw> call testcompton.f77(1.165,193.)
c   paw> func/plot anaPower 0 0.125
c   paw> func/plot ds0dk 0 0.125
c   paw> call reportFOM
c   paw> exit

      real function testCompton(E,lambda)
      real E,lambda
      common /comptonc/Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      double precision Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      Ebeam = E !GeV
      Ephoton = 1234.9/lambda !eV
      theta_photon = 180.
      end

      real function tcm(kf)
      real kf
      common /comptonc/Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      double precision Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      double precision Pbeam,kstar,s,costcm
      double precision gamma_cm,beta_cm
      real melectron
      parameter (melectron=0.511e-3)
      Pbeam = sqrt(Ebeam**2 - melectron**2)
      s = melectron**2 + 2*(Ephoton*1e-9)*(Ebeam+Pbeam)
      kstar = (s-melectron**2)/(2*sqrt(s))
      gamma_cm = (Ebeam + Ephoton*1e-9)/sqrt(s)
      beta_cm = (Pbeam - Ephoton*1e-9)/(Ebeam + Ephoton*1e-9)
      costcm = (kf/(gamma_cm*kstar) - 1)/beta_cm
      if (costcm.le.1) then
        tcm = acos(costcm)
      else
        tcm = 0
      endif
      end

      real function ds0dkf(kf)
      real kf
      common /comptonc/Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      double precision Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      double precision Pi
      parameter (Pi = 3.1415926)
      theta_cm = tcm(kf) * 180/Pi
      call compton_xsect (Ebeam, Ephoton, theta_photon, theta_gamma,
     1                    E_gamma, theta_cm, xsect, pol_xsect)
      ds0dkf = xsect * 10	! in b/GeV
      end

      real function Egamma(tcm)
      real tcm
      common /comptonc/Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      double precision Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      double precision xs
      xs = ds0dkf(tcm)
      Egamma = E_gamma
      end

      real function anaPower(kf)
      real kf
      common /comptonc/Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      double precision Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      double precision xs
      xs = ds0dkf(kf)
      anaPower = pol_xsect/xsect
      end

      subroutine reportfom(binGeV)
      real binGeV
      common /comptonc/Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      double precision Ebeam,Ephoton,theta_photon,theta_gamma,
     +                 E_gamma,theta_cm,xsect,pol_xsect
      double precision Xsum,Ysum,Zsum,Ydif
      real kf,w,xs
      integer i
      Xsum = 0
      Ysum = 0
      Zsum = 0
      Ydif = 0
      do i=1,999999
        kf = (i+0.5)*binGeV
        xs = ds0dkf(kf)
        if (theta_cm.eq.0) goto 1
        ap = anaPower(kf)
        w = ap
        Xsum = Xsum + xs
        Ysum = Ysum + xs*w
        Zsum = Zsum + xs*w*w
        Ydif = Ydif + xs*ap*w
      enddo
    1 continue
      print *, 'figure of merit (ideal) is',Xsum*Zsum/Ydif**2
      print *, '<A> in this case is',Ydif/Ysum
      Xsum = 0
      Ysum = 0
      Zsum = 0
      Ydif = 0
      do i=1,999999
        kf = (i+0.5)*binGeV
        xs = ds0dkf(kf)
        if (theta_cm.eq.0) goto 2
        ap = anaPower(kf)
        w = 1
        Xsum = Xsum + xs
        Ysum = Ysum + xs*w
        Zsum = Zsum + xs*w*w
        Ydif = Ydif + xs*ap*w
      enddo
    2 continue
      print *, 'figure of merit (w=1) is',Xsum*Zsum/Ydif**2
      print *, '<A> in this case is',Ydif/Ysum
      Xsum = 0
      Ysum = 0
      Zsum = 0
      Ydif = 0
      do i=1,999999
        kf = (i+0.5)*binGeV
        xs = ds0dkf(kf)
        if (theta_cm.eq.0) goto 3
        ap = anaPower(kf)
        w = kf
        Xsum = Xsum + xs
        Ysum = Ysum + xs*w
        Zsum = Zsum + xs*w*w
        Ydif = Ydif + xs*ap*w
      enddo
    3 continue
      print *, 'figure of merit (w=E) is',Xsum*Zsum/Ydif**2
      print *, '<A> in this case is',Ydif/Ysum
      end
