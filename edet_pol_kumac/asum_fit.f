      function asum_fit(x) 
      DOUBLE PRECISION FITPAD(24),FITFUN
      COMMON/HCFITD/FITPAD,FITFUN
       
      real en,me,pi,hbarc,lambda,alpha,ro,k,gamma,a
      real cr1,cr2,cr3,cr,rho,a1,a2,a3,as,distance
      real maxdist,cedge,p2,numberofparam,rhotest
      real rhc1,rhc2,rhc3,rhc4
 
      common/KCWORK/VECTOR(100)
      
      dimension x(1)

      maxdist = VECTOR(1)
      cedge = VECTOR(2)
      p2 = VECTOR(3)
      numberofparam = VECTOR(4)      
      en = VECTOR(5)
      me = VECTOR(6)
      pi = VECTOR(7)
      hbarc = VECTOR(8)
      lambda = VECTOR(9)
      rhc1 = VECTOR(10)
      rhc2 = VECTOR(11)
      rhc3 = VECTOR(12)
      rhc4 = VECTOR(13)
      nstrips = VECTOR(14)
      strwid = VECTOR (15)
      
      alpha=1.0/137
      ro=alpha*hbarc/me   
  
      k=2*pi*hbarc/(lambda*1e6)
      gamma=en/me
      a=1.0/(1.0+4*k*gamma/me)
       
      if(numberofparam.eq.1) then
       distance=maxdist-(strwid*1000.)*(cedge-X(1))*p2
      endif
      
      if(numberofparam.eq.2) then
       distance=maxdist-(strwid*1000.)*(cedge-X(1))*fitpad(2)
c       distance=maxdist-(strwid*1000.)*(cedge-X(1))
      endif 

      if(numberofparam.eq.3) then
        distance=maxdist-(strwid*1000.)*(fitpad(3)-X(1))*fitpad(2)
c        distance=maxdist-(strwid*1000.)*(fitpad(3)-X(1))
      endif 

      rho=(rhc1)+(rhc2)*distance
      rho=rho+(rhc3)*distance**2
      rho=rho+(rhc4)*distance**3
 
*old   rho=-0.11426E-04+0.58985E-01*distance
*      rho=rho-0.13994E-03*distance**2
*      rho=rho+0.30729E-06*distance**3
  
      cr1=(rho*rho*(1.0-a)*(1.0-a))/(1.0-rho*(1.0-a)) 
      cr2=((1.0-rho*(1.0+a))/(1.0-rho*(1.0-a)))**2    
      cr3=2.0*pi*ro*ro*a			      
      cr=cr3*(cr1+1.0+cr2)			      

      a1=2.0*pi*ro*ro*a/cr			      
      a2=(1.0-rho*(1.0+a))			      
      a3=1.0/(1-rho*(1-a))**2			      
      as=a1*a2*(1-a3)				      
        
c      write(*,*)x(1),distance,rho,rhot,rhc1,rhc2,rhc3,rhc4
     
        asum_fit=fitpad(1)*as       
c        asum_fit=0.85*as       
       
      END                
