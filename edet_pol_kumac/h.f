   function h(x)

eb=1.159
me=0.000511
pi=3.141592
hbarc=0.197
lambda=532.0
thbend = 10.3
zdrift = 2.275 
mfield = 0.55264
lmag = 1.25
ldet=1.95-24.5/100
strwid=0.0002

       

* incident electron (electron gamma-factor (dimensionless))
        gamma=eb/me

* incident photon  photon energy (GeV)
        k=2*pi*hbarc/(lambda*1e6)

*outgoing photon// max recoil photon energy (GeV)

        a=1/(1+4*k*gamma/me) 
        kmax=4*a*k*gamma*gamma 
        p_beam=sqrt(eb*eb-me*me)
        r=(p_beam/(mfield))*hbarc*1.0E-15/(2*5.788381E-14*me) 
 
   
	h=(r*tan(x)-lmag)/tan(x)
c      	h2=r*(1-cos(x))

     end

