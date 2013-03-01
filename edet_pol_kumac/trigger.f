      REAL FUNCTION trigger()

      include '?'
*
c      common/KCWORK/VECTOR(100)
*
       REAL VECTOR(96)
       INTEGER i,id
       REAL clustpl1(47)
       REAL clustpl2(47)
       REAL clustpl3(47)
       REAL clusttotal(47)
       REAL threshold 
       REAL rnd
       REAL VECTOR2(96),VECTOR3(96)
       
       threshold = 1.  ! number of hits required in the same zone in different planes to set of trigger
       ineff = 0.  ! Uses inefficiencies assigned to each strip which average to 80%; 0 = no, 1 = yes 

       do i=1,96
           VECTOR(i)=1
           VECTOR2(i)=1
           VECTOR3(i)=1
       enddo

       do i=1,47
          clustpl1(i) = 0.0
          clustpl2(i) = 0.0
          clustpl3(i) = 0.0
          clusttotal(i) = 0.0
       enddo

c no inefficiencies
c narayan comments
c nht_pl1: total no.of hits in plane1 in a given ?(MPS, run, corresponding to one e-gamma collition in laser-hut)
c id: the nth hit amongst the nht_pl1
c nstrip_pl1 : nth strip in plane1 (that was hit)
      if(ineff.eq.0)then   

      if(nht_pl1.gt.0)then
       do id=1,nht_pl1
           if(nstrip_pl1(id).gt.0.5.and.nstrip_pl1(id).lt.2.5)then
                clustpl1(1) = 1.
           endif
           do i=2,47
               if(nstrip_pl1(id).gt.(2.0*i-1.5)
     >           .and.nstrip_pl1(id).lt.(2.0*i+0.5))then
                    clustpl1(i-1) = 1.
                    clustpl1(i) = 1.
               endif
           enddo
           if(nstrip_pl1(id).gt.94.5.and.nstrip_pl1(id).lt.96.5)then
                clustpl1(47) = 1.
           endif
       enddo       
      endif

      if(nht_pl2.gt.0)then
       do id=1,nht_pl2
           if(nstrip_pl2(id).gt.0.5.and.nstrip_pl2(id).lt.2.5)then
                clustpl2(1) = 1.
           endif
           do i=2,47
               if(nstrip_pl2(id).gt.(2.0*i-1.5)
     >           .and.nstrip_pl2(id).lt.(2.0*i+0.5))then
                    clustpl2(i-1) = 1.
                    clustpl2(i) = 1.
               endif
           enddo
           if(nstrip_pl2(id).gt.94.5.and.nstrip_pl2(id).lt.96.5)then
                clustpl2(47) = 1.
           endif
       enddo       
      endif
  
      if(nht_pl3.gt.0)then
       do id=1,nht_pl3
           if(nstrip_pl3(id).gt.0.5.and.nstrip_pl3(id).lt.2.5)then
                clustpl3(1) = 1.
           endif
           do i=2,47
               if(nstrip_pl3(id).gt.(2.0*i-1.5)
     >           .and.nstrip_pl3(id).lt.(2.0*i+0.5))then
                    clustpl3(i-1) = 1.
                    clustpl3(i) = 1.
               endif
           enddo
           if(nstrip_pl3(id).gt.94.5.and.nstrip_pl3(id).le.96.5)then
                clustpl3(47) = 1.
           endif
       enddo
      endif

      endif 

c with inefficiencies

      if(ineff.eq.1)then   
      if(nht_pl1.gt.0)then
       do id=1,nht_pl1
           if(nstrip_pl1(id).gt.0.5.and.nstrip_pl1(id).lt.2.5)then
                rnd = rndm()
                nstr=nstrip_pl1(id)
                if(rand.lt.VECTOR(nstr))then
                     clustpl1(1) = 1.
                endif
           endif
           do i=2,47
               if(nstrip_pl1(id).gt.(2.0*i-1.5)
     >           .and.nstrip_pl1(id).lt.(2.0*i+0.5))then
                    rnd = rndm()
                    nstr=nstrip_pl1(id)
                    if(rand.lt.VECTOR(nstr))then
                        clustpl1(i-1) = 1.
                        clustpl1(i) = 1.
                    endif
               endif
           enddo
           if(nstrip_pl1(id).gt.94.5.and.nstrip_pl1(id).lt.96.5)then
                rnd = rndm()
                nstr=nstrip_pl1(id)
                if(rand.lt.VECTOR(nstr))then
                     clustpl1(47) = 1.
                endif
           endif
       enddo       
      endif

      if(nht_pl2.gt.0)then
       do id=1,nht_pl2
           if(nstrip_pl2(id).gt.0.5.and.nstrip_pl2(id).lt.2.5)then
                rnd = rndm()
                nstr=nstrip_pl2(id)
                if(rand.lt.VECTOR2(nstr))then
                     clustpl2(1) = 1.
                endif
           endif
           do i=2,47
               if(nstrip_pl2(id).gt.(2.0*i-1.5)
     >           .and.nstrip_pl2(id).lt.(2.0*i+0.5))then
                    rnd = rndm()
                    nstr=nstrip_pl2(id)
                    if(rand.lt.VECTOR2(nstr))then
                        clustpl2(i-1) = 1.
                        clustpl2(i) = 1.
                    endif
               endif
           enddo
           if(nstrip_pl2(id).gt.94.5.and.nstrip_pl2(id).lt.96.5)then
                rnd = rndm()
                nstr=nstrip_pl2(id)
                if(rand.lt.VECTOR2(nstr))then
                     clustpl2(47) = 1.
                endif
           endif
       enddo       
      endif
  
      if(nht_pl3.gt.0)then
       do id=1,nht_pl3
           if(nstrip_pl3(id).gt.0.5.and.nstrip_pl3(id).lt.2.5)then
                rnd = rndm()
                nstr=nstrip_pl3(id)
                if(rand.lt.VECTOR3(nstr))then
                     clustpl3(1) = 1.
                endif
           endif
           do i=2,47
               if(nstrip_pl3(id).gt.(2.0*i-1.5)
     >           .and.nstrip_pl3(id).lt.(2.0*i+0.5))then
                    rnd = rndm()
                    nstr=nstrip_pl3(id)
                    if(rand.lt.VECTOR3(nstr))then
                        clustpl3(i-1) = 1.
                        clustpl3(i) = 1.
                    endif
               endif
           enddo
           if(nstrip_pl3(id).gt.94.5.and.nstrip_pl3(id).lt.96.5)then
                rnd = rndm()
                nstr=nstrip_pl3(id)
                if(rand.lt.VECTOR3(nstr))then
                     clustpl3(47) = 1.
              endif
           endif
       enddo       
      endif

      endif

c  Sum up clusters
      
      do i=1,47
         clusttotal(i) = clustpl1(i) + clustpl2(i)  + clustpl3(i)
      enddo
      
      trigger = 0.0
      
      do i=1,47
         if(clusttotal(i).ge.threshold)then
            trigger = 1
         endif 
      enddo
      
      RETURN
      END

