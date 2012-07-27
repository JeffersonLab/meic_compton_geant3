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
       REAL clustpl4(47)
       REAL clusttotal(47)
       REAL threshold 
       REAL rnd
       REAL VECTOR2(96),VECTOR3(96),VECTOR4(96)
       

c        write(6,*) 'cheese', VECTOR

       threshold = 4.  ! number of hits required in the same zone in different planes to set of trigger

       ineff = 0.  ! Uses inefficiencies assigned to each strip which average to 80%; 0 = no, 1 = yes 

c Jumble up inefficiencies

c       do i=40,96
c           VECTOR2(i-39) =  VECTOR(i)
c       enddo

c       do i=1,39
c           VECTOR2(i+96-39) =  VECTOR(i)
c       enddo

c       do i=65,96
c           VECTOR3(i-64) =  VECTOR2(i)
c       enddo

c       do i=1,64
c           VECTOR3(i+96-64) =  VECTOR2(i)
c       enddo

c       do i=27,96
c           VECTOR4(i-26) =  VECTOR3(i)
c       enddo

c       do i=1,26
c           VECTOR4(i+96-26) =  VECTOR3(i)
c       enddo

       do i=1,96
           VECTOR(i)=1
           VECTOR2(i)=1
           VECTOR3(i)=1
           VECTOR4(i)=1
       enddo

       do i=1,48
           id = 2 * i
           VECTOR4(id)=0.0
       enddo


       do i=1,47
          clustpl1(i) = 0.0
          clustpl2(i) = 0.0
          clustpl3(i) = 0.0
          clustpl4(i) = 0.0
          clusttotal(i) = 0.0
       enddo


c no inefficiencies

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



      if(nht_pl4.gt.0)then
       do id=1,nht_pl4
           if(nstrip_pl4(id).gt.0.5.and.nstrip_pl4(id).lt.2.5)then
                clustpl4(1) = 1.
           endif
           do i=2,47
               if(nstrip_pl4(id).gt.(2.0*i-1.5)
     >           .and.nstrip_pl4(id).lt.(2.0*i+0.5))then
                    clustpl4(i-1) = 1.
                    clustpl4(i) = 1.
               endif
           enddo
           if(nstrip_pl4(id).gt.94.5.and.nstrip_pl4(id).le.96.5)then
                clustpl4(47) = 1.
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



      if(nht_pl4.gt.0)then
       do id=1,nht_pl4
           if(nstrip_pl4(id).gt.0.5.and.nstrip_pl4(id).lt.2.5)then
                rnd = rndm()
                nstr=nstrip_pl4(id)
                if(rand.lt.VECTOR4(nstr))then
                     clustpl4(1) = 1.
                endif
           endif
           do i=2,47
               if(nstrip_pl4(id).gt.(2.0*i-1.5)
     >           .and.nstrip_pl4(id).lt.(2.0*i+0.5))then
                    rnd = rndm()
                    nstr=nstrip_pl4(id)
                    if(rand.lt.VECTOR4(nstr))then
                        clustpl4(i-1) = 1.
                        clustpl4(i) = 1.
                    endif
               endif
           enddo
           if(nstrip_pl4(id).gt.94.5.and.nstrip_pl4(id).lt.96.5)then
                rnd = rndm()
                nstr=nstrip_pl4(id)
                if(rand.lt.VECTOR4(nstr))then
                     clustpl4(47) = 1.
                endif
           endif
       enddo       
      endif

      

      endif







c  Sum up clusters

       do i=1,47
         clusttotal(i) = clustpl1(i) + clustpl2(i)
     >                   + clustpl3(i) + clustpl4(i)
       enddo

       trigger = 0.0
       
       do i=1,47
         if(clusttotal(i).ge.threshold)then
            trigger = 1
         endif 
       enddo


 




       RETURN
       END

