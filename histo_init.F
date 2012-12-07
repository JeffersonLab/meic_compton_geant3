	subroutine histo_init

c This routine declares the histograms and ntuples to be filled by
c the simulation
c
c PW 20-Oct-93
c  - original code
c
c Richard Jones June 19, 2003
c  - got rid of histograms in favor of ntuples
c  - one ntuple for each kind of simulation
c  - all ntuples have the same structure

	implicit none

	include 'user.inc'

	integer iostat
	integer lrec
	integer IQUEST(100)

C Use this if you need really big ntuples
	COMMON/QUEST/IQUEST
	lrec=8191
	IQUEST(10)=65000
 	call hropen (90, 'HISTOS', 'ntuples/compton.hbook','NQ', 
     >   lrec, iostat)
c 	call hropen (90, 'HISTOS', 'ntuples/compton.hbook','N', 
c     >   4096, iostat)
	if (iostat .ne. 0) then
	  write (*,*) ' Error in hropen ', iostat
	  return
	endif

	call hbset('BSIZE',8176,iostat)
	if (event_type_brem .ne. 0) then
	  call hbnt(1,'Beamline bremsstrahlung products',' ')
	  call hbname(1,'brem',Egam_dep,chform)
	endif
	if (event_type_back .ne. 0) then
	   call hbnt(2,'Compton backscatter products',' ')
	  call hbname(2,'back',Egam_dep,chform)
	endif
	if (event_type_halo .ne. 0) then
	  call hbnt(3,'Beam halo background products',' ')
	  call hbname(3,'halo',Egam_dep,chform)
	endif
	if (event_type_sync .ne. 0) then
	  call hbnt(4,'Beam synchrotron radiation products',' ')
	  call hbname(4,'sync',Egam_dep,chform)
	endif
	if (event_type_source .ne. 0) then
	  call hbnt(5,'Radioactive source products',' ')
	  call hbname(5,'source',Egam_dep,chform)
	endif
	
	end
