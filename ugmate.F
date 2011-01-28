      subroutine ugmate

c This routine reads from the file materials.database and adds materials
c into the GEANT materials tables
c
c July-1994, Pat Welch, Oregon State University
c
c Richard Jones June 7, 2003
c  - replaced literal (and invisible) tabs in quoted strings with \t.

      implicit none

      include 'params.inc'
      integer MAX_ELEMENTS
      parameter (MAX_ELEMENTS = 10)

      character*32 element_name(MAX_ELEMENTS) ! names of the elements
      real A(MAX_ELEMENTS)      ! atomic weights in amu
      real Z(MAX_ELEMENTS)      ! charge in |e|
      real W(MAX_ELEMENTS)      ! fractional weights or numbers
      character*132 line        ! input line buffer
      character*32 name		! material name
      integer nelements		! number of elements
      real density              ! density of material in g/cm^3
      integer material		! material number
      integer line_length       ! length of line read in
      integer name_end		! past end of name field
      integer i			! index variable
      integer element_count     ! element index variable

      integer lnblnk            ! returns length of non-blank string

      open (unit=61, file='materials.database',
     >     err=1000, status='old')

 10   continue                  ! loop over the file
      read (61, '(a)', end=2000) line
      call ugmate_lineparse (line, name, name_end, line_length)
      if (line_length .eq. 0) go to 10 ! blank or comment line
      read (line(name_end:line_length), *, err=500) 
     >     nelements, density, material

      if (abs(nelements) .ge. MAX_ELEMENTS) then
         write (*,*) ' Maximum number of elements exceeded'
         write (*,*) nelements, MAX_ELEMENTS
         go to 2000
      endif

      element_count = 0
 20   continue                  ! loop over secondary cards
      read (61, '(a)', end=600) line
      call ugmate_lineparse (line, element_name(element_count+1), 
     >     name_end, line_length)
      print*,line_length
      if (line_length .eq. 0) go to 20
      element_count = element_count + 1	! element we're on
      print*,element_count
      read(line(name_end:line_length),*,err=700) 
     >     A(element_count), Z(element_count), w(element_count)
      print*,A(element_count), Z(element_count), w(element_count)
      if (element_count .lt. abs(nelements)) go to 20 ! loop over elements

      if (nelements .eq. 1) then ! a single element material
         call gsmate(material, name // '$', A(1), Z(1), density,
     >        W(1), 23.2, 0, 0)
      else
         call gsmixt (material, name // '$', A, Z, density, 
     >        nelements, W)
      endif

      go to 10

 500  continue			! error in first line read
      write (*,*) 'format error in reading first line of materials'
      write (*,*) line(1:line_length)
      go to 2000

 600  write (*,*) ' EOF in material ', name(1:lnblnk(name))
      go to 2000

 700  continue			! error in first line read
      write (*,*)
     c     ' format error in reading secondary line of materials'
      write (*,*) line(1:line_length)
      go to 2000

 2000 continue
      close (unit=61)           ! close the open file

 1000 continue			! error on opening the file
      end

      subroutine ugmate_lineparse(line, name, name_end, line_length)
c
c If line_length is zero on return then a blank line
c
      implicit none

      character*(*) line        ! line to parse
      character*(*) name        ! name field
      integer name_end          ! index past name
      integer line_length       ! length of line
      
      integer name_start        ! beginning of name
      integer i                 ! index variable
      
      integer lnblnk            ! non-blank length of string
      
      line_length = lnblnk(line) ! get length of the line
      if (line_length .eq. 0) return ! blank line
      do i = 1, line_length     ! strip out comments
         if (line(i:i) .eq. '*') then
	    line_length = i - 1
	    go to 10
         endif
      enddo
 10   continue
      if (line_length .eq. 0) return ! if a comment line
      line_length = lnblnk(line(1:line_length))
      if (line_length .eq. 0) return ! if a comment line
      do i = 1, line_length
         if ((line(i:i) .ne. ' ') .and. 
     >        (line(i:i) .ne. '\t')) then ! if not a blank or tab
	    name_start = i
	    go to 20
         endif
      enddo
      line_length = 0
      return

 20   continue
      do i = name_start, line_length ! look of length of name
         if ((line(i:i) .eq. ' ') .or. 
     >        (line(i:i) .eq. '\t	')) then ! if a blank or tab
	    name = line(name_start:i-1) ! get a copy of the name
	    name_end = i        ! past end of name
	    go to 30
         endif
      enddo
      line_length = 0
      return

 30   continue

      end
