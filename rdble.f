c     MM 29/8/00 Subroutine to read in a double precision number from
c     stardard input. A prompt is output to the user and a default value
c     given. The default is passed as a character string!!
      subroutine rdble(x,prompt,default)
      implicit none
      double precision x
      integer i
      character*64 inchar
      character*(*) prompt,default
      SAVE zerochar
      character*64 zerochar
      SAVE first
      logical first
      data first/.true./

      if (first) then
         first=.false.
         do i=1,64
            zerochar(i:i)=' '
         enddo
      endif

      WRITE(*,'(a,1x,a,a,a,$)') prompt,'[',default,']: '
      inchar=zerochar
      READ(*,'(a)') inchar
      if (inchar(1:1).eq.' ') then
         read(default,*) x
      else
         read(inchar,*) x
      endif

      return
      end
