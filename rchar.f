c     MM 30/8/00 Subroutine to read in a character string from standard
c     input. The calling program must pass a user prompt and a default
c     string if <cr> is hit by the user.
      subroutine rchar(string,prompt,default)
      implicit none
      integer i
      character*(*) string,prompt,default
      character*64 inchar
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

      inchar=zerochar
      WRITE(*,'(a,a,a,a,$)') prompt,' [',default,']: '
      READ(*,'(a)') inchar
      if (inchar(1:1).eq.' ') then
         read(default,'(a)') string
      else
         read(inchar,'(a)') string
      endif

      return
      end
