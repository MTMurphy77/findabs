c     MM 5/2/01 Subroutine to take a character string and separate it
c     into several character variables (contained in a character array
c     on output).
      subroutine sepchar(charline,sepline)
      implicit none
      include 'charlen_inc.f'
      integer i,sindex,eindex,oeindex
      character*(longchar) charline,sepline(*)
      SAVE zerochar
      character*(longchar) zerochar
      SAVE first
      logical first
      data first/.true./
      
      if (first) then
         first=.false.
         do i=1,longchar
            zerochar(i:i)=' '
         enddo
      endif
      
      i=0
      eindex=0
      oeindex=0
 1    i=i+1
      oeindex=oeindex+eindex
      call getlen(charline(oeindex+1:longchar),sindex,eindex)
      sepline(i)=zerochar
      sepline(i)(1:eindex-sindex+1)=charline(oeindex+sindex:
     :     oeindex+eindex)
      if (oeindex+eindex.ge.longchar) return
      goto 1

      end
