c MM 16/12/99 Subroutine to find the start and end
c indexes of the word in a character string.
      subroutine getlen(charstring,sindex,eindex)
      implicit none
      character*(*) charstring
      integer sindex,eindex

      sindex=1
 1    if (charstring(sindex:sindex).eq.' ') then
         sindex=sindex+1
         goto 1
      endif

      eindex=sindex
 2    if (charstring(eindex:eindex).ne.' ') then
         eindex=eindex+1
         goto 2
      endif

      eindex=eindex-1

      return
      end
