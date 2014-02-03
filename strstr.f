c     MM 16/08/06 Subroutine to return the index of the first instance
c     of the first n letters of the string, srchstr, in the input
c     string, input. If it is not found then zero is returned.

      integer function strstr(input,srchstr,n)
      implicit none
      integer n,i,sidx1,eidx1
      character*(*) input,srchstr
      
      call getlen(input,sidx1,eidx1)
      do i=sidx1,eidx1-n+1
         if (input(i:i+n).eq.srchstr(1:n)) then
            strstr=i
            return
         endif
      enddo

      strstr=0

      return
      end
