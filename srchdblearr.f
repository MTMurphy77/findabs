c     MM 6/2/01 Subroutine to search an ordered double precision
c     array. The result is the integer array index of the last entry
c     which has a value less than or equal to the third argument. The
c     number of array entries is returned if this condition is not
c     satisfied (i.e. all entries are less than value)
      subroutine srchdblearr(array,n,value,result)
      implicit none
      integer n
      double precision array(n),value
      integer result
      integer i

      i=1
      do while (value.gt.array(i).and.n.gt.i)
         i=i+1
      enddo

      if (n.ge.i) result=i-1
      if (i.eq.1) result=n

      return
      end
