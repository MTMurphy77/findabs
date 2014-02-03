c     MM 9/2/01 Subroutine to search through a double precision array to
c     find wheather or not any of the entries are with a given tolerance
c     limit of the specified number, arg. The index number of the first
c     entry satisfying this criteria is returned. If no match is found
c     then an integer 1 greater than the number of array elements is
c     returned.
      subroutine srchdbletol(array,narray,arg,tol,resindex)
      implicit none
      integer narray
      double precision array(narray),arg,tol
      integer resindex

      do resindex=1,narray
         if (array(resindex).lt.arg+tol.and.array(resindex)
     :        .gt.arg-tol) return
      enddo
      resindex=resindex+1

      return
      end
