c     MM 9/2/01 Subroutine to round a double precision number off to a
c     given number of significant figures and return it as a character
c     string of specified length (including the decimal point). The
c     number appears in the string in decimal format. If the argument
c     were written as an exponent with the rounding applied then the
c     exponent must be less than 62.
      subroutine rounddble(arg,nsig,result,length)
      implicit none
      include 'charlen_inc.f'
      double precision arg
      double precision round
      integer nsig,length
      integer i,sindex,eindex,ndec,nzero,resindex
      character*(namelen) result
      SAVE zerochar
      character*202 tempchar,zerochar
      logical minus
      SAVE first
      logical first
      data first/.true./
      
      if (first) then
         first=.false.
         do i=1,200
            zerochar(i:i)=' '
         enddo
      endif
      tempchar=zerochar
      result=zerochar(1:namelen)
      write(tempchar(1:202),'(f202.100)') arg

      call getlen(tempchar,sindex,eindex)
      if (index(tempchar(sindex:eindex),'-').eq.1) sindex=sindex+1
      ndec=102-sindex
      if (ndec.eq.1.and.tempchar(sindex:sindex).eq.'0') then
         nzero=0
         do while (tempchar(sindex+2+nzero:sindex+2+nzero).eq.'0')
            nzero=nzero+1
         enddo
         round=dnint(arg*10.d0**(nsig+nzero))
         if (dabs(round).eq.10.d0**(nsig)) nzero=nzero-1
         write(tempchar(1:202),'(f202.100)') round
         call getlen(tempchar,sindex,eindex)
         resindex=1
         if (index(tempchar(sindex:eindex),'-').eq.1) then
            sindex=sindex+1
            resindex=2
            result(1:1)='-'
         endif   
         if (nzero.gt.-1) then
            result(resindex:resindex+1)='0.'
            resindex=resindex+2
            do i=1,nzero
               result(resindex+i-1:resindex+i-1)='0'
            enddo
            resindex=resindex+nzero
         else
            result(resindex:resindex)=tempchar(sindex:sindex)
            result(resindex+1:resindex+1)='.'
            resindex=resindex+2
            sindex=sindex+1
            nsig=nsig-1
         endif
         do i=1,nsig
            result(resindex+i-1:resindex+i-1)=
     :           tempchar(sindex+i-1:sindex+i-1)
         enddo
         length=resindex+nsig-1
      elseif (ndec.lt.nsig) then
         round=dnint(arg*10.d0**(nsig-ndec))
         if (dabs(round).eq.10.d0**(nsig)) ndec=ndec+1
         write(tempchar(1:202),'(f202.100)') round
         call getlen(tempchar,sindex,eindex)
         resindex=1
         if (index(tempchar(sindex:eindex),'-').eq.1) then
            sindex=sindex+1
            resindex=2
            result(1:1)='-'
         endif   
         do i=1,ndec
            result(resindex+i-1:resindex+i-1)=
     :           tempchar(sindex+i-1:sindex+i-1)
         enddo
         resindex=resindex+ndec
         sindex=sindex+ndec
         result(resindex:resindex)='.'
         resindex=resindex+1
         do i=1,nsig-ndec
            result(resindex+i-1:resindex+i-1)=
     :           tempchar(sindex+i-1:sindex+i-1)
         enddo
         length=resindex+nsig-ndec-1
         if (nsig-ndec.eq.0) length=length-1
      else
         round=dnint(arg/(10.d0**(ndec-nsig)))
         if (dabs(round).eq.10.d0**(nsig)) ndec=ndec+1
         write(tempchar(1:202),'(f202.100)') round
         call getlen(tempchar,sindex,eindex)
         resindex=1
         if (index(tempchar(sindex:eindex),'-').eq.1) then
            sindex=sindex+1
            resindex=2
            result(1:1)='-'
         endif
         do i=1,nsig
            result(resindex+i-1:resindex+i-1)=
     :           tempchar(sindex+i-1:sindex+i-1)
         enddo
         resindex=resindex+nsig
         sindex=sindex+nsig
         do i=1,ndec-nsig
            result(resindex+i-1:resindex+i-1)='0'
         enddo
         length=resindex+ndec-nsig-1
         result(length+1:length+1)='.'
      endif


      return
      end
