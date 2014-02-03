c     MM 6/2/01 Subroutine to convert the ion, level and rest wavelength
c     information for a transition into a transition name (character
c     string with known length).
c
c     opt=0: Include only the ion, level and integer wavelength in name string.
c     opt=1: Same as opt=0 but also includes "f=[osc. strength]" in the name string.
      subroutine transname(ion,level,lambda0,f,name,length,opt)
      implicit none
      include 'charlen_inc.f'
      include 'ratomdat_inc.f'
      double precision lambda0,f
      integer length
      integer sindex,eindex,i,opt
      character*(ionlen) ion,level
      character*(namelen) name
      SAVE zerochar
      character*(namelen) zerochar
      SAVE first
      logical first
      data first/.true./
      
      if (first) then
         first=.false.
         do i=1,namelen
            zerochar(i:i)=' '
         enddo
      endif
      
      name=zerochar

      call getlen(ion,sindex,eindex)
      length=eindex-sindex+1
      name(1:length)=ion(sindex:eindex)

      call getlen(level,sindex,eindex)
      name(length+1:length+eindex-sindex+1)=level(sindex:eindex)
      length=length+eindex-sindex+1

      if (lambda0.ge.1000.d0.and.lambda0.lt.10000.d0) then
         write(name(length+1:length+4),'(i4)') int(lambda0)
         length=length+4
      elseif (lambda0.lt.1000.d0) then
         write(name(length+1:length+3),'(i3)') int(lambda0)
         length=length+3
      endif

      if (opt.eq.1) then
         write(name(length+1:),'(a,f8.6)') " f=",f
         length=length+10
      endif
      
      return
      end
