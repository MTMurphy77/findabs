c     MM 8/2/01 Subroutine to find all current redshift subdirectories
c     in the current directory. The result is an array of double
c     precision redshifts corresponding to the directory names.
c
c     Requires: /bin/ls /bin/rm /usr/bin/awk
c
      subroutine findolddirs(zdirs,nzdirs)
      implicit none
      include 'findabs_inc.f'
      include 'charlen_inc.f'
      double precision zdirs(maxzdirs)
      integer nzdirs
      character*(namelen) dirnm
      logical number

      call system(
     : '/bin/ls -lF | /usr/bin/awk '//char(39)//'{print $9}'//
     : char(39)//' | grep "/" > .findolddirs_temp.txt')
      OPEN(unit=1,file='.findolddirs_temp.txt',status='old')
      nzdirs=0
 1    READ(1,'(a)',end=2) dirnm
      if (number(dirnm)) then
         nzdirs=nzdirs+1
         read(dirnm,*) zdirs(nzdirs)
      endif
      goto 1
      
 2    close(1)
      call system('/bin/rm .findolddirs_temp.txt')

      return
      end
