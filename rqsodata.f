c     MM 31/01/01 Subroutine to read in the QSO data (wlen, data and sig
c     arrays) from known data file names
      subroutine rqsodata(wl_obs,flx,sig,npix)
      implicit none
      include 'findabs_inc.f'
      include 'charlen_inc.f'
      double precision wl_obs(maxnpix),flx(maxnpix),sig(maxnpix)
      double precision darray(maxnpix)
      double precision swl,ewl
      double precision dummy
      integer npix
      integer i,sindex,eindex,sindex2,eindex2
      integer strstr
      character*(namelen) datnm,signm,zerochar

      do i=1,namelen
         zerochar(i:i)=' '
      enddo
      
 1    datnm=zerochar
      call rchar(datnm,'QSO data file?','qso.dat')
      call getlen(datnm,sindex,eindex)
      if (strstr(datnm,'.fits',5).ne.0) then
         swl=0.d0
         ewl=0.d0
         call rUVESpoplerFITS(datnm,swl,ewl,wl_obs,flx,
     :        sig,darray,darray,darray,maxnpix,npix,1)
         return 
      endif
      OPEN(unit=1,file=datnm,status='old',err=2)
      goto 3
          
 2    WRITE(*,'(2(a))') 'WARNING: Could not open file ',
     :     datnm(sindex:eindex)
      goto 1

 3    i=0
 4    i=i+1
      READ(1,*,err=5,end=6) wl_obs(i),flx(i)
      goto 4
 5    call getlen(datnm,sindex,eindex)
      WRITE(*,'(a,i6,a,a)') 'STOPPING: Error on line ',i,
     :     ' of file ',datnm(sindex:eindex)
      stop
 6    close(1)
      npix=i-1
      
 7    signm=zerochar
      call getlen(datnm,sindex,eindex)
      signm=datnm(sindex:eindex-4)//'.sig.dat'
      OPEN(unit=1,file=signm,status='old',err=8)
      WRITE(*,'(2(a))') 'INFO: Using sigma data from ',
     :     signm(1:eindex+4)
      goto 10

 8    call rchar(signm,'Sigma data file?',
     :     'sig.dat')
      OPEN(unit=1,file=signm,status='old',err=9)
      goto 10
      
 9    call getlen(signm,sindex,eindex)
      WRITE(*,'(2(a))') 'WARNING: Could not open file ',
     :     signm(sindex:eindex)
      goto 8

 10   i=0
 11   i=i+1
      READ(1,*,err=12,end=13) dummy,sig(i)
      if (dabs(dummy-wl_obs(i)).gt.wlentol) then
         call getlen(datnm,sindex,eindex)
         call getlen(signm,sindex2,eindex2)
         WRITE(*,'(6(a),i6)') 'STOPPING: Descrepency between ',
     :        'wavelength scales of ',datnm(sindex:eindex),
     :        ' and ',signm(sindex2:eindex2),' on line ',i
         stop
      endif
      goto 11
 12   call getlen(signm,sindex,eindex)
      WRITE(*,'(a,i6,a,a)') 'STOPPING: Error on line ',i,
     :     ' of file ',signm(sindex:eindex)
      stop
 13   close(1)
      
      if (i-1.ne.npix) then
         call getlen(datnm,sindex,eindex)
         call getlen(signm,sindex2,eindex2)
         WRITE(*,'(5(a))') 'STOPPING: Descrepency between ',
     :        'number of pixels in',datnm(sindex:eindex),
     :        ' and ',signm(sindex2:eindex2)
         stop
      endif
      
      return
      end
