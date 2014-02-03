c Subroutine to read in a UVES_popler formatted FITS file
c opt=0 : Read in wl, fl, er, ef, co, st
c opt=1 : Read in wl, fl, er
c opt=2 : Read in wl, fl, er, ef
c opt=3 : Read in wl, fl, er, co
c opt=4 : Read in wl, fl, er, st
c opt=5 : Read in wl, fl, er, ef, st
c opt=6 : Read in wl, fl, er, co, st
      subroutine rUVESpoplerFITS(filename,swl,ewl,wl,fl,er,ef,co,st,
     :     maxn,n,opt)
      implicit none
      integer maxn
      double precision swl,ewl,w0
      double precision wl(maxn),fl(maxn),er(maxn),ef(maxn),co(maxn)
      double precision st(maxn)
      double precision crval,cdelt
      integer status,unit1,readwrite,blocksize
      integer n,opt,nax,ndat,crpix,dcflag
      integer spix,epix,uves,sindex,eindex
      integer i,j
      integer iax(9)
      logical lundef
      character*(*) filename
      character*64 errfile
      character*80 comment

      status=0
c     Get an unused Logical Unit Number to use to open the FITS file.
      call ftgiou(unit1,status)
c     Open FITS file
      readwrite=0
      status=0
      call ftopen(unit1,filename,readwrite,blocksize,status)
c     Get the image size
      status=0
      call ftgknj(unit1,'NAXIS',1,2,iax,nax,status)
c     Make sure it's a UVES_popler formatted file 
      status=0
      uves=0
      call ftgkys(unit1,'UP_ARR01',filename,comment,status)
      if(status.ne.0) then
         uves=1
      endif
c     Get data length
      ndat=iax(1)
      if (uves.eq.0.and.nax.ne.2) then
         WRITE(*,'(a,a)') 'Stopping: Number of axes must be 2 in file ',
     :        filename
         stop
      else if (uves.gt.0.and.nax.gt.2) then
         WRITE(*,'(a,a,a)') 'Stopping: Number of axes is > 2 in non-UVES
     :         file ',filename,'. I do not understand this format.'
         stop
      else if (uves.gt.0.and.nax.eq.0) then
         WRITE(*,'(a,a,a)') 'Stopping: Number of axes is 0 in non-UVES
     :         file ',filename,'. I do not understand this format.'
         stop
      endif
      if (uves.gt.0.and.(opt.eq.0.or.opt.gt.1)) then
         WRITE(*,'(a,a,a,a)') 'Stopping: Format is non-UVES for file',
     :         filename,' but you have requested more than just the',
     :         'wl, flx and err arrays. I do not know how to do that.'
         stop
      endif
c     Read the wavlength coefficients
      status=0
c     Assume DC-FLAG is zero (i.e. linear wavelength scale) unless
c     otherwise specified.
      dcflag=0
      call ftgkyj(unit1,'DC-FLAG',dcflag,comment,status)
      status=0
      call ftgkyj(unit1,'CRPIX1',crpix,comment,status)
      status=0
      call ftgkyd(unit1,'CRVAL1',crval,comment,status)
      status=0
      call ftgkyd(unit1,'CDELT1',cdelt,comment,status)
c     Determine center wavelength of first pixel
      if (crval.lt.5.0) then
         w0=10.0**crval
      else
         w0=crval
      endif
c     Determine starting and ending pixels
      if (swl.lt.1.e-8.or.ewl.lt.1.e-8) then
         spix=1
         epix=ndat
      else
         if (dcflag.eq.0) then
            spix=int(crpix+(swl-w0)/cdelt)
            if (spix.lt.1) spix=1
            epix=int(crpix+(ewl-w0)/cdelt)
            if (epix.gt.ndat) epix=ndat
         else
            spix=int(crpix+log10(swl/w0)/cdelt)
            if (spix.lt.1) spix=1
            epix=int(crpix+log10(ewl/w0)/cdelt)
            if (epix.gt.ndat) epix=ndat
         endif
      endif
      n=epix-spix+1
      if (n.lt.10) then
         WRITE(*,'(a,i8,a)') 'Stopping: ',n,' pixels to be read in'
         WRITE(*,'(a,i2)') '   but minimum allowed is ',10
         stop
      endif
      if (n.gt.maxn) then
         WRITE(*,'(a,i8,a)') 'Stopping: ',n,' pixels to be read in'
         WRITE(*,'(a,i2)') '   but maximum allowed is ',maxn
         stop
      endif
c     Fill wavelength array
      j=0
      if (dcflag.eq.0) then
         do i=spix,epix
            j=j+1
            wl(j)=w0+cdelt*(i-crpix)
         enddo
      else
         do i=spix,epix
            j=j+1
            wl(j)=w0*10.0**(cdelt*(i-crpix))
         enddo
      endif
c     Read the flux data
      lundef=.false.
      status=0
      call ftgpvd(unit1,0,spix,n,1.0,fl,lundef,status)
c     Read the error data. If the file format is not UVES and the number
c     of axes is only 1, close the current fits file and attempt to open
c     an error array file with a similar name prefix.
      if (uves.eq.0.or.(uves.eq.1.and.nax.eq.2)) then
         lundef=.false.
         status=0
         call ftgpvd(unit1,0,spix+ndat,n,-1.e16,er,lundef,status)
      else
c     Close the FITS file
         call ftclos(unit1,status)
         status=0
c     Assume the name of the error file is the same as the flux file but
c     with .sig.fits replacing .fits
         call getlen(filename,sindex,eindex)
         errfile=filename(sindex:eindex-5)//'.sig.fits'
c     Get an unused Logical Unit Number to use to open the FITS file.
        call ftgiou(unit1,status)
c     Open FITS file
         readwrite=0
         status=0
         WRITE(*,*) errfile
         call ftopen(unit1,errfile,readwrite,blocksize,status)
c     Get the image size
         status=0
         call ftgknj(unit1,'NAXIS',1,2,iax,nax,status)
c     Read the error data
         lundef=.false.
         status=0
         call ftgpvd(unit1,0,spix,n,-1.e16,er,lundef,status)
      endif
c     Read the expeted fluctuation data
      if (opt.eq.0.or.opt.eq.2.or.opt.eq.5) then
         lundef=.false.
         status=0
         call ftgpvd(unit1,0,spix+2*ndat,n,-1.e16,ef,lundef,status)
      endif
c     Read the continuum data
      if (opt.eq.0.or.opt.eq.3.or.opt.eq.6) then
         lundef=.false.
         status=0
         call ftgpvd(unit1,0,spix+3*ndat,n,1.0,co,lundef,status)
      endif
c     Read the status data
      if (opt.eq.0.or.(opt.ge.4.and.opt.le.6)) then
         lundef=.false.
         status=0
         call ftgpvd(unit1,0,spix+4*ndat,n,-20.0,st,lundef,status)
      endif
c     Close the FITS file
      call ftclos(unit1,status)

      return
      end
