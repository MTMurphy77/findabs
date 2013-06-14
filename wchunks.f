c     MM 12/2/01 Subroutine to write out the chunks of data in the given
c     directory with a given absorption redshift.
      subroutine wchunks(wl_obs,flx,sig,npix,ion,level,lambda0,
     :     zabs,dname,dlen,group,ntrans)
      implicit none
      include 'findabs_inc.f'
      include 'charlen_inc.f'
      include 'constants_inc.f'
      include 'ratomdat_inc.f'
      double precision wl_obs(maxnpix)
      double precision flx(maxnpix)
      double precision sig(maxnpix)
      double precision lambda0(maxngroups,maxtrpergr)
      double precision zabs
      integer npix,group,ntrans
      integer flen,i,j,dlen,spix,epix
      character*(ionlen) ion(maxngroups,maxtrpergr)
      character*(ionlen) level(maxngroups,maxtrpergr)
      character*(namelen) dname
      character*(namelen) fname
      
      do i=1,ntrans
         call transname(ion(group,i),level(group,i),lambda0(group,i),
     :        0.0,fname,flen,0)
         OPEN(unit=1,file=dname(1:dlen)//'/'//fname(1:flen)//'.dat',
     :        status='old',err=1)
         WRITE(*,'(5(a))') 'Overwriting existing file ',
     :        dname(1:dlen),'/',fname(1:flen),'.dat'
         goto 2

 1       OPEN(unit=1,file=dname(1:dlen)//'/'//fname(1:flen)//'.dat',
     :        status='new',err=1)

 2       OPEN(unit=2,file=dname(1:dlen)//'/'//fname(1:flen)//
     :        '.sig.dat',status='unknown')
         OPEN(unit=3,file=dname(1:dlen)//'/'//fname(1:flen),
     :        status='unknown')
         call srchdblearr(wl_obs,npix,lambda0(group,i)*
     :        (1.d0+zabs)*(1.d0-chunkspan*1.d3/c),spix)
         call srchdblearr(wl_obs(spix),npix-spix+1,lambda0(group,i)*
     :        (1.d0+zabs)*(1.d0+chunkspan*1.d3/c),epix)
         epix=epix+spix
         if (spix.eq.npix.and.epix.eq.npix) then
            WRITE(*,'(a)') 'WARNING: no data available for ',
     :           fname(1:flen)
         else
            do j=spix,epix
               WRITE(1,'(f15.9,3x,f15.9)') wl_obs(j),flx(j)
               WRITE(2,'(f15.9,3x,g18.9)') wl_obs(j),sig(j)
               WRITE(3,'(f15.9,3x,f15.9,3x,g18.9)') wl_obs(j),flx(j),
     :              sig(j)
            enddo
         endif
         close(1)
         close(2)
         close(3)
      enddo

      return
      end
