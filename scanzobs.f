c     MM 15/2/01 Subroutine to scan the spectrum for obscure transitions
c     with known redshifts. A very weak selection criteria for
c     absorption is appplied by asking that a 5 sigma depletion in flux
c     is recorded over at least a small (user-specified) number of
c     pixels over a larger (user-specified) velocity range either side
c     of the fiducial redshift.
      subroutine scanzobs(wl_obs,flx,sig,npix,ngroups,groupnm,ntrans,
     :     lambda0,f,ion,level,zaccept,nzaccept,ion_acc,lev_acc,
     :     lam0_acc,f_acc,dir_acc)
      implicit none
      include 'findabs_inc.f'
      include 'charlen_inc.f'
      include 'ratomdat_inc.f'
      include 'constants_inc.f'
      double precision lambda0(maxngroups,maxtrpergr)
      double precision f(maxngroups,maxtrpergr)
      double precision wl_obs(maxnpix)
      double precision flx(maxnpix)
      double precision sig(maxnpix)
      double precision zaccept(maxzdirs)
      double precision lam0_acc(maxzdirs)
      double precision f_acc(maxzdirs)
      double precision ew,sigew
      real xmin,xmax,range,lamabs,pgx,pgy
      real x(maxnpix),y(maxnpix)
      integer ngroups,ntrans(maxngroups),npix,nzaccept
      integer i,j,k,l,sindex,eindex,lenz_char,spix,epix
      integer spix_acc,epix_acc,length,viewed(maxtrpergr)
      character*(ionlen) ion(maxngroups,maxtrpergr)
      character*(ionlen) level(maxngroups,maxtrpergr)
      character*(longchar) groupnm(maxngroups)
      character*(ionlen) ion_acc(maxzdirs),lev_acc(maxzdirs)
      character*(namelen) dir_acc(maxzdirs)
      character*1 pgchar
      character*(namelen) z_char,name
      character*(questchar) inchar

      call PGOPEN('?PGPLOT Device? (<cr>=def, "?" for list):')
      call PGSUBP(1,2)
      call PGSCR(0,0.184,0.31,0.31)
      call PGSCR(15,0.0,0.0,0.0)
      call PGASK(.false.)

      do i=1,nzaccept
         call srchdblearr(wl_obs,npix,lam0_acc(i)*(1.d0+
     :        zaccept(i))*(1.d0-0.5d0*velspan*1.d3/c),spix_acc)
         call srchdblearr(wl_obs(spix_acc),npix-spix_acc+1,
     :        lam0_acc(i)*(1.d0+zaccept(i))*(1.d0+0.5d0*velspan*
     :        1.d3/c),epix_acc)
         epix_acc=epix_acc+spix_acc
         call PGBBUF
         call PGPANL(1,1)
         call PGERAS
         xmin=real(wl_obs(spix_acc))
         xmax=real(wl_obs(epix_acc))
         range=xmax-xmin
         xmin=xmin-llimit*range
         xmax=xmax+rlimit*range
         lamabs=real(lam0_acc(i)*(1.d0+zaccept(i)))
         do l=1,epix_acc-spix_acc+1
            x(l)=real(wl_obs(spix_acc+l-1))
            y(l)=real(flx(spix_acc+l-1))
         enddo
         call PGSVP(vpllimit,vprlimit,vpdlimit,vpulimit)
         call PGSWIN((xmin-lamabs)*real(c/1.d3)/lamabs,
     :        (xmax-lamabs)*real(c/1.d3)/lamabs,ymin,ymax)
         call PGSCI(15)
         call PGRECT((xmin-lamabs)*real(c/1.d3)/lamabs,
     :        (xmax-lamabs)*real(c/1.d3)/lamabs,ymin,ymax)
         call PGSCI(5)
         call PGSCH(2.3)
         call PGBOX('BST',0.0,0,'',0.0,0)
         call PGSCI(7)
         call PGBOX('N',0.0,0,'',0.0,0)
         call PGSWIN(xmin,xmax,ymin,ymax)
         call PGSCI(5)
         call PGSLW(2)
         call PGSCH(2.3)
         call PGBOX('CST',0.0,0,'BCST',0.0,0)
         call PGSCI(7)
         call PGBOX('MV',0.0,0,'NV',0.0,0)
         call PGSCI(3)
         call PGMTXT('B',2.2,0.5,0.5,'Velocity (km/s)')
         call PGMTXT('L',2.5,0.5,0.5,'Normalized Flux')
         call PGMTXT('T',1.8,0.5,0.5,'Wavelength (A)')
         call PGSCI(1)
         call rounddble(zaccept(i),5,z_char,lenz_char)
         call PGMTXT('T',1.8,1.0,1.0,'z='//z_char(1:lenz_char))
         call PGBIN(epix_acc-spix_acc+1,x,y,.true.)
         call transname(ion_acc(i),lev_acc(i),
     :        lam0_acc(i),f_acc(i),name,length,1)
         call PGMTXT('B',-1.0,0.015,0.0,name(1:length))
         call PGEBUF

         WRITE(*,'(2(a))') 'Searching for obscure transitions at z=',
     :        z_char(1:lenz_char)
         do j=1,ngroups
            call getlen(groupnm(j),sindex,eindex)
            call rchar(inchar,'Search for '//
     :           groupnm(j)(sindex:eindex)//'?','y')
            if (inchar(1:1).eq.'n') goto 1
            k=0
            do while (k.lt.ntrans(j))
 6             k=k+1
               viewed(k)=0
               call srchdblearr(wl_obs,npix,(1.d0+zaccept(i))*
     :              (1.d0-vrange_obs*1.d3/c)*lambda0(j,k),spix)
               if (spix.eq.npix.or.spix.eq.npix-1) goto 5
               call srchdblearr(wl_obs(spix),npix-spix+1,(1.d0+
     :              zaccept(i))*(1.d0+vrange_obs*1.d3/c)*
     :              lambda0(j,k),epix)
               epix=epix+spix
               l=spix-1
               ew=0.0
               sigew=1.0
c               do while (l.le.epix-newpix+1.and.
c     :              ew/sigew.lt.ewsiglim)
c                  l=l+1
c                  call ewregion(wl_obs(l),flx(l),sig(l),
c     :                 newpix,ew,sigew)
c               enddo
c               if (ew/sigew.ge.ewsiglim) then
                  viewed(k)=1
                  call PGBBUF
                  call PGPANL(1,2)
                  call PGERAS
                  call srchdblearr(wl_obs,npix,lambda0(j,k)*(1.d0+
     :                 zaccept(i))*(1.d0-0.5d0*velspan*1.d3/c),spix)
                  call srchdblearr(wl_obs(spix),npix-spix+1,
     :                 lambda0(j,k)*(1.d0+zaccept(i))*(1.d0+
     :                 0.5d0*velspan*1.d3/c),epix)
                  epix=epix+spix
                  xmin=real(wl_obs(spix))
                  xmax=real(wl_obs(epix))
                  range=xmax-xmin
                  xmin=xmin-llimit*range
                  xmax=xmax+rlimit*range
                  lamabs=real(lambda0(j,k)*(1.d0+zaccept(i)))
                  do l=1,epix-spix+1
                     x(l)=real(wl_obs(spix+l-1))
                     y(l)=real(flx(spix+l-1))
                  enddo
                  call PGSVP(vpllimit,vprlimit,vpdlimit,vpulimit)
                  call PGSWIN((xmin-lamabs)*real(c/1.d3)/lamabs,
     :                 (xmax-lamabs)*real(c/1.d3)/lamabs,ymin,ymax)
                  call PGSCI(15)
                  call PGRECT((xmin-lamabs)*real(c/1.d3)/lamabs,
     :                 (xmax-lamabs)*real(c/1.d3)/lamabs,ymin,ymax)
                  call PGSCI(5)
                  call PGSCH(2.3)
                  call PGBOX('BST',0.0,0,'',0.0,0)
                  call PGSCI(7)
                  call PGBOX('N',0.0,0,'',0.0,0)
                  call PGSWIN(xmin,xmax,ymin,ymax)
                  call PGSCI(5)
                  call PGSLW(2)
                  call PGSCH(2.3)
                  call PGBOX('CST',0.0,0,'BCST',0.0,0)
                  call PGSCI(7)
                  call PGBOX('MV',0.0,0,'NV',0.0,0)
                  call PGSCI(3)
                  call PGMTXT('B',2.2,0.5,0.5,'Velocity (km/s)')
                  call PGMTXT('L',2.5,0.5,0.5,'Normalized Flux')
                  call PGMTXT('T',1.8,0.5,0.5,'Wavelength (A)')
                  call PGSCI(1)
                  call PGBIN(epix-spix+1,x,y,.true.)
                  call transname(ion(j,k),level(j,k),
     :                 lambda0(j,k),f(j,k),name,length,1)
                  call PGMTXT('B',-1.0,0.015,0.0,name(1:length))
                  call transname(ion(j,k),level(j,k),
     :                 lambda0(j,k),f(j,k),name,length,0)
                  call PGEBUF
                  
                  call PGSCI(2)
c                  call PGBAND(6,0,0.0,0.0,pgx,pgy,pgchar)
                  call PGBAND(7,0,0.0,0.0,pgx,pgy,pgchar)
                  if (pgchar(1:1).eq.'b') then
 7                   if (k.eq.1) then
                        k=0
                        goto 6
                     elseif (viewed(k-1).eq.1) then
                        k=k-2
                        goto 6
                     else
                        k=k-1
                        goto 7
                     endif
                  elseif (pgchar(1:1).eq.'s') then
c     Skip to next species
                     l=k
                     do while (l.lt.ntrans(j))
                        l=l+1
                        if ((ion(j,k).ne.ion(j,l)).or.
     :                       (level(j,k).ne.level(j,l))) then
                           k=l-1
                           goto 6
                        endif
                     enddo
                  elseif (pgchar(1:1).eq.'q') then
                     k=ntrans(j)
                  elseif (pgchar(1:1).eq.'X') then
                     call getlen(dir_acc(i),sindex,eindex)
                     OPEN(unit=1,file=dir_acc(i)(sindex:eindex)//'/'//
     :                    name(1:length)//'.dat',status='new',err=2)
                     goto 3

 2                   WRITE(*,'(5(a))') 'WARNING: ',dir_acc(i)(sindex:
     :                    eindex),'/',name(1:length),
     :                    '.dat already exists!'
                     goto 4
                     
 3                   OPEN(unit=2,file=dir_acc(i)(sindex:eindex)//'/'//
     :                    name(1:length)//'.sig.dat',
     :                    status='new',err=2)
                     OPEN(unit=3,file=dir_acc(i)(sindex:eindex)//'/'//
     :                    name(1:length),status='new',err=2)
                     
                     call srchdblearr(wl_obs,npix,lambda0(j,k)*
     :                    (1.d0+zaccept(i))*(1.d0-chunkspan*
     :                    1.d3/c),spix)
                     call srchdblearr(wl_obs(spix),npix-spix+1,
     :                    lambda0(j,k)*(1.d0+zaccept(i))*(1.d0+
     :                    chunkspan*1.d3/c),epix)
                     epix=epix+spix
                     do l=spix,epix
                        WRITE(1,'(f15.9,3x,f15.9)') wl_obs(l),flx(l)
                        WRITE(2,'(f15.9,3x,g18.9)') wl_obs(l),sig(l)
                        WRITE(3,'(f15.9,3x,f15.9,3x,g18.9)')
     :                       wl_obs(l),flx(l),sig(l)
                     enddo
                     close(1)
                     close(2)
                     close(3)
 4                   continue
                  endif
c               endif
 5             continue
            enddo
 1          continue
         enddo
      enddo

      return
      end
