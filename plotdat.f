c     MM 6/2/01 Subroutine to plot two panels of data, one for each of
c     the first two transitions of a group. The user can either accept
c     or reject the candidate absorption system and asked to place a
c     marker as to where to recommence the search for new absorbers.
      subroutine plotdat(wl_obs,flx,npix,zabs,group,lambda0,f,
     :     ion,level,pixstart,pixend,accept)
      implicit none
      include 'findabs_inc.f'
      include 'charlen_inc.f'
      include 'constants_inc.f'
      include 'ratomdat_inc.f'
      double precision wl_obs(maxnpix)
      double precision flx(maxnpix)
      double precision zabs
      double precision lambda0(maxngroups,maxtrpergr)
      double precision f(maxngroups,maxtrpergr)
      integer npix
      integer accept
      real x(npix),y(npix)
      real xmin,xmax,range,lamabs,pgx,pgy,dummyx(2),dummyy(2)
      integer group
      integer pixstart(maxtrpergr),pixend(maxtrpergr)
      integer i,j,length
      character*1 pgchar
      character*(ionlen) ion(maxngroups,maxtrpergr)
      character*(ionlen) level(maxngroups,maxtrpergr)
      character*(namelen) name,zabs_char
      SAVE first
      logical first
      data first/.true./
      
      if (first) then
         first=.false.
         call PGOPEN('?PGPLOT Device? (<cr>=def, "?" for list):')
         call PGSUBP(1,2)
         call PGSCR(0,0.184,0.31,0.31)
         call PGSCR(15,0.0,0.0,0.0)
         call PGASK(.false.)
      endif

      dummyx(1)=0.0
      dummyx(2)=0.0
      dummyy(1)=ymin
      dummyy(2)=ymax
      do i=2,1,-1
         call PGBBUF
         call PGPAGE
         xmin=real(wl_obs(pixstart(i)))
         xmax=real(wl_obs(pixend(i)))
         range=xmax-xmin
         xmin=xmin-llimit*range
         xmax=xmax+rlimit*range
         lamabs=real(lambda0(group,i)*(1.d0+zabs))
         do j=1,pixend(i)-pixstart(i)+1
            x(j)=real(wl_obs(pixstart(i)+j-1))
            y(j)=real(flx(pixstart(i)+j-1))
         enddo
         call PGSVP(vpllimit,vprlimit,vpdlimit,vpulimit)
         call PGSWIN((xmin-lamabs)*real(c/1.d3)/lamabs,(xmax-
     :        lamabs)*real(c/1.d3)/lamabs,ymin,ymax)
         call PGSCI(15)
         call PGRECT((xmin-lamabs)*real(c/1.d3)/lamabs,(xmax-
     :        lamabs)*real(c/1.d3)/lamabs,ymin,ymax)
         call PGSCI(14)
         call PGSLS(2)
         call PGLINE(2,dummyx,dummyy)
         call PGSLS(1)
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
         call PGBIN(pixend(i)-pixstart(i)+1,x,y,.true.)
         call transname(ion(group,i),level(group,i),lambda0(group,i),
     :        f(group,i),name,length,0)
         call PGMTXT('B',-1.0,0.015,0.0,name(1:length))
         call PGEBUF
      enddo
      write(zabs_char(1:10),'(f10.8)') zabs
      call PGMTXT('B',2.2,1.0,1.0,'z='//zabs_char(1:10))
      call PGSCI(2)
      call PGBAND(6,0,0.0,0.0,pgx,pgy,pgchar)
      if (pgchar.ne.'X') then
         if (pgchar.eq.'D') then
            accept=-1
            call srchdblearr(wl_obs,npix,dble(pgx),pixstart(1))
         elseif (pgchar.eq.'q') then
            accept=-2
         else
            accept=0
            call srchdblearr(wl_obs,npix,dble(pgx),pixstart(1))
         endif
      else
         accept=1
         zabs=dble(pgx)/lambda0(group,1)-1.d0
      endif
      
c      call PGCLOS
c      stop
      return
      end
