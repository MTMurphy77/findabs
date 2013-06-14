c     MM 5/2/01 Program to identify and extract absorption systems from
c     a QSO spectrum. The systems are identified using the Equivalent
c     Width method outlined by Lanzetta, Turnshek & Wolfe (1987, ApJ,
c     322, 739) to identify the onset of an absorption feature. It then
c     looks at the corresponding wavelengths for other "related"
c     transitions to see if there is absorption there as well. Also, the
c     transitions that are related to each other are defined in a user
c     defined atomic data file.
      program findabs
      implicit none
      include 'findabs_inc.f'
      include 'charlen_inc.f'
      include 'ratomdat_inc.f'
      double precision lambda0(maxngroups,maxtrpergr)
      double precision f(maxngroups,maxtrpergr)
      double precision gamma(maxngroups,maxtrpergr)
      double precision amass(maxngroups,maxtrpergr)
      double precision wl_obs(maxnpix)
      double precision flx(maxnpix)
      double precision sig(maxnpix)
      double precision zem
      double precision zselect(maxngroups,maxabspergr)
      double precision zaccept(maxzdirs)
      double precision lam0_acc(maxzdirs)
      double precision f_acc(maxzdirs)
      integer ngroups,ntrans(maxngroups),npix
      integer nselect(maxngroups),nzaccept
      character*(ionlen) ion(maxngroups,maxtrpergr)
      character*(ionlen) level(maxngroups,maxtrpergr)
      character*(longchar) groupnm(maxngroups)
      character*(ionlen) ion_acc(maxzdirs),lev_acc(maxzdirs)
      character*(namelen) dir_acc(maxzdirs)
      character*(questchar) inchar

      WRITE(*,'(a)') 'FINDABS: Find absorption systems in QSO'
      WRITE(*,'(a)') '         spectra'
      WRITE(*,'(a)') 'Last modified: 19/2/01 by Michael Murphy'
      WRITE(*,'(a)') ' '

      call ratomdat(ion,level,lambda0,f,gamma,amass,ngroups,
     :     groupnm,ntrans,.false.)

      call rqsodata(wl_obs,flx,sig,npix)

      call rdble(zem,'Emission redshift of QSO?','0.d0')

      call scanz(wl_obs,flx,sig,npix,zem,ngroups,groupnm,ntrans,
     :     lambda0,f,ion,level,zselect,nselect)

      call wrizabs(wl_obs,flx,sig,npix,ion,level,lambda0,f,zselect,
     :     nselect,groupnm,ngroups,ntrans,zaccept,nzaccept,ion_acc,
     :     lev_acc,lam0_acc,f_acc,dir_acc)

      call rchar(inchar,'Search for "obscure" transitions?','y')
      if (inchar(1:1).eq.'n') stop

      call ratomdat(ion,level,lambda0,f,gamma,amass,ngroups,
     :     groupnm,ntrans,.true.)

      call scanzobs(wl_obs,flx,sig,npix,ngroups,groupnm,ntrans,
     :     lambda0,f,ion,level,zaccept,nzaccept,ion_acc,lev_acc,
     :     lam0_acc,f_acc,dir_acc)

      stop
      end
