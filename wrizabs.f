c     MM 8/2/01 Subroutine to find and make directories as determined
c     from the absorption redshifts and the transitions in the different
c     groups.
      subroutine wrizabs(wl_obs,flx,sig,npix,ion,level,lambda0,f,
     :     zselect,nselect,groupnm,ngroups,ntrans,zaccept,nzaccept,
     :     ion_acc,lev_acc,lam0_acc,f_acc,dir_acc)
      implicit none
      include 'findabs_inc.f'
      include 'charlen_inc.f'
      include 'constants_inc.f'
      include 'ratomdat_inc.f'
      double precision lambda0(maxngroups,maxtrpergr)
      double precision f(maxngroups,maxtrpergr)
      double precision wl_obs(maxnpix)
      double precision flx(maxnpix)
      double precision sig(maxnpix)
      double precision zselect(maxngroups,maxabspergr)
      double precision ozdirs(maxzdirs)
      double precision zaccept(maxzdirs)
      double precision lam0_acc(maxzdirs)
      double precision f_acc(maxzdirs)
      integer ngroups,ntrans(maxngroups),npix
      integer nselect(maxngroups),nzaccept
      integer nozdirs,lenzabs_char
      integer sindex,eindex,i,j,zdirindex,ozdirindex
      character*(ionlen) ion(maxngroups,maxtrpergr)
      character*(ionlen) level(maxngroups,maxtrpergr)
      character*(ionlen) ion_acc(maxzdirs),lev_acc(maxzdirs)
      character*(longchar) groupnm(maxngroups)
      character*(namelen) dir_acc(maxzdirs)
      character*(namelen) outfile,zabs_char,zabs_char2
      character*(questchar) inchar,exist

 1    call rchar(outfile,'Output file?','findabs.out')
      OPEN(unit=1,file=outfile,status='unknown')

      do i=1,ngroups
         call getlen(groupnm(i),sindex,eindex)
         WRITE(1,'(a)') groupnm(i)(sindex:eindex)
         do j=1,nselect(i)
            call rounddble(zselect(i,j),dirsigfig,zabs_char,
     :           lenzabs_char)
            WRITE(1,'(a)') zabs_char(1:lenzabs_char)
         enddo
         if (i.ne.ngroups) WRITE(1,'(a)') ''
      enddo
      close(1)

      call rchar(inchar,
     :     'Create subdirectories and write data files?','y')
      if (inchar(1:1).ne.'n') then
         nzaccept=0
         do i=1,ngroups
            do j=1,nselect(i)
               call rounddble(zselect(i,j),dirsigfig,zabs_char,
     :              lenzabs_char)
               if (zselect(i,j).lt.ztol) then
                  lenzabs_char=dirsigfig+1
                  zabs_char='0.0000000000000000000'
               endif
               WRITE(*,'(2(a))') 'Trying z=',zabs_char(1:lenzabs_char)
               call findolddirs(ozdirs,nozdirs)
               ozdirindex=1
 2             call srchdbletol(ozdirs(ozdirindex),
     :              nozdirs-ozdirindex+1,zselect(i,j),ztol,
     :              zdirindex)
               if (zdirindex+ozdirindex-1.le.nozdirs) then
                  call rounddble(ozdirs(zdirindex+ozdirindex-1),
     :                 dirsigfig,zabs_char2,lenzabs_char)
                  if (zselect(i,j).lt.ztol) then
                     lenzabs_char=dirsigfig+1
                     zabs_char='0.0000000000000000000'
                  endif
                  WRITE(*,'(5(a))') 'z=',zabs_char(1:lenzabs_char),
     :                 ' within "tol" of existing directory ',
     :                 zabs_char2(1:lenzabs_char),'/'
                  call rchar(inchar,'Add to existing directory?','y')
                  if (inchar(1:1).eq.'n') then
                     if (zabs_char(1:lenzabs_char).eq.
     :                    zabs_char2(1:lenzabs_char)) call 
     :                    rounddble(ozdirs(zdirindex+ozdirindex-1)
     :                    +1.d0/dble(10.d0**(dirsigfig-1)),dirsigfig,
     :                    zabs_char,lenzabs_char)
                     call rchar(inchar,'Make new directory z='//
     :                    zabs_char(1:lenzabs_char),'y')
                     if (inchar(1:1).ne.'n') then
                        call system('/bin/mkdir '//zabs_char(1:
     :                       lenzabs_char))
                        call wchunks(wl_obs,flx,sig,npix,ion,level,
     :                       lambda0,zselect(i,j),zabs_char,
     :                       lenzabs_char,i,ntrans(i))
                        nzaccept=nzaccept+1
                        zaccept(nzaccept)=zselect(i,j)
                        ion_acc(nzaccept)=ion(i,1)
                        lev_acc(nzaccept)=level(i,1)
                        lam0_acc(nzaccept)=lambda0(i,1)
                        f_acc(nzaccept)=f(i,1)
                        dir_acc(nzaccept)=zabs_char(1:lenzabs_char)//
     :                       ' '
                     else
                        ozdirindex=zdirindex+1
                        goto 2
                     endif
                  else
                     call wchunks(wl_obs,flx,sig,npix,ion,level,
     :                    lambda0,zselect(i,j),zabs_char2,
     :                    lenzabs_char,i,ntrans(i))
                  endif
               else
                  WRITE(*,'(2(a))') 'Making new directory z=',
     :                 zabs_char(1:lenzabs_char)
                  call system('/bin/mkdir '//zabs_char(1:
     :                 lenzabs_char))
                  call wchunks(wl_obs,flx,sig,npix,ion,level,
     :                 lambda0,zselect(i,j),zabs_char,
     :                 lenzabs_char,i,ntrans(i))
                  nzaccept=nzaccept+1
                  zaccept(nzaccept)=zselect(i,j)
                  ion_acc(nzaccept)=ion(i,1)
                  lev_acc(nzaccept)=level(i,1)
                  lam0_acc(nzaccept)=lambda0(i,1)
                  f_acc(nzaccept)=f(i,1)
                  dir_acc(nzaccept)=zabs_char(1:lenzabs_char)//' '
               endif
            enddo
         enddo
      endif
      
      return
      end
