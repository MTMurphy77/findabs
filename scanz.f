c     MM 5/2/01 Subroutine to scan the z-space of a QSO spectrum to find
c     absorption systems which are defined as groups of transitions
      subroutine scanz(wl_obs,flx,sig,npix,zem,ngroups,groupnm,ntrans,
     :     lambda0,f,ion,level,zselect,nselect)
      implicit none
      include 'findabs_inc.f'
      include 'charlen_inc.f'
      include 'constants_inc.f'
      include 'ratomdat_inc.f'
      double precision wl_obs(maxnpix)
      double precision flx(maxnpix)
      double precision sig(maxnpix)
      double precision zem
      double precision lambda0(maxngroups,maxtrpergr)
      double precision f(maxngroups,maxtrpergr)
      double precision zabs,ew,sigew,lam2
      double precision zselect(maxngroups,maxabspergr)
      integer ngroups,ntrans(maxngroups),npix
      integer i,j,k,sindex,eindex,spix,dummypix
      integer pixstart(maxtrpergr),pixend(maxtrpergr)
      integer nselect(maxngroups)
      integer accept,scan,fwdrev
      character*(questchar) inchar
      character*(ionlen) ion(maxngroups,maxtrpergr)
      character*(ionlen) level(maxngroups,maxtrpergr)
      character*(longchar) groupnm(maxngroups)
      logical forest,sforest

      do i=1,ngroups
         nselect(i)=0
         call getlen(groupnm(i),sindex,eindex)
         call rchar(inchar,'Search for '//groupnm(i)(sindex:eindex)//
     :        '?','y')
         if (inchar.eq.'n') goto 1
         spix=1
         forest=.false.
         if (wl_obs(1).lt.(1.d0+zem)*Lya) forest=.true.
         if (forest) then
            call rchar(inchar,
     : 'Search Lya forest for '//groupnm(i)(sindex:eindex)//'?',
     : 'y')
            if (inchar(1:1).eq.'n') call srchdblearr(wl_obs,
     :           npix,(1.d0+zem)*Lya,spix)
         endif
         
         j=spix
         scan=1
         fwdrev=1
         do while (j.le.npix-newpix+1 .and. scan.eq.1)
            call ewregion(wl_obs(j),flx(j),sig(j),newpix,
     :           ew,sigew)
            if (ew/sigew.ge.ewsiglim) then
               zabs=wl_obs(j)/lambda0(i,1)-1.d0
               if (zabs.lt.minz) goto 2 
               lam2=lambda0(i,2)*(1.d0+zabs)
               if (lam2.gt.wl_obs(1).and.lam2.lt.wl_obs(npix)) then
                  call srchdblearr(wl_obs,npix,lam2,dummypix)
                  call ewregion(wl_obs(dummypix),flx(dummypix),
     :                 sig(dummypix),newpix,ew,sigew)
                  if (ew/sigew.ge.ewsiglim) then
                     do k=1,ntrans(i)
                        call srchdblearr(wl_obs,npix,lambda0(i,k)*
     :                       (1.d0+zabs)*(1-velspan/1.d1*3.d3/c),
     :                       pixstart(k))
                        call srchdblearr(wl_obs,npix,lambda0(i,k)*
     :                       (1.d0+zabs)*(1+velspan/1.d1*7.d3/c),
     :                       pixend(k))
                        if (wl_obs(pixstart(k)).gt.lambda0(i,k)*
     :                       (1.d0+zabs)*(1-velspan/1.d1*3.d3/c))
     :                       pixstart(k)=1
                     enddo
                     call plotdat(wl_obs,flx,npix,zabs,i,lambda0,f,
     :                    ion,level,pixstart,pixend,accept)
                     if (accept.eq.1) then
                        j=pixend(1)-1
                        nselect(i)=nselect(i)+1
                        zselect(i,nselect(i))=zabs
                     elseif (accept.eq.0) then
                        j=pixstart(1)-1
                     elseif (accept.eq.-1) then
                        j=pixstart(1)-1
                        fwdrev=-1
                     elseif (accept.eq.-2) then
                        scan=0
                     endif
                  endif
               endif
            endif
 2          j=j+fwdrev
            if (j.lt.1) then
               j=1
               fwdrev=1
            endif
         enddo
 1       continue
      enddo
      call PGCLOS

      return
      end
