c     MM 6/2/01 Subroutine to calculate the equivalent width of
c     absorption for a given normalized flux and sigma array. This is
c     the aperture method used by Lanzetta, Turnshek & Wolfe (1987, ApJ,
c     322, 739).
      subroutine ewregion(wl_obs,flx,sig,npix,ew,sigew)
      implicit none
      integer npix
      double precision wl_obs(npix),flx(npix),sig(npix),ew,sigew
      double precision dlam
      integer i

      ew=0.d0
      sigew=0.d0
      dlam=(wl_obs(npix)-wl_obs(1))/dble(npix)
      do i=1,npix
         ew=ew+(1.d0-flx(i))*dlam
         sigew=sigew+(sig(i)*dlam)**2
      enddo
      sigew=dsqrt(sigew)

      return
      end
