      integer maxnpix
c     Maximum number of pixels per spectrum
      parameter (maxnpix=500000)

      integer maxngroups
c     Maximum number of groups of "related" transitions
      parameter (maxngroups=100)

      integer maxtrpergr
c     Maximum number of transitions per group allowed
      parameter (maxtrpergr=1000)

      integer maxabspergr
c     Maximum number of absorption systems to be selected per group
      parameter (maxabspergr=20)

      integer dirsigfig
c     Number of significant figures to be used when creating
c     subdirectories labelled by absorption redshift
      parameter (dirsigfig=5)

      integer maxzdirs
c     Maximum number of previous redshift directories allowed to be
c     present
      parameter (maxzdirs=100)

      integer newpix
c     Number of equivalent width pixels. This is the number of pixels
c     summed over to determine whether or not a feature os statistically
c     significant or not. It should be about the number of pixels in two
c     resolution elements.
      parameter (newpix=7)

      double precision ewsiglim
c     This is the significance limit for selection of absorption
c     features
      parameter (ewsiglim=5.d0)

      double precision wlentol
c     Tolerance for difference between the wavelength scales in
c     corresponding data and sigma files in units of the wavelength
c     scale unit
      parameter (wlentol=1.d-8)

      double precision ztol
c     Tolerance for difference between the redshifts indicated by
c     directory names and directories to be created.
      parameter (ztol=0.01d0)

      double precision minz
c     Minimum allowed absorption redshift. Usually this is a small
c     negative quantity.
      parameter (minz=-0.001d0)

      double precision velspan
c     Velocity range (in km/s) to plot after detection of a candidate
c     absorption system
      parameter (velspan=500.d0)

      double precision chunkspan
c     Velocity range EITHER SIDE of the chosen center of a selected
c     absorption system which is to be written to an output
c     file. (i.e. The chunk written to disk will be 2*chunkspan in size)
      parameter (chunkspan=750.d0)

      double precision vrange_obs
c     Velocity range EITHER SIDE of the fiducial redshift for searching
c     for "obscure" transitions.
      parameter (vrange_obs=100.d0)

      double precision Lya
c     Rest wavelength for Lyman-alpha transition in Angstroms
      parameter (Lya=1215.67d0)

c Viewport size control

      real vpdlimit
c Bottom of view port set
      parameter (vpdlimit=0.14)

      real vpulimit
c Top of view port set
      parameter (vpulimit=0.85)

      real vpllimit
c Left edge of view port set
      parameter (vpllimit=0.10)

      real vprlimit
c Right edge of view port set
      parameter (vprlimit=0.97)

c Intra-viewport control

      real llimit
c Fraction of vertical range to be left free to the left
      parameter (llimit=0.03)

      real rlimit
c Fraction of vertical range to be left free to the right
      parameter (rlimit=0.03)

c Vertical range to plot

      real ymax
c Maximum of vertical plot range
      parameter (ymax=1.3)

      real ymin
c Maximum of vertical plot range
      parameter (ymin=-0.3)
