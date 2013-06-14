c     MM 5/2/01 Subroutine to read in the atomic data required to
c     idenitfy lines. The data should be organised in groups: each group
c     of transitions should be headed by a name (e.g. MgII doublet or
c     Lines common to DLAs) and should be arrranged in order of search
c     priority.
      subroutine ratomdat(ion,level,lambda0,f,gamma,amass,ngroups,
     :     groupnm,ntrans,obscure)
      implicit none
      include 'findabs_inc.f'
      include 'charlen_inc.f'
      include 'ratomdat_inc.f'
      double precision lambda0(maxngroups,maxtrpergr)
      double precision f(maxngroups,maxtrpergr)
      double precision gamma(maxngroups,maxtrpergr)
      double precision amass(maxngroups,maxtrpergr)
      integer ngroups,ntrans(maxngroups)
      integer sindex,eindex,ios,i,j
      character*(ionlen) ion(maxngroups,maxtrpergr)
      character*(ionlen) level(maxngroups,maxtrpergr)
      character*(longchar) groupnm(maxngroups)
      character*(namelen) infile,atomdr
      character*(longchar) zerochar,charline,sepline(maxatomwords)
      logical obscure

      do i=1,longchar
         zerochar(i:i)=' '
      enddo

      atomdr=zerochar
      call getenv('FINDABS_ATOM_DIR',atomdr)
      if (atomdr(1:1).ne.' ') then
         call getlen(atomdr,sindex,eindex)
         OPEN(unit=1,file=atomdr(sindex:eindex)//'/findabs_atom.dat',
     :        status='old',err=1)
         infile=atomdr(sindex:eindex)//'/findabs_atom.dat'
         eindex=eindex-sindex+17
         sindex=1
      else
 1       infile=zerochar(1:namelen)
         call rchar(infile,'File containing group and atomic data?',
     :        'findabs_atom.dat')
         OPEN(unit=1,file=infile,status='old',err=2)
         goto 3
         
 2       call getlen(infile,sindex,eindex)
         WRITE(*,'(2(a))') 'WARNING: Could not open file ',
     :        infile(sindex:eindex)
         goto 1
      endif

 3    i=0
 4    i=i+1
      groupnm(i)=zerochar
      READ(1,'(a)',end=7,iostat=ios) groupnm(i)
      if (ios.ne.0) then
         call getlen(infile,sindex,eindex)
         WRITE(*,'(2(a))') 'STOPPING: Error reading file ',
     :     infile(sindex:eindex)
         stop
      endif
      
 5    if ((groupnm(i)(1:1).eq.'*'.and..not.obscure).or.
     :     (groupnm(i)(1:1).ne.'*'.and.obscure)) then
         i=i-1
         do while (groupnm(i+1)(1:1).ne.' ')
            READ(1,'(a)',end=7) groupnm(i+1)
         enddo
         goto 4
      endif
      j=0
 6    j=j+1
      charline=zerochar
      READ(1,'(a)',end=7) charline
      if (charline(1:1).eq.' ') then
         ntrans(i)=j-1
         goto 4
      endif
      call sepchar(charline,sepline,longchar)
      ion(i,j)(1:ionlen)=sepline(1)(1:ionlen)
      level(i,j)(1:ionlen)=sepline(2)(1:ionlen)
      read(sepline(3),*) amass(i,j)
      read(sepline(4),*) lambda0(i,j)
      read(sepline(5),*) f(i,j)
      read(sepline(6),*) gamma(i,j)
      goto 6

 7    close(1)
      ntrans(i)=j-1
      ngroups=i

      return
      end
