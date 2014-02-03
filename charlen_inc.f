c     MM 5/2/01 Include file containing the typical lengths of character
c     strings used for various subroutines and programs.
      integer questchar
c     Maximum character length of strings entered by the user upon prompt
      parameter (questchar=6)

      integer namelen
c     Maximum character length of file names and other internal name strings
c     SHOULD NOT EXCEED 100
      parameter (namelen=64)

      integer longchar
c     Maximum character length of input lines from findabs_atom.dat file
c     MUST BE GREATER THAN namelen
      parameter (longchar=120)
