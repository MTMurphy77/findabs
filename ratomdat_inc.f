c     MM 12/2/01 Include file containing parameters relevant to reading
c     in an atomic data file
c
      integer ionlen
c     Length of Ion and Level character string (e.g. "Mg" and "IV" etc)
      parameter (ionlen=6)

      integer maxnatom
c     Maximum number of entries in the atom.dat file.
      parameter (maxnatom=1000)

      integer maxatomwords
c     Maximum number of words in each line of the findabs_atom.dat file
c     (including comments)
      parameter (maxatomwords=10)
