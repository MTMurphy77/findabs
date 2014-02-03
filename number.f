c MM 21/0/99 In which we determine whether the input letter is
c an integer or not
      logical function number(inchar)
      character*1 inchar
      
      number=.false.
      if ((ichar(inchar(1:1)).lt.58).and.
     :     (ichar(inchar(1:1)).gt.47)) number=.true.

      return
      end
