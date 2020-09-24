PROGRAM logistic
      IMPLICIT NONE
      real(8)::a,d,u,c,x=0.8d0
      INTEGER a_
      !---PARAMETER---!
      d=2.8d0
      u=4.0d0
      c=0.00001d0
      !------!
      DO a_=INT(d/c),INT(u/c)
        a=a_*c
        x=a*x*(1.0d0-x)
        WRITE(10,*) a,x
      END DO
      !---GNUPLOT---!
      OPEN(13,file="dummy.plt")
        WRITE(13,*) "reset"
        WRITE(13,*) "plot 'fort.10' with dots"
        WRITE(13,*) 'pause -1 "click OK to quit"'
      CLOSE(13)
      call system("gnuplot dummy.plt")
END PROGRAM logistic