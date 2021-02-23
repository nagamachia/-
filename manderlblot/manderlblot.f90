PROGRAM Manderlblot
    implicit none
    integer::i,j,flag,l,imax=300,iter
    real(8)::x,y,dx,dy,n
    complex(8)::c,z,imag=(0d0,1.0d0)
DO iter=1,5 !繰り返し回数は5回
    dx=2.0d0/imax
    dy=2.0d0/imax
    DO i=-imax,imax
    DO j=-imax,imax
        x=dx*i
        y=dy*j
        c=cmplx(x,y)
        z=(0.0d0,0.0d0)
        flag=0
        DO l=1,200
            !z=((exp(z)-exp(-z*iter))/2.0d0)**iter+c !漸化式
            z=tan(z**iter**iter)+c
            IF(abs(z)>2.0d0)then
                flag=1
                exit
            END IF
        END DO
        IF(flag==0)then
            write(iter*10,*) x,y
        END IF
    END DO
    END DO
END DO
!以下、gnuplotでグラフを描かせ、png形式で保存します。
    open(13,file="gnupdummy.plt")
        write(13,*) "reset       #初期化"
        write(13,*) "plot 'fort.10' with dots #プロット"
            write(13,*) "set term windows 1"
        write(13,*) "plot 'fort.20' with dots #プロット"
            write(13,*) "set term windows 2"
        write(13,*) "plot 'fort.30' with dots #プロット"
            write(13,*) "set term windows 3"
        write(13,*) "plot 'fort.40' with dots #プロット"
            write(13,*) "set term windows 4"
        write(13,*) "plot 'fort.50' with dots #プロット"
            write(13,*) "set term windows 5"
        write(13,*) 'pause -1 "click OK to quit gnuplot" #表示させ続ける'
    close(13)
    call system("gnuplot gnupdummy.plt")
END