PROGRAM gasket1
    implicit none
    integer::k,clock,nRand
    integer,allocatable::seed(:)
    real(4)::x,y,rand(100000)
    real(4)::p0(2),p1(2),p2(2),p(2),select
    complex(4)::c,z,imag=(0,1.0)
    p0=(/ 0,0 /);p1=(/ 1,0 /);p2=(/ 1,1 /)
    x=0;y=0
    print *,"フラクタル・シェルピンスキーのギャスケット"
!----乱数発生-------------------------
    call random_seed(size=nRand)
    allocate(seed(nRand))
    call system_clock(count=clock)
    seed=clock
    call random_seed(put=seed)
    call random_number(rand)
    rand=rand*10
    rand=int(rand)
!----乱数による分岐-------------------
    DO k=1,100000
        IF(mod(rand(k),3.0)==0)then
            p=p0
        ELSEIF(mod(rand(k),3.0)==1)then
            p=p1
        ELSE
            p=p2
        END IF
        x=(x+p(1))/2.0    !--負の値にしたらおもしろい、大きくするとつまらない。
        y=(y+p(2))/2.0
        write(10,*) x,y
    END DO
!----GNUPLOT--------------------------
    open(13,file="gnuplotdummy.plt")
        write(13,*) "reset           #初期化"
        write(13,*) "plot 'fort.10' with dots #プロット"
!        write(13,*) "set term x11 1"
        write(13,*) 'pause -1 "click OK to quit gnuplot" #表示させ続ける'
    close(13)
        call system("gnuplot gnuplotdummy.plt")
END