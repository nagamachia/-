PROGRAM fractal2
    implicit none
    integer::i,j,k,flag,l,lmax=200,clock,nRand,loop,cleft,cup,cright,cdown
    integer,allocatable::seed(:)
    real(4)::x,y,n,rand(20000)
    real(4)::left(2),right(2),up(2),down(2),path(2),r(2),select
    left=(/-1.0,0.0/);right=(/1.0,0.0/);up=(/0.0,1.0/);down=(/0.0,-1.0/)
    x=0;y=0;r=(/x,y/)
    print *,"���_����̃����_���E�H�[�N"

    call random_seed(size=nRand)
    allocate(seed(nRand))
    DO loop=1,5
    !----��������-------------------------
        call system_clock(count=clock)
        seed=clock
        call random_seed(put=seed)
        call random_number(rand)
        rand=rand*10
    !   rand=int(rand)
    !-------------------------------------
        cleft=0;cup=0;cright=0;cdown=0!�J�E���^�[������
        DO k=1,20000
            IF(0<rand(k) .and. rand(k)<2.5)then!1/4�̊m���ō��A��A�E�A����I��
                path=left!���֐i��
                cleft=cleft+1
            ELSEIF(2.5<rand(k) .and. rand(k)<5.0)then
                path=up!��֐i��
                cup=cup+1
            ELSEIF(5.0<rand(k) .and. rand(k)<7.5)then
                path=right!�E�֐i��
                cright=cright+1
            ELSEIF(7.5<rand(k) .and. rand(k)<10.0)then
                path=down!���֐i��
                cdown=cdown+1
            END IF
            r=r+path
            write(10*loop,*) r
        END DO
        print *,"��",cleft,"��",cup,"�E",cright,"��",cdown
    END DO
!----GNUPLOT------------------------
    OPEN(13,file="gnupdummy.plt")
        write(13,*) "reset       #������"
        write(13,*) "plot 'fort.10' with line #�v���b�g"
            write(13,*) "set term windows 1"
        write(13,*) "plot 'fort.20' with line #�v���b�g"
            write(13,*) "set term windows 2"
        write(13,*) "plot 'fort.30' with line #�v���b�g"
            write(13,*) "set term windows 3"
        write(13,*) "plot 'fort.40' with line #�v���b�g"
            write(13,*) "set term windows 4"
        write(13,*) "plot 'fort.50' with line #�v���b�g"
            write(13,*) "set term windows 5"
        write(13,*) 'pause -1 "click OK to quit gnuplot" #�\������������'
    close(13)
    call system("gnuplot gnupdummy.plt")
END