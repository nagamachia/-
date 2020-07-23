PROGRAM fractal_tree
    IMPLICIT NONE
    REAL(8)::x,y,a(25),p(2)
    REAL(8),PARAMETER::L=100.0d0
    REAL(8),allocatable::rand(:)
    INTEGER::i,clock,nRand
    INTEGER::N=50000,c1=0,c2=0,c3=0,c4=0,c5=0,c6=0
    INTEGER,allocatable::flag(:),seed(:)
    !---パラメータ---!
    a=(/0.05d0,0.6d0,0.05d0,-0.5d0,1.0d0,0.46d0,-0.15d0,0.39d0,&
    &0.38d0,0.6d0,0.47d0,0.15d0,0.17d0,0.42d0,1.1d0,0.43d0,0.28d0,&
    &-0.25d0,0.45d0,1.0d0,0.42d0,0.26d0,-0.35d0,0.31d0,0.7d0 /)
    !---乱数発生randベクトル---!
    allocate(rand(N),flag(N))
    call random_seed(size=nRand)
    allocate(seed(nRand))
    call system_clock(count=clock)
    seed=clock
    call random_seed(put=seed)
    call random_number(rand)
    !---randベクトルを区間の大きさに割り当てる---!
    rand=rand*L
    !---条件分岐---!
    DO i=1,N
        IF(rand(i) .ge. 0.0d0 .and. rand (i) .le. 10.0d0)THEN
            flag(i)=1
            c1=c1+1
        ELSEIF(rand(i) .ge. 10.0d0 .and. rand(i) .le. 20.0d0)THEN
            flag(i)=2
            c2=c2+1
        ELSEIF(rand(i) .ge. 20.0d0 .and. rand(i) .le. 40.0d0)THEN
            flag(i)=3
            c3=c3+1
        ELSEIF(rand(i) .ge. 40.0d0 .and. rand(i) .le. 60.0d0)THEN
            flag(i)=4
            c4=c4+1
        ELSEIF(rand(i) .ge. 60.0d0 .and. rand(i) .le. 80.0d0)THEN
            flag(i)=5
            c5=c5+1
        ELSE
            flag(i)=6
            c6=c6+1
        ENDIF
    END DO
    !---漸化式から次の点をプロット---!
    x=0.5d0;y=0.0d0
    WRITE(10,*) x,y
    DO i=1,N
        IF(flag(i)==1)THEN
            x=0.05d0*x
            y=0.6d0*y
            WRITE(10,*) x,y
        ELSEIF(flag(i)==2)THEN
            x=0.05d0*x
            y=-0.5d0*y+1.0d0
            WRITE(10,*) x,y
        ELSEIF(flag(i)==3)THEN
            x=0.46d0*x-0.15d0*y
            y=0.39d0*x+0.38d0*y+0.6d0
            WRITE(10,*) x,y
        ELSEIF(flag(i)==4)THEN
            x=0.47d0*x-0.15d0*y
            y=0.17d0*x+0.42d0*y+1.1d0
            WRITE(10,*) x,y
        ELSEIF(flag(i)==5)THEN
            x=0.43d0*x+0.28d0*y
            y=-0.25d0*x+0.45d0*y+1.0d0
            WRITE(10,*) x,y
        ELSE
            x=0.42d0*x+0.28d0*y
            y=-0.35d0*x+0.31d0*y+0.7d0
            WRITE(10,*) x,y
        ENDIF
    END DO
    print *,c1,c2,c3,c4,c5,c6
END PROGRAM fractal_tree