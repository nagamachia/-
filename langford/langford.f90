PROGRAM langford
    implicit none
    real(8)::f1,f2,f3,x,y,z,k1(3),k2(3),k3(3),k4(3)
    real(8)::x0=0d0,y0=1.0d0,dt=0.01d0,t=0d0,tmax=500.0d0,a=0.6d0
!---連立微分方程式の関数---!
    f1(t,x,y,z)=(z-0.7d0)*x-3.5d0*y
    f2(t,x,y,z)=3.5d0*x+(z-0.7d0)*y
    f3(t,x,y,z)=a+z-z**3.0d0/3.0d0-(x**2.0d0+y**2.0d0)*(1.0d0+0.25d0*z)
!--------------------------!
    x=x0;y=y0
    DO
        k1(1)=dt*f1(t,x,y,z)
        k1(2)=dt*f2(t,x,y,z)
        k1(3)=dt*f3(t,x,y,z)

        k2(1)=dt*f1(t+dt/2.0d0,x+k1(1)/2.0d0,y+k1(1)/2.0d0,z+k1(1)/2.0d0)
        k2(2)=dt*f2(t+dt/2.0d0,x+k1(2)/2.0d0,y+k1(2)/2.0d0,z+k1(2)/2.0d0)
        k2(3)=dt*f3(t+dt/2.0d0,x+k1(3)/2.0d0,y+k1(3)/2.0d0,z+k1(3)/2.0d0)

        k3(1)=dt*f1(t+dt/2.0d0,x+k2(1)/2.0d0,y+k2(1)/2.0d0,z+k2(1)/2.0d0)
        k3(2)=dt*f2(t+dt/2.0d0,x+k2(2)/2.0d0,y+k2(2)/2.0d0,z+k2(2)/2.0d0)
        k3(3)=dt*f3(t+dt/2.0d0,x+k2(3)/2.0d0,y+k2(3)/2.0d0,z+k2(3)/2.0d0)

        k4(1)=dt*f1(t+dt,x+k3(1),y+k3(1),z+k3(1))
        k4(2)=dt*f2(t+dt,x+k3(2),y+k3(2),z+k3(2))
        k4(3)=dt*f3(t+dt,x+k3(3),y+k3(3),z+k3(3))

        x=x+(k1(1)+2.0d0*k2(1)+2.0d0*k3(1)+k4(1))/6.0d0
        y=y+(k1(2)+2.0d0*k2(2)+2.0d0*k3(2)+k4(2))/6.0d0
        z=z+(k1(3)+2.0d0*k2(3)+2.0d0*k3(3)+k4(3))/6.0d0
        write(10,*) x,y,z
        t=t+dt
        IF(t>tmax)then
            exit;
        END IF
    END DO
!----GNUPLOT----
    OPEN(13,file="gnupdummy.plt")
        write(13,*) "reset"
        write(13,*) "splot 'fort.10' with line lw 0.5"
        write(13,*) "pause -1"
    close(13)
    call system("gnuplot gnupdummy.plt")
END