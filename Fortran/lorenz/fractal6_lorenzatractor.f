      PROGRAM FRACTAL6_LORENZATRACTOR                                   
      IMPLICIT NONE                                                     
      REAL(8)::F1,F2,F3,X,Y,Z                                           
      REAL(8)::X0=0D0,Y0=1.01D0,Z0=0.0D0,                               
     &DT=0.01D0,T=0D0,TMAX=100D0,A=0.6D0                                
      REAL(8),DIMENSION(:,:),ALLOCATABLE::K                             
      INTEGER::I,J,N                                                    
!---˜A—§”÷•ª•û’öŽ®‚ÌŠÖ”---!                                            
!      F1(T,X,Y,Z)=10.0D0*(Y-X)                                          
      F1(T,X,Y,Z)=10.0D0*(Y-X)                                          
      F2(T,X,Y,Z)=25.0D0*X-Y-X*Z                                        
      F3(T,X,Y,Z)=-(7.0D0/3.0D0)*Z+X*Y                                  
!-------------------------!                                             
      ALLOCATE(K(4,4))                                                  
      X=X0;Y=Y0;Z=Z0                                                    
      DO                                                                
          K(1,1)=DT*F1(T,X,Y,Z)                                         
          K(1,2)=DT*F2(T,X,Y,Z)                                         
          K(1,3)=DT*F3(T,X,Y,Z)                                         
C                                                                       
          K(2,1)=DT*F1(T+DT/2.0D0,                                      
     &     X+K(1,1)/2.0D0,Y+K(1,1)/2.0D0,Z+K(1,2)/2.0D0)                
          K(2,2)=DT*F2(T+DT/2.0D0,                                      
     &     X+K(1,1)/2.0D0,Y+K(1,1)/2.0D0,Z+K(1,2)/2.0D0)                
          K(2,3)=DT*F3(T+DT/2.0D0,                                      
     &     X+K(1,1)/2.0D0,Y+K(1,1)/2.0D0,Z+K(1,2)/2.0D0)                
C                                                                       
          K(3,1)=DT*F1(T+DT/2.0D0,                                      
     &     X+K(2,1)/2.0D0,Y+K(2,1)/2.0D0,Z+K(2,2)/2.0D0)                
          K(3,2)=DT*F2(T+DT/2.0D0,                                      
     &     X+K(2,1)/2.0D0,Y+K(2,1)/2.0D0,Z+K(2,2)/2.0D0)                
          K(3,3)=DT*F3(T+DT/2.0D0,                                      
     &     X+K(2,1)/2.0D0,Y+K(2,1)/2.0D0,Z+K(2,2)/2.0D0)                
C                                                                       
          K(4,1)=DT*F1(T+DT,X+K(3,1),Y+K(3,2)/2.0D0,Z+K(3,2))           
          K(4,2)=DT*F2(T+DT,X+K(3,1),Y+K(3,2)/2.0D0,Z+K(3,2))           
          K(4,3)=DT*F3(T+DT,X+K(3,1),Y+K(3,2)/2.0D0,Z+K(3,2))           
C                                                                       
          X=X+(K(1,1)+2.0D0*K(2,1)+2.0D0*K(3,1)+K(4,1))/6.0D0           
          Y=Y+(K(1,2)+2.0D0*K(2,2)+2.0D0*K(3,2)+K(4,2))/6.0D0           
          Z=Z+(K(1,3)+2.0D0*K(2,3)+2.0D0*K(3,3)+K(4,3))/6.0D0           
C                                                                       
          WRITE(20,*) X,Y,Z                                             
          T=T+DT                                                        
          IF(T>TMAX)THEN                                                
              EXIT;                                                     
          END IF                                                        
      ENDDO                                                             
!----GNUPLOT----!                                                       
      OPEN(13,FILE="gnupdummy.plt")                                     
          WRITE(13,*) "reset"                                           
          WRITE(13,*) "splot 'fort.20' with line lw 0.5"                
          WRITE(13,*) "pause -1"                                        
      CLOSE(13)                                                         
      CALL system("gnuplot gnupdummy.plt")                              
      END                                                               