      program read_TQ
c reads qvtime.dat or temptime.dat produced by EMPM
C WRITEs a new binary file with simpler structure

c	nt:	number of timesteps
c	nx1: number of grid cells
c	exphdr: experiment header
c	qv1: water vapor mixing ratio (qvtime.dat)
c   qv1: temperature (temptime.dat)
c   dx: grid size (m)
 
      parameter(nt = 1001, nx1=12000)
      character*80 exphdr
      real*4 qv1(nx1)
      real*8 dx, time2
      
      open (10,file=
     $ './qvtime.dat',
c    $ 'output/temptime.dat',
     $ status='old',access='sequential',form='unformatted')

C  writes file to current directory     
      open (20,file=
     $'../output/qvtime_easy.dat',
     $ status='new',access='sequential',
     $ form='unformatted')
      
      read(10) exphdr
      read(10) nx,dx
      
      print*, exphdr, nx, dx
      
      read(10,end=99) time2     
      read(10) qv1
      
      print *, 'time =',time2,' qv(nx)=',qv1(nx)
      
      do it = 1, nt
      read(10,end=99) time1 
       read(10) qv1
       print *, 'time =',time1,' qv(nx)=',qv1(nx)
       write(20) qv1
      end do
      
      stop
      
   99 print *, 'end of file at it=',it
   
      stop
      end
c234567890c234567890c234567890c234567890c234567890c234567890c23456789012
