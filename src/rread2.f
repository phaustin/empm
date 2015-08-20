        real*8 b(50),c(20,51)
	real time(20)
        open(10,file='rtime.dat',form='unformatted')
        do i=1,20
          read(10)time(i),b
           do j=1,50
             c(i,j)=b(j)
           enddo
        enddo
          do i=1,20
            write(30,100)time(i),(c(i,j),j=1,50)
C    write(31,100)c(i,1),(c(i,j),j=3952,4001)
          enddo
 100       format(51(1x,e12.5))
	stop
	end
