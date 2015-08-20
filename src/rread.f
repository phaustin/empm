        real*8 b(4000),c(1000,4000)
	real time(20)
        open(10,file='stime.dat',form='unformatted')
        do i=1,1000
          read(10)time(i),b
           do j=1,4000
             c(i,j)=b(j)
           enddo
        enddo
          do i=1,1000
            write(30,100)time(i),(c(i,j),j=1,50)
            write(31,100)time(i),(c(i,j),j=3001,3050)
          enddo
 100       format(51(1x,e12.5))
	stop
	end
