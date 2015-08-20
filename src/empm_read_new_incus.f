      program empm_read
c   same code as used to write on incus; should work on incus
c
c read the history data of droplet
c radius and locations 
c
c234567

      parameter(mip=1000)
      parameter (m_index = 4000)
      integer t_index(mip)
      real time_r(m_index,mip), time(mip)
      real*8 time_x(m_index,mip)

      ipreal = 1000
      dt = 0.75
c234567
      open(62,
c    $ file='output/rtime.dat',
     $ file='output/rtime.dat',     
     $ form='unformatted')
     
           open(63,
     $ file='output/xtime.dat',     
     $ form='unformatted')

	open(unit = 97, 
c    $ file = 'output/t_index.dat',
     $ file = 'output/t_index.dat',
     $ form='formatted')

	do ip = 1, ipreal
	  read(97,*)t_index(ip)
           read(62) time_time
           read(63) time_time
           time(ip) = time_time
         write(*,*) ip, t_index(ip), time_time
           read(62) (time_r(i,ip), i = 1, m_index)
           read(63) (time_x(i,ip), i = 1, m_index)

	enddo

	close(unit = 62)
		close(unit = 63)
        close(unit = 97)

c  check for correct reading
	    open(10,file='output/mean_vol_radius.dat')
        do ip = 1, ipreal
        rv3_sum = 0.
          do i = 1,m_index
            rv3_sum = rv3_sum + time_r(i,ip)**3
          end do
            r_avg = ( rv3_sum/t_index(ip) )**(1./3.)
        write(10,*) time(ip),r_avg
        end do
        close(10)
 
	    open(10,file='output/mean_location.dat')
        do ip = 1, ipreal
        x_sum = 0.
          do i = 1,m_index
            x_sum = x_sum + time_x(i,ip)
          end do
            x_avg = ( x_sum/t_index(ip) )
        write(10,*) time(ip),x_avg
        end do
        close(10)

        stop 64

        end
