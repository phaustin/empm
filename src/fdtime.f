
c
c  time.f calculate the time interval, t_prime, between
c  entrainment
c

        subroutine fdtime(t_prime,psigma,t_mean,iseed3)
C data PI/76.92/!mb
        data PI/100./ !mb for 1.0 km^-1 case
C data PI/66.67/!mb for 1.5 km^-1 case
C        data PI/200./!mb for 0.5 km^-1
c data PI/58.82/!mb
C data PI/50./!mb
C data PI/400./!mb for 0.25 km^-1 case
        data w/2.0/      ! rising velocity nomianl case
C* data w/1.0/
	real*8 psigma
        integer*4 iseed3
        real   sratio
c
c  Find t_mean first
c
c write(*,*)'tmen=',t_mean
c do i=1,100

* add the following to modify psigma in order to calculate t_mean.
* see formula (3.2) in theis.
c        psigma=psigma/(1.-psigma)
        sratio=psigma/(1.-psigma)
        print *, 'sratio in fdtime.f is ', sratio

        t_mean=sratio/w*(PI*10.)
ct_mean=.05
c if(psigma.eq.0.)then
        if(sratio.eq.0.)then
        t_mean=1.0e12
        endif

c
c  c is a random # betwwen 0,1 ,alog(1./1-c) will have a mean time 1.
c  and an exponetial distribution, Poisson distribution
c
C open(26,file='fdtime.dat1')
C read(26,*)c
        c = RAND1(iseed3)

        dem=-alog(1.-c)
c*      t_prime=t_mean*dem
c
c this is for ave. time of entm event
c
c*        t_prime = t_mean ! due to entm only
* change time for entrainment event occurs at t=375.1 s, so there is only
* 1 entrainment event only!

         t_prime = 375.1 ! due to entm only

c
c using a average t_prime for each entm event 010796
c
CCt_prime=28.
        write(*,*)'t_mean ', 't_prime '
        write(*,*)t_mean,t_prime
        return
        end

