****************************************************
*
* This is a program to do the triplet mapping for
* droplet inside the linear eddy domain. 
*
*******************************************************

	subroutine drop_map(n,i1,m1,ncell
     1  ,flag,index,ibound1,ibound2)

	dimension index(ibound1:ibound2)
	integer j1,j2
	
c.
c. if this is not the 1st time enter this
c. subroutine, then it is not necessary to 
c. set the I.C.s and B.C.s again, go to 1110
c.


	if (flag.eq.1.)goto 1110
c.
c. Do triplet mapping of droplet
c.
c. set I.C.s and B.C.s
c.
	do j=1,ncell
	index(j)=j
	index(j+ncell)=j
Cindex(j-ncell)=j
	end do

1110	continue
c.
c. Be careful not confused here, N is amount of
c. eddy grid points, I1-M1+1 is the 1st point
c. of triplet mapping (because that index(j)=j,
c. so I1 in LEM is equal I1-M1+1 in DROP_MAP.f 
c. here. Similarily NN (last point of mapping)
c. is I1-M1+N. ALso think about if there is
c. difference of X(i) in LEM and DROP)
c.
	CALL TRIPLET1(N,I1-M1+1,index)
c write(*,*)N,I1-M1+1
c do j=1,ncell 
c write(*,*)j,index(j)
c enddo
	
	NN=I1-M1+N

c
c check if index(j) is negative
c
	do j=I1-M1+1,I1-M1+N

        if (index(j).le.0)then
	write(*,*)n,I1
        write(*,*)'line 128 m',j,'index(m)',index(j)
        end if

	end do
c.
c. set the 1st point (j1) 
c.
	j1=I1-M1+1
	j2=NN
Cwrite(95,*)'jk=',jk,N
Cdo i=j1,j2
Cwrite(95,*)i,index(i)
Cenddo


C
C return to index to the initial value
C

Cdo mp=j1,j2
Cj=mp
Cindex(j-ncell)=index(j)
Cend do

	if(j2.gt.ncell)then
  	do mp=j1,j2
	j=mp
     	index(j-ncell)=index(j)
        enddo
	else 
         do mp=j1,j2
	 j=mp
     	 index(j+ncell)=index(j)
 	 enddo
	endif


        return

        end



*********************************************************
*
*       This is a subroutine to perform triplet mapping
*
*********************************************************

        SUBROUTINE TRIPLET1(KK,K1,A)
	parameter(ibound1=0,ibound2=50000)
        Dimension A(ibound1:ibound2),E(ibound1:ibound2)


c
c       kk is the grid number for the eddy, due to the triplet
c       mapping, M is the 1/3 eddy grid pts
c       the logic of triplet mapping can be described as following
c       example
c       initial scalar: 1,2,3,4,5,6,7,8,9
c       after mapping:/  1,4,7,8,5,2,3,6,9
c
        M=kk/3
        MA=(3*M-1)
        MB=3*M-2
        MC=3*M-3

c**************************************************
c
c       STEP1
c
c**************************************************

        DO K=K1,K1+M-1
        E(K)=A(3*M-MA+K1-1)
        MA=MA-3
        END DO

c********************************************************
c
c       STEP2 and STEP2.1 2nd segment and Block Inversion
c
c*********************************************************
        DO K=M+K1, 2*M+K1-1
        E(K)=A(3*M-MB+K1-1)
        MB=MB-3
        END DO
c
c       block inversion of the 2nd segment
c
        MB1=INT((M-1)/2)+(M+K1)
        MW=2*M+K1-1
        DO K=M+K1,MB1
        B1=E(K)
        E(K)=E(MW)
        
        E(MW)=B1
        MW=MW-1
        END DO

c**********************************************************
c
c       step3 3rd segment
c
c**********************************************************
        DO K=2*M+K1,3*M+K1-1
        E(K)=A(3*M-MC+K1-1)
        MC=MC-3
        END DO

        DO K=K1,KK+K1-1
        A(K)=E(K)
        END DO

        RETURN
        END







