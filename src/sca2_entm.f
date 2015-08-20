***************************************************************************
* 
* <Revised date: 03-13-95>
* revise to let ccn contained in entrained clear air 
*
* PORCEDURES FOR RUNNING THIS PROGRAM:
* 1. Using Make command and entm test.data > file name to run DGM code.
* 2. Run Dr. Austin's DGM code to get the output data
*    a. radius b.concerntration c. mass of aerosol d. Qv, Temp...
*    Then read the data.
* 3. Put input data at the beginning of this prgram.BL,teta.....
* 4. Run the code by typing the command of f77 ......(see commmand file)
* (note that intkb.f and fcnkb.f in DGM.f has been changed in this code in
* order to be used intkb0.f and fcnkb0.f)
*
* sca2.f: main program of linear eddy model mixing
* d.f   : droplet tripplet mapping and sedimentation in the LEM domain
* dgm.f : droplet growth model (by diffusion) modified from Dr. P.Austin's
*         code.
*
*
*
* some input data:(for sca2.f)
* nondimensional                   dimensional
* Re                            teta (Komolgorov length scale) 
* L- domain length (BL=1)       BL
* Sc-                           Dtur (turbulent diffusivity)
*                               Dm (molecular diffusivity)
*                               Dtemp (temperature diffusivity)
*           Iteration1 (iteration in one period)         
*           Iteration2 (iteration in one realization)             
*           Mrealization
*           iseed (random # seed)
*           A(I,J) initial scalar condition
* also some input data for this combined program
* ndrop: original droplet categories
* psigma : how many portion of this parcel compared to LEM domain
* qv_cloud: initial qv at cloud
* qv_e: initial qv at parcel
* t_c: initial temp at cloud
* t_e: initial temp at parcel
*
* definition:(wait later to define, note that some of them are nondimen-
*             sional and some are dimensional, do not be confused)
*       teta- Komolgorov length scale
*	BL  - dimensionless domain size (nomalized by domain size L) 	
*       Sc  - Schmit #
*       T  -  total time elasped
*       TD -  time per diffusion step
*       TC -  time steps for turbulent convection 
*       NGRID-grid points in the domain
*       DX -  size between two grid points (dimensionless)
*	Iteration1 - 1st loop index
*	iteration2 - 2nd loop index
*       M0 -  the 1st pt. at half domain distance left to Boundary
*             wall
*       M1 -  1st pt. in the flow domain
*       M2 -  left middle pt. in the domain
*       M3 -  right middle pt in the domain (next to M2)
*       M4 -  last pt in the domain (at the right wall boundary)
*       M6 -  the last pt. at one domain distance right to boundary
*             wall
*	Mrealization - desired # of realizations
*       mreal - index for actual realization
*       DL - chosen random length scale for triple mapping           
*
*       A(2000,3) - the value of scalar
*       B(2000,3) - also the value of scalar at temporary purpose.
*	PDF - the power density function of scalar A
*	PDFave - the averaged PDF at realizations
*	SCALAR - the domain grid points used for PDF calculation.
*	VA1 - the R.M.S. of scalar
*	VA2 - the kurtosis of scalar
*	VA3 - the superskewness of scalar
*	VA4 - the dissipation rate
*       VA5 - the correlation factor
* 	RAND - the random number generator in the UNIX
*
* OUTPUT DATA FILES:
*       in sc2.f: (unit=6)  sca2.dat, radius change at each ip
*                 (unit=60) s2.dat, Qv and Temp at each ip
*                 (unit=50) sca_ave.dat, Qv, Ql, Totwater at each ip
*                 (unit=10) s1atat.dat, scalar1 statistics at each ip
*                 (unit=9)  s2stat.dat, scalar2 statistics at each ip
*                 (unit=22) sca2p1.dat, scalar1 pdf
*                 (unit=23) sca2p2.dat, scalar2 pdf
*                 (unit=20) sca2.dat1, scalar1 average at each ip and mreal
*                 (uint=21) sca2.dat2, scalar2 average at each ip and mreal
*        in dgm.f:(unit=16) dgm.dat, supersaturation
*        in d.f   :(unit=11) d.data, iteration
*         
****************************************************************************
c
c The following parameters are for epsilon equals .01
c

c        Parameter(BL=20.,BL_inte=20.,teta=.01,Dtur=1.17,Dm=4.64e-5,
c     1  Dtemp=4.64e-5)

c
c The following parameters are for epsilon equals 1e-6
c

        Parameter(BL=20.,BL_inte=20.,teta=.01,Dtur=0.054288,
     1  Dm = 2.1544e-6,
     2  Dtemp = 2.1544e-6)


c
c The following parameters are for epsilon equals .005
c
c        Parameter(BL=20.,BL_inte=20.,teta=.01,Dtur=0.9283,
c     2  Dm=3.684e-5,
c     1  Dtemp=3.684e-5)


c
c The following parameters are for epsilon equals .01
c

c
c The following parameters are for epsilon equals .01
c but resolving the Kolmogorov scale, using the real Dm Dtemp

C       Parameter(BL=5.,BL_inte=5.,teta=.0012,Dtur=.184,
C    $  Dm=2.747e-5,
C    1  Dtemp=2.1e-5)

c
c The following parameters are for epsilon equals .01
c but not resolving the Kolmogorov scale

C       Parameter(BL=5.,BL_inte=5.,teta=.01,Dtur=.184,
C    $  Dm=4.5e-5,
C    1  Dtemp=4.5e-5)

c
c The following parameters are for epsilon equals .01
c but resolving the Kolmogorov scale, using the real Dm Dtemp

C       Parameter(BL=1.,BL_inte=1.,teta=.0012,Dtur=.0215,
C    $  Dm=2.747e-5,
C    1  Dtemp=2.1e-5)

c
c The following parameters are for epsilon equals 1.e-4
c but resolving the Kolmogorov scale, using the real Dm Dtemp

C       Parameter(BL=1.,BL_inte=1.,teta=.0036,Dtur=4.64e-3,
C    $  Dm=2.747e-5,
C    1  Dtemp=2.1e-5)

c
c The following parameters are for epsilon equals 1.e-4
c but resolving the Kolmogorov scale, using the real Dm Dtemp
 
C       Parameter(BL=5.,BL_inte=5.,teta=.0036,Dtur=0.04,
C    $  Dm=2.747e-5,
C    1  Dtemp=2.1e-5)

c
c The following parameters are for epsilon equals 1e-6
c but resolving the Kolmogorov scale, using the real Dm Dtemp
CParameter(BL=5.,BL_inte=5.,teta=.012,Dtur=0.0085,
C    $  Dm=2.747e-5,
C    1  Dtemp=2.1e-5)



c
c The following parameters are for epsilon equals 1e-6
c but resolving the Kolmogorov scale, using the real Dm Dtemp
c       Parameter(BL=1.,BL_inte=1.,teta=.012,Dtur=1.e-3,
c    $  Dm=2.747e-5,
c    1  Dtemp=2.1e-5)

c
c The following parameters are for epsilon equals 1e-6
c but not resolving the Kolmogorov scale, using the real Dm Dtemp
C       Parameter(BL=1.,BL_inte=1.,teta=.01,Dtur=1.e-3,
C    $  Dm=2.15e-6,
C    1  Dtemp=2.15e-6)


c
c The following parameters are for epsilon equals 1e-4
c but not resolving the Kolmogorov scale, using the real Dm Dtemp
C       Parameter(BL=1.,BL_inte=1.,teta=.01,Dtur=4.64e-3,
C    $  Dm=1.e-5,
C    1  Dtemp=1.e-5)

c
c The following parameters are for epsilon equals 1e-2
c but not resolving the Kolmogorov scale, using the real Dm Dtemp
c using Dm=10*Dm
C       Parameter(BL=1.,BL_inte=1.,teta=.01,Dtur=0.0215,
C    $  Dm=4.6e-4,
C    1  Dtemp=4.6e-4)

c
c The following parameters are for epsilon equals 1e-10
c but resolving the Kolmogorov scale, using the real Dm Dtemp
c       Parameter(BL=1.,BL_inte=1.,teta=.12,Dtur=4.64e-5,
c    $  Dm=2.747e-5,
c    1  Dtemp=2.1e-5)


c
c The following parameters are for epsilon equals .01
c
C       Parameter(BL=100.,BL_inte=100.,teta=.01,Dtur=10.0,Dm=2.1e-4,
C    $  Dtemp=2.1e-4)


C       Parameter(BL=100.,BL_inte=20.,teta=.01,Dtur=1.17,Dm=4.64e-5,
C    $  Dtemp=4.64e-5)




c
c The following parameters are for epsilon equals .001
c

C Parameter(BL=20.,BL_inte=20.,teta=.01,Dtur=0.54,Dm=2.14e-5,
C    1  Dtemp=2.14e-5)

c
c The following parameters are for epsilon = .1
c

C       Parameter(BL=20.,BL_inte=20.,teta=.01,Dtur=2.52,Dm=1.e-4,
C    1  Dtemp=1.e-4)

c
c The following parameters are for epsilon = .0001
c

C       Parameter(BL=20.,BL_inte=20.,teta=.01,Dtur=0.25,Dm=1.e-5,
C    1  Dtemp=1.e-5)


C       Parameter(BL=100.,BL_inte=20.,teta=.01,Dtur=0.25,Dm=1.e-5,
C    1  Dtemp=1.e-5)


c
c The following parameters are for epsilon = .00001
c

C     Parameter(BL=20.,BL_inte=20.,teta=.01,Dtur=0.12,Dm=4.6e-6,
C    1  Dtemp=4.6e-6)

c
c The following parameters are for epsilon = .000001
c

C       Parameter(BL=20.,BL_inte=20.,teta=.01,Dtur=0.05,Dm=2.1e-6,
C    1  Dtemp=2.1e-6)

C       Parameter(BL=100.,BL_inte=20.,teta=.01,Dtur=0.05,Dm=2.1e-6,
C    1  Dtemp=2.1e-6)


Cparameter(NE=49,NE1=5000)
Cparameter(idimen=5000)

        parameter(NE=49,NE1=5000)
        parameter(idimen=5000)

c	parameter(psigma=.1) define at the head of the code
c	parameter(qv_c=15.729e-3, qv_e=15.729e-3)
c	parameter(t_c=293.56, t_e=293.56)
        parameter(qv_c=1.57292e-02, t_c=293.56)

c	parameter(qv_cloud=15.726e-3, qv_parcel=15.726e-3)
c	parameter(temp_cloud=293.4, temp_parcel=293.4)

        parameter(MI=32)
        real*8 parry(MI)
        real*8 heiarry(MI)
        real*8 tearry(MI)
        real*8 qwarry(MI)
        real*8 Z
        real*8 p_e,t_e,qv_e
	integer*4 igrid
	parameter(mip=1000)
	parameter(igrid=50000,ibound1=0,ibound2=50000)

	DIMENSION A(0:igrid,2),B(0:igrid,2),SA1(0:igrid),SA2(0:igrid)
      	DIMENSION PDF1(mip,101), PDF2(mip,101)
        DIMENSION SCALAR1(101), SCALAR2(101)
	DIMENSION PDFave1(mip,101), PDFave2(mip,101)

CDIMENSION SA1VA1(mip), SA1VA1ave(mip)
CDIMENSION SA1VA2(mip), SA1VA2ave(mip)
C       DIMENSION SA1VA3(mip), SA1VA3ave(mip)
C       DIMENSION SA1VA4(mip), SA1VA4ave(mip)
C       DIMENSION SA1VA5(mip), SA1VA5ave(mip)

C       DIMENSION SA2VA1(mip), SA2VA1ave(mip)
C       DIMENSION SA2VA2(mip), SA2VA2ave(mip)
C       DIMENSION SA2VA3(mip), SA2VA3ave(mip)
C       DIMENSION SA2VA4(mip), SA2VA4ave(mip)
C       DIMENSION SA2VA5(mip), SA2VA5ave(mip)

	dimension dsize(idimen),csize(idimen),rnr(idimen)
	dimension D_diff(2)
        DIMENSION jcell_pop(0:10,igrid)
        integer   index
        dimension index(ibound1:ibound2)
cd_m(600,7)
C
C Note in order to make computer time shorter, the array must
C be made as smaller as possible, so Q(20,2000)
C means that the ip (periods of iteration in one
C realization should be kept less than 20. For
C example iteration1=25 then iteration2=500
C

	real*8 temp_ave, qv_ave, qltot,ql
	real*8 qv,qltot0,totwatmix
     1  ,qltot0_mass
	real*8 a,b,sa1,sa2,dsize,csize,rnr
	real*8 pdf1,pdf2,scalar1,scalar2,pdfave1,pdfave2
	real*8 radius,t_use
	real*8 ppsigma,psigma
	real*8 qv_sum,temp_sum
	real*8 qv_cal,ww
c
c NEW PARAMETER
c
	real*8 dis(NE),www,new_con(NE1)
	integer ndrop
	real*8 radius_drop(NE1),mass_drop(NE1),x(idimen)
	real*8 DX
	real*8 con_drop(NE1)
	integer icat(idimen)
	real*8 vel_ver, press, height,press0,height0
	real*8 qlwater(igrid), SuS(igrid),qwater(idimen)
	real*8 es,qs
	real*8 ttemp
	real*8 ew

	real*8  qw(igrid),sl(igrid),Tv(igrid),buoy(igrid)
        real*8 Th1(igrid),Th2(igrid),Th3(igrid),Th4(igrid)
        real*8 th1_mean(0:mip),th2_mean(0:mip),th3_mean(0:mip)
     $        ,th4_mean(0:mip)
	real*8 th1_rms(0:mip),th2_rms(0:mip),th3_rms(0:mip),
     $  th4_rms(0:mip)
	
	real*8 th1_mean_ave(0:mip),th2_mean_ave(0:mip),
     $  th3_mean_ave(0:mip),th4_mean_ave(0:mip)
        real*8 th1_rms_ave(0:mip),th2_rms_ave(0:mip),
     $  Th3_rms_ave(0:mip),th4_rms_ave(0:mip)

	real*8 th1_pdf(mip,101),th1_scalar(101)
	real*8 th2_pdf(mip,101),th2_scalar(101)
	real*8 th3_pdf(mip,101),th3_scalar(101)
	real*8 th4_pdf(mip,101),th4_scalar(101)
	real*8 th1_pdfave(mip,101),th2_pdfave(mip,101),
     $  th3_pdfave(mip,101)
	real*8 th4_pdfave(mip,101)

c
c add 0512 for h and SuS
c
        real*8  hm(igrid)
        real*8 Th5(igrid),Th6(igrid),Th7(igrid),Th8(igrid)
        real*8 th5_mean(0:mip),th6_mean(0:mip),th7_mean(0:mip)
     $        ,th8_mean(0:mip)
        real*8 th5_rms(0:mip),th6_rms(0:mip),th7_rms(0:mip),
     $  th8_rms(0:mip)
        real*8 th5_mean_ave(0:mip),th6_mean_ave(0:mip),
     $  th7_mean_ave(0:mip),th8_mean_ave(0:mip)
        real*8 th5_rms_ave(0:mip),th6_rms_ave(0:mip),
     $  Th7_rms_ave(0:mip),th8_rms_ave(0:mip)
        real*8 th5_pdf(mip,101),th5_scalar(101)
        real*8 th6_pdf(mip,101),th6_scalar(101)
        real*8 th7_pdf(mip,101),th7_scalar(101)
        real*8 th8_pdf(mip,101),th8_scalar(101)
        real*8 th5_pdfave(mip,101),th6_pdfave(mip,101)
     $         ,th7_pdfave(mip,101)
        real*8 th8_pdfave(mip,101)

c
c modified for droplet spectrum calculation 03-22-94
c
	real*8 rdr(101), fcon(0:mip,101) 
	real*8 mrnr(idimen)
	real*8 fcon_ave(0:mip,101)
        real*8 TD, TC


	integer ncat,iicat
	integer iteration,iteration1
c integer spoint, fpoint
c
	real*8 press_dgm,height_dgm
	real*8 v(idimen)
Cdimension dx_map(igrid)
	real*8 dx_map(igrid)
	real*8 en_dsize(NE1)
C	DOUBLE PRECISION  RAND
	real mov_fcon(36)
	integer qwbin, qwcell
	parameter(qwbin=61,qwcell=50000)
	real mov_qw_pdf(qwbin),mov_qw(qwcell)
     $      ,mov_qw_scalar(qwbin)     
	real mov_qw_min, mov_qw_max
c
c change mov_qw to mov_s
c

C       real mov_s_pdf(qwbin),mov_s(qwcell)
C    $      ,mov_s_scalar(qwbin)     
C       real mov_s_min, mov_s_max


	real rstd(mip),rmean(mip),rstdsum, rsum
	real s_std(mip),s_mean(mip)
	real*8 r_max(mip)
	integer r_max_index(mip)
        integer actndrop(mip)
	integer nij(idimen)
c
c modified 04-12-95 to let rand can be
c run in orca (SUN system)
c
        real RAND1
        INCLUDE 'dim.inc'

	
	include "interp.h"
c
c modified 05-08-95
c
	parameter (m_index = 4000)! for k = 0.5
Creal*8 time_his(m_index, 3)
	real time_r(m_index,mip)
        real*8 time_x(m_index,mip)
	real time_null(m_index)
	real time_time
	integer t_index(mip), t_ip
	integer nt
	integer index_drop(m_index)

	integer*4 time, time_start, time_end
	real*8 pjw ! a variable for cheking x(i)

	integer*4 iseed1, iseed2, iseed3, iseed4
	integer*4 iseed5, iseed6

* add total number of entained blob
        integer n_blob, n_blob_final
        parameter (n_blob = 9)
        integer n_max
        parameter (n_max = 30)
        integer Xn, spoint(n_max), fpoint(n_max)
        integer i, j, k, m
        integer M2(n_max), M3(n_max)

* info for the run
        character*80 run_info

* add temporary array
        real a_temp1(0:igrid), a_temp2(0:igrid)
        
***********************************************************************
        parameter(run_info =
     &  'run c_eps_1e6, 1-entm, d = 0.1 D, dissip rate = 1e-6, F = 0.5,
*       _________________
     &  nominal RH       ' )

        real r_mean(36), r2_mean(36), r3_mean(36)

* for new used specified time to record history data
        real start_t, end_t
        real hist_array        
        integer iteration3, iteration_s, iteration_e
        real*8 t_interval
        
c
c begin
c
	time_start = time()

        write(*,*)run_info
        
        write(*,*) 'BL = ', BL
        write(*,*) 'BL_inte = ', BL_inte
        write(*,*) 'teta = ', teta
        write(*,*) 'Dtur = ', Dtur
        write(*,*) 'Dm = ', Dm
        write(*,*) 'Dtemp = ', Dtemp


	write(*,*)'with drop sedimentation'
	
C*      iseed1 = 100000
C*      iseed2 = 110000
C*      iseed3 = 120000
C*      iseed4 = 130000
C*      iseed5 = 140000
C*      iseed6 = 150000

        iseed1 = time()
        iseed2 = time()+10000000
        iseed3 = time()+20000000
        iseed4 = time()+30000000
        iseed5 = time()+40000000
        iseed6 = time()+50000000

c       iseed1 =  1119503597
c       iseed2 =  1119513597
c       iseed3 =  1119523597
c       iseed4 =  1119533597
c       iseed5 =  1119543597
c       iseed6 =  1119553597

	print *,'ini seeds',iseed1,iseed2,iseed3,
     $  iseed4,iseed5,iseed6
C* do a initila loop to cal random number
C	do i =1, 100
C	   idum1 = rand1(iseed1)
C	   idum2 = rand1(iseed2)
C          idum3 = rand1(iseed3)
C          idum4 = rand1(iseed4)
C          idum5 = rand1(iseed5)
C          idum6 = rand1(iseed6)
C	end do
        print *,'ini seeds',iseed1,iseed2,iseed3,
     $  iseed4,iseed5,iseed6


	psigma=0.1 ! was 0.1
C psigma=0.03
C psigma=0.
c psigma=psigma/(1.-psigma)
c
c generate the random number
c
c      iseed=78
c CALL SRAND(ISEED)
c. note 10023 is the iseed for the nominal cases
C******* iseed=10023

Ciseed=10023-- this is the ealier one. 060796
c iseed=1002

Ciseed=5637

c
c 04-01-95
c read intepolated data of the Kohr curve.
c note  the range now is from rh=0.9 to 1.0
c which is probably too small for entrained
c air, it need to be modified later.
c
c
c     splinearray is passed to fcnkb and psplint through interp.h
c

C     open(unit=56,file="splinepoints.input",access='sequential',
C    $     status='old',form='formatted')

C     read(56,*) (((splinearray(i,ii,iii),i=1,ncats),ii=1,nvecs),
C    $     iii=1,npoints)


c
c Read Raga's Hawaiian sounding
c
c open (51,file='Raga_ob_low.dat')
	open (51,file='Raga_ob.dat')


Copen (51,file='Raga_ob_70.dat')
        do i=1,MI
        read(51,*)parry(i),heiarry(i),qwarry(i),tearry(i)
 	enddo
	do i = 1, MI
       write(*,55)parry(I),heiarry(I),qwarry(i),tearry(I)
 55     format( 4( 1x,f12.6))
        enddo

    
c
c T is time, TC, TD are time step both for turbulent convection and
c diffision. NGRID is grid points. DX is grid size. M1, M2, M3,
c M4 and M6 is the interval both in the boundary and domain.(No M5)
c Iteration1 is the desired subloop iteration index in one realization
c Iteration2 is the desired loop iteration index for one realization.
c  
c       BL_inte=100.

        Re=(BL_inte/teta)**(4./3.)
        PI=3.1415926535897931
        T=0.
c
c original fomula for fast mixing (small teta)
c
        NGRID=NINT(BL_inte/(teta/6.))*(BL/BL_Inte)
C
C add the value of NGRID to test the size of the cell
C in the case without resolving kscale
C
CNGRID=25000

c new fomula for slow mixing (1e-10 big teta)
c
c       NGRID=NINT(BL_inte/(teta/60.))*(BL/BL_Inte)

        write(*,*)NGRID,teta
        DX=(BL/float(NGRID))
CDX=0.0016667d0
CBL_new=DX*float(ngrid-1)
Cwrite(*,*)'new bl is', bl_new
        D_max=AMAX1(Dtemp,Dm)
        TD=.5*(.5*(DX**2.)/D_max)
	write(*,*)'TD=',TD
	TD=0.015
CTD=3.75e-2
CTD=3.125e-3
CTD=3.75e-4
c
c add a sensitivity test for TD 090596
c

C*TD_new=3.75e-4!sec
C*TD_new=0.058
C*       write(*,*)'old_TD',TD,'new_TD',TD_new
C       TD=TD_new
C TD=0.069
        Tau_L=BL_inte**2./Dtur
        tlambda=(54/5.*Re**1.25)/(BL_inte*Tau_L)
        TC=1./(BL*tlambda)

C.
C. comapre TD and TC to decide how many triplet mapping
C. per diffusion
C.
        if (TD.ge.TC)then
        mtrip=int(TD/TC)
	write(*,*)'mtrip=',mtrip,'TD=',TD,'TC=',TC
	idtrip=1
CTD_new=TC*float(mtrip)
	TD_new=TD
        else
C TD=TC
	idtrip=int(TC/TD)+1
	TD_new=TC/float(idtrip)
        mtrip=1
        write(*,*)'TD is smaller than TC, so let TD=TC/idtrip'
        end if
        write(*,*)'TD_new=',TD_new,'TC=',TC,'DX=',dx,'mtrip=',mtrip
	write(*,*)'Td_old=',TD
	write(*,*)'idtrip=',idtrip

c
c decide which time step to use
c
C	T_use=DMAX1(TD,TC)

	T_use=TD_new
c T_use=.001
c T_use=.1
c t_use=.002
	
CIteration1=11
CIteration2=11000

* ite for set c
       Iteration1=50
       Iteration2=50000

* add used specified start/end time and time intervals for writing
* out droplet history and field data.
       start_t = 0.75  ! sec.
       end_t = 750.    ! sec.
       if((start_t .lt. 0.75) .or. (end_t .gt. 750.))then
          stop 'time must between 0 and 750, try again'
       endif
       

* t_interval must be interger of n * t_use, t_use in most run is 
* 0.015 s.
       t_interval = 0.75 ! sec.
       divide = t_interval / t_use
       write(*,*)'t_interval/t_use', divide
       if (mod(divide, 1.) .gt. 0) then
               write(*,*)mod(divide, 1.)
               stop 't_interval devide by t_use is not an integer 
     $ of t_use'
       endif
              

* add iteration3 for t_interval
       iteration3 = int( t_interval / t_use )

* round of start time and end time to nearest whole number
       start_t = anint(start_t / t_interval) * t_interval
       end_t   = anint(end_t / t_interval) * t_interval 
       iteration_s = start_t / t_use
       interation_e = end_t / t_use

* find the array number for history data
       hist_array = (end_t - start_t) / t_interval + 1

       write(*,*) 'start/end time for writing history data'
       write(*,*) start_t, end_t
       write(*,*) 'time interval is ', t_interval
       write(*,*) 'array of hist. data is', hist_array
       write(*,*) iteration_s, iteration_e

* check if hist_array exceed the max allowed value
       if(hist_array  .gt. mip) then
          stop 'history data array exceeds max.'
       endif  



        
c       Iteration1=1
c       Iteration2=1000


CIteration1=50
CIteration2=31250

CIteration1=50
CIteration2=37500

Cipreal=Iteration2/Iteration1 


CIteration1=4167
CIteration2=50004



C	Iteration1=240
C Iteration2=240000
CIteration1=25
C	Iteration2=25000

CIteration1=50
CIteration2=50000

CIteration1=9066
CIteration2=108792

CIteration1=906
CIteration2=10872

CIteration1=417
CIteration2=5004
CIteration1=191
CIteration2=2292
c.
c. read droplet data from the output of Dr. Austin's DGM code
c. drop.data900 means output after 900 seconds.
c.

C OPEN (UNIT=7,FILE='drop.data.new2')
        OPEN (UNIT=7,FILE='drop.data.new')
        do i=1,NE+6
           read(7,*)radius_drop(i)
        enddo

C
C#MODIFY
C

	press=radius_drop(NE+4)
	vel_ver=2.
C*	vel_ver=1.0
	height=radius_drop(NE+6)

	press0=press
	height0=height

c.
c. claculate the drop concerntration, the previous concerntration
c. rnr(i) is taken from the result of DGM, which is not suitable
c. for LEM
c.

	con=1./(DX*.001*.001)
        DO i=1,idimen
        rnr(i)=con
        end do

	write(*,601)(radius_drop(i),i=1,NE+6)
 601 	format(1x,e12.6)
	read(7,*)(mass_drop(i),con_drop(i),i=1,NE)
	do i=1,NE
	con_drop(i)=con_drop(i)*1.24
	enddo
	
	write(*,602)(mass_drop(i),con_drop(i),i=1,NE)
 602	format(1x,2(1x,e12.6))
	close(7)

	ip=0
Ccall rspec(NE,radius_drop,con_drop,rdr,fcon,ip,NRBIN,NE1)

c
c Try put realization here
c
        Mrealization=1
        DO 999 Mreal=1,Mrealization
        open(57,file='rmov.dat',form='unformatted')
	open(58,file='qwmov.dat',form='unformatted')
	open(62,file='rtime.dat',form='unformatted')
* add binary files for T and qv
	open(66,file='qvtime.dat',form='unformatted')
	open(67,file='temptime.dat',form='unformatted')
        open(68,file='xtime.dat',form='unformatted')
	open(69,file='indextime.dat',form='unformatted')
	open(unit=36,file='scree.dat')
	open(78,file='super.dat')
* add binay files for r_mean, r2_mean, r3_mean
        open(80,file='r_mean_time.dat', form = 'unformatted')
        open(83,file='r2_mean_time.dat', form = 'unformatted')
        open(84,file='r3_mean_time.dat', form = 'unformatted')
	write(*,*)'TEST,TEST****'
	write(94,*)'mreal=',Mreal
	
        ITERATION=0

c
c determine the new droplet distribution of the present
c domain size
c
        call accu(DX,BL,dis,www,new_con,mass_drop,ndrop)
        write(*,*)ndrop,'ndrop'
        num_tot=0

        do i=1,NE
        k=i
        if(dis(i).eq.0.)goto 390
        do j=1,dis(i)
        num_tot=num_tot+1
        dsize(num_tot)=radius_drop(k)
        csize(num_tot)=mass_drop(k)
        rnr(num_tot)=con
        icat(num_tot)=k
        enddo

 390    continue

        enddo

c
c ADD CALCULATION FOR SAMPLING DROPLET SPECTRA
c
        do i=1,ndrop
        mrnr(i)=rnr(i)*dx/BL
        enddo

        nip=iteration2/iteration1+1
        nndrop=ndrop

Ccall rspec(nndrop,dsize,mrnr,rdr,fcon,nip,NRBIN,idimen)
C       call raverage(fcon_ave,fcon,NRBIN,nip,mreal)

        qltot0=0.
        do k=1,ndrop
        qltot0=qltot0+dsize(k)**3
        end do
        qltot0_mass=3.14159*4./3*1000.*qltot0
        qltot0=qltot0_mass/(BL*.001*.001)
        qv=radius_drop(NE+1)
        totwatmix=Qv+qltot0
c write(*,*)'qv,qc,Qw',qltot0,Qv,totwatmix

Cm1=ngrid/2+1
	m1=1
c.
c. call assigd.f to assign the location of the parcel
c. when there is parcel
c. x(i) are the initial positions od the droplets
c.
C ndrop_old=ndrop
	write(*,*)'before assign'
        call assigd(ndrop,ngrid,jcell_pop,dx
     1  ,m1,x,bl,igrid,iseed4)
c write(*,*)(icat(i),i=1,48)
c.
c: ndrop. calculate the initial total liquid water mixing
c. ratio, for these new set of droplets
c.
c write(*,*)ndrop,'ndrop'
        qltot0=0.
        do k=1,ndrop
        qltot0=qltot0+dsize(k)**3
        end do
        qltot0_mass=3.14159*4./3*1000.*qltot0
        qltot0=qltot0_mass/(BL*.001*.001)

c.
c. set previously read unused droplet property to zero
c.
C       do i=ndrop+1,ndrop_old
C       rnr(i)=0.
C       csize(i)=0.
C       dsize(i)=0.
C       icat(i)=0.
C       end do
c.
c. calculate initial water vapor mixing ratio Qv
c. modify psigma to ppsigma according the following
c. scalar assigning scheme.
c.
cppsigma=float(m3-m2+1)/float(ngrid)
c       Qv=qv_c*(1.-ppsigma)+qv_e*ppsigma
c       write(*,*)'new psigma is',ppsigma

c.
c. calculate the initial total water mixing ratio
c.

        totwatmix=Qv+qltot0
c write(*,*)'qv,qc,Qw',qltot0,Qv,totwatmix

c
c 05-09-95 modified to track all the droplets
c including the ones that depleted after entrainment
c events
c
	do i=1,ndrop
	  index_drop(i)=i
	enddo
	ndrop_index=ndrop

c
c  Begin the entrainment
c 

        t_entm=0. !total time elapsed
        t_prime=0. !time interval for entm
        p_e=press0 !mb

c 
c Loop for entrainment inteval begins here.
c

 5500   continue

	call fdtime(t_prime, psigma,t_mean,iseed3)
Ccall fdtime(t_prime, psigma,t_mean)
	time0=t_entm
c***EMPM_checking
c let t_prime = 2.0 s for checking errors
c
C	t_prime = 2.0
c
	t_entm=t_entm+t_prime
c* criterior for no entm event before 375 s.
C       if(ITERATION.eq.0)t_entm = t_entm + 375.
c
c
Ct_entm=1.e6
Ct_entm=300.
c
c Interpolate the Z, qw_e (or qv_e), and T_e
c from Raga's observed data at this time step
c : t_entm.
c
c
ccheck empm
c
	call intpol(time0,vel_ver,MI,parry,heiarry,p_e,Z)
        call intpol(time0,vel_ver,MI,parry,qwarry,p_e,qv_e)
        call intpol(time0,vel_ver,MI,parry,tearry,p_e,t_e)
 	open(76,file='entm.dat')
C       write(76,4646)mreal,t_prime,t_entm,p_e,qv_e,t_mean
 	write(76,4646)mreal,time0,p_e,qv_e,t_e,t_mean
 4646   format(I12,2x,5(2x,e15.6))


* set velocity = 0. after desired entrainement event
* (note: must put this after the intrepolation)
* also set qv_e = 0.0057311 to reduce the RH

        if(time0 .ge. 375.1)then
           vel_ver=0.
c           qv_e = 0.25 * qv_e
        endif

        write(*,*)'vel= ', vel_ver
        write(*,*) 'qv_e = ', qv_e
        write(*,*) 't_e = ', t_e
c.
c. call domain.f to find spoint and fpoint to
c. decide locations of M1, M2,M3.....
c.
C       call domain(psigma,ngrid,Xn,spoint,fpoint,iseed6)

* add n_blob in subroutine domain0701.f        
        call domain(psigma,ngrid,Xn,spoint,fpoint,iseed6, n_blob,
     &              n_blob_final)

C       M1=NGRID/2+1
        M1=1
c M2=SPOINT+M1
c M3=FPOINT+M1

        do i = 1, n_blob_final
          M2(i) = spoint(i)+ M1 - 1
          M3(i) = fpoint(i)+ M1 - 1
        enddo

        M4=NGRID
        M6=2*NGRID+M1-1

c write(*,*)M1,M2,M3,M4,M6
        write(*,*)M1,(M2(i), M3(i), i = 1, n_blob_final), M4, M6
	do i=1,m6
	SuS(i)=0.
	enddo

c
c excluded the droplets inside the insert blob of
c clear air
c
	if(iteration.eq.0)goto 6000
        
        do j=1,ngrid
        jcell_pop(0,j)=0
        end do

        ncut=0
        do k=1,ndrop
        ichange=k
        i=k-ncut
	j=int(x(k)/dx)+m1
Cj=int(x(k)/dx)+m1

        x(i)=x(ichange)
        dsize(i)=dsize(ichange)
        csize(i)=csize(ichange)
        rnr(i)=rnr(ichange)
        icat(i)=icat(ichange)
	qwater(i)=qwater(ichange)
c
c modified 05-12-95
c
	index_drop(i)=index_drop(ichange)

c        if((j.ge.m2).and.(j.le.m3))then
c        ncut=ncut+1
c        else
c        jcell_pop(0,j-m1+1)=jcell_pop(0,j-m1+1)+1
c        jcell_pop(jcell_pop(0,j-m1+1),j-m1+1)=i
c        endif
* change the code due to blob number > 1

        do i = 1, n_blob_final
          if( (j .ge. m2(i) ) .and. (j .le. m3(i)) )then
            ncut=ncut+1
            goto 622
          endif
        enddo
        jcell_pop(0,j-m1+1)=jcell_pop(0,j-m1+1)+1
        jcell_pop(jcell_pop(0,j-m1+1),j-m1+1)=i
 622    continue
        
        enddo
        ndrop=ndrop-ncut
	
c
c if run a case without entrained CCN
c go directly to 6000
c
	 goto 6000

c
c consider entrained ccns here
c
c  open(29,file='en_ccn_low.dat')
        open(29,file='en_ccn.dat')
Copen(29,file='en_ccn70.dat')
        do i=1,49 
        read(29,*)en_dsize(i)
cwrite(*,*)en_dsize(i)
        enddo
	close(29)
c
c sample the ccns suign accu0701.f
c
	ndrop_used=ndrop
	write(*,*)'ndrop',ndrop
	entm_bl=psigma*BL
	
        call accu(DX,entm_bl,dis,www,new_con,mass_drop,nccn)
        num_tot=ndrop

        do i=1,NE
        k=i
        if(dis(i).eq.0.)goto 391
        do j=1,dis(i)
        num_tot=num_tot+1
c
c 05-09-95 modified
c
        ndrop_index=ndrop_index+1
        dsize(num_tot)=en_dsize(k)
        csize(num_tot)=mass_drop(k)
        rnr(num_tot)=con
        icat(num_tot)=k
	qwater(num_tot)=4.18879*(dsize(num_tot)**3.)
     $  *1000.*con
        write(*,*)ndrop_index,dsize(num_tot)
     $  ,qwater(num_tot)
1920    format(i6,2(2x,e12.6))
c
c 05-09-95 modified
c
	index_drop(num_tot)=ndrop_index
        enddo

 391    continue

        enddo
	ndrop=ndrop+nccn
	write(*,*)'ndrop',ndrop,'ccn',nccn

c
c assign the ccn into the portion between m2 and m3
c
	call assigd2(ndrop_used,nccn,m2,m3,jcell_pop,
     $  dx,m1,x,bl,igrid,iseed5)

c
c  establish the initial scalar distribution
c
	
 6000   continue
	write(*,*)'mreal=',mreal, 'ite=',iteration
     $  ,'ndrop=',ndrop
	nparcel=0
        if (iteration .ge. 1) go to 6003

        DO 4  I=M1,M4
        A(I,1)=qv_c
 4      A(I,2)=T_c

CDO 7 I=1,M1-1
C       A(I,1)=A(I+NGRID,1)
C7      A(I,2)=A(I+NGRID,2)

        DO 8 I=M4+1,M6
        A(I,1)=A(I-NGRID,1)
        A(I,2)=A(I-NGRID,2)
 8      CONTINUE
        GOTO 6020


 6003      continue
c        pN=(nparcel/2.)
c        do i=m4,m1,-1
c        a(i,1)=a((i-int(pN)),1)
c        a(i,2)=a((i-int(pN)),2)
c        enddo

c        do 6006 I=M4,M3+1,-1
c        A(I,1)=A((I-nparcel),1)
c 6006   A(I,2)=A((I-nparcel),2)

* add for number of blob > 1
        do 6006 k = 1, n_blob_final
          print *, 'k, M2(k), M3(k)'
          print *, k, M2(k), M3(k)
          do 6005 I = M2(k) ,M3(k)
            A(I, 1) = qv_e
            A(I, 2) = T_e
 6005     continue     
 6006   continue


c          do 6005 I = M2 ,M3
c            A(I,1)=qv_e
c 6005       A(I,2)=T_e

C       DO 6007 I=1,M1-1
C       A(I,1)=A(I+NGRID,1)
C6007   A(I,2)=A(I+NGRID,2)

        DO 6008 I=M4+1,M6
        A(I,1)=A(I-NGRID,1)
        A(I,2)=A(I-NGRID,2)
 6008   CONTINUE

 6020      CONTINUE
Cif(iteration.ge.0)goto 1234

copen(121,file='ic.input')
Cread(121,*)ndrop,press0,height0
Cwrite(*,*)'ndrop',ndrop,'press0',press0,height0
Cpress=press0
Cheight=height0
Copen(122,file='cdrop.input')
Cdo i=1,ndrop
Cread(122,1222)nij(i),dsize(i),csize(i),rnr(i),x(i),icat(i)
C1222 	format(1I5,1x,4(e12.6,1x),1I5)
Cwrite(*,1222)(i),dsize(i),csize(i),rnr(i),x(i),icat(i)
Cenddo

Copen(123,file='cpro.input')
Cdo i=m1,m4
Cread(123,1233)a(I,1),a(I,2)
C1233	format(2(1x,f12.6))
Cwrite(*,1233)a(I,1),a(I,2)
Cenddo

C1234   continue
C
CendChecking
C

CDO I=1,M1-1
C         A(I,1)=A(I+NGRID,1)
C         A(I,2)=A(I+NGRID,2)
cENDDO

c        DO I=M4+1,M6
c          A(I,1)=A(I-NGRID,1)
c          A(I,2)=A(I-NGRID,2)
c        ENDDO

c... let initial jcell_pop(0,j)=0
        do j=m1,m4
          jcell_pop(0,j-m1+1)=0
        end do

        do i=1,ndrop
          pjw=x(i)/dx+m1
          j=int(pjw)
          jcell_pop(0,j-m1+1)=jcell_pop(0,j-m1+1)+1
          jcell_pop(jcell_pop(0,j-m1+1),j-m1+1)=i
        end do

* write out the initial info for the run
       if(iteration .eq. 0) then
         do i = 1, ngrid
            a_temp1(i) = a(i, 1)
            a_temp2(i) = a(i, 2)
         enddo
         write(66) run_info
         write(66) ngrid, dx 
         write(66) iteration * t_use
c        write(66) (a(i,1), i = 1, ngrid)
         write(66) (a_temp1(i), i = 1, ngrid)
         write(67) run_info
         write(67) ngrid, dx 
         write(67) iteration * t_use
c        write(67) (a(i,2), i = 1, ngrid)
         write(67) (a_temp2(i), i = 1, ngrid)
       endif

c.
c. Generate random# both for FL and P1, FL is to decide the length
c. of Eddies size (DL) for tripplet mapping, P1 is to choose the position 
c. (P2) for triplet mapping, N is the grid pts. inside the random
c. eddy size (note that N must be able devided by 3 completely).
c.
cC OPEN (UNIT=6, FILE='sca2.dat')
cwrite(*,*)'initial water total=',totwatmix
c
ccheck empm
c
c
CIf (mod(iteration,iteration1).eq.0)then
Cdo i = 1, ndrop
C        write(*,*)i,radius,"before trip"
Cend do
Cend if

c.
c. flag is to decide where to enter the subprogram d.f
c.
Cif(iteration.eq.0)then
	  flag=0.
Cendif
10 	CONTINUE
C
C check empm error
C
Cwrite(*,*)iteration,(dsize(i),i=1,2),'bf trip'
Cwrite(95,*)'iteration=',iteration
Ccall flush(6)
C write(69,*)'iteration=',iteration
	ITERATION=ITERATION+1
c.
c. Do the triplet mapping, note sometime several
c. mapping occurs only one diffusion.
c.
cwrite(*,*)'before mixing and sedimentation'

	if(mod(iteration,idtrip).gt.0)goto 3030

        DO 15 jk=1,mtrip
C       FL=RAND1(iseed)
	FL=RAND1(iseed1)
        DL=(FL*(BL_inte**(-5./3.)-teta**(-5./3.))
     $  +teta**(-5./3.))**(-3./5.)
        NI=INT(DL/DX)
        PJ=FLOAT(NI)
        N=NINT(PJ/3.)*3
C	P1=RAND1(iseed)
	P1=RAND1(iseed2)
        P2=P1*NGRID
CWRITE(*,*)'N',N,FL,DL,P1,P2
c.
c. perform triplet mapping, I1 is the 1st pt. of the eddy, NN is
c. the last point
c.
        I1=INT(P2)+M1
        NN=N+I1-1
c      open(unit=24,file='drop')
Cwrite(*,*)'jk, N',jk,N
Cwrite(*,*)'before TRIPLET'
        CALL TRIPLET(N,I1,A)

c............
c... call d.f to calulate the droplet new location
c... after triplet mapping and sedimentation
c... i.e., new x(i)
c... note that N, I1, X are eddy grid points ,1st point and
c... droplet positions; jcell_pop are index for droplet position
c............
        CALL DROP_MAP(N,I1,M1,ngrid,flag
     1  ,index,ibound1,ibound2)
        flag=1.
cdo j=1,ngrid
cif(j.ne.index(j))then
cwrite(*,*)j,index(j)
cendif
cenddo

c.
c. reestablish th B.C.s#1, during the triplet mapping, the scalar
c. value outside the domain boundary might be changed. because of the
c. periodic B.C.s we need to built the B.C.s after each mapping
c.
c. Ns is the set of scalars, in this case there are 2 scalars.
c.
        Ns=2
        D_diff(1)=Dm
        D_diff(2)=Dtemp
        DO 38 II=1,Ns
         DO 33 K=I1,NN
C  IF (K .LT. M1) THEN
C A(K+NGRID,II)=A(K,II)
C ENDIF
            IF (K .GT. M4)THEN
              A(K-NGRID,II)=A(K,II)
	    ELSE
	      A(K+NGRID,II)=A(K,II)
            ENDIF
   33     CONTINUE
c.
c.      reestablish B.C.s #2 before peforming diffusion
c.
c         DO 35 J=1,M1-1
c  35     A(J,II)=A(J+NGRID,II)
c         DO 37 J=M4+1,M6
c  37     A(J,II)=A(J-NGRID,II)

  38    CONTINUE
 15     CONTINUE
	

ccheck empm running errors
c
c
Cwrite(*,*)iteration,(dsize(i),i=1,2),"af trip"

C...
C... Excute diffusion by Euler approximation
C... Remember to compare TC and TD, in order to choose the
C... correct time step
C...
CA(0,1)=A(M4,1)
CA(0,2)=A(M4,2)
C       DO 41 II=1,Ns
C          DO 40 K=M1,M4
C  40      B(K,II)=A(K,II)+
C    1     T_use*D_diff(II)*(A(K-1,II)-2.*A(K,II)+A(K+1,II))/(DX**2.)
C             DO K=M1,M4
C             A(K,II)=B(K,II)
C             END DO

C41     continue
c.
c.
c.
c
c calculate dx_map of j_cell
c
        do 30 j=1,ngrid
         m=j
c  write(*,*)m,index(m)
c  dx_map(index(m))=DBLE(m-index(m))*dx
          dx_map(index(m))= (m-index(m)) * dx
       if(dx_map(index(m)).lt.(-1*BL))then
        write(*,*)m,dx_map(index(m))
       endif
 30     continue
c.
c. 3. Dx_map(j) is change in the location of cell j
c. by triplet mapping.
c. Move drops in each cell by this amount.
c.



        do m=m1,m4
          j=m
            if (jcell_pop(0,j-m1+1).gt.0)then
              do k=1,jcell_pop(0,j-m1+1) !for all drops in cell
               i=jcell_pop(k,j-m1+1)

Cif(i.eq.3234)then
Cwrite(*,996)iteration,i,x(i)
Cendif

C write(96,996)j,i,x(i),dx_map(j-m1+1)
C996	format(2(i7),2(2x,e27.15))
c                   if((j.eq.4) .and. (i .eq. 750))then
c                      write(*, 748)j, i, x(i), dx_map(j-m1+1)
c                      write(*, 749) dx, dx_map(j-m1+1)/dx
c                   endif
c 748               format(2(I6),2(2x,e27.15))
c 749               format(2(2x,e27.15))

                   x(i)=x(i)+dx_map(j-m1+1)

Cwrite(96,997)x(i)
C997	format(e27.12)
                 if((x(i).gt.BL).or.(x(i).lt.0.d0))then
                   write(*,*)
		   write(*,*)'jcell= ', j
                   write(*,747)i,x(i),dx_map(j-m1+1)
 747               format(I6,2(2x,e27.15))
 		   write(*,*)'x(i) out of domain'
                 end if
Cif ((x(i).gt.0.830).and. (x(i).lt.0.831))then
C          write(91,*)i,x(i),dx_map(j-m1+1),x(i)-dx_map(j-m1+1)
Cendif
             end do
           end if
        end do
c
c set dx_map to zero again
c
        do j=1,ngrid
          m=j
          dx_map(index(m))=0.
Cwrite(*,*)'m,index(m)',m,index(m)
            index(m)=m
          index(m+ngrid)=m
Cindex(m-ngrid)=m
	enddo

c
c put the diffusion here because sometime Td<tc --082896
c
 3030	continue

        Ns=2
        D_diff(1)=Dm
        D_diff(2)=Dtemp
C...
C... Excute diffusion by Euler approximation
C... Remember to compare TC and TD, in order to choose the
C... correct time step
C...
        A(0,1)=A(M4,1)
        A(0,2)=A(M4,2)
        DO 41 II=1,Ns
           DO 40 K=M1,M4
   40      B(K,II)=A(K,II)+
     1     T_use*D_diff(II)*(A(K-1,II)-2.*A(K,II)+A(K+1,II))/(DX**2.)
              DO K=M1,M4
              A(K,II)=B(K,II)
              END DO

 41     continue

c.
c. calculate the termainal velocity v(i) for
c. each droplet
c.
	rhow=1000.
	rhoa=1.24
	dyn_vis=1.488e-5
	g=9.8
        do i=1,ndrop
        c=(2.*g*rhow)/(9.*dyn_vis*rhoa)
        v(i)=c*(dsize(i)**2.)
Cv(i)=0.
        end do
c...
c... including the terminal velocity for each drop
c...

        do mp=1,ndrop
          i=mp
          x(i)=x(i)+v(i)*t_use
            if (x(i).gt.BL)then
	    x(i)=x(i)-BL
            end if
	    if(x(i).lt.0.)then
	    x(i)=x(i)+BL
	    endif 
        enddo
c...
c... calculate the cell position for each drop
c... after sedimetation
c...

c... let initial jcell_pop(0,j)=0
        do j=m1,m4
          jcell_pop(0,j-m1+1)=0
        end do

        do i=1,ndrop
Cj=INT(x(i)/dx)+m1
	pjw=x(i)/dx+m1
Cj=int(pjw)
	j=int(pjw)
	
c
c check x(i) out of domain
c
C
C if((i.eq.1790).and.(ip.gt.90))then
C    print *, 'drop 1790', ' x(i)', ' j'
C    print *, x(i), j
C  end if

            if((x(i).gt.BL).or.(x(i).lt.0))then
              write(*,*)'after sedimetation,x(i)=',x(i)
              write(*,*)'j=,ite,i,x(i),dx,v(i)',j,ite,i,
     $        x(i),dx,v(i)
            end if
          jcell_pop(0,j-m1+1)=jcell_pop(0,j-m1+1)+1
          jcell_pop(jcell_pop(0,j-m1+1),j-m1+1)=i
        end do
c
c write out the droplets' positions
c

Cwrite(91,*)'iteration=',iteration
Cdo j=1,m4-m1+1
Cif((jcell_pop(0,j).ge.2).and.
C    $  (mod(iteration,iteration1).eq.0))then
Cwrite(91,911)j,jcell_pop(0,j),(x(jcell_pop(i,j))
c    $   ,i=1,jcell_pop(0,j))
C    $   ,(jcell_pop(i,j),i=1,jcell_pop(0,j))
C911    format(I5,1x,I5,2(1x,e25.12),2(1x,I5))
Ccall flush(6)
C       endif
Cenddo

c
c
c
c qltot=0.
Cdo i=m1,m4
Cqlwater(i)=0.
Cenddo

c.
c. call DGM for each drop
c.

c
c initialize the 1st aerosols category
c
	ncat=31
Cif(mod(iteration,10).gt.0)goto 481
Cif(mod(iteration,1000).gt.0)goto 481
	if(mod(iteration,1).gt.0)goto 481

c
c modified 07.06.95
c
Cqltot=0.

        do i=m1,m4
        qlwater(i)=0.
        enddo

	do i = 1, m_index
	  time_null(i) = 0.0
	end do



	Do 480 i=1,ndrop
c.
c. find which cell the drop is
c.
	pjw=x(i)/dx + m1
C	j_cell=INT(pjw)+M1
        j_cell=INT(pjw)
 	radius=dsize(i)
 	iicat=icat(i)

	if(iteration.eq.1)then
	press=press0
	height=height0
	else
	press=press_dgm
	height=height_dgm
	endif

c***empm_check
cwrite dsize(i)before and after dgm
c
c
Cif(i.le.2)then
C write(*,*)iteration,i,dsize(i),"before drop"
Cendif

cwrite(11,*)ite,press,height,'h,p'
	call dgm(radius,A(j_cell,1),A(j_cell,2)
     $  ,SuS(j_cell),press,vel_ver
     1  ,height,qwater(i)
     2  ,csize(i),rnr(i)
     3  ,1,t_use,iteration,iteration1,ncat,iicat)

	dsize(i)=radius
c	qltot=qltot+qlwater(i)
	qlwater(j_cell)=qlwater(j_cell)+qwater(i)
c	write(11,*)i,dsize(i)

	if (qlwater(j_cell).lt.0.)then
        write(85,*)j_cell,
     1 'qlwater(j_cell)=',qlwater(j_cell),qwater(i)
     2 ,dsize(i)
	endif

c
c 11-11-96
c add time series properties here.
c
	
c**check empm
c
c
Cif(i.le.2)then
C       write(*,*)iteration,i,dsize(i),"af drop"
Cendif

         if(MOD(iteration,iteration1).eq.0)then
c
c find the supersaturation using qv/qvs-1
c
          ttemp=A(j_cell,2)
          es=ew(ttemp)*100.
          qs=.622*es/(press-es)
          SuS(j_cell)=a(j_cell,1)/qs-1.
          endif

* change to add new criterior for used specified time
* to record droplet history data
          
c
c write down the time series
c
          t = iteration * t_use
         if( (MOD(iteration,iteration3).eq.0) .and.
     $       ( t .ge. start_t               ) .and.
     $       ( t .le. end_t                 ) 
     $      ) then
c          nt = iteration/iteration1
           nt = (iteration - iteration_s)/iteration3 + 1
           i_index=index_drop(i)
           time_r(i_index,nt) = radius * 1.e6
           time_x(i_index,nt)=x(i)
           time_null(i_index)=1.0
Ctime_s(itime)=Sus(j_cell)
         endif



  480   continue
cwrite(*,*)'after dgm'
c
c

Cwrite(*,*)"vel_ver, qwarry(10)",vel_ver,qwarry(10)

  481   continue
C
C write out time history of droplet radius, supersaturation
C ,and droplet locations
C

c if(MOD(iteration,iteration1).eq.0)then
c t_ip = iteration/iteration1
          t = iteration * t_use
         if( (MOD(iteration,iteration3).eq.0) .and.
     $       ( t .ge. start_t               ) .and.
     $       ( t .le. end_t                 ) 
     $      ) then
          t_ip = (iteration - iteration_s)/iteration3 + 1
 	  t_index(t_ip) = ndrop_index
C  write(*,*)'ndrop_index= ',ndrop_index
	  Do i = 1, m_index
	    if (time_null(i) .eq. 0.0)then
	      time_r(i,t_ip) = 0.0
	      time_x(i,t_ip) = 0.0
	    end if
	  end do

 	 endif


c
cpress=press0-1.09*9.81*vel_ver*iteration*t_use
c       height=height0+vel_ver*t_use*iteration
	press_dgm=press
	height_dgm=height
cwrite(101,*)iteration,press_dgm
c
c try to include the effect of rising velocity
c DT/Dt due to the temperature change
c
        cp=1005.
        g=9.81

        temp_in=-(g/cp)*vel_ver*t_use
        do i=m1,m4
           a(i,2)=a(i,2)+temp_in
        enddo
c.
c. average qv,Temp for different gridcells because of
c. homogeneous mixing
c.
        qv_sum=0.
        temp_sum=0.
        do i=m1,m4
        qv_sum=a(i,1)+qv_sum
        temp_sum=a(i,2)+temp_sum
        end do

        qv_ave=qv_sum/float(ngrid)
        temp_ave=temp_sum/float(ngrid)

c 
c This is for instant mixing case
c      
c	do i=m1,m4
c       a(i,1)=qv_ave
c       a(i,2)=temp_ave
c       end do

	qltot=0
	do i=m1,m4
	qltot=qltot+qlwater(i)
	enddo

c.
c. established periodic B.C.s
c.

        DO 47 II=1,Ns
          DO 45 J=M4+1,M6
  45      A(J,II)=A(J-NGRID,II)
 47     CONTINUE

	
c IF(MOD(ITERATION,iteration1).EQ. 0.and.
c1  Mreal.eq.Mrealization)then


	IF(MOD(ITERATION,iteration1).EQ. 0.)then

c
c TRY TO ADD THE SUS(i) here
c
        do i=m1,m4
        ttemp=a(i,2)
        es=ew(ttemp)*100.
        qs=.622*es/(press-es)
        SuS(i)=a(i,1)/qs-1.
        enddo

	s_datamax=-.99
	s_datamin=+0.1
	call max_min(s_datamax,s_datamin,SuS,m1,m4,igrid)
	sdiff=s_datamax-s_datamin
	write(75,*)s_datamax,s_datamin,sdiff
Ccall flush(6)

c
c calculate r_max 11-11-96
c
C       dropmax=.5
	dropmax = 0.0
	IP=Iteration/Iteration1
        call rmax(dropmax,dsize,ndrop,index_rmax)
	r_max(ip)=dropmax
C print *, 'index_rmax ', index_rmax
	r_max_index(ip)=index_drop(index_rmax)


C open(92,file='ql_inst.dat',form='unformatted')
C open(82,file='t_inst.dat',form='unformatted')
C open(72,file='qv_inst.dat',form='unformatted')

C write(92)(qlwater(i),i=m1,m4)
C write(82)(a(i,2),i=m1,m4)
C write(72)(a(i,1),i=m1,m4)
	
c.
c. average qv,Temp for different Drops
c.
	qv_sum=0.
	temp_sum=0.

	do i=m1,m4
	qv_sum=a(i,1)+qv_sum
	temp_sum=a(i,2)+temp_sum
	end do

	qv_ave=qv_sum/float(ngrid)
	temp_ave=temp_sum/float(ngrid)
	write(94,493)iteration,qv_ave,Temp_ave,qltot/float(ngrid)
     1  ,qv_ave+qltot/float(ngrid)
 493	format(1x,I5,5(1x,e23.16))

	ENDIF


c write(11,*)'iteration=',iteration

	T=T_use*ITERATION
c
c write the spectral data to a movie.file
c modified 10-13-95
c
	if(mod(iteration,iteration1).eq.0)then
        do i= 1,ndrop
           mrnr(i)=rnr(i)*DX/BL
        enddo
	   call mov_rsp(ndrop,dsize,mrnr,rdr,mov_fcon,NRBIN,idimen,
     $                  r_mean, r2_mean, r3_mean)
	   write(57)mov_fcon
* add new binary files for mean of r, r2, r3 with 1 um bin
           write(80)r_mean
           write(83)r2_mean
           write(84)r3_mean

 	do i=m1,m4
  	 mov_qw(i)=A(i,1)+qlwater(i)
 	enddo
 	mov_qw_min=0.0
 	mov_qw_max=0.12
 	call mov_prob(m1,m4,mov_qw_min,mov_qw_max,
     1  mov_qw,mov_qw_pdf,mov_qw_scalar,qwcell,qwbin)
	write(58)mov_qw_pdf

* write out field data
c         do i = 1, ngrid
c            a_temp1(i) = a(i, 1)
c            a_temp2(i) = a(i, 2)
c         enddo
c       write(66) T ! this is time
c       write(66) (a_temp1(i), i = 1, ngrid)
c       write(67) T ! this is time
c       write(67) (a_temp2(i), i = 1, ngrid)

c
c change to mov_s...
c
C       do i=m1,m4
C         mov_s(i)=SuS(i)
C       enddo
C       mov_s_min=-0.20
C       mov_s_max=0.10
C       call mov_prob(m1,m4,mov_s_min,mov_s_max,
C    1    mov_s,mov_s_pdf,mov_s_scalar,qwcell,qwbin)
C       write(58)mov_s_pdf


	endif
* add used specified time to record data

         if( (MOD(iteration,iteration3).eq.0) .and.
     $       ( t .ge. start_t               ) .and.
     $       ( t .le. end_t                 ) 
     $      ) then

* write out field data
            do i = 1, ngrid
              a_temp1(i) = a(i, 1)
              a_temp2(i) = a(i, 2)
            enddo
            write(66) T ! this is time
            write(66) (a_temp1(i), i = 1, ngrid)
            write(67) T ! this is time
            write(67) (a_temp2(i), i = 1, ngrid)

          endif
       
c write(103,*)T_use,Iteration,T

c  
c if not desired iteration, go back to do loop again
c
c IF(MOD(ITERATION,iteration1).GT. 0) GOTO 10

c
c if not desired iteration, go back to do loop again
c including the effect of entrainment time interval
c
        IF((MOD(ITERATION,iteration1).GT. 0).and.
     $  (T.ge.t_entm))then
	goto 5500
	endif
        IF(MOD(ITERATION,iteration1).GT. 0) GOTO 10

c
c Ip is the number of iteration periods in one realization
c
	Ip=INT(iteration/iteration1)
	write(*,*)'Iperiod=',Ip

c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c modified droplet calculations to exclude droplet radius at
c 0-1 um. (12-13-95)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c
c comment out the condition for radius at 0-1 um
c
        rsum=0.
        rstdsum=0.
        nacdrop=ndrop
        do i=1,ndrop
C         if(dsize(i).lt.1.e-6)then
C           nacdrop=nacdrop-1
C           goto 700
C         endif
          rsum=rsum+dsize(i)*1.0e6
 700      continue
        enddo
        rmean(ip)=rsum/float(nacdrop)
        nacdrop=ndrop
        do i=1,ndrop
C         if(dsize(i).lt.1.e-6)then
C           nacdrop=nacdrop-1
C           goto 800
C         endif
          rstdsum=rstdsum+(dsize(i)*1.0e6-rmean(ip))**2.
 800    continue
        enddo
        rmean(ip)=rmean(ip)
        rstd(ip)=((rstdsum/float(nacdrop))**.5)
        actndrop(ip)=nacdrop
        write(77,177)T,rmean(ip),rstd(ip),actndrop(ip)
 177    format(3(1x,f12.4),1(1x,I4))


c::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
c calculate supersaturation based on dropelts (11-11-96)
c:::::::::::::::::::::::::::::::::::::::::::::::::::::::::::::
        s_sum=0.
        s_stdsum=0.
        do i=1,ndrop
	  pjw=x(i)/dx
          j_cell=INT(pjw)+M1
          s_sum=s_sum+SuS(j_cell)
        enddo
        s_mean(ip)=s_sum/float(ndrop)
        do i=1,ndrop
          pjw=x(i)/dx
          j_cell=INT(pjw)+M1
          s_stdsum=s_stdsum+(SuS(j_cell)-s_mean(ip))**2.
        enddo
        s_mean(ip)=s_mean(ip)
        s_std(ip)=((s_stdsum/float(ndrop))**.5)
        write(78,178)T,s_mean(ip),s_std(ip)
  178    format(3(1x,f14.7))


c
c calculate the droplet spectrum
c

c
c modify the droplet concerntration rnr(i) to global con
c initially it is only con in a grid cell
c
	do i= 1,ndrop
	mrnr(i)=rnr(i)*DX/BL
	enddo
c
c claculate drop spec.
c

C	call rspec(ndrop,dsize,mrnr,rdr,fcon,ip,NRBIN,idimen)
C	call raverage(fcon_ave,fcon,NRBIN,ip,mreal)
c
c check
c
c condrop=0.
c do i=1,ndrop
c condrop=condrop+mrnr(i)
c enddo
c write(66,*)'3total con=, ndrop', condrop,ndrop
c con3=0
c do k=1,NRBIN+1
c con3=con3+fcon(ip,k)
c enddo
c write(66,*)'total fcon(1:12,k)=',ip,con3


C...
C... begin  the statistics calculation
C...
C
C separate scalar A(I,1) to SA1(I); and A(I,2) to SA2(I)
C
	DO I=M1,M4 
	SA1(I)=A(I,1)
	SA2(I)=A(I,2)
	END DO

	pNmin1=0.
	pNmin2=270.!K
	pNmax1=0.04 !Kg/Kg
	pNmax2=315. !K
c
c call PROB subroutine to calculate the pdf distribution
c of the scalar. There are 100 bins.
c
	
	CALL PROB(M1,M4,pNmin1,pNmax1,SA1,PDF1,SCALAR1,NBIN,Ip,
     $  mip)
	CALL PROB(M1,M4,pNmin2,pNmax2,SA2,PDF2,SCALAR2,NBIN,Ip,
     $  mip)


c
c by privious subprog. we get the PDF1, PDF2 at
c this realization and iperiod, now it's ready to
c find PDFave1,PDFave2 (the average of the PDF1 and
c PDF2 at differnt realizations
c

        CALL AVERAGE(PDFave1,PDF1,NBIN,Ip,Mreal,mip)
        CALL AVERAGE(PDFave2,PDF2,NBIN,Ip,Mreal,mip)

C
C cancel all the cal for s1stat.dat and s2stat.dat
C
C
C PHI is a subprogram to calculate Phiprime, phiprim2,
C phiprim4, phiprim6.
C

cc       CALL PHI(M1,M4,SA1,s1w1,s1w2,s1w4,s1w6,igrid)
C
C SA1VA1 is the R.M.S. of scalar
C SA1VA2 is the kurtosis of scalar
C SA1VA3 is the superskewness of scalar
C SA1VA4 is the dissipation rate
C SA1VA5 is the correlation factor
C
C same of salar2 such that SA2VA1....
C
C	write(85,*)'THIS IS'
cc       SA1VA1(ip)=s1w1
cc       SA1VA2(ip)=s1w4/s1w2**2.
cc       SA1VA3(ip)=s1w6/s1w2**3.

cc       CALL PHI(M1,M4,SA2,s2w1,s2w2,s2w4,s2w6,igrid)
cc       SA2VA1(ip)=s2w1
cc       SA2VA2(ip)=s2w4/s2w2**2.
cc       SA2VA3(ip)=s2w6/s2w2**3.
c
c DISSIPATION a subroutine to calculate the dissipation rate
c and the correlation.
c
cc       CALL DISSIPATION(M1,M4,SA1,s1d4,s1d5,DX,Dm,igrid)
cc       SA1VA4(ip)=s1d4
cc       SA1VA5(ip)=s1d5

cc       CALL DISSIPATION(M1,M4,SA2,s2d4,s2d5,DX,Dtemp,igrid)
cc       SA2VA4(ip)=s2d4
cc       SA2VA5(ip)=s2d5
c
c average these statistical value of scalars at
c different realization
c
cc       CALL AVERAGE_1(SA1VA1ave,SA1VA1,SA1VA2ave
cc    1  ,SA1VA2,SA1VA3ave,SA1VA3,SA1VA4ave,SA1VA4
cc    2  ,SA1VA5ave,SA1VA5,Ip,Mreal,mip)

cc       CALL AVERAGE_1(SA2VA1ave,SA2VA1,SA2VA2ave
cc    1  ,SA2VA2,SA2VA3ave,SA2VA3,SA2VA4ave,SA2VA4
cc    2  ,SA2VA5ave,SA2VA5,Ip,Mreal,mip)

C
C ADD OTHER STATISTICAL CALCULATION
C
	cp=1005.
	Tv_e=T_e*(1+0.608*qv_e)
	ip=iteration/iteration1

	do i=m1,m4
	qw(i)=sa1(i)+qlwater(i)
	sl(i)=cp*sa2(i)+9.81*height-2.5e6*qlwater(i)
	Tv(i)=sa2(i)*(1.+.608*qw(i)-qlwater(i))
	Buoy(i)=Tv(i)-Tv_e
c
c add 0512
c
	hm(i)=cp*sa2(i)+9.81*height+2.5e6*sa1(i)
	enddo

C
C calculate mean, average and pdf of the above properties.
C
	wdrop=float(ndrop)
	do i=m1,m4
	th1(i)=qlwater(i)
	th2(i)=qw(i)
	th3(i)=sl(i)
c th4(i)=buoy(i)
	th4(i)=wdrop
c
c add 0512 for h and supers
c
	th5(i)=hm(i)
	th6(i)=SuS(i)
c note th7 and th8 are dummy variable, because I am lazy to
c write another subroutine
c th7(i)=hm(i)
c th8(i)=SuS(i)

c change here 2/8/95 add th7(i)=qv(i), th8(i)=temp(i)
c
	th7(i)=sa1(i)
	th8(i)=sa2(i)
	enddo
	
	call mean_rms(m1,m4,TH1,Th2,TH3,Th4,Th1_mean,TH2_mean,
     $  TH3_mean,Th4_mean,Th1_rms,Th2_rms,TH3_rms,TH4_rms,ip,mip,
     $   igrid)

	
C*th1_min=-.0
C*th1_max=20.e-3
C*
C*th2_min=0.005
C*th2_max=60.e-3

C*th3_min=0.08e6
C*th3_max=0.35e6


c th4_min=-12.
c th4_max=10.


C*call prob(m1,m4,th1_min,th1_max,th1,
C*    1  th1_pdf,th1_scalar,nbin,ip,mip)
C*call average(th1_pdfave,th1_pdf,nbin,ip,mreal,mip)

C*       call prob(m1,m4,th2_min,th2_max,th2
C*    2  ,th2_pdf,th2_scalar,nbin,ip,mip)
C*       call average(th2_pdfave,th2_pdf,nbin,ip,mreal,mip)

C*       call prob(m1,m4,th3_min,th3_max,th3
C*    3   ,th3_pdf,th3_scalar,nbin,ip,mip)
C*       call average(th3_pdfave,th3_pdf,nbin,
C*    4  ip,mreal,mip)

c       call prob(m1,m4,th4_min,th4_max,th4,th4_pdf,th4_scalar,nbin,ip)
c       call average(th4_pdfave,th4_pdf,nbin,ip,mreal)

c
c  Average mean and r.m.s. for different realizations
c
        call average_2(TH1_mean_ave,th1_mean,th2_mean_ave,th2_mean,
     1      TH3_mean_ave,TH3_mean,TH4_mean_ave,TH4_mean,ip,mreal,mip)
        call average_2(TH1_rms_ave,Th1_rms,TH2_rms_ave,TH2_rms,
     1      TH3_rms_ave,TH3_rms,TH4_rms_ave,TH4_rms,ip,mreal,mip)


c
c add 0512 for h ad Sus
c

        call mean_rms(m1,m4,TH5,Th6,TH7,Th8,Th5_mean,TH6_mean,
     $  TH7_mean,Th8_mean,Th5_rms,Th6_rms,TH7_rms,TH8_rms,ip,mip,
     $  igrid)


C*       th5_min=80000
C*       th5_max=350000

         th6_min=-0.8 ! was -0.6
         th6_max=.1

C*       th7_min=0.
C*       th7_max=0.02 !kg/kg


C*       th8_min=277.
C*       th8_max=298. !K


C*       call prob(m1,m4,th5_min,th5_max,th5,th5_pdf
C*    1  ,th5_scalar,nbin,ip,mip)
C*       call average(th5_pdfave,th5_pdf,nbin,ip,mreal,mip)

         call prob(m1,m4,th6_min,th6_max,th6,th6_pdf
     1  ,th6_scalar,nbin,ip,mip)
         call average(th6_pdfave,th6_pdf,nbin,ip,mreal,mip)

C*       call prob(m1,m4,th7_min,th7_max,th7,th7_pdf,
C*    1  th7_scalar,nbin,ip,mip)
C*       call average(th7_pdfave,th7_pdf,nbin,ip,mreal,mip)

C*       call prob(m1,m4,th8_min,th8_max,th8,th8_pdf,
C*    1  th8_scalar,nbin,ip,mip)
C*       call average(th8_pdfave,th8_pdf,nbin,ip,mreal,mip)

c
c  Average mean and r.m.s. for different realizations
c
        call average_2(TH5_mean_ave,th5_mean,th6_mean_ave,th6_mean,
     $      TH7_mean_ave,TH7_mean,TH8_mean_ave,TH8_mean,ip,mreal,mip)
        call average_2(TH5_rms_ave,Th5_rms,TH6_rms_ave,TH6_rms,
     $      TH7_rms_ave,TH7_rms,TH8_rms_ave,TH8_rms,ip,mreal,mip)


c
c only write the averaged result at the final realization
c
c IF (Mreal .LT. Mrealization) GOTO 53
c
c write out the scalar statistics
c
c write out the scalar distribution
c
c
c for scalar1
c
c	OPEN (unit=10, file='s1stat.dat')
c       WRITE(10,560)T,SA1VA1ave(ip),SA1VA2ave(ip),
c    1  SA1VA3ave(ip),SA1VA4ave(ip),SA1VA5ave(ip)
c560    FORMAT(1x,E8.3,5(2x,E12.4))
c
c for scalar2
c
c       OPEN (unit=9, file='s2stat.dat')
c       WRITE(9,561)T,SA2VA1ave(ip),SA2VA2ave(ip),
c    1  SA2VA3ave(ip),SA2VA4ave(ip),SA2VA5ave(ip)
c561    FORMAT(1x,E8.3,5(2x,E12.4))

 53 	CONTINUE

c
c check iteration see if it reached the desired iteration for
c one realization
c      
	if((iteration.lt.iteration2).and.(t.ge.t_entm))then
	goto 5500
	endif


Cif((t_entm.gt.1.e5).and.(sdiff.le.0.001))goto 995


	IF (ITERATION .LT. iteration2)GOTO 10
c
c initialize the iteration in order to perform another realization
c
c	Iteration=0

c write(11,*)'realization=',mreal
c
c 	continue another realization
c
c999	CONTINUE

 995	continue

        open (unit=22,file='sca2p1.dat',form='unformatted')
        open (unit=23,file='sca2p2.dat',form='unformatted')
C       write(22,3000)'#,realization=',mreal
C       write(23,3000)'#,realization=',mreal

	ipreal=int(iteration/iteration1)
	write(*,*)'ipreal=',ipreal
        Do jj=1,ipreal
C
C NBIN is the grid cells used for PDF calculation
C Note that the array "m" in PDFave(m,K) is the iteration
C periods in realization
C
	if(jj.eq.1)then
	write(22)scalar1
	write(23)scalar2
	endif
        WRITE(22)(PDFave1(jj,k),k=1,NBIN+1)
        WRITE(23)(PDFave2(jj,k),k=1,NBIN+1)
        END DO
c
c write out the history data of droplet
c radius and locations 
c

	open(unit = 97, file = 't_index.dat')
c do ip = 1, ipreal
        do ip = 1, hist_array
c time_time = ip*t_use*iteration1
          time_time = (ip-1) * t_use * iteration3 + start_t
	  write(62)time_time
C  write(62)(time_r(i,ip), i = 1,t_index(ip))
	  write(62)(time_r(i,ip), i = 1,m_index)
	  write(68)time_time
C  write(68)(time_x(i,ip),i=1,t_index(ip))
	   write(68)(time_x(i,ip),i=1,m_index)
	  write(97,*)t_index(ip)
	enddo
	close(unit = 97)

        open(unit=38,file='ave.dat')
C       write(38,3000)'#,realization=',mreal
	do ip=1,ipreal
	T=ip*t_use*iteration1
        write(38,388)T,TH1_mean_ave(ip),Th1_rms_ave(ip)
     $   ,TH2_mean_ave(ip)
     $   ,TH2_rms_ave(ip)
     $   ,TH3_mean_ave(ip),Th3_rms_ave(ip)
     $   ,Th4_mean_ave(ip),TH4_rms_ave(ip)
     $   ,TH5_mean_ave(ip),Th5_rms_ave(ip)
     $   ,Th6_mean_ave(ip),TH6_rms_ave(ip)
     $   ,TH7_mean_ave(ip),Th7_rms_ave(ip)
     $   ,Th8_mean_ave(ip),TH8_rms_ave(ip)
     $   ,rmean(ip),rstd(ip),actndrop(ip),r_max(ip),r_max_index(ip)
	enddo
 388	format(19(1x,e14.6),1x,1i10,1x,e14.6,1x,1i10)

	open(unit=79,file='findex.dat')
	do i=1,ndrop
	   write(79,179)i,index_drop(i),dsize(i)*1.e6
	enddo
 179    format(2(1x,i6),1x,e12.4)


C*       open(39,file='ql.pdf',form='unformatted')
Cwrite(39,3000)'#,realization=',mreal
Copen(40,file='qw.pdf',form='unformatted')
Copen(41,file='sl.pdf',form='unformatted')

C*       write(40,3000)'#,realization=',mreal
C*       write(41,3000)'#,realization=',mreal
c      write(42,3000)'#,realization=',mreal

C*       open(43,file='h.pdf',form='unformatted')
         open(44,file='sus.pdf',form='unformatted')

C       write(43,3000)'#,realization=',mreal
C       write(44,3000)'#,realization=',mreal


        Do jj=1,ipreal
C
C NBIN is the grid cells used for PDF calculation
C Note that the array "m" in PDFave(m,K) is the iteration
C periods in realization
C

         if(jj.eq.1)then
C*       write(39)th1_scalar
C*       write(40)th2_scalar
C*       write(41)th3_scalar
C*       write(43)th5_scalar
         write(44)th6_scalar
         endif

C*       WRITE(39)(th1_pdfave(jj,K),k=1,nbin+1)
C*       WRITE(40)(th2_pdfave(jj,K),k=1,nbin+1)
C*       WRITE(41)(th3_pdfave(jj,K),k=1,nbin+1)

c       WRITE(42,575)th4_scalar(K),th4_pdfave(1,K),th4_pdfave(2,K),
c    1  th4_pdfave(3,k),th4_pdfave(4,k),th4_pdfave(5,k),
c    2  th4_pdfave(6,K),th4_pdfave(7,K),th4_pdfave(8,K),
c    3  th4_pdfave(9,K),th4_pdfave(10,K),th4_pdfave(11,K),
c    4  th4_pdfave(12,K)
c
c add 0512
c
C*       WRITE(43)(th5_pdfave(jj,K),k=1,nbin+1)
         WRITE(44)(th6_pdfave(jj,K),k=1,nbin+1)
         END DO


c
c initialize the iteration in order to perform another realization
c
        Iteration=0

Cwrite(11,*)'realization=',mreal
        write(10,3000)'#,realization=',mreal
        write(9,3000)'#,realization=',mreal

	close(unit=36)
cC close(unit=6)
	close(unit=60)
cC close(unit=10)
cC close(unit=9)
	close(unit=22)
	close(unit=23)
C*close(unit=39)
C*close(unit=40)
C*close(unit=41)
C*close(unit=42)
C*close(unit=43)
        close(unit = 66)
        close(unit = 67)
	close(unit=44)
	close(unit=70)
	close(unit=38)
	close(unit=11)
	close(unit=57)
        close(unit=80)
        close(unit=83)
        close(unit=84)
        write(50,3000)'#,realization=',mreal
Cclose(unit=50)
 3000   format(a20,i4)
c
c       continue another realization
c


 999    CONTINUE

	time_end = time()
	write(*,*)'elpased time in min. ',
     $  (time_end - time_start)/60.

	
	STOP
	END



****************************************************************************
*
*       This is a subroutine to perform triplet mapping
*
****************************************************************************

	SUBROUTINE TRIPLET(KK,K1,A)
Cparameter(igrid=250000)
        Dimension A(0:50000,2),E(0:50000,2)
	real*8 A,E,B1,B2
c
c kk is the grid number for the eddy, due to the triplet
c mapping, M is the 1/3 eddy grid pts
c the logic of triplet mapping can be described as following
c example
c initial scalar: 1,2,3,4,5,6,7,8,9
c after mapping:  1,4,7,8,5,2,3,6,9
c
        M=kk/3
        MA=(3*M-1)
        MB=3*M-2
        MC=3*M-3
c
c STEP1
c
        DO K=K1,K1+M-1
        E(K,1)=A(3*M-MA+K1-1,1)
        E(K,2)=A(3*M-MA+K1-1,2)
        MA=MA-3
        END DO
c
c STEP2 and STEP2.1 2nd segment and Block Inversion
c
        DO K=M+K1, 2*M+K1-1
        E(K,1)=A(3*M-MB+K1-1,1)
	E(K,2)=A(3*M-MB+K1-1,2)
        MB=MB-3
        END DO
c
c block inversion of the 2nd segment
c
Cwrite(*,*) M, K1, INT((M-1)/2)
        MB1=INT((M-1)/2)+(M+K1)
        MW=2*M+K1-1
        DO K=M+K1,MB1
        B1=E(K,1)
	B2=E(K,2)
        E(K,1)=E(MW,1)
	E(K,2)=E(MW,2)
        E(MW,1)=B1
	E(MW,2)=B2
        MW=MW-1
        END DO
c
c step3 3rd segment 
c
        DO K=2*M+K1,3*M+K1-1
        E(K,1)=A(3*M-MC+K1-1,1)
	E(K,2)=A(3*M-MC+K1-1,2)
        MC=MC-3
        END DO

        DO K=K1,KK+K1-1
        A(K,1)=E(K,1)
	A(K,2)=E(K,2)
        END DO

        RETURN
        END

c***************************************************************************
c
c       This is a subroutine to calculate the pdf distribution
c       of the scalar. There are 100 bins.
c
c       PDF: pwer density function
c       SCALAR: scalar values divided by 100 intervals
c       NBIN: bin numbers for PDF
c       Ipe: every period of one realization, if periods=25 iteration
c       then they are 25, 50, 75, 100...... iteration for periods 1,
c       2, 3, 4, ......
c
c***************************************************************************

        SUBROUTINE PROB(M1,M4,pNmin,pNmax,A,PDF,SCALAR,NBIN,Ipe,
     $  mip)

        integer*4 igrid
        parameter(igrid=50000)
        real*8 A(0:igrid), PDF(mip,101),SCALAR(101)

        NBIN=100
        DO K=1,NBIN
c
c set initail PDF values zero
c
        PDF(Ipe,K)=0.
        END DO
c
c       PDFAREA: the total area of PDF
c       JJ: locate the A(K) into approciate bin #
c       pNmin: min. of scalar value
c       pNmax: max. of scalar value
c
        pNlength=pNmax-pNmin
        PDFAREA=0.
        DO K=M1,M4
        JJ=INT(NBIN*((A(K)-pNmin)/pNlength))+1
        if(jj.ge.101.or. jj.le.0)then
        write(*,*)'pNmin pNmax =', pNmin, ' ',  pNmax
        write(*,*)'jj = ', jj, ' a(k)= ', a(k)
        JJ=100
        endif
c
c accumulate the count # at each interval of JJ
c
        PDF(Ipe,JJ)=PDF(Ipe,JJ)+1.
        END DO
c
c       calculate the total PDFAREA
c
        DDX=(pNlength)/float(NBIN)
        DO K=1,NBIN
        PDFAREA=PDFAREA+PDF(Ipe,K)*DDX
        END DO
c
c       normlaized the PDF and calculate the real scalar
c       value at each bin#
c
        DO K=1,NBIN+1
        PDF(Ipe,K)=PDF(Ipe,K)/PDFAREA
        SCALAR(K)=(K-1.)*DDX+pNmin
        END DO

        RETURN
        END



c***************************************************************************
c
c This is a subroutine to average the  stastics at realizations
c In here it is the PDF.
c STATint: the input value of instaneous value at current realization
c STATave: the ave. value at different real.
c MR: number of realization
c
c***************************************************************************

        SUBROUTINE AVERAGE( STATave, STATint, K,Ipe, MR, mip)
        real*8 STATave(mip,101), STATint(mip,101)


        DO JJ=1,K+1
        STATave(Ipe,JJ)=STATave(Ipe,JJ)*(MR-1)/MR+(STATint(IPe,JJ)/MR)
        END DO
        RETURN
        END

c***************************************************************************
c
c This is a subroutine to average the scalars  at realizations
c Note the array is increased to 2000
c STATint: the input value of instaneous value at current realization
c STATave: the ave. value at different real.
c MR: number of realization
c
c***************************************************************************

C	SUBROUTINE AVERAGE_SCALAR(STATave, STATint,M1,M4, Ipe, MR)
C       real*8 STATave(20,2000), STATint(20,2000)
C       DO JJ=M1,M4
C       STATave(Ipe,JJ)=STATave(Ipe,JJ)*(MR-1)/MR+(STATint(IPe,JJ)/MR)
C       END DO
C       RETURN
C       END

c***************************************************************************
C
C       This is a subprogram to calculate Phiprime, phiprim2,
C       phiprim4, phiprim6.
C       Phiprime=< phi' >
C       Phiprime2=< (phi')^2 >
C	phiprime4=< (phi')^4 >
C	Phiprome6=< (phi')^6 >
C
C***************************************************************************
c comment out
c
cc       SUBROUTINE PHI(M1,M4,BB,PHIRMS,PHIPRIM2,PHIPRIM4,PHIPRIM6,igrid)
cc	integer*4 igrid
cc       real*8 BB(0:igrid)
cc       G1=0.
cc       DO I=M1,M4
cc       G1=G1+BB(I)
cc       END DO

cc       PHIMEAN=G1/(M4-M1+1)
cc       PHIPRIM=0.
cc       PHIPRIM2=0.
cc       PHIPRIM4=0.
cc       PHIPRIM6=0.
cc       DO I=M1,M4
cc       PHIPRIM=PHIPRIM+(BB(I)-PHIMEAN)
cc       PHIPRIM2=PHIPRIM2+((BB(I)-PHIMEAN))**2.
cc       PHIPRIM4=PHIPRIM4+((BB(I)-PHIMEAN))**4.
cc       PHIPRIM6=PHIPRIM6+((BB(i)-PHIMEAN))**6.
cc       END DO

cc       PHIPRIM=PHIPRIM/(M4-M1+1)
cc       PHIPRIM2=PHIPRIM2/(M4-M1+1)
cc       PHIRMS=PHIPRIM2**.5
cc       PHIPRIM4=PHIPRIM4/(M4-M1+1)
cc       PHIPRIM6=PHIPRIM6/(M4-M1+1)
cc       RETURN
cc       END

****************************************************************************
*
*       This a subroutine to calculate the dissipation rate
*       and the correlation.
*
*       BB: scalar field
*       Depsilon: dissipation rate
*       Dro2: correlation factor
*       DM: diffusivity coefficent
*
****************************************************************************

cc       SUBROUTINE DISSIPATION(M1,M4,BB,Depsilon,Dro2,DX,DM,igrid)
ccinteger*4 igrid
cc       real*8 BB(0:igrid)
cc
c 
c Whta's DM here? Sould we change these statistics subroutine to
c become dimensional?
c
c       DM=.035

cc       Dro=0.
cc       DD2=0.


cc       Depsilon=0.
CC TN = FLOAT(M4-M1+1)

CC     BB(M4+1)=BB(M1)
CC      DO 10 K=M1,M4
c
c       differentiate BB(K)
c
c       D1=(BB(K+1)-BB(K))/DX
c       Depsilon=D1**2.+Depsilon
c       D2=BB(K)**2.
c      DD2=DD2+D2
c       D3=DM*(D1**2.)
cD3 = 1.0
c       Dro=Dro+D2*D3
c10      CONTINUE

c       Depsilon=DM*Depsilon/TN
c       DD2=DD2/TN
c       Dro1=Dro/TN
c       Dro2=Dro1/(DD2*Depsilon)-1.
c       RETURN
c       END


****************************************************************************
*
* This is to ave. the statistics quantities.-int is the input
* value, -ave is the ave. value.
* where VA1 - rms of scalar
*       VA2 - kurtosis
*       VA3 - superskewness
*       VA4 - dissipation rate
*       VA5 - correlation factor
*       ipe _ period of interval during the realization
*        MR _ realization
*
****************************************************************************


c       SUBROUTINE AVERAGE_1(sVA1ave,VA1int,sVA2ave,VA2int
c    1  ,sVA3ave, VA3int, sVA4ave, VA4int, sVA5ave, VA5int,ipe,MR,mip)

c       DIMENSION sVA1ave(mip), VA1int(mip)
c       DIMENSION sVA2ave(mip), VA2int(mip)
c       DIMENSION sVA3ave(mip), VA3int(mip)
c       DIMENSION sVA4ave(mip), VA4int(mip)
c       DIMENSION sVA5ave(mip), VA5int(mip)



c       sVA1ave(ipe)=sVA1ave(ipe)*(MR-1)/MR+VA1int(ipe)/MR
c       sVA2ave(ipe)=sVA2ave(ipe)*(MR-1)/MR+VA2int(ipe)/(MR)
c       sVA3ave(ipe)=sVA3ave(ipe)*(MR-1)/MR+VA3int(ipe)/(MR)
c       sVA4ave(ipe)=sVA4ave(ipe)*(MR-1)/MR+VA4int(ipe)/(MR)
c       sVA5ave(ipe)=sVA5ave(ipe)*(MR-1)/MR+VA5int(ipe)/(MR)

c       RETURN
c       END

