!  VVWM_PFAM.f90 
!  Written by Dirk F. Young (Virginia, USA).
!  FUNCTIONS:
!  VVWM_PFAM - Entry point of console application.
!

!****************************************************************************
!
!  PROGRAM: VVWM_PFAM
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************

 program VVWM_PFAM

!This is the version of VVWM that Reads the PFAM output

use PrimarySubroutine
use Variables, ONLY: aer_aq, anae_aq,hydro,koc,MWT,photo,sol,nchem,heat_henry,RFLAT,temp_ref_aer,temp_ref_anae,vapr,  &
                     xAerobic, xBenthic, xUnflood, area,xhydro,xphoto,lat,baseflow,benthic_depth,temp_ref_henry,  &
                     bulk_density, chl,doc1,doc2,bnmas,d_over_dx,depthmixingcell,lengthmixingcell,widthmixingcell,dfac, &
                     burialFlag,froc1,froc2,depth_0,metfilename,outputfilename,plmas,porosity,prben,qt, &
                     outputfilename2,outputfilename3,outputfilename4, simtypeflag,sused,watershed_area,watershed_cn , &
                     Total_applications, dates_of_application, drift_output, paddy_area 
                                         
                     

use nonInputVariables , ONLY:  Mass, PaddyFlow,num_records,startday,firstAppDay,firstAppMonth
use ALLOCATIONmodule

!use utilities_module  !deletable along with the QA line below!

implicit none
integer :: ierror,length,i,j
character (len=256) command
real :: dummy
real(8) :: water1, water2,mass1(3),mass2(3)


call get_command_argument(1,command,length)
open (UNIT = 12, FILE = command, STATUS = 'old', IOSTAT = ierror) 


do i=1,19  !skip the heading material
   read(12,*)
end do


read (12,*) startday
read (12,*) num_records
read (12,*) paddy_area      !area  !area of paddy
read (12,*)       !benthic_depth
read (12,*)       !depth_0   !this is mostly PFAM specific (it is the approximation for zero depth)
read (12,*) nchem

read (12,*) watershed_area
read (12,*) watershed_cn
read (12,*) widthMixingCell
read (12,*) depthMixingCell
read (12,*) lengthMixingCell
read (12,*) baseflow
read (12,*) Total_applications !Number of Spray Drift Events

allocate(dates_of_application(Total_applications))
allocate(drift_output(Total_applications))

read (12,*) dates_of_application
read (12,*) drift_output  !kg/ha

read (12,*)
read (12,*)
read (12,*)

call allocation

mass = 0.0
       
do i=1, num_records
    read(12,*) dummy, water1, dummy, water2, dummy,dummy, ((dummy,dummy, mass1(j), mass2(j),dummy,dummy),j=1,nchem)
    !check units below still not fixed   XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX    XXXXXXXXXXXXXXXXXXXXXXXXx
    !Daily Flow is only the flow from the rice paddies at this point.  Additional water from the watershed is 
    !added in later    I think this is fixed now
    
    PaddyFlow(i) = (water1 +water2)  !this is in meters
    
    do j=1, nchem
       mass(i,1,j) = mass1(j)+mass2(j)
    end do
end do


 !write(98,*) mass(dates_of_application,1,1)
 
 
PaddyFlow=PaddyFlow*paddy_area/86400.  !paddy flow now in m3/sec

do i=1, 6  !skip 
   read(12,*)
end do

read(12,*) aer_aq(1)  
read(12,*) anae_aq(1)
read(12,*)                       !drysoil(1)     
read(12,*) photo(1)         
read(12,*) hydro(1)         
read(12,*) MWT(1)           
read(12,*) VAPR(1)          
read(12,*) SOL(1)           
read(12,*) koc(1)           
read(12,*) temp_ref_aer(1)  
read(12,*) temp_ref_anae(1) 
read(12,*)                     !temp_ref_dry(1)
read(12,*) RFLAT(1)  
read(12,*) heat_henry(1)
read(12,*) temp_ref_henry(1)   

read(12,*)
read(12,*)  firstAppDay
!do i=1, 10
!    read(12,*) !   application info
!end do
read(12,*)  firstAppMonth
read(12,*)
!do i=1, 21
!    read(12,*) !   application info
!end do

 !*********** Location ******************
read(12,'(A256)') metfilename
read(12,*) LAT         

do i=1, 80
    read (12,*)   
end do 

!*****Output File
read(12,'(A256)') outputfilename 
outputfilename = outputfilename(1:len_trim(outputfilename)-4)//  '_ReceivingBodies.txt'
outputfilename2 = outputfilename(1:len_trim(outputfilename)-4)//  '_daily.txt'

outputfilename3 = outputfilename(1:len_trim(outputfilename)-4)//  '_Benthic.txt'
outputfilename4 = outputfilename(1:len_trim(outputfilename)-4)//  '_Benthic_daily.txt'


!****  degradate Information ***************************

read(12,*) 

do i = 2, 3
   read(12,*) aer_aq(i) 
   read(12,*) anae_aq(i)
   read(12,*)                          !drysoil(i)   
   read(12,*) photo(i)         
   read(12,*) hydro(i)         
   read(12,*) MWT(i)           
   read(12,*) VAPR(i)          
   read(12,*) SOL(i)           
   read(12,*) koc(i)           
   read(12,*) temp_ref_aer(i)  
   read(12,*) temp_ref_anae(i) 
   read(12,*)                        !temp_ref_dry(i)
   read(12,*) RFLAT(i)  
   read(12,*) heat_henry(i)
   read(12,*) temp_ref_henry(i)   

   read(12,*) xAerobic(i-1)  ! transforms are associated with the parent, because calcs are performed at parent time
   read(12,*) xBenthic(i-1)  ! hence, "i-1" is used here
   read(12,*) xUnflood(i-1)
   read(12,*) xPhoto(i-1)
   read(12,*) xHydro(i-1)
end do



!*********************************************************************************
       
!Common Standard Water Body parameters
D_over_dx     = 1e-8  

benthic_depth =0.05
porosity      =0.5
bulk_density  =1.35
FROC1         =0.04
FROC2         =0.04
SUSED         =30.0
CHL           =5.0E-003
DOC1          =5.0  
DOC2          =0.0 
PLMAS         =0.0
BNMAS         =0.0
QT            =2.0
DFAC          =1.19
PRBEN         =0.5

burialFlag=0

! Make Pond Run
    AREA    =10000.
    depth_0 = 2.0
    SimTypeFlag =2    !1=vvwm, 2=constant vol w/o flow, 3 = const vol w/flow

    !Add in spray Drift
    mass(dates_of_application,1,1) = mass(dates_of_application,1,1) + drift_output *area/10000.  !area in m2 drift is in kg/ha
    !write(98,*) mass(dates_of_application,1,1)
    call PrimarySubroutineVVWM   


! Make Reservoir Run
    AREA         =52555.
    depth_0       =2.73 
    SimTypeFlag =3    !1=vvwm, 2=constant vol w/o flow, 3 = const vol w/flow
    !Add in spray Drift
    mass(dates_of_application,1,1) = mass(dates_of_application,1,1) + drift_output *area/10000.  !area in m2 drift is in kg/ha
    !write(98,*) mass(dates_of_application,1,1)
    call PrimarySubroutineVVWM  

!Make Mixing Cell Run
    AREA      =  widthMixingCell*lengthMixingCell
    depth_0   =  depthMixingCell
    SimTypeFlag =4    !1=vvwm, 2=constant vol w/o flow, 3 = const vol w/flow, 4 = mixing cell
    !Add in spray Drift
    mass(dates_of_application,1,1) = mass(dates_of_application,1,1) + drift_output *area/10000.  !area in m2 drift is in kg/ha
    !write(98,*) mass(dates_of_application,1,1)
    call PrimarySubroutineVVWM  
    
    
    
call PrimarySubroutineVVWM  






end program VVWM_PFAM

