!  PFAMpp1.f90 
!  Written by Dirk F. Young (Virginia, USA).
!  Console Application
!
!****************************************************************************
!  Program: PFAMpp1.f90
!  
!  This is a post processor for PFAM.
!  Data is taken from the text file output from the main model
!  and processed here. The intent is that when this is compiled
!  program can be easily change out according to user needs as it
!  is completely independent from the main program.
!
!****************************************************************************

program PFAMpp1
use utilities_pp1
implicit none


! VARIABLES TO BE READ IN FROM FILE:
integer :: startday
integer :: num_records
real(8) benthic_depth, area, bulk_density
real(8):: minimum_depth 
integer :: nchem

integer :: Total_Applications                             !mmf 3/2015
integer,allocatable,dimension(:):: dates_of_application   !mmf 3/2015

real(8),allocatable,dimension(:):: daily_depth
real(8),allocatable,dimension(:):: depth_release     !vector of water releases (m)
real(8),allocatable,dimension(:):: makeupwater
real(8),allocatable,dimension(:):: overflow

real(8),allocatable,dimension(:):: m1_store
real(8),allocatable,dimension(:):: m2_store

real(8),allocatable,dimension(:,:):: mavg1_store
real(8),allocatable,dimension(:,:):: mavg2_store
real(8),allocatable,dimension(:,:):: release_mass
real(8),allocatable,dimension(:,:):: washout_mass 
real(8),allocatable,dimension(:,:):: conc1
real(8),allocatable,dimension(:,:):: conc2
real(8),allocatable,dimension(:,:):: avg_conc1
real(8),allocatable,dimension(:)  :: initial_daily_conc

real(8),allocatable,dimension(:)  :: benthic_total

!mmf  3/2015 - Read in ReceivingBodies_daily.txt, and get mixing cell concentrations:
real(8),allocatable,dimension(:) :: dy, dypeak,dyavg    !day, daily peak, daily avg

!LOCAL VARIABLES
integer :: outfilelength
character(len=11),dimension(3) :: chemical
integer :: i,j,n,k

real(8),dimension(366):: store_conc1, store_Fourday_yearly
real(8) :: peak_yearly,daily_yearly,Fourday_yearly
real(8), dimension(4) :: Fourday

integer :: year, month, day
real(8) :: volume1, volume2

!variables for processing command line argumants
integer:: length, status,ierror
character(len=256):: PPfilename
character(len=256) :: outputfilename      !file name with data to be read
character(len= 30):: dummy
character(len=256) :: processed_outputfile
character(len=256) :: summary_outputfile              !mmf 3/2015 - Additional summary output needed for PFAM Scenario Dvlpmt

real(8),allocatable,dimension(:,:)::  released_conc
real(8) :: C90
real(8),parameter :: min_depth = 0.001   !min Depth for which a concentration will be calculated
real(8) :: depth_mask

character(1),allocatable,dimension(:) :: applied_yn   !mmf 3/2015 - Yes/No - holds whether application date or not

!****************Get output file information from command line ******************
call get_command_argument(1,outputfilename,length)! outputfilename, the full path and name of file to be read (intermediate output from PFAM)


!This is the full path and file name for the post processed file
!PPfilename = trim(outputfilename)//".pp1"  
processed_outputfile  = outputfilename(1:len_trim(outputfilename)-4) // "_ProcessedOutput.txt"
summary_outputfile = outputfilename(1:len_trim(outputfilename)-4) // "_Summary.txt"  !mmf 3/2015

open (UNIT = 32, FILE =processed_outputfile, STATUS = 'unknown', IOSTAT = ierror)
if (ierror /= 0) then
    write(11,*) "There is a problem creating the post proccessed output file."
end if

!************************************************************************************
!*************************************************************************************
!Get Data from Intermediate Output File (the one produded by the main model)
open (UNIT = 31, FILE =outputfilename, STATUS = 'OLD', ACTION = 'READ', IOSTAT = ierror) 

if (ierror /= 0) then
    write(11,*) "No intermediate output file"
    stop
end if


do i=1,19   ! skip lines to get to data
    read (31,*)
end do

read (31,*) startday
read (31,*) num_records  
read (31,*) area
read (31,*) benthic_depth, bulk_density
read (31,*) minimum_depth
read (31,*) nchem
read (31,*)  !watershed area
read (31,*)  !watershed curve number
read (31,*)  !width
read (31,*)  !depth
read (31,*)  !length
read (31,*)  !base flow
read (31,*)  Total_Applications       !Total # Applications - Read this line, added by mmf 3/2015

allocate(dates_of_application(Total_Applications),STAT=status)
read (31,*)  dates_of_application(:)  !Read this line, added by mmf 3/2015

read (31,*)  
read (31,*)  


!skip 2 more lines
read (31,*)
read (31,*)

allocate(daily_depth(num_records), STAT= status)
allocate(depth_release(num_records), STAT= status) 
allocate(makeupwater(num_records), STAT= status)
allocate(overflow(num_records), STAT= status)

allocate(m1_store(num_records), STAT= status)
allocate(m2_store(num_records), STAT= status)

allocate(mavg1_store(num_records,nchem), STAT= status)
allocate(mavg2_store(num_records,nchem), STAT= status)

allocate(release_mass(num_records,nchem), STAT= status)
allocate(washout_mass(num_records,nchem), STAT= status)

allocate(conc1(num_records,nchem), STAT= status)
allocate(conc2(num_records,nchem), STAT= status)

allocate(avg_conc1(num_records,nchem), STAT= status)

allocate(released_conc(num_records,nchem), STAT= status)

allocate(initial_daily_conc(num_records), STAT= status)


allocate(benthic_total(num_records), STAT= status)

do i=1,num_records
          !      1              2                3           4             5            6             7                   8             9                 10                        11             12       
 read (31,*) daily_depth(i),depth_release(i),makeupwater(i),overflow(i), m1_store(i),m2_store(i), ((mavg1_store(i,n),mavg2_store(i,n), release_mass(i,n),washout_mass(i,n), conc1(i,n), conc2(i,n)),  n=1,nchem   )
end do

!********************************************************************************************
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   
!XXXXXXXX                         PROCESS OUTPUT                                   XXXXXXXXXX
!XXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXXX   

    call get_date (STARTDAY, YEAR,MONTH,DAY)    
    WRITE(32,*) 'Contents:'   
    WRITE(32,*) 'Part 1: Record of Water and Mass Releases'
    WRITE(32,*) 'Part 2: Record of Water and Soil/Benthic Concentration'
    WRITE(32,*) 'Part 3: 1-in-10 Year EECs Occuring in the Paddy'
    WRITE(32,*) '*******************************************************************************'
    WRITE(32,*) 'Part 1: Record of Water and Mass Releases'
    WRITE(32,*) '*******************************************************************************'
    WRITE(32,*) 'Records for all events in which pesticide mass was released whether from'
    WRITE(32,*) 'manual releases or from spill over from precipitation.'
    WRITE(32,*) '*******************************************************************************'
    
  !  WRITE(32,'(a11, 21x,3i12))') 'chemical id',  (n, n=1,nchem)
    WRITE(32,2500) 'Event#','Date','Release', 'Released' ,   ('Released    ', n=1,nchem)
    WRITE(32,3000) 'Type          Volume(m3)', (('Conc',n,'(ug/L) '), n=1,nchem)
    
    2500    Format(a6,2x,a4,10x,a7,4x ,4(4x,a8) )
    3000    Format(23x,a24,2x,  3(a4,i1,a7) )
    
    j=0  
    do i=1,num_records
        if (depth_release(i)> 0.) then
           call get_date (startday+i-1, YEAR,MONTH,DAY)
                j=j+1
                released_conc(j,:) = release_mass(i,:)/ (depth_release(i)*area) *1000000.
     
             WRITE(32,5000)  j, month,day,year, " manual release ",   depth_release(i)*area,  (released_conc(j,n),  n=1,nchem) !  ,
        end if
        if (overflow(i)> 0.) then
             call get_date (startday+i-1, YEAR,MONTH,DAY)
             j=j+1          
             released_conc(j,:) = washout_mass(i,:)/(overflow(i)*area)*1000000.
      
             if (daily_depth(i)<=minimum_depth)then
                  WRITE(32,5000)  j, month,day,year,"dry field runoff", overflow(i)*area , (released_conc(j,n),  n=1,nchem)  !
             else
                  WRITE(32,5000)  j, month,day,year,"     overflow   " ,overflow(i)*area, (released_conc(j,n),  n=1,nchem)  ! 
             end if     
        end if   
    end do

    5000    Format(I5, 1x,I2,'/'I2,'/'I4,1x,a16,3x, E10.3,10(2x,E10.3) )

    WRITE(32,*)
    write(32,'("Maximum released concentration = ",  5E10.3, " ppb")')   (maxval(released_conc(:,n)),  n=1,nchem)
    write(32,'("Index for max released concen. = ",  5I10)')   (maxloc(released_conc(:,n)),  n=1,nchem)
    WRITE(32,*)


    !***************************************************************************************************************
    !***********    Write daily concentrations of water body *********************************
    WRITE(32,*) '*******************************************************************************'
    WRITE(32,*) 'Part 2: Record of Water and Soil/Benthic Concentrations' 
    WRITE(32,*)   
    WRITE(32,*) 'Irrig is the amount of water added to maintain weir requirements.'

    WRITE(32,*) 

    WRITE(32,'(A75,3a48)') '****************************  DAILY RECORD  **********************************',('************************************************',n=1,nchem-1)
    WRITE(32,'(A26,2x,4a48)')  '                    Irrig.' ,('Water       Benthic     Benthic  Benthic Areal ', n=1,nchem   )
    WRITE(32,'(A26,2x,4a48))') 'Date      Depth(m)  H2O(m)' ,('Total(ug/L) Total(ug/L) Pore(ug/L) M2/A(kg/m2) ', n=1,nchem   )
    WRITE(32,'(a11, 9x,20i12))') 'chemical id',  (n,n,n,n, n=1,nchem)
    WRITE(32,'(A75,3a48)') '******************************************************************************',('************************************************',n=1,nchem-1)
    Volume2 = benthic_depth*area


    do i= 1, num_records
       call get_date (startday+i-1, YEAR,MONTH,DAY)    
       volume1 = daily_depth(i)*area
       if (daily_depth(i)<=minimum_depth) then  
          WRITE(32,7000) MONTH,Day, YEAR, daily_depth(i),makeupwater(i), ("---------",mavg2_store(i,n)/Volume2*1000000., conc2(i,n)*1000000.,mavg2_store(i,n)/area, n=1,nchem)
       else
           
          WRITE(32,6000) MONTH,Day, YEAR, daily_depth(i),makeupwater(i), ( mavg1_store(i,n)/volume1*1000000., mavg2_store(i,n)/Volume2*1000000., conc2(i,n)*1000000., mavg2_store(i,n)/area, n=1,nchem)
       
       end if
    end do
    
    WRITE(32,*) '*******************************************************************************'
    WRITE(32,*) 'Part 3: Summary of Paddy Concentration Rankings'
    write(32,*)
    

    Chemical(1) = 'Parent     '
    Chemical(2) = 'Degradate 1'
    Chemical(3) = 'Degradate 2'
    do n=1, nchem
    
       Write (32,*) '******** ' // trim(chemical(n)) //  ' 1-in-10 Year Return Concentrations ******************'
       Write (32,*)
       depth_mask = min_depth
       
       select case (n)
       case (1)
           initial_daily_conc =  m1_store/daily_depth/area  !For parent, the "super Peak" is used, i.e., the conc immediately after application
       case default
           initial_daily_conc =conc1(:,n)  !degradate use 24 hour avg
       end select
       
       write(32,*)'********* WATER COLUMN CONCENTRATION (ug/L) ***********'
       call Get_yearly_highs(startday, num_records, 100 ,1     , daily_depth,  depth_mask   , initial_daily_conc, C90)
       write(32,*) 'Water Column Peak                = ', C90*1000000.
     
       !Highest Four-Day Average Water Column
       call Get_yearly_highs(startday, num_records, 100 ,4      , daily_depth,  depth_mask   , conc1(:,n), C90)
       write(32,*) 'Water Column 4-day Avg           = ', C90*1000000.

       !Highest 21-Day Average Water Column
       call Get_yearly_highs(startday, num_records, 100 ,21      , daily_depth,  depth_mask   , conc1(:,n), C90)
       write(32,*) 'Water Column 21-day Avg          = ', C90*1000000.
       
       !Highest 60-Day Average Water Column
       call Get_yearly_highs(startday, num_records, 100 ,60      , daily_depth,  depth_mask   , conc1(:,n), C90)
       write(32,*) 'Water Column 60-day Avg          = ', C90*1000000.
       
       !************  BENTHIC PORE WATER  CONCENTRATION  *****************************
       Write (32,*)
        write(32,*)'********* BENTHIC PORE WATER CONCENTRATION (ug/L) ***********'
       depth_mask=0.0  !no need to use a minimum conc
       !Highest Peak benthic Average Water Column
       call Get_yearly_highs(startday, num_records, 100 , 1      , daily_depth,  depth_mask   , conc2(:,n), C90)
       write(32,*) 'Benthic Pore Water  Peak         = ', C90*1000000.
    
       !Highest Peak benthic Average Water Column
       call Get_yearly_highs(startday, num_records, 100 , 4      , daily_depth,  depth_mask   , conc2(:,n), C90)
       write(32,*) 'Benthic Pore Water 4-day Avg     = ', C90*1000000.
    
    
      !Highest 21-Day benthic Average Water Column
      call Get_yearly_highs(startday, num_records, 100 , 21      , daily_depth,  depth_mask   , conc2(:,n), C90)
      write(32,*) 'Benthic Pore Water 21-day Avg    = ', C90*1000000.
    
      !Highest 60-Day benthic Average Water Column
      call Get_yearly_highs(startday, num_records, 100 , 60      , daily_depth,  depth_mask   , conc2(:,n), C90)
      write(32,*) 'Benthic Pore Water 60-day Avg    = ', C90*1000000.

      
      !************  BENTHIC TOTAL CONCENTRATION  *****************************
      benthic_total = mavg2_store(:,n)/(benthic_depth*area)/(bulk_density)*1000000.  !ppb per dry solid
      Write (32,*)
      write(32,*)'********* BENTHIC TOTAL CONCENTRATION (Mass/Dry Mass)***********'
      !Highest 1-day benthic Average
      call Get_yearly_highs(startday, num_records, 100 , 1      , daily_depth,  depth_mask  , benthic_total, C90)
      write(32,*) 'Benthic Total Conc. Peak         =', C90
    
      !Highest 1-day benthic Average
      call Get_yearly_highs(startday, num_records, 100 , 4      , daily_depth,  depth_mask  , benthic_total, C90)
      write(32,*) 'Benthic Total Conc. 40-Day Avg   =', C90
    
      !Highest 21-Day benthic Average
      call Get_yearly_highs(startday, num_records, 100 , 21      , daily_depth,  depth_mask   , benthic_total, C90)
      write(32,*) 'Benthic Total Conc. 21-day Avg   =  ', C90
     
      !Highest 60-Day benthic Average
      call Get_yearly_highs(startday, num_records, 100 , 60      , daily_depth,  depth_mask   , benthic_total, C90)
      write(32,*) 'Benthic Total Conc. 60-day Avg   =  ', C90
      
      
    
    end do
    
    
    

    6000    Format(I2,'/'I2,'/'I4,1x, F7.4,1x, F7.4,  20(1x,E11.3))   
    7000    Format(I2,'/'I2,'/'I4,1x, F7.4,1x, F7.4,  20(A12,1x,E11.3,1x,E11.3,1x,E11.3)) 
    8000    Format(I2,'/'I2,'/'I4,1x, E12.3,1x, E12.3,1x, E12.3)
   
    !*****************************************************
close(31)
close(32)

!**************************************************************************************************************************************************
!***************** SUMMARY OUTPUT FILE for PFAM Scenario Development Project, as requested by Chuck/Katrina - mmf 3/2015 **************************
!**************************************************************************************************************************************************
!Get daily peak and daily avg mixing cell concentrations (ppb) from "_ReceivingBodies_daily.txt"
open (UNIT = 12, FILE = outputfilename(1:len_trim(outputfilename)-4) // "_ReceivingBodies_daily.txt" , STATUS = 'unknown', IOSTAT = ierror) 
allocate(dy(num_records), STAT= status)
allocate(dypeak(num_records), STAT= status)
allocate(dyavg(num_records), STAT= status)

read(12,*)            !"Variable Volume Water Model, Version 0.0"
do i= 1, 7            !Skip headers
    read(12,*) 
end do
do i=1,num_records    !Std Pond
    read(12,*)
end do
do i=1,10             !Skip headers
    read(12,*)
end do
do i=1,num_records    !Index Reservoir
    read(12,*)
end do
do i=1,9              !Skip headers
    read(12,*)
end do
do i=1,num_records    !Mixing Cell 1
    read(12,*) dy(i),dypeak(i),dyavg(i)  !Day   Daily Peak (ppb)  Daily Average (ppb)
end do

!Create summary output file  - mmf 3/2015
open (UNIT = 36, FILE = summary_outputfile, STATUS = 'unknown', IOSTAT = ierror)
if (ierror /= 0) then
    write(11,*) "There is a problem creating the summary output file."
end if
   
    WRITE(36,*) 'Summary Output File for PFAM Scenario Development Project - 3/2015'
    WRITE(36,*) '*******************************************************************************'

    WRITE(36,2501) 'Day','Date','App','RelType','RelVol(m3)', 'RelConc(ug/L)', 'Depth  ' ,'WaterTot(ug/L)','BenthicTot(ug/L)','BenthicPore(ug/L)','BenthicA(kg/m2)','DailyPeakMixCell(ug/L)','DailyAvgMixCell(ug/L)'                
    2501 Format(a5,2x,a4,8x,a3,9x,a7,8x,a10,2x,a13,2x,a5,6x,a14,1x,a16,1x,a17,2x,a17,2x,a22,2x,a22,2x,a21) 
    
    j=0
    n=1
    allocate(applied_yn(num_records), STAT=status)         !mmf 3/2015  - applied_yn denotes whether application date or not (Y or N)
    
    do i=1,num_records
        call get_date(startday+i-1, YEAR,MONTH,DAY)        !mmf 3/2015
        do k=1,Total_Applications
            if (dates_of_application(k) == i) then
                applied_yn(i) = 'Y'
                exit
            else
                applied_yn(i) = 'N'
            end if
        end do
        
        if (depth_release(i)> 0.) then
            j=j+1
            released_conc(j,:) = release_mass(i,:)/ (depth_release(i)*area) *1000000.
            WRITE(36,5001)  i,month,day,year,applied_yn(i)," manual release ",depth_release(i)*area,(released_conc(j,n),  n=1,nchem),daily_depth(i),( mavg1_store(i,n)/volume1*1000000., mavg2_store(i,n)/Volume2*1000000., conc2(i,n)*1000000., mavg2_store(i,n)/area, n=1,nchem),dypeak(i),dyavg(i)  
        end if
        if (overflow(i)> 0.) then
             j=j+1          
             released_conc(j,:) = washout_mass(i,:)/(overflow(i)*area)*1000000.
      
             if (daily_depth(i)<=minimum_depth)then
                  WRITE(36,5001)  i,month,day,year,applied_yn(i),"dry field runoff", overflow(i)*area ,(released_conc(j,n),  n=1,nchem),daily_depth(i),( mavg1_store(i,n)/volume1*1000000., mavg2_store(i,n)/Volume2*1000000., conc2(i,n)*1000000., mavg2_store(i,n)/area, n=1,nchem),dypeak(i),dyavg(i)  
             else 
                  WRITE(36,5001)  i,month,day,year,applied_yn(i),"     overflow   ",overflow(i)*area,(released_conc(j,n),  n=1,nchem),daily_depth(i),( mavg1_store(i,n)/volume1*1000000., mavg2_store(i,n)/Volume2*1000000., conc2(i,n)*1000000., mavg2_store(i,n)/area, n=1,nchem),dypeak(i),dyavg(i)  
             end if    
        else
             WRITE(36, 5001)  i,month,day,year,applied_yn(i),"      None      ",0.0,0.0,daily_depth(i),( mavg1_store(i,n)/volume1*1000000., mavg2_store(i,n)/Volume2*1000000., conc2(i,n)*1000000., mavg2_store(i,n)/area, n=1,nchem),dypeak(i),dyavg(i)  
        end if   
    
    end do

5001 Format(I5,2x,I2,'/'I2,'/'I4,2x,a3,3x,a16,4x,E10.3,2x,E10.3,5x,E10.3,1x,E10.3,5x,E10.3,7x,E10.3,9x,E10.3,15x,E10.3,15x,E10.3)   !20(2x,E10.3))  !,40(E11.4, 1x))
    
                                                                                                          
    end program PFAMpp1
    
    
    
