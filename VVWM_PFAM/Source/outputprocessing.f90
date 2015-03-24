module outputprocessing
!  Written by Dirk F. Young (Virginia, USA).
contains
    subroutine output_processor(index)
    use utilities
    use variables
    use noninputvariables, ONLY: num_records,  &
                                 mavg1_store , &    !array of daily avg. mass 
                                 m1_store,     & 
                                 v,            &    !array of daily volumes 
                                 num_years,    &
                                 first_app_date,  &     ! indexes for first applications of each year
                                 k_burial,  &
                                 k_aer_aq,  &
                                 k_flow,    &
                                 k_hydro,   &
                                 k_photo,   &
                                 k_volatile,   &
                                 k_anaer_aq,   &
                                 gamma_1,  &
                                 gamma_2,  &
                                 fw1,  &
                                 fw2,  &
                                 firstyear, lastyear,startday,firstAppDay,firstAppMonth, aqconc_avg1
                                    
    implicit none
    integer, intent(in) :: index
    real(8):: peak_90	
    real(8):: c4_90
    real(8):: c21_90
    real(8):: c60_90
    real(8):: c90_90
    real(8):: c365_90

    real(8):: benthicpeak_90	
    real(8):: benthic4_90
    real(8):: benthic21_90
    real(8):: benthic60_90
    real(8):: benthic90_90
    real(8):: benthic365_90
    
    real xxx

    real(8),dimension(num_records)::c1
    real(8),dimension(num_years)  :: peak
    real(8),dimension(num_records):: holderarray  !switched to using same array for all averages, to save stack overflow possibility
   
    real(8):: simulation_average            !mmf 3/2015 - for entire simulation average in receiving water body
    
    real(8),dimension(num_years) :: c4_max  !the peak 4-day average within the 365 days after application
    real(8),dimension(num_years) :: c21_max
    real(8),dimension(num_years) :: c60_max
    real(8), dimension(num_years):: c90_max
    real(8),dimension(num_years) :: c365_max !the peak 365-day average within the 365 days after application
    
    integer :: i, ierror,j
    integer ::date_time(8)
    real(8) :: convert					!conversion factor kg/m3 
    integer :: lowYearFlag
    open (UNIT = 11, FILE = outputfilename, STATUS = 'unknown', IOSTAT = ierror)
    open (UNIT = 12, FILE = outputfilename2, STATUS = 'unknown', IOSTAT = ierror)


    c1 = m1_store/v			!vector of daily peaks not averages,

    
    j=0
    do i=firstyear,lastyear
       j=j+1
       first_app_date(j)= jd(i+1900,firstAppMonth,firstAppDay)-startday+1  !jd (YEAR,MONTH,DAY)
    end do
   !in cases where their are few days and a couple of years, the above statement can give a date longer than the days
   ! so the next statement corects this rare problem
     where (first_app_date >num_records)first_app_date=num_records

         !****Calculate Acute values *******************
!    call pick_max(num_years,num_records, first_app_date,c1, peak)

     call window_average(aqconc_avg1(:,index),1,num_records,holderarray)
     call pick_max(num_years,num_records, first_app_date,holderarray,peak) 
     
     call window_average(aqconc_avg1(:,index),4,num_records,holderarray)
     call pick_max(num_years,num_records, first_app_date,holderarray,c4_max) 

     call window_average(aqconc_avg1(:,index),21,num_records,holderarray)
     call pick_max(num_years,num_records, first_app_date,holderarray,c21_max)
     
     call window_average(aqconc_avg1(:,index),60,num_records,holderarray)
     call pick_max(num_years,num_records, first_app_date,holderarray,c60_max)
    
     call window_average(aqconc_avg1(:,index),90,num_records,holderarray)
     call pick_max(num_years,num_records, first_app_date,holderarray,c90_max)
     
     simulation_average = sum(aqconc_avg1(:,index))/num_records  !mmf 3/2015
    
    !treat the 365 day average somewhat differently:
    !In this case, we simply are calculating the average for the 365 day forward from the
    !day of application     
     call window_average(aqconc_avg1(:,index),365,num_records,holderarray)
     forall (i=1:num_years-1) c365_max(i) = holderarray(min(first_app_date(i)+365,num_records))
     c365_max(num_years) = holderarray(num_records)   !due to short year
     
    
    !**find 90th percentiles
    call percent90(peak,num_years, peak_90, lowYearFlag)
    call percent90( c4_max,num_years, c4_90, lowYearFlag)
    call percent90(c21_max,num_years, c21_90, lowYearFlag)
    call percent90(c60_max,num_years, c60_90, lowYearFlag)
    call percent90(c90_max,num_years, c90_90, lowYearFlag)
    call percent90(c365_max,num_years, c365_90, lowYearFlag)

    Write(11,*) "Variable Volume Water Model, Version 0.0"

    call date_and_time(VALUES = date_time)
    write(11,*) 
    write(11,*)  '*******************************************'
    write(11,'("Performed on: ", i2,"/",i2,"/",i4,2x,"at " ,i2,":",i2) ') date_time(2),date_time(3),date_time(1),date_time(5),date_time(6)
    
    select case (SimTypeFlag)
       case(2)
          write(11,*)  "Standard Pond, Area = 10000 m2, Depth = 2 m"
       case(3)
          write(11,*)  "Standard Reservoir, Area = 52555 m2,  Depth = 2.73 m "
       case(4)
          write(11,'("MIXING CELL, Width = ", F8.1, "  Depth= "  ,F8.1,  "  Length = ",F8.1)'  ),  widthMixingCell, depthMixingCell,  lengthMixingCell 
    end select
    
    write(11,*)  'Chemical # ', index
    write(11,*)

    convert = 1000000.
    peak_90 = peak_90*convert
    c4_90   = c4_90*convert
    c21_90  = c21_90*convert
    c60_90  = c60_90*convert
    c90_90  = c90_90*convert
    c365_90 = c365_90*convert
    peak= peak*convert
    c4_max  = c4_max*convert
    c21_max = c21_max*convert
    c60_max = c60_max*convert
    c90_max = c90_max*convert
    c365_max= c365_max*convert
    
    simulation_average = simulation_average*convert   !mmf 3/2015

    if (LowYearFlag ==1)then
      Write(11,*) '* Insufficient years for 1-10 calculation, Only Maximums are Reported Here'
    end if

    write(11, '(''Peak 1-in-10       = '', ES10.2,'' ppb'')'     ) peak_90
    write(11, '(''Chronic 1-in-10    = '', ES10.2,'' ppb'')'     ) c365_90
    write(11, '(''Simulation Avg     = '', G10.3,'' ppb'')'  )   simulation_average
    write(11,*)
    write(11, '(''4-day avg 1-in-10  = '', ES10.2,'' ppb'')'  ) c4_90
    write(11, '(''21-day avg 1-in-10 = '', ES10.2,'' ppb'')'  ) c21_90
    write(11, '(''60-day avg 1-in-10 = '', ES10.2,'' ppb'')'  ) c60_90
    write(11, '(''90-day avg 1-in-10 = '', ES10.2,'' ppb'')'  ) c90_90

    write (11,*)
    write (11,*) 'YEAR    Peak      4-day      21-day     60-day     90-day   Yearly Avg'
    do I=1, num_years  
      write(11,'(I3,1x,6ES11.2)') i,peak(i),c4_max(i), c21_max(i),c60_max(i),c90_max(i),c365_max(i)
    end do

    write(11,*)
    write (11,*) 
    
    write (11,*) "***********************************************************************"
    write (11,*) "Effective compartment halflives averaged over simulation duration:"
    write (11,*)
    xxx = sum(k_flow)
    if (xxx > 0.) then
        write (11,*) "washout halflife (days) =           ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero washout"
    end if
    
    xxx = sum(k_aer_aq)
    if (xxx > 0.) then
        write (11,*) "water col metab halflife (days) =   ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero metabolic"
    end if
    
    
    xxx = sum(k_hydro*fw1)  
    if (xxx > 0.) then
        write (11,*) "hydrolysis halflife (days)  =       ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero hydrolysis"
    end if
   
    xxx =  sum(k_photo*fw1)
    if (xxx > 0.) then
        write (11,*) "photolysis halflife (days)  =       ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero photolysis"
    end if    
    
    
    xxx =  sum(k_volatile*fw1)
    if (xxx > 0.) then
        write (11,*) "volatile halflife (days)  =         ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero volatility"
    end if      
    
    
    xxx =  sum(gamma_1)
    if (xxx > 0.) then
        write (11,*) "total water col halflife (days) =   ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero degradation in water column"
    end if    
    
    write (11,*)
    
    xxx = sum(k_burial)
    if (xxx > 0.) then
        write (11,*) "burial halflife (days)  =           ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero burial"
    end if
    
    
    xxx = sum(k_anaer_aq)
    if (xxx > 0.) then
        write (11,*) "benthic metab halflife (days) =     ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero benthic metab"
    end if
    
       xxx = sum(k_hydro*fw2)
    if (xxx > 0.) then
        write (11,*) "benthic hydrolysis halflife (days) =", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero benthic hydrolysis"
    end if 
    
    
    xxx = sum(gamma_2)
    if (xxx > 0.) then
        write (11,*) "total benthic halflife (days) =     ", 0.69314/(xxx/num_records)/86400
    else
        write (11,*) "zero benthic total degradation"
    end if 
    write (11,*) "***********************************************************************"
    
    write (11,*)
    call WriteInputsWithDescriptors(11, index)
    
    
    write (11,*)
    write (11,*) "***********************************************************************"



!Put daily concentrations in a separate File

    Write(12,*) "Variable Volume Water Model, Version 0.0"

    call date_and_time(VALUES = date_time)
    write(12,*) 
    write(12,*)  '*******************************************'
    write(12,'("Performed on: ", i2,"/",i2,"/",i4,2x,"at " ,i2,":",i2) ') date_time(2),date_time(3),date_time(1),date_time(5),date_time(6)
    
    select case (SimTypeFlag)
       case(2)
          write(12,*)  "Standard Pond"
       case(3)
          write(12,*)  "Standard Reservoir"
       case(4)  
          write(14,*)  "Mixing Cell"
    end select 
          

    
    write(12,*)  'Chemical # ', index
    write(12,*)

    write (12,*) " Day     Daily Peak (ppb)  Daily Average (ppb)" 
    !concentrations are now ll in ppb

    do i = 1, num_records
        write (12, '(I6,4X, ES12.3, 6x, ES12.3 )')  i,c1(i)*convert,aqconc_avg1(i,index)*convert 
    end do
    write (12,*) "***********************************************************************"
    write (12,*)    
       
  
    end subroutine output_processor


    
    subroutine benthic_output_processor(index)
    use utilities
    use variables
    use noninputvariables, ONLY: num_records,  &
                                 num_years,    &
                                 first_app_date,  &     ! indexes for first applications of each year
                                 k_burial,  &
                                 k_aer_aq,  &
                                 k_flow,    &
                                 k_hydro,   &
                                 k_photo,   &
                                 k_volatile,   &
                                 k_anaer_aq,   &
                                 gamma_1,  &
                                 gamma_2,  &
                                 fw1,  &
                                 fw2,  &
                                 firstyear, lastyear,startday,firstAppDay,firstAppMonth, aqconc_avg2
                                    
    implicit none
    integer, intent(in) :: index
    real(8):: peak_90	
    real(8):: c4_90
    real(8):: c21_90
    real(8):: c60_90
    real(8):: c90_90
    real(8):: c365_90

    real xxx

    real(8),dimension(num_years)  :: peak
    real(8),dimension(num_records):: holderarray  !switched to using same array for all averages, to save stack overflow possibility

    real(8),dimension(num_years) :: c4_max  !the peak 4-day average within the 365 days after application
    real(8),dimension(num_years) :: c21_max
    real(8),dimension(num_years) :: c60_max
    real(8), dimension(num_years):: c90_max
    real(8),dimension(num_years) :: c365_max !the peak 365-day average within the 365 days after application
    
    real(8):: sim_average  !mmf 3/2015
        
    integer :: i, ierror,j
    integer ::date_time(8)
    real(8) :: convert					!conversion factor kg/m3 
    integer :: lowYearFlag
    open (UNIT = 13, FILE = outputfilename3, STATUS = 'unknown', IOSTAT = ierror)
    open (UNIT = 14, FILE = outputfilename4, STATUS = 'unknown', IOSTAT = ierror)

    
    j=0
    do i=firstyear,lastyear
       j=j+1
       first_app_date(j)= jd(i+1900,firstAppMonth,firstAppDay)-startday+1  !jd (YEAR,MONTH,DAY)
    end do
   !in cases where their are few days and a couple of years, the above statement can give a date longer than the days
   ! so the next statement corects this rare problem
     where (first_app_date >num_records)first_app_date=num_records

     call window_average(aqconc_avg2(:,index),4,num_records,holderarray)
     call pick_max(num_years,num_records, first_app_date,holderarray,c4_max) 

     call window_average(aqconc_avg2(:,index),21,num_records,holderarray)
     call pick_max(num_years,num_records, first_app_date,holderarray,c21_max)
     
     call window_average(aqconc_avg2(:,index),60,num_records,holderarray)
     call pick_max(num_years,num_records, first_app_date,holderarray,c60_max)
    
     call window_average(aqconc_avg2(:,index),90,num_records,holderarray)
     call pick_max(num_years,num_records, first_app_date,holderarray,c90_max)
     
     sim_average = sum(aqconc_avg2(:,index))/num_records  !mmf 3/2015
     
    
    !treat the 365 day average somewhat differently:
    !In this case, we simply are calculating the average for the 365 day forward from the
    !day of application     
     call window_average(aqconc_avg2(:,index),365,num_records,holderarray)
     forall (i=1:num_years-1) c365_max(i) = holderarray(min(first_app_date(i)+365,num_records))
     c365_max(num_years) = holderarray(num_records)   !due to short year
     


    !****Calculate Acute values *******************
    
    !for benthic I am taking the daily average as the peak, as opposed to the beginning day peak for waer column.  Little difference.
    call pick_max(num_years,num_records, first_app_date,aqconc_avg2(:,index), peak)

   
    !**find 90th percentiles
    call percent90(peak,num_years, peak_90, lowYearFlag)
    call percent90( c4_max,num_years, c4_90, lowYearFlag)
    call percent90(c21_max,num_years, c21_90, lowYearFlag)
    call percent90(c60_max,num_years, c60_90, lowYearFlag)
    call percent90(c90_max,num_years, c90_90, lowYearFlag)
    call percent90(c365_max,num_years, c365_90, lowYearFlag)


    Write(13,*) "Benthic Pore Water Concentrations:  Variable Volume Water Model, Version 0.0"

    call date_and_time(VALUES = date_time)
    write(13,*) 
    write(13,*)  '*******************************************'
    write(13,'("Performed on: ", i2,"/",i2,"/",i4,2x,"at " ,i2,":",i2) ') date_time(2),date_time(3),date_time(1),date_time(5),date_time(6)
    
    select case (SimTypeFlag)
       case(2)
          write(13,*)  "Standard Pond, Area = 10000 m2, Depth = 2 m"
       case(3)
          write(13,*)  "Standard Reservoir, Area = 52555 m2,  Depth = 2.73 m "
       case(4)
          write(13,'("MIXING CELL, Width = ", F8.1, "  Depth= "  ,F8.1,  "  Length = ",F8.1)'  ),  widthMixingCell, depthMixingCell,  lengthMixingCell
          
    end select
    
    write(13,*)  'Chemical # ', index
    write(13,*)

    convert = 1000000.
    peak_90 = peak_90*convert
    c4_90   = c4_90*convert
    c21_90  = c21_90*convert
    c60_90  = c60_90*convert
    c90_90  = c90_90*convert
    c365_90 = c365_90*convert
    peak= peak*convert
    c4_max  = c4_max*convert
    c21_max = c21_max*convert
    c60_max = c60_max*convert
    c90_max = c90_max*convert
    c365_max= c365_max*convert

    sim_average = sim_average*convert   !mmf 3/2015
    
    if (LowYearFlag ==1)then
      Write(13,*) '* Insufficient years for 1-10 calculation, Only Maximums are Reported Here'
    end if

    write(13, '(''Peak 1-in-10       = '', ES10.2,'' ppb'')'     ) peak_90
    write(13, '(''Chronic 1-in-10    = '', ES10.2,'' ppb'')'     ) c365_90
    write(13, '(''Simulation Avg     = '', G10.3,'' ppb'')'  )   sim_average
    write(13,*)
    write(13, '(''4-day avg 1-in-10  = '', ES10.2,'' ppb'')'  ) c4_90
    write(13, '(''21-day avg 1-in-10 = '', ES10.2,'' ppb'')'  ) c21_90
    write(13, '(''60-day avg 1-in-10 = '', ES10.2,'' ppb'')'  ) c60_90
    write(13, '(''90-day avg 1-in-10 = '', ES10.2,'' ppb'')'  ) c90_90

    write (13,*)
    write (13,*) 'YEAR    Peak      4-day      21-day     60-day     90-day   Yearly Avg'
    do I=1, num_years  
      write(13,'(I3,1x,6ES11.2)') i,peak(i),c4_max(i), c21_max(i),c60_max(i),c90_max(i),c365_max(i)
    end do

    write(13,*)
    write (13,*) 
    
    write (13,*) "***********************************************************************"
    write (13,*) "Effective compartment halflives averaged over simulation duration:"
    write (13,*)
    xxx = sum(k_flow)
    if (xxx > 0.) then
        write (13,*) "washout halflife (days) =           ", 0.69314/(xxx/num_records)/86400
    else
        write (13,*) "zero washout"
    end if
    
    xxx = sum(k_aer_aq)
    if (xxx > 0.) then
        write (13,*) "water col metab halflife (days) =   ", 0.69314/(xxx/num_records)/86400
    else
        write (13,*) "zero metabolic"
    end if
    
    
    xxx = sum(k_hydro*fw1)  
    if (xxx > 0.) then
        write (13,*) "hydrolysis halflife (days)  =       ", 0.69314/(xxx/num_records)/86400
    else
        write (13,*) "zero hydrolysis"
    end if
   
    xxx =  sum(k_photo*fw1)
    if (xxx > 0.) then
        write (13,*) "photolysis halflife (days)  =       ", 0.69314/(xxx/num_records)/86400
    else
        write (13,*) "zero photolysis"
    end if    
    
    
    xxx =  sum(k_volatile*fw1)
    if (xxx > 0.) then
        write (13,*) "volatile halflife (days)  =         ", 0.69314/(xxx/num_records)/86400
    else
        write (13,*) "zero volatility"
    end if      
    
    
    xxx =  sum(gamma_1)
    if (xxx > 0.) then
        write (13,*) "total water col halflife (days) =   ", 0.69314/(xxx/num_records)/86400
    else
        write (13,*) "zero degradation in water column"
    end if    
    
    write (13,*)
    
    xxx = sum(k_burial)
    if (xxx > 0.) then
        write (13,*) "burial halflife (days)  =           ", 0.69314/(xxx/num_records)/86400
    else
        write (13,*) "zero burial"
    end if
    
    
    xxx = sum(k_anaer_aq)
    if (xxx > 0.) then
        write (13,*) "benthic metab halflife (days) =     ", 0.69314/(xxx/num_records)/86400
    else
        write (13,*) "zero benthic metab"
    end if
    
       xxx = sum(k_hydro*fw2)
    if (xxx > 0.) then
        write (13,*) "benthic hydrolysis halflife (days) =", 0.69314/(xxx/num_records)/86400
    else
        write (13,*) "zero benthic hydrolysis"
    end if 
    
    
    xxx = sum(gamma_2)
    if (xxx > 0.) then
        write (13,*) "total benthic halflife (days) =     ", 0.69314/(xxx/num_records)/86400
    else
        write (13,*) "zero benthic total degradation"
    end if 
    write (13,*) "***********************************************************************"
    
    write (13,*)
    call WriteInputsWithDescriptors(11, index)
    
    
    write (13,*)
    write (13,*) "***********************************************************************"



!Put daily concentrations in a separate File

    Write(14,*) "Benthic Pore Water Concentrations:  Variable Volume Water Model, Version 0.0"

    call date_and_time(VALUES = date_time)
    write(14,*) 
    write(14,*)  '*******************************************'
    write(14,'("Performed on: ", i2,"/",i2,"/",i4,2x,"at " ,i2,":",i2) ') date_time(2),date_time(3),date_time(1),date_time(5),date_time(6)
    
    select case (SimTypeFlag)
       case(2)
          write(14,*)  "Standard Pond"
       case(3)
          write(14,*)  "Standard Reservoir"
         case(4)  
           write(14,*)  "Mixing Cell"
    end select
    
    write(14,*)  'Chemical # ', index
    write(14,*)

    write (14,*) " Day    Daily Average (ppb)" 
    !concentrations are now ll in ppb

    do i = 1, num_records
        write (14, '(I6,4X, ES12.3)')  i,aqconc_avg2(i,index)*convert 
    end do
    write (14,*) "***********************************************************************"
    write (14,*)    
       

    
    end subroutine benthic_output_processor   
    
    
    
    
    

!***************************************************************
subroutine pick_max (num_years,num_records,bounds,c, output)
!    !this subroutine choses the maximum values of subsets of the vector c
!    !the subsets are defined by the vector "bounds"
!    !maximum values of "c" are chosen from within the c indices defined by "bounds"
!    !output is delivered in the vector "output"
    implicit none
    integer, intent(in) :: num_records
    integer, intent(in) :: num_years
    integer, intent(in) :: bounds(num_years)
    real(8), intent(in), dimension(num_records) :: c
    real(8), intent(out),dimension(num_years) :: output

    integer :: i


   forall (i = 1: num_years-1) output(i) = maxval( c(bounds(i):bounds(i+1)-1) )
  
   output(num_years)= maxval( c(bounds(num_years):num_records) )

end subroutine pick_max



!***************************************************************
subroutine percent90(c_in, n, c_out, lowYearFlag)
    !CALCULATES THE 90TH PERCENTILES
    implicit none

    integer,intent(in) :: n						!number of items in list
    real(8), intent(in), dimension(n):: c_in	!list of items
    real(8),intent(out):: c_out					!output of 90th centile of peaks
    real(8):: f,DEC	  
    integer:: m    
    real(8),dimension(n):: c_sorted
    integer, intent(out) :: LowYearFlag  !if n is less than 10, returns max value and LowYearFlag =1
    LowYearFlag =0



    call hpsort(n,c_sorted, c_in)  !returns a sorted array


    f = 0.9*(n+1)
    m=int(f)
    DEC = f-m  	
    
if (n<10)then
c_out = c_sorted(n)
LowYearFlag = 1
else 
    c_out = c_sorted(m)+DEC*(c_sorted(m+1)-c_sorted(m))
end if

end subroutine percent90
!***************************************************************



!****************************************************************
subroutine hpsort(n,ra,b)
!  from numerical recipes  (should be upgraded to new f90 routine)
	implicit none
	integer,intent(in):: n
	real(8),intent(out),dimension(n)::ra !ordered output array
	real(8),intent(in),dimension(n):: b  !original unordered input array

	integer i,ir,j,l
	real(8) rra
	
	ra=b	! this added to conserve original order

	if (n.lt.2) return

	l=n/2+1
	ir=n
10	continue
	if(l.gt.1)then 
	l=l-1
	rra=ra(l)
	else 
	rra=ra(ir)	
	ra(ir)=ra(1)
	ir=ir-1
	if(ir.eq.1)then 
	ra(1)=rra 
	return
	endif
	endif
	i=l 
	j=l+l
20	if(j.le.ir)then 
		if(j.lt.ir)then
			if(ra(j).lt.ra(j+1))j=j+1 
		endif
		if(rra.lt.ra(j))then 
			ra(i)=ra(j)
			i=j
			j=j+j
		else 
			j=ir+1
		endif
		goto 20
		endif
	ra(i)=rra
	goto 10
end subroutine hpsort
!***********************************************************



end module outputprocessing