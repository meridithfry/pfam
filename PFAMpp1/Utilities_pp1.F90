module utilities_pp1
!  Written by Dirk F. Young (Virginia, USA).
    contains

    pure elemental integer function jd (YEAR,MONTH,DAY)
     !calculate the days since 1/1/1900 given year,month, day, from Fliegel and van Flandern (1968)
    !Fliegel, H. F. and van Flandern, T. C. (1968). Communications of the ACM, Vol. 11, No. 10 (October, 1968). 

     implicit none
     integer, intent(in) :: year,month,day

      JD= day-32075+1461*(year+4800+(month-14)/12)/4+367*(month-2-(month-14)/12*12) /12-3*((year+4900+(month-14)/12)/100)/4 -2415021

   end function jd
    
    
    

   !******************************************************************************
 pure subroutine get_date (date1900, YEAR,MONTH,DAY)
 !computes THE GREGORIAN CALENDAR DATE (YEAR,MONTH,DAY) given days since 1900
   implicit none

   integer,intent(out) :: YEAR,MONTH,DAY

   integer,intent(in) :: date1900  !days since 1900
   integer :: L,n,i,j

   L= 2483590 + date1900

   n= 4*L/146097

   L= L-(146097*n+3)/4
   I= 4000*(L+1)/1461001
   L= L-1461*I/4+31
   J= 80*L/2447

   day= L-2447*J/80
   L= J/11
   month = J+2-12*L
   year = 100*(N-49)+I+L

 !   YEAR= I
 !  MONTH= J
 !  DAY= K

 end subroutine get_date
   !******************************************************************************

 
 Subroutine Get_yearly_highs(startday, num_records, num_years,relevant_count, daily_depth, min_depth, daily_conc, c_out)
    !Sorts through calendar years tro get highest calendar year values
    !startday = the jd since 1900 of the start of the simulation
    implicit none
    integer, intent(in) :: startday, num_records
    integer, intent(in) :: num_years
    integer, intent(in) :: relevant_count                    !avg period
    real(8), intent(in) :: daily_depth(num_records) !this is the mask by which conc are eliminated
    real(8), intent(in) :: min_depth                !limit on the mask
    real(8), intent(in) :: daily_conc(num_records)  
    
    real(8), intent(out) ::  c_out
    
    real(8), dimension(num_years)     ::  Max_for_year
    real(8), dimension(num_records)   :: avg_conc       !holds the set of concentrations to be averaged
    real :: max_yearly_conc
    

    integer :: i
    integer :: year, month, day
    integer :: lowyearflag  
    integer :: first_day, last_day
    
    real(8) :: Local_daily_conc(num_records) 
    
    Max_for_year = 0.0
    

    where (daily_depth > min_depth) 
        local_daily_conc = daily_conc
    elsewhere
        local_daily_conc = 0.0
    end where
    
    
    avg_conc(1:relevant_count-1) = 0.0
    
    do concurrent (i= relevant_count:num_records)
         avg_conc(i) = sum(local_daily_conc(i-relevant_count+1:i)) /  relevant_count
    end do 
    
    
    
    call get_date (startday, YEAR,MONTH,DAY)  !get the first year

    i=0
    first_day = 1
    do 
        last_day =  jd(year+i, 12,31)-startday+1
        i=i+1 
        if (last_day<num_records) then 
            Max_for_year(i) = maxval(avg_conc(first_day:last_day))
            first_day=last_day+1
        else
            Max_for_year(i) = maxval(avg_conc(first_day:num_records))
            exit
        end if
        
    end do

    
   call  percent90(Max_for_year(1:i), i, c_out, lowYearFlag)
    

  end Subroutine Get_yearly_highs
 
 
 
 
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
 

end module utilities_pp1