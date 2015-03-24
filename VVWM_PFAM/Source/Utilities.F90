module utilities
!Written by Dirk F. Young (Virginia, USA).
implicit none

contains
!###################################################################################
   pure elemental integer function jd (YEAR,MONTH,DAY)
     !calculate the days since 1/1/1900 given year,month, day, from Fliegel and van Flandern (1968)
    !Fliegel, H. F. and van Flandern, T. C. (1968). Communications of the ACM, Vol. 11, No. 10 (October, 1968). 

     implicit none
     integer, intent(in) :: year,month,day
      JD= day-32075+1461*(year+4800+(month-14)/12)/4+367*(month-2-(month-14)/12*12) /12-3*((year+4900+(month-14)/12)/100)/4 -2415021
    end function jd

!###################################################################################
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


!###################################################################################
subroutine window_average(list, m, n, listout)
!This subroutine finds the running average of the current data plus prevoius "m-1" data
! of the input array "list".
 
implicit none
    integer, intent(in) ::  n			!size of the list
    integer, intent(in) ::  m			!averaging window
    real(8), intent(in) :: list(n)		!data to be averaged
    real(8), intent(out):: listout(n)	!output of averages
    integer:: i

    forall(i=m:n)   listout(i) = sum(list(i-m+1:i))/dble(m)	!calc for days with enough previous data
    forall(i=1:m-1) listout(i) = sum(list(1:i))/dble(i)		!calculation for first days with insufficient data for complete window
end subroutine window_average

!###################################################################################
subroutine WriteInputsWithDescriptors(unitnumber, index)
   !This routine prints out the inputs along with a description ofthe terms
   !Time and Date stamped also
    use variables
    implicit none
    integer, intent(in) :: unitnumber, index
  
    write(unitnumber,*)  '**************** Inputs *******************'
    write(unitnumber,*) 'Chemical # ', index
    write(unitnumber,*) aer_aq(index)       , 'Water Column System Halflife(except photo & hydrolysis = '
    write(unitnumber,*) anae_aq(index)      , 'Benthic Compartment Halflife (except hydrolysis) = '

    write(unitnumber,*) photo(index)        , 'Photolysis Halflife =        ' 
    write(unitnumber,*) hydro(index)        , 'Hydrolysis Halflife =        '
    write(unitnumber,*) MWT(index)          , 'Molecular Wieght =           '
    write(unitnumber,*) VAPR(index)         , 'Vapor Pressure =             '
    write(unitnumber,*) SOL(index)          , 'Solubility =                 '
    write(unitnumber,*) koc(index)          , 'Koc =                        '
    write(unitnumber,*) temp_ref_aer(index) , 'Aerobic Reference Temper =   '
    write(unitnumber,*) temp_ref_anae(index), 'Anaerobic Reference Temper = '   

    write(unitnumber,*) RFLAT(index)        , 'Reference Latitude =         '

    write(unitnumber,*) heat_henry(index)   , 'Enthalpy of Henry =          '
    write(unitnumber,*) temp_ref_henry(index),'Henry Reference Temperature  '
  
   if (index>1) then
      write(unitnumber,*) xAerobic(index-1), 'Aerobic Molar transformation Fraction, Deg #1'  ! transforms are associated with the parent, because calcs are performed at parent time
      write(unitnumber,*) xBenthic(index-1), 'Benthic Molar transformation Fraction, Deg #1'  ! hence, "i-1" is used here
      write(unitnumber,*) xUnflood(index-1), 'Dry Soil Molar transformation Fraction, Deg #1'
      write(unitnumber,*) xPhoto(index-1),   'Photo Molar transformation Fraction, Deg #1'
      write(unitnumber,*) xHydro(index-1),   'Hydroly Molar transformation Fraction, Deg #1'
   end if
 
    write(unitnumber,*)  '****************** End of Run ************************'
    write(unitnumber,*)  '******************************************************'
    write(unitnumber,*)
   end subroutine   WriteInputsWithDescriptors


end module utilities