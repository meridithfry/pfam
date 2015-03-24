module mass_inputs
!  Written by Dirk F. Young (Virginia, USA).
contains

    subroutine ParentMass2
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
        !Given the days and months of the mass applied, this routine sets up a vector with the daily mass applied
        !for the entire simulation.
        
        !This version incorporates slow release of pesticide
        !!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! 
        use utilities_module
        use variables, ONLY: area, minimum_depth, applicationDay, applicationMonth, applicationMass, &
                             slowRelease,num_apps, max_apps, dates_of_application,drift,drift_output,Total_Applications
        use nonInputVariables, ONLY:num_records,daily_depth,startday,& 
                                    m1_input ,&   !subroutine output: mass to water column
                                    m2_input      !subroutine output: mass to benthic
        implicit none
        ! **************Variables Section********************************************************************    
        integer:: firstyear, lastyear, dummy, numyears, status,i   
        integer, allocatable, dimension(:,:) :: app_dates
        real(8), allocatable, dimension(:,:) :: release_fraction
         
        integer, dimension(max_apps) :: release_duration
             
        real(8) :: totalmass(max_apps)

        real(8) :: store, new, total
        integer :: j,k, lastpt
        !*************************************************************
        !Section to find out how many years are in the metfile *******
        call get_date (startday, firstyear ,dummy,dummy)
        call get_date (startday+num_records-1, lastyear,dummy,dummy)
          numyears = lastyear-firstyear+1  
        !*************************************************************
        allocate(app_dates(num_apps ,numyears), STAT = status)
        

        forall(j=1:num_apps,i=1:numyears) app_dates(j,i)= jd(firstyear+i-1,applicationMonth(j),applicationDay(j))-startday+1
 
        
        !!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
        !Info may be redundant, but needed for output to Receiving Water Bodies.
        !Perhaps consider using the dates_of_application rather than app_dates in future revs
        Total_Applications = num_apps *numyears
        allocate(dates_of_application(Total_Applications))
        allocate(drift_output(Total_Applications))
        
        k=0
        do i=1, numyears
            do j=1, num_apps
                k=k+1
                dates_of_application(k) = jd(firstyear+i-1,applicationMonth(j),applicationDay(j))-startday+1
                drift_output(k) = drift(j)*applicationMass(j)
            end do
        end do
        
        !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%!
        
        totalmass = applicationMass*area/10000.  !10000 m2 per hA ,Convert to Absolute Mass; area is in sq meters; apply_mass is kg/ha 
        m2_input=0.
        m1_input = 0.
          
        where (slowrelease > 0)
           release_duration= -log(0.05)/slowrelease + 2          
        elsewhere
           release_duration = 1 
        end where
  
 
      allocate(release_fraction(max_apps, maxval(release_duration)), STAT = status)
      release_fraction = 0.0

       !Calculate daily released fraction of pesticide mass 
       do k=1, num_apps
          store = 1.
          total= 0.
          do j = 1, release_duration(k)-1
            new = exp(-slowrelease(k)*j)
            release_fraction(k,j) = store-new
             store = new
            total = release_fraction(k,j)+ total
          end do
          release_fraction(k, release_duration(k)) = 1.-total
       end do

       !populate mass into input vector disperse the input mass
       do concurrent(i=1:numyears)
          do concurrent(j=1:num_apps)
            if (app_dates(j,i)>0 .AND.  app_dates(j,i) < num_records) then 
              lastpt = min(app_dates(j,i)+release_duration(j)-1, num_records)
              m1_input(app_dates(j,i):lastpt) = m1_input(app_dates(j,i):lastpt)+totalmass(j)*release_fraction(j,1:release_duration(j))
            end if
          end do    
       end do

    !At this point, all mass is in the vector m1_input, regardless of the water level.  THe following
    !lines place the mass in m1_input into the vector m2_input if the water level is at the minimum depth.
     where (daily_depth == minimum_depth)
         m2_input = m1_input
         m1_input=0.
     end where
            
   end subroutine ParentMass2
            

   
   !*******************************************************************************    
   subroutine DegradateProduction(j)
      use nonInputVariables, ONLY: num_records,degradateProduced1,degradateProduced2, v1,v2,    &
               k_photo, k_hydro, k_aer_aq,capacity_1,aqconc_avg1,aqconc_avg2,k_anaer_aq,capacity_2 
      use variables, ONLY: xPhoto, xHydro, xAerobic,xBenthic,Time_int, mwt
      implicit none               
      integer,intent(in) :: j
      real(8) :: MWTRatio

      MWTRatio = MWT(j+1)/MWT(j)
    
      degradateProduced1 = MWTRatio*(xPhoto(j)*k_photo*v1 + xHydro(j)*k_hydro*v1 + xAerobic(j)*k_aer_aq*capacity_1)*aqconc_avg1(:,j)*Time_int
      degradateProduced2 = MWTRatio*(xHydro(j)*k_hydro*v2 + xBenthic(j)*k_anaer_aq*capacity_2)*aqconc_avg2(:,j)*Time_int 
             
      !Degradate production is delayed one time step to approximate the process and to maintain analytical solution for time step  
      degradateProduced1(2:num_records)= degradateProduced1(1:num_records-1)
      degradateProduced2(2:num_records)= degradateProduced2(1:num_records-1)
      degradateProduced1(1)= 0.
      degradateProduced2(1)= 0.

   end subroutine DegradateProduction
        
end module mass_inputs