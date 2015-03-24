module volumeAndwashout
!Written by Dirk F. Young (Virginia, USA).
contains

   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine constant_volume_calc
        !This subroutine calculates volume and washout rate
        use utilities
        use variables, ONLY: area, depth_0
        use nonInputVariables, Only: num_records, & 
                                     DailyFlow,   &   !input (m3/sec) array of daily runoff
                                     k_flow,      &   !output array of daily wash out rates (per second)
                                     v,           &   !for this case, v = constant, but keep as array to be consistent with rest of program
                                     daily_depth 
        implicit none


        real(8),dimension(num_records):: q_avg					!30-day flow average
        real(8):: v_0											! water body volume

        v_0 = area*depth_0
        
        call window_average(DailyFlow,30,num_records,q_avg) !calculate 30-day previous average
        
        k_flow = q_avg/v_0   !array of washout rates
        v= v_0
        
        daily_depth = depth_0 
        
    end subroutine constant_volume_calc
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine volume_calc
        !This subroutine calculates volume and washout rate
        
        use variables, ONLY: area, depth_0, depth_max, minimum_depth, DELT
        use nonInputVariables, Only: num_records, &
                                     k_flow,      & !output array of daily wash out rates (per second)
                                     DailyFlow,   & !input (m3/sec) array of daily runoff
                                     v,           & !output, 
                                  evap,           & !daily evaporation (m)
                                 precip,          & !daily precipitation (m)
                                 daily_depth        !OUTPUT (m)
              
        implicit none
        integer:: day
        real(8):: v_0								!initial water body volume 
        real(8):: v_max								!maximum water body volume
        real(8):: v_min								!minimum water body volume
        real(8):: v_previous
        real(8):: check
        real(8),dimension(num_records)::vol_net
        real(8),dimension(num_records)::evap_area
        real(8),dimension(num_records)::precip_area

        v_0 = area*depth_0
        v_max = area*depth_max
        v_min = area*minimum_depth
        k_flow = 0.		!sets all values of the array to zero
        v_previous = v_0

        precip_area = precip*area /86400.	!m3/s
        evap_area = evap*.7*area /86400.    !m3/s, evap factor (.7) 

        vol_net = (DailyFlow-evap_area+precip_area)*delt  !volume of water added in day; whole array operations

        do day = 1,num_records
            check = v_previous + vol_net(day)
            if (check > v_max) then
                v(day) = v_max
                k_flow(day) = (check-v_max)/delt/v_max   !day # and washout VOLUME
	        else if (check < v_min) then
                v(day) = v_min
            else
                v(day) = check
            end if
            v_previous = v(day)
            !*******************************************************
            !ADDED FOR MARK CORBIN*********************************
                write(60,*)day,v(day),k_flow(day)*delt*v_max
            !********************************************************
        end do

	daily_depth = v/area !whole array operation



    end subroutine volume_calc
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine mixing_cell
        !This subroutine calculates washout rate for a mixing cell in which flow is not bufferred
        !by a 30 day average as was done in the other subroutines.
        use utilities
        use variables, ONLY: area, depth_0, baseflow
        use nonInputVariables, Only: num_records, & 
                                     DailyFlow,   &   !input (m3/sec) array of daily runoff
                                     k_flow,      &   !output array of daily wash out rates (per second)
                                     v,           &   !for this case, v = constant, but keep as array to be consistent with rest of program
                                     daily_depth 
        implicit none


        real(8),dimension(num_records):: q_avg					!30-day flow average
        real(8):: v_0											! water body volume

        v_0 = area*depth_0
        
    !    call window_average(DailyFlow,30,num_records,q_avg) !calculate 30-day previous average

        q_avg = DailyFlow + baseflow
       
        
        k_flow = q_avg/v_0   !array of washout rates
        v= v_0
        
        daily_depth = depth_0 
        
    end subroutine mixing_cell
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%























    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    subroutine burial_calc(index)
    use variables, ONLY: koc, froc2, burialFlag
    use nonInputVariables, Only: num_records,& 
                                 capacity_2, & !input 
                                 k_burial, &   !output(kg/sec)
                                 burial        !input sediment rate
    implicit none
    integer, intent(in) :: index
    real(8) :: kd_sed_2
    
    real(8) :: koc_local 
koc_local = KOC(index)


    if (burialFlag == 1) then
      kd_sed_2 = koc_local*FROC2*.001       !Kd of sediment  [m3/kg]
      k_burial=  burial* kd_sed_2/capacity_2
       else
        k_burial = 0.
    end if
   
  
    end subroutine burial_calc
    !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%







end module volumeAndwashout