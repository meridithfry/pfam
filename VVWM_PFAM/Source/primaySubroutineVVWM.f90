module PrimarySubroutine
!Written by Dirk F. Young (Virginia, USA).
contains
!***************************************************************************
! THIS IS THE ANALYTICAL SOLUTION TO THE EFED SUBSETS OF THE EXAMS MODEL. 
!
!
! Attempt was made to put all EXAMS partameters in CAPITAL LETTERS
! errors are reported in fort.11
!___________________________________________________________________________
subroutine PrimarySubroutineVVWM
use variables, ONLY: simtypeflag, area, nchem
use degradation
use solute_capacity
use mass_transfer
use volumeAndwashout
!use readprzmfiles
use ProcessMetfiles
use outputprocessing
use nonInputVariables, only:k_flow  , num_years,PaddyFlow, DailyFlow, runoff
use ALLOCATIONmodule
use coreCalculations
use MassInOut
implicit none              


!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

integer :: eof_met			!flag to idicate end of met file error
integer :: ierror			!met file open error flag

real(8) :: latitude  !latitude of scenario asa taken fro the metfile
integer :: index

!Not needed for this application because PFAM already counted the metfile
!!*************  Met file Calculations  *****************************************
!! This routine counts the records in the metfile and gives a value to the num_records parameter
!! in the module nonInputVariables.  Allocations then follow below using num_records.
!
!    call count_met(ierror) !count met file records for array allocation
!	    if (ierror /=0) then
!			write (11,*) 'can not find the met file'
!			return
!	    end if
!
!    CALL ALLOCATION
!
!	!******************************************************************************
	call read_metfile(eof_met) 
	if (eof_met /=0) then
		write (11,*) 'error: met file is messed up'
		return
	end if

	!******************************************************************************
	!outputs daily mass loading, m (kg) and array of daily runoff, q (m3/sec)
	!also gives the vector of first application dates (first_app_date) in absolute day counts
    CALL allocation2
	!call read_przm_files  

	!****************************************************************
	!Washout and volume calculations for individual cases
    !First calculate overall flow into recieving water body
    dailyflow = paddyflow + runoff

    select case (simtypeflag)
        case (4) !mixing cell 
                 call mixing_cell
		case (3) !reservoir constant volume,flow
				call constant_volume_calc 
		case (2)  !pond constant volume, no flow
				call constant_volume_calc 
				k_flow=0.  !for this case zero out washout
		case (1) !variable volume, flow
				call volume_calc
	end select

    call omega_mass_xfer

do index = 1, nchem
	!*******************************************
    call solute_holding_capacity(index)   
    call hydrolysis(index) 
    !call get_latitude (latitude)
    call photolysis(index)
    call metabolism (index)
    call burial_calc (index)
	call volatilization(index)       
	call MassInputProcessing(index)   !in module MassInOut
	call gamma_one  !process the individual degradation rates into overall parameters:
	call gamma_two
	call reducer2
	
	call MainLoop(index)
	
	if (nchem > index) then
	    call DegradateProduction(index)  !in module MassInOut
	end if
	
	call output_processor(index)
    call benthic_output_processor(index)
    
	!**********************************************************
end do




end subroutine PrimarySubroutineVVWM

end module PrimarySubroutine