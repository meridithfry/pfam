!  PFAMFortran.f90 
!****************************************************************************
!  Written by Dirk F. Young (Virginia, USA).
!  PROGRAM: PFAMFortran
!
!  PURPOSE:  Entry point for the console application.
!
!****************************************************************************
program PFAMFortran
use PrimarySubroutineRoutines
use Variables
use utilities_module  !deletable along with the QA line below!

implicit none
integer :: ierror,length,i
character (len=256) command
logical  vvwm

character (len=256) outputfile
character (len=256) outputpath

call get_command_argument(1,command,length)

open (UNIT = 12, FILE = command, STATUS = 'old', IOSTAT = ierror) 
!nchem = 1  !  This is the index for the parent


read(12,*) aer_aq(1)  
read(12,*) anae_aq(1)
read(12,*) drysoil(1)     
read(12,*) photo(1)         
read(12,*) hydro(1)          
read(12,*) MWT(1)             !Line 6          
read(12,*) VAPR(1)          
read(12,*) SOL(1)           
read(12,*) koc(1)           
read(12,*) temp_ref_aer(1)  
read(12,*) temp_ref_anae(1) 
read(12,*) temp_ref_dry(1)
read(12,*) RFLAT(1)            !Line 13

read(12,*) heat_henry(1)
read(12,*) temp_ref_henry(1)   

read(12,*) num_apps  !Line 16, The actual number of applications 


read(12,*) (applicationDay(i),i=1, num_apps)
read(12,*) (applicationMonth(i),i=1, num_apps )
read(12,*) (applicationMass(i),i=1, num_apps )
read(12,*) (slowRelease(i),i=1, num_apps )
!The following Lines are not used in PFAM but are transferred to the output for vvwm use
read(12,*)  (drift(i),i=1, num_apps )
read(12,*)  !Line 22 spare line
read(12,*)  !Line 23 spare line


!make the unused dates and massses ineffective
forall (i=1:max_apps, i>num_apps)
 applicationDay(i)=1
 applicationMonth(i)=1
 applicationMass(i)=0.
 slowrelease(i) = 0.
end forall

 !*********** Location ******************
read(12,'(A256)') metfilename   !Line 24
read(12,*) LAT                  !Line 25

!*************** FLOOD *****
read(12,*) NumberFloodEvents    !Line 26 contains actual number of events
read(12,*) event0_day   
read(12,*) event0_month  


read(12,*) (EventDay(i),i=1, NumberFloodEvents)       !Line 29
read(12,*) (EventFill(i),i=1, NumberFloodEvents)
read(12,*) (EventWeir(i),i=1, NumberFloodEvents)
read(12,*) (EventMinimum(i),i=1, NumberFloodEvents)
read(12,*) (EventWashout(i),i=1, NumberFloodEvents)  !Line 33


!In order to facilitate transfer, unused Event slots are filled with the same values as the
!last used Event.  In this way, the line number for a particular variable will always be the same
!regardless of the number of events used.  And because the unused events contain the same values, nothing
!changes in the program. 
forall (i=1:11, i > numberFloodEvents)
 eventDay(i)=eventDay(numberFloodEvents)
 eventFill(i)=eventFill(numberFloodEvents)
 eventWeir(i)=eventWeir(numberFloodEvents)
 eventMinimum(i)= eventMinimum(numberFloodEvents)
 eventWashout(i)= eventWashout(numberFloodEvents)
end forall

!*** PLANTING***********************************************
read(12,*) PlantZero_day      !Line 34
read(12,*) PlantZero_month    !Line 35
read(12,*) PlantFull          !Line 36
read(12,*) PlantRemove
read(12,*) CanopyMax          !Line 38

!****Physical Inputs ***************
read(12,*) D_over_dx          !Line 39 
read(12,*) depth_0       
read(12,*) benthic_depth 
read(12,*) porosity      
read(12,*) bulk_density  
read(12,*) FROC1         
read(12,*) FROC2         
read(12,*) SUSED              !Line 46       
read(12,*) CHL           
read(12,*) DOC1          
read(12,*) wierleakage        
read(12,*) !    PLMAS         
read(12,*) !    BNMAS         
read(12,*) QT            
read(12,*) DFAC          
read(12,*) AREA   
read(12,*) leakage 

!*****Output File
read(12,'(A256)')  outputpath
read(12,'(A256)')  outputfile

outputfilename = trim(outputpath) // trim(outputfile)




!****  degradate Information ***************************
read(12,*) nchem !number of extra chemicals, 1 means only parent



do i = 2, 3
    
  ! the following  if statement prevents possible errors in reading the degradate variables if the 
  !degradtes are NOT going to be calculted. In that case the VB interfeace does not check
  !to see if valid numbers are entered so it is poosibe for example that someone enters a 
  !character into hydrolysis text box which then this fortan progran would attempt to read as a number.
   if (nchem >= i) then 
                    
   
   read(12,*) aer_aq(i) 
   read(12,*) anae_aq(i)
   read(12,*) drysoil(i)   
   read(12,*) photo(i)         
   read(12,*) hydro(i)         
   read(12,*) MWT(i)           
   read(12,*) VAPR(i)          
   read(12,*) SOL(i)           
   read(12,*) koc(i)           
   read(12,*) temp_ref_aer(i)  
   read(12,*) temp_ref_anae(i) 
   read(12,*) temp_ref_dry(i)
   read(12,*) RFLAT(i)  
   read(12,*) heat_henry(i)
   read(12,*) temp_ref_henry(i)   
   read(12,*) xAerobic(i-1)  ! transforms are associated with the parent, because calcs are performed at parent time
   read(12,*) xBenthic(i-1)  ! hence, "i-1" is used here
   read(12,*) xUnflood(i-1)
   read(12,*) xPhoto(i-1)
   read(12,*) xHydro(i-1)
   
   
   else 
   
   read(12,*) !aer_aq(i) 
   read(12,*) !anae_aq(i)
   read(12,*) !drysoil(i)   
   read(12,*) !photo(i)         
   read(12,*) !hydro(i)         
   read(12,*) !MWT(i)           
   read(12,*) !VAPR(i)          
   read(12,*) !SOL(i)           
   read(12,*) !koc(i)           
   read(12,*) !temp_ref_aer(i)  
   read(12,*) !temp_ref_anae(i) 
   read(12,*) !temp_ref_dry(i)
   read(12,*) !RFLAT(i)  
   read(12,*) !heat_henry(i)
   read(12,*) !temp_ref_henry(i)   
   read(12,*) !xAerobic(i-1)  ! transforms are associated with the parent, because calcs are performed at parent time
   read(12,*) !xBenthic(i-1)  ! hence, "i-1" is used here
   read(12,*) !xUnflood(i-1)
   read(12,*) !xPhoto(i-1)
   read(12,*) !xHydro(i-1)
   
   end if
end do




   read(12,*) vvwm !CONTAINS THE LOGICAL EXPRESSION ABOUT WHETHER TO PERFORM THE POST PROCESSING OF VVWM

   if (vvwm) then  
        read(12,*) watershed_area !these are not used in this program but are caried over 
        read(12,*) watershed_cn   !for later post processing
        read(12,*) widthMixingCell
        read(12,*) depthMixingCell
        read(12,*) lengthMixingCell
        read(12,*) baseflow  
   else 
        watershed_area =0.
        watershed_cn   =0.
        widthMixingCell =0.
        depthMixingCell =0.
        lengthMixingCell=0.
        baseflow  =0.
   end if

!*********************************************************************************
       
!Now that all the inputs are loaded into the Variables module, call the main model routine
call PrimarySubroutine    !HERE is the call to the Main routine:

end program PFAMFortran

