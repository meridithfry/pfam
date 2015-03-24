Module MassInOut
!  Written by Dirk F. Young (Virginia, USA).
implicit none

contains

subroutine MassInputProcessing(index)
       !THIS SUBROUTINE RETURNS VALUES FOR input masses into each compartment 
        use variables, ONLY: PRBEN
        use nonInputVariables, Only: mass,                & 
                                     m1_input,            & !OUTPUT mass added to littoral region (kg) 
                                     m2_input,            & !OUTPUT mass added to bethic region (kg)
                                     degradateProduced1,  &
                                     degradateProduced2 
                                       
                                     !INPUT !mass(:,2)mass coming in by runoff and spraydrift (kg) 
                                     !INPUT mass(:,2) mass coming in on sediment (kg)
        implicit none  	
        integer,intent(in) :: index !chemical number  (parent =1)
        integer ::i
        
        !CALCULATE INPUT MASSES
        m1_input = mass(:,1,index) +(1.-PRBEN)*mass(:,2,index)    !note: rfx includes spraydrift and runoff
        m2_input = PRBEN*mass(:,2,index) 
        
        !******* Add in any degradate mass produced by parent from subsequent parent run******
        if (index>1) then                 !j=1 is the parent.  The following call is for the manual pesticide applications.

          m1_input = m1_input + degradateProduced1   
          m2_input = m2_input + degradateProduced2
        end if
        
        
end subroutine MassInputProcessing


!**********************************************************************************************
subroutine DegradateProduction(index)
!when this routine is run, it calculates the production of degradate from parent
!Thus, the "index" refers to the parent chemical
      use nonInputVariables, ONLY: num_records,degradateProduced1,degradateProduced2, v1,v2,    &
               k_photo, k_hydro, k_aer_aq,capacity_1,aqconc_avg1,aqconc_avg2,k_anaer_aq,capacity_2 
      use variables, ONLY: xPhoto, xHydro, xAerobic,xBenthic,DELT, mwt
      implicit none               
      integer,intent(in) :: index
      real(8) :: MWTRatio
      
      MWTRatio = MWT(index+1)/MWT(index)
    
      degradateProduced1 = MWTRatio*(xPhoto(index)*k_photo*v1 + xHydro(index)*k_hydro*v1 + xAerobic(index)*k_aer_aq*capacity_1)*aqconc_avg1(:,index)*DELT
      degradateProduced2 = MWTRatio*(xHydro(index)*k_hydro*v2 + xBenthic(index)*k_anaer_aq*capacity_2)*aqconc_avg2(:,index)*DELT 
             
      !Degradate production is delayed one time step to approximate the process and to maintain analytical solution for time step  
      degradateProduced1(2:num_records)= degradateProduced1(1:num_records-1)
      degradateProduced2(2:num_records)= degradateProduced2(1:num_records-1)
      degradateProduced1(1)= 0.
      degradateProduced2(1)= 0.

end subroutine DegradateProduction
        


end Module MassInOut