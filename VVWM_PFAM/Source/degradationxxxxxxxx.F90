module degradation
contains


subroutine gamma_one
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   !This subroutine calculates the Gamma_1 --the overall littoral degradation rate
   !metabloism is coded to accept sorbed phase degradation independently, but 
   !standard water bodies equate sorbed and aqueous metabloism rate
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   use nonInputVariables, Only: num_records,  &
                                 k_aer_aq,    &  !input aqueous-phase aerobic rate (per sec), see meatabolism.f90
                                 k_aer_s,     &  !input aqueous-phase aerobic rate (per sec), see meatabolism.f90
                                 k_volatile,  &  !input volatilization rate (per sec), see volatilization.f90
                                 k_hydro,     &  !input aqueous-phase hydrolysis rate (per sec), see hydrolysis.f90
                                 k_flow,      &  !input washout rate (per sec), see washout.f90   
                                 k_photo,     &                               
                                 fw1,         &  !fraction of solute in water phase, see solute_holding_capacity.f90
                                 gamma_1         !output overall aqueous-phase first-order rate littoral (per sec)
                                                           
  implicit none
  gamma_1 = k_flow+ (k_photo + k_hydro +k_volatile) *fw1  +k_aer_aq*fw1 + k_aer_s*(1.-fw1)

end subroutine gamma_one



subroutine gamma_two
use nonInputVariables, Only: num_records,     & 
                                 k_hydro,     & 
                                 k_anaer_aq,  &
                                 k_anaer_s,   &
                                 k_burial,    &         !mass rate of burial (1/s)
                                 fw2,         &
                                 gamma_2         !OUTPUT
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    !This subroutine calcualtes the overall rates for given
    ! the individual rates for all processes
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   implicit none

   integer i
   
   gamma_2  = k_anaer_aq*fw2 +k_anaer_s*(1.-fw2)+ k_hydro*fw2 + k_burial
   
   do i=1, num_records
   if (k_burial(i) >0.) write (28,*) i, "********************************************************"
   write(28,*) log(2.)/k_anaer_aq(i)/86400,  log(2.)/k_burial(i)/86400
   end do
   
end subroutine gamma_two


subroutine photolysis(index)
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
   !calculates photolysis rate, creates a vector k_photo of daily photolysis rates, Rates vary due to depth only
   !K_photo in per sec
   !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
       use variables, ONLY:LAT, RFLAT,photo,SUSED,CHL,DOC1,CLOUD,dfac
       use nonInputVariables, Only: num_records,temp_avg,daily_depth,k_photo  !output photolysi rate (per sec)
       implicit none
       
       integer,intent(in):: index  !index of chemical

       real(8):: A
       real(8):: KDP		!calculated EXAMS parameter  					
       real(8),dimension(num_records) :: term3
       real(8) term1,term2,term4
       real(8) :: reflatitude, photorate
       
       reflatitude = RFLAT(index)
       photorate = photo(index)
       
       A= 0.141 +101.*CHL+6.25*DOC1 +.34*SUSED   !EXAMS 2.98 section 2.3.3.2.2

       term1 = 191700.+87050.*cos(0.0349*LAT)    !latitude correction
       term2 = 191700.+87050.*cos(0.0349*reflatitude)
       term3 = (1.-exp(-dfac*daily_depth*A))/dfac/daily_depth/A
       term4 = 1.-0.056*cloud

       KDP  = 8.0225368e-6/photorate !KDP  = 0.69314718/photo/24/3600.  !EXAMS parameter (per sec)

       k_photo = KDP*term1/term2*term3*term4  !effective photolysis rate (per sec)

       where (temp_avg <= 0) !eliminate volatilization and photolysis when the pond freezes
		  k_photo = 0.
	   end where

end subroutine photolysis




!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine hydrolysis
  !calculates the hydrolysis rate, given the half life 

  use variables, ONLY:  hydro,minimum_depth
  use nonInputVariables, Only: num_records, &
                               daily_depth, &
                               k_hydro  ! output hydrolysis rate (per sec)
  
  implicit none
  k_hydro  = 0.69314718/hydro/86400.
  
!Since the field is supposed to be unflooded when depth is minimum depth, we will eliminmate hydrolysis since 
!we are saying that no water exists in the benthic during this period.
  where (daily_depth <= minimum_depth*1.001) k_hydro = 0.
           

end subroutine hydrolysis
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%




!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
subroutine metabolism
  !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  !This subroutine calculates the first-order metabolic degradation rate, given the 
  ! half life inputs for areobic and anaerobic sorbed and aqueous phases.
  !%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
    use variables, ONLY:aer_aq, anae_aq, temp_ref_aer ,temp_ref_anae,QT
    use nonInputVariables, Only: num_records,  &
                                 k_aer_aq,     &   !output of aqueous-phase aerobic metabolism rate (per sec)
                                 k_anaer_aq,   &   !output of aqueous-phase anaerobic metabolism rate (per sec)
                                 k_aer_s,      &   !output of sorbed-phase aerobic metabolism rate (per sec)
                                 k_anaer_s,    &  !output of sorbed-phase anaerobic metabolism rate (per sec)
                                 temp_avg         !temperature (in this case it has already been processed to be 30 day average)
                                 
    implicit none                      
 
   !*****Rewrite for memory Conservation 10/2/2009*************************
    k_aer_aq = 8.0225368e-6/aer_aq*QT**((temp_avg - temp_ref_aer)/10.)     !k_aer_aq  = 0.69314718/aer_aq/86400.
    k_aer_s  = k_aer_aq                                                    !effective solid metab rate (per sec)
   
    k_anaer_aq = 8.0225368e-6/anae_aq*QT**((temp_avg - temp_ref_anae)/10.) !effective aq metab rate (per sec)
    k_anaer_s  = k_anaer_aq                                                !effective sorbed rate      
         
     
    ! *************OLD VERSION****************************************************** 
!    real(8):: aer_s                                     !input of sorbed-phase anaerobic halflife (days)
!    real(8):: anae_s                                    !input of sorbed-phase anaerobic halflife (days)
!
!    real(8),dimension(num_records):: tempfactor_aer     !aerobic temperature correction factor p67 eq2-133
!    real(8),dimension(num_records):: tempfactor_anae    !anaerobic temperature correction factor p67 eq2-133
!   
!    aer_s   = aer_aq    ! at this time, sorbed and aqueous degradation rates are the 
!    anae_s  = anae_aq   ! same in the EFED standard water bodies.
!  
!    tempfactor_aer  = QT**((temp_avg - temp_ref_aer)/10.)
!    tempfactor_anae = QT**((temp_avg - temp_ref_anae)/10.)
!   
!           k_aer_aq = 8.0225368e-6/aer_aq*tempfactor_aer     !k_aer_aq  = 0.69314718/aer_aq/86400.
!           k_aer_s  = 8.0225368e-6/aer_s*tempfactor_aer      !effective solid metab rate (per sec)
!    
!           k_anaer_aq = 8.0225368e-6/anae_aq*tempfactor_anae !effective aq metab rate (per sec)
!           k_anaer_s  = 8.0225368e-6/anae_s*tempfactor_anae  !effective sorbed rate
!***************************************************************************************************      
    
end subroutine metabolism
!%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



subroutine volatilization(wind_height)
!this subroutine calculates the effective first-order volatilization rate

use variables, ONLY: MWT,SOL,VAPR,area
use nonInputVariables, Only: num_records, &
                             k_volatile,  &     !output: effective first-order decay rate due to volatilization
                             wind,        &     !input: wind speed (m/s),measured at wind_height
                             temp_avg,    &     !input: temperature (C)
                             v                  !input: array of daily pond volume (m3)
implicit none

		 
real(8),intent(in):: wind_height !height of wind measurement (m)


!real(8), dimension(num_records) :: KO2			!exams parameter, but in m/s
!real(8), dimension(num_records) :: K_overall	!overall resistance
real(8), dimension(num_records) :: RL			!liquid phase resistance
real(8), dimension(num_records) :: RG			!gas phase resistance
!real(8), dimension(num_records) :: VW			!p.36 EXAMS (eq 2.82)
real(8) :: R			! gas constant
real(8) :: HENRY		!Henry's constant
	
!real(8), dimension(num_records) :: wind_10cm	!wind speed referenced to 10cm

real(8),dimension(num_records) :: U			!wind speed adjusted to 10m above surface


HENRY = VAPR/760./(SOL/MWT)			!EXAMS p188 version 2.98
R=	8.2057e-5						!gas constant (atm m3/mol)


!******calculate liquid resistance*********
!	U = 4./log10(wind_height*1000.)*wind  !adjust wind ht to 10m (see paragraph under eqn 2-85)
!	where (U < 5.5)					!default conditions p.36
!		KO2 = 4.19e-6*sqrt(U)			!m/s
!	elsewhere (U>=5.5)
!		KO2 = 3.2e-7*U*U				!m/s
!	end where
!	KO2 =KO2*1.024**(temp_avg-20.)			!temp adjustment p. 36
!	RL = 1./KO2/sqrt(32./MWT)			!(s/m) EXAMS (eq 2-77)

!******Rewrite calculate liquid resistance*********
	U = 4./log10(wind_height*1000.)*wind  !adjust wind ht to 10m (see paragraph under eqn 2-85)

	where (U < 5.5)					!default conditions p.36
		RL = 4.19e-6*sqrt(U)		!m/s, RL Is really KO2, just trying to save some memory
	elsewhere (U>=5.5)
		RL = 3.2e-7*U*U				!m/s
	end where

	RL =RL*1.024**(temp_avg-20.)			!temp adjustment p. 36
	RL = 1./RL/sqrt(32./MWT)			!(s/m) EXAMS (eq 2-77)

!******calculate gas resistance*****************************************
!	wind_10cm = U/2.	!convert to wind speed at 10 cm
	!VW = 0.1857+5.68*U						            !(m/hr) EXAMS 2-82 optimized
	!RG = R*(temp_avg+273.15)/VW/HENRY/sqrt(18./MWT)		!(hr/m) EXAMS 2-76
	
	RG = R*(temp_avg+273.15)/(0.1857+5.68*U)/HENRY/sqrt(18./MWT)
	RG = RG*3600.									    !(s/m)
!************************************************************************

!K_overall = 1./(RL+RG)							!(m/s)  EXAMS 2-78
!k_volatile = AREA*K_overall/v					!per sec						              
k_volatile = AREA /(RL+RG)/v					!per sec


	where (temp_avg <= 0.) !eliminate volatilization when the pond freezes
		k_volatile = 0.
	end where



end subroutine volatilization



end module