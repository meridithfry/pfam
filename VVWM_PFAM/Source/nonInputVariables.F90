module nonInputVariables
!  Written by Dirk F. Young (Virginia, USA).
implicit none


integer :: num_records		!number of met file records



real(8),allocatable,dimension(:) :: k_aer_aq		!aqueous-phase aerobic rate (per sec)
real(8),allocatable,dimension(:) :: k_anaer_aq		!aqueous-phase anaerobic rate (per sec)
real(8),allocatable,dimension(:) :: k_aer_s			!sorbed-phase aerobic rate (per sec)
real(8),allocatable,dimension(:) :: k_anaer_s		!sorbed-phase anaerobic rate (per sec)
real(8),allocatable,dimension(:) :: k_volatile		!first order volatilization rate (per sec)
real(8),allocatable,dimension(:) :: k_photo			!photolysis rate (1/sec)
real(8),allocatable,dimension(:) :: daily_depth		!daily water body depths
real(8),allocatable,dimension(:) :: k_hydro         !hydrolysis rate (per sec)
real(8),allocatable,dimension(:) :: k_flow		    !array of daily wash out rates (per second)
real(8),allocatable,dimension(:) :: k_burial
real(8),allocatable,dimension(:) :: theta			!solute holding capacity ratio [--]
real(8),allocatable,dimension(:) :: capacity_1		!solute holding capacity of region 1 [m3]
real(8)							 :: capacity_2		!solute holding capacity of region 2 [m3]
real(8),allocatable,dimension(:) :: fw1				!fraction of region 1 solute in aqueous phase
real(8)							 :: fw2				!fraction of region 2 solute in aqueous phase
real(8)							 :: omega			!mass transfer coefficient
real(8),allocatable,dimension(:) :: gamma_1			!effective littoral degradation
real(8),allocatable,dimension(:) :: gamma_2			!effective benthic degradation

real(8),allocatable,dimension(:):: wind			!wind speed (m/s) at 10 cm
real(8),allocatable,dimension(:):: temp_avg		!average 30 day previous temperature
real(8),allocatable,dimension(:):: evap			!evaporation (m)
real(8),allocatable,dimension(:):: precip		!precipitation (m)

real(8),allocatable,dimension(:,:,:):: mass		!daily mass loading from runoff (column 1) and erosion (column 2) (kg), chemical (column 3) 

real(8),allocatable,dimension(:)  :: PaddyFlow	!(m3/sec) array of daily flow from Paddy
real(8),allocatable,dimension(:)  :: DailyFlow	!(m3/sec) array of total daily flow from Paddy and watershed
real(8),allocatable,dimension(:)  :: runoff		!(m3/s) runoff of adjacent watershed
real(8),allocatable,dimension(:)  :: burial		!(kg/sec) array of daily burial rate

real(8),allocatable,dimension(:)  :: v			!array of daily pond volumes 




real(8), allocatable, dimension(:)	:: A,B,E,F							!final coefficients for the 2 simultaneous equations

real(8), allocatable, dimension(:)	:: m1_input,m2_input				!at start of time step: the mass input in litt and benthic
real(8), allocatable, dimension(:)	:: m1_store,m2_store,mavg1_store	!array of daily peak/avg. mass in littoral and benthic
real(8),allocatable,dimension(:,:) :: aqconc_avg1 ,aqconc_avg2          ! daily average aqueous concentrations

integer :: firstAppDay,firstAppMonth
integer,allocatable,dimension(:) :: first_app_date		!array of yearly first application dates (absolute days)	

integer :: num_years
integer :: startday
integer :: firstyear, lastyear

real(8), allocatable, dimension(:)  :: v1  !volume of water column, also assumed to be the aqueous volume w/SS negligible
real(8):: v2                               !aqueous volume of pore water in sediment


real(8),allocatable,dimension(:) ::degradateProduced1,degradateProduced2

end module nonInputVariables