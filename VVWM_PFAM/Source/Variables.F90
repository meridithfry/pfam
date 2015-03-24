module variables
!Written by Dirk F. Young (Virginia, USA).
!Only variables that are imported in from the User Interface are kept in this module.  This rule is 
!intended to facilititate changing outthe interface, as only and all the parameters in this module need to be populated.
!Common variables that are calculated and used internally between routines are transported as arguments.

    implicit none
    save
    !**********************************************************
    character(len=256) :: metfilename    !met file name
    character(len=256) :: outputfilename !output file name
    character(len=256) :: outputfilename2
    character(len=256) :: outputfilename3 
    character(len=256) :: outputfilename4
    
    !***********chemical inputs*****************************
    integer :: nchem             !number of chemicals (parent =1, up to 2 degradtates in addition, max nchem = 3)
    real(8) :: aer_aq(3)         !input aqueous-phase aerobic halflife (days) delivered from window interface
    real(8) :: anae_aq(3)        !anaerobic aquatic
     
    real(8) :: photo(3)          !near-surface photolysis half life (days
    real(8) :: hydro(3)          !input hydrolysis half life (days)
    real(8) :: koc(3)            !Koc value (ml/g)
    real(8) :: RFLAT(3)             !input latitude for photolysis study 
    
    real(8) :: MWT(3)            !molecular wt (g/mol)
    real(8) :: VAPR(3)           !vapor pressure (torr)
    real(8) :: SOL(3)            !solubility (mg/L) 
    
    real(8) :: temp_ref_aer(3)   !temp at which aerobic study conducted  
    real(8) :: temp_ref_anae(3)  !temp at which anaerobic study conducted  
    
    real(8) :: QT                !EXAMS Q10 values eq. 2-133
    
    real(8) :: heat_henry(3)
    real(8) :: temp_ref_henry(3) 
    
    real(8) :: xAerobic(3)
    real(8) :: xBenthic(3)
    real(8) :: xUnflood(3)
    real(8) :: xPhoto(3)
    real(8) :: xHydro(3)
    
    real(8) :: paddy_area     !Area of Paddy that is sending water and pest to the water body
    real(8) :: afield         !area of adjacent runoff-producing field(m2)    
    real(8) :: area           !water body area (m2)     
    real(8) :: depth_0        !initial water body depth
    real(8) :: depth_max	  !maximum water body depth
    
    integer :: SimTypeFlag    !1=vvwm, 2=constant vol w/o flow, 3 = const vol w/flow, 4 = mixing cell
    integer :: burialflag    !1 = burial, else no burial
    
    real(8):: D_over_dx 
    real(8):: PRBEN
    real(8):: benthic_depth
    real(8):: porosity     !volume water/total volume
    real(8):: bulk_density  !dry mass/total volume  g/ml
    real(8):: FROC2
    real(8):: DOC2
    real(8):: BNMAS
    real(8):: DFAC
    real(8):: SUSED
    real(8):: CHL
    real(8):: FROC1
    real(8):: DOC1
    real(8):: PLMAS
    
    real(8)::LAT
 
    real(8), parameter :: CLOUD = 0.
    real(8), parameter :: minimum_depth = 0.00001     !minimum water body depth
    real(8), parameter :: wind_height = 6.	 !height at which wind measurements are reported (m)
   
    real(8), parameter :: DELT	= 86400.  !simulation  TIME INTERVAL IS ONE DAY


    real(8):: watershed_cn
    real(8):: watershed_area
    real(8):: widthMixingCell
    real(8):: depthMixingCell
    real(8):: lengthMixingCell
    real(8):: baseflow  !(m3/sec)
    
    integer, allocatable, dimension(:) :: dates_of_application
    real(8), allocatable, dimension(:) :: drift_output
    integer :: Total_Applications
    
    
    

end module variables