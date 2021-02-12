!
!
!****************************************************************************
!
!  SUBROUTINE: LecDat
!
!  PURPOSE:  Read input data.
!
!****************************************************************************
subroutine LecDat()
!
!
!
!	Variables globales
	use fslamGlobals_structures
	use fslamGlobals_shared
	use fslamGlobals_private
!
!
	implicit double precision (a-h,o-z)
!
!
!
!	Abrimos ficheros de entrada de datos de control
	open(100,file=fname_input,status='old',form='formatted')
!
	write(6,'("Reading configuration file...",/)')
!
!	Entrada de parametros de input.dat:
    read(100,*) iOutput             !Create GIS results
!
    read(100,*) iResearch           !Output all results (safety factor and probability of failure)
!
    read(100,*) iPROB_uncond_stable     !Create raster PROB_uncond_stable.asc
!
    read(100,*) iPROB_uncond_unst       !Create raster PROB_uncond_unst.asc
!
    read(100,*) iInfiltration           !Create raster Infiltration.asc
!
    read(100,*) iInitial_h_z            !Create raster Initial_h_z.asc
!
    read(100,*) iPROB_failure_initial_cond  !Create raster PROB_failure_initial_cond.asc
!
    read(100,*) iPROB_failure_final_cond    !Create raster PROB_failure_final_cond.asc
!
    read(100,*) iSF_initial_cond        !Create raster SF_initial_cond.asc
!
    read(100,*) iSF_final_cond          !Create raster SF_final_cond.asc
!
    read(100,*) iRunoff                 !Enable runoff module
!
!	Cerramos el fichero
    close(100)
!
!	Minimum Z for TRIGRS
	Zmin = 0.1d-2
!
!    
!
end subroutine
