!
!
!****************************************************************************
!
!  SUBROUTINE: LecFilenames
!
!  PURPOSE:  Read input data filenames.
!
!****************************************************************************
subroutine LecFilenames()
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
	open(100,file='./filenames.txt',status='old',form='formatted')
!
	write(6,'("Reading input filenames file...",/)')
!
!	Entrada de parametros de filenames.txt:
    read(100,'(A)') fname_input         !Input data file
!
    read(100,'(A)') fname_topo          !Topo filename
!
    read(100,'(A)') fname_Rainfall_ant  !Antecdedent rainfall
!
    read(100,'(A)') fname_Rainfall      !Event rainfall
!
    read(100,'(A)') fname_zones         !Soil map
!
    read(100,'(A)') fname_soil_dat      !Soil property table    
!
    read(100,'(A)') fname_lulc          !Create raster PROB_uncond_stable.asc
!
    read(100,'(A)') fname_hmtu          !hmtu filename
!
    read(100,'(A)') fname_res           !Results folder
!
!	Cerramos el fichero
    close(100)
!    
!
    end subroutine
