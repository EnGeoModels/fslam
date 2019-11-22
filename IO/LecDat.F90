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
	open(100,file='./data/input.dat',status='old',form='formatted')
!
	write(6,'("Reading configuration file...",/)')
!
!	Entrada de parametros de input.dat:
    read(100,*) densw				!Water density
!
    read(100,*) iOutput             !Create GIS results
!
    read(100,*) climateChangeFactor !Read climate change factor for 24hr precipitation
!
    read(100,*) iResearch           !Output all results (safety factor and probability of failure)
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
