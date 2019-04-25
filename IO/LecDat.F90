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
    read(100,*) iter				!Numero de iteraciones
    read(100,*) iCells				!Number of cells to calcultate
    read(100,*) densw				!Water density
    read(100,*) nAntRain			!Nummer of antecedent rains
    nAntRain = nAntRain + 1
    
	read(100,*) nIntervals			!Number of duration intervals
	nIntervals = nIntervals + 1
	
	read(100,*) nRainInc			!Number of rain increments
	nRainInc = nRainInc + 1
	
	read(100,*) MinAntRain			!Minimum antecedent rain (mm/d)
	MinAntRain = MinAntRain / (24.d0*3600.d0*1000.d0)
	
	read(100,*) MinDuration			!Minimum event duration (hr)
	MinDuration = MinDuration * 3600.d0
	
	read(100,*) MinRain				!Minimum event rain (mm/hr)
	MinRain = MinRain / (3600.d0*1000.d0)
	
	read(100,*) MaxAntRain			!Maximum antecedent rain (mm/d)
	MaxAntRain = MaxAntRain / (24.d0*3600.d0*1000.d0)
	
	read(100,*) MaxDuration			!Maximum event duration (hr)
	MaxDuration = MaxDuration * 3600.d0
	
	read(100,*) MaxRain				!Maximum event rain (mm/hr)
	MaxRain = MaxRain / (3600.d0*1000.d0)
!
	read(100,*) LevelsNumber		!Number of TRIGRS levels
	read(100,*) mmax				!Number of terms in the finite depth infiltration eq.
!
    read(100,*) iOutput             !Create GIS results
!
!	Cerramos el fichero
    close(100)
!
!	Minimum Z for TRIGRS
	Zmin = 0.1d-2
!
!
!	Create dynamic variables vectors
	ALLOCATE(AntRainVec(nAntRain))
!	
	do i = 1, (nAntRain-1)
		AntRainVec(i) = (MaxAntRain-MinAntRain) / (nAntRain - 1) * (i-1) + MinAntRain
	enddo
	AntRainVec(nAntRain) = MaxAntRain
!    
!
end subroutine
