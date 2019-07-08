!****************************************************************************
!
!  fslam: Fast Shallow Landslide Assessment Model
!
!  Code developed in the  SMuCPhy project, founded by the Ministerio de 
!  Economia y Competitividad del Gobierno de España and coordinated by UPC
!  BarcelonaTECH
!
!  Coordinated by Department of Civil and Environmental Engineering
!
!****************************************************************************
!
!
!****************************************************************************
!
!  SUBROUTINE: LecZones
!
!  PURPOSE:  Read zones grid.
!
!****************************************************************************
subroutine LecZones()
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
!	Malla de sources
	open(100,file='./data/zones.asc',status='old')
!
!	Comprobamos que sea el mismo header que la topografia
	read(100,*) dummy,imx			!numero nodos X
	if(imx.ne.mx) goto 1000
    
	read(100,*) dummy,imy			!numero nodos Y
	if(imy.ne.my) goto 1000
    
	read(100,*) dummy,txcorner		!Coordenada X esquina superior izq.
!	if(txcorner.ne.xcorner) goto 1000
    
	read(100,*) dummy,tycorner      !Coordenada X esquina superior izq.
!	if(tycorner.ne.ycorner) goto 1000
    
	read(100,*) dummy,tdx           !Delta X
!	if(tdx.ne.dx) goto 1000

	read(100,*) dummy,nodata        !nodata ESRI
!
!	Leemos la malla de zonas
	do j = 1, my
		read(100,*) (zones(i,j), i =1, mx)
	end do
!   
!	Cerramos el fichero
    close(100) 
!
!	Discard boundary
	zones(:, 1)  = nodata
	zones(:, my) = nodata
	zones(1, :)  = nodata
	zones(mx, :) = nodata

!
!	Number of zones
	numberZones = MAXVAL(zones)
	ALLOCATE(Soils(numberZones))
	ALLOCATE(Gausskh(numberZones))
	ALLOCATE(Gausskv(numberZones))
	ALLOCATE(GaussC(numberZones))
	ALLOCATE(Gaussphi(numberZones))
	ALLOCATE(Gaussz(numberZones))
	ALLOCATE(GaussDiff(numberZones))
	ALLOCATE(GaussDens(numberZones))
    ALLOCATE(GaussPor(numberZones))
	ALLOCATE(Unconditional(iCells,iter))
!
!   Stochastic model
    if (iStochastic .EQ. 1) then
        ALLOCATE(SoilsVariablesMatrix(numberZones))
        ALLOCATE(SoilsParameterMatrix(numberZones)) 
        ALLOCATE(VectParams(nParams))
        ALLOCATE(VectVar(nVariables))
        ALLOCATE(AuxMat(numberZones,nVariables*nVariables*nVariables,4))
        ALLOCATE(AuxSF(numberZones,nVariables*nVariables*nVariables))
        ALLOCATE(AuxProb(numberZones,nVariables*nVariables*nVariables))
        ALLOCATE(C_term(numberZones,nVariables*nVariables*nVariables))
        ALLOCATE(aux_term(numberZones,nVariables*nVariables*nVariables))
        ALLOCATE(sum_term(numberZones,nVariables*nVariables*nVariables))
        ALLOCATE(inf_term(numberZones,nVariables*nVariables*nVariables))
        ALLOCATE(tan_term(numberZones,nVariables*nVariables*nVariables))
        ALLOCATE(por_term(numberZones,nVariables*nVariables*nVariables))              
    endif
!
!	Initialize cells stability condition
	Unconditional = .FALSE.
!
!
!
!	No error
	goto 2000
!
!	Error
!
!	Se ha producido un error en la malla, no es igual a la de topo
1000 write(6,100)
100  format('Zones grid and topo grid non corresponding',/)
!
!
2000 continue
!   
end subroutine
