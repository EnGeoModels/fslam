!
!
!****************************************************************************
!
!  SUBROUTINE: LecP0
!
!  PURPOSE:  Read P0.
!
!****************************************************************************
subroutine LecTopo()
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
!	Open topo file
    open(100,file='./data/topo.asc',status='old')
!
	write(6,'("Reading topo data",/)')
!
!	Entrada de la topografia topo.dat
    read(100,*) dummy,mx        !numero nodos X
    read(100,*) dummy,my        !numero nodos Y
    read(100,*) dummy,xcorner   !Coordenada X esquina superior izq.
    read(100,*) dummy,ycorner   !Coordenada X esquina superior izq.
    read(100,*) dummy,dx        !Delta X
    dy = dx						!Square grid
    read(100,*) dummy,nodata	!nodata ESRI
!
!
!	Dimensionamos las variables dinámicas
	ALLOCATE(zones(mx,my))			!Zones delimitation grid
	ALLOCATE(auxcumflow(mx,my,0:7))	!Auxiliar grid
	ALLOCATE(cumflow(mx,my))		!Cumflow grid
	ALLOCATE(slopeGrid(mx,my))		!Slopes grid
	ALLOCATE(topoIni(mx,my))		!Topo grid
	ALLOCATE(topo(mx,my))			!Corrected topo grid
	ALLOCATE(UncIns(mx,my))			!Unconditionally instable
	ALLOCATE(UncEst(mx,my))			!Unconditionally stable	
	ALLOCATE(FS_mu(mx,my))			!FS mean	
	ALLOCATE(FS_std(mx,my))			!FS Standard deviation	
	ALLOCATE(PFGrid(mx,my))         !Probability of failure value
    ALLOCATE(Rainfall(mx,my))       !Rainfall raster
    ALLOCATE(h_wt(mx,my))           !Water table raster
    ALLOCATE(Infiltration(mx,my))     !Rainfall increment due to climate change
    ALLOCATE(p0Grid(mx,my))         !p0 raster
    ALLOCATE(h_z(mx,my))            !Saturation degree
	ALLOCATE(FS_C1(mx,my))			!FS first component	
	ALLOCATE(FS_C2(mx,my))			!FS second component
	ALLOCATE(FS_C3(mx,my))			!FS third component
!
!
!	Leemos la malla de terreno
    do j = 1, my
        read(100,*) (topoIni(i,j), i =1, mx)
    end do
!   
!	Cerramos el fichero
    close(100) 
!   
end subroutine
