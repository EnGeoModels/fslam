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
	open(100,file='./data/soil.asc',status='old')
!
!	Comprobamos que sea el mismo header que la topografia
	read(100,*) dummy,imx			!numero nodos X
	if(imx.ne.mx) goto 1000
!    
	read(100,*) dummy,imy			!numero nodos Y
	if(imy.ne.my) goto 1000
!    
	read(100,*) dummy,txcorner		!Coordenada X esquina superior izq.
	if(txcorner.ne.xcorner) goto 1000
!    
	read(100,*) dummy,tycorner      !Coordenada X esquina superior izq.
	if(tycorner.ne.ycorner) goto 1000
!    
	read(100,*) dummy,tdx           !Delta X
	if(tdx.ne.dx) goto 1000
!
	read(100,*) dummy,tnodata        !nodata ESRI
	if(tnodata.ne.nodata) goto 1000
!
!   Header output
    write(6,'("soil.asc header info:")')
    write(6,'("mx      = ",I6)') imx
    write(6,'("my      = ",I6)') imy
    write(6,'("xcorner = ",F12.3)') txcorner
    write(6,'("ycorner = ",F12.3)') tycorner    
    write(6,'("dx      = ",F12.6)') tdx
    write(6,'("nodata  = ",F12.6,/)') nodata    
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
!   Find maximum
	numberZones = 0
	do j=1,my
		do i=1,mx
			if( zones(i,j) .NE. nodata .AND. zones(i,j) .GT. numberZones ) then
				numberZones = zones(i,j)
			endif
		enddo
	enddo
!
!	Number of zones
	ALLOCATE(Soils(numberZones))
	ALLOCATE(GaussKs(numberZones))
	ALLOCATE(GaussC(numberZones))
	ALLOCATE(Gaussphi(numberZones))
	ALLOCATE(Gaussh(numberZones))
	ALLOCATE(GaussDens(numberZones))
   	ALLOCATE(GaussPor(numberZones))
!
!	No error
	goto 2000
!
!	Error
!
!	Se ha producido un error en la malla, no es igual a la de topo
1000 write(6,100)
100  format('soil.asc grid and dem.asc grid non corresponding',/)
!
!
2000 continue
!   
end subroutine
