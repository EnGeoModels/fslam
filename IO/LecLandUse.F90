!
!
!****************************************************************************
!
!  SUBROUTINE: LecLandUse
!
!  PURPOSE:  Read land use grid.
!
!****************************************************************************
subroutine LecLandUse()
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
	open(100,file='./data/lulc.asc',status='old')
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
		read(100,*) (lulc(i,j), i =1, mx)
	end do
!   
!	Cerramos el fichero
    close(100) 
!
!	Discard boundary
	lulc(:, 1)  = nodata
	lulc(:, my) = nodata
	lulc(1, :)  = nodata
	lulc(mx, :) = nodata

!
!	Number of zones
	numberLandUses = MAXVAL(lulc)
	ALLOCATE(LandUses(numberLandUses))
    ALLOCATE(GaussCr(numberLandUses))
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
