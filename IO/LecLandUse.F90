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
    write(6,'("lulc.asc header info:")')
    write(6,'("mx      = ",I6)') imx
    write(6,'("my      = ",I6)') imy
    write(6,'("xcorner = ",F12.3)') txcorner
    write(6,'("ycorner = ",F12.3)') tycorner    
    write(6,'("dx      = ",F12.6)') tdx
    write(6,'("nodata  = ",F12.6,/)') nodata    
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
!   Compute number of zones
	numberLandUses = 0
	do j=1,my
		do i=1,mx
			if( lulc(i,j) .NE. nodata .AND. lulc(i,j) .GT. numberLandUses ) then
				numberLandUses = lulc(i,j)
			endif
		enddo
	enddo

!
!	Number of zones
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
100  format('lulc.asc grid and dem.asc grid non corresponding',/)
!
!
2000 continue
!   
end subroutine
