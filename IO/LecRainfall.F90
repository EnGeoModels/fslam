!
!
!****************************************************************************
!
!  SUBROUTINE: LecRainfall
!
!  PURPOSE:  Read rainfall raster.
!
!****************************************************************************
subroutine LecRainfall()
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
    open(100,file='./data/rainfall.asc',status='old')
!
	write(6,'("Reading rainfall data",/)')
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
!
!	Leemos la malla de terreno
    do j = 1, my
        read(100,*) (Rainfall(i,j), i =1, mx)
    end do
!   
!	Cerramos el fichero
    close(100) 
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
!
end subroutine
