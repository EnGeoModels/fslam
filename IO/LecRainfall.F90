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
!	Open rainfall file
    open(100,file=fname_Rainfall,status='old')
!
	write(6,'("Reading rainfall data",/)')
!
!	Comprobamos que sea el mismo header que la topografia
	read(100,*) dummy,imx			!numero nodos X
	if(imx.ne.mx) goto 1000
!    
	read(100,*) dummy,imy			!numero nodos Y
	if(imy.ne.my) goto 1000
!    
	read(100,*) dummy,txcorner		!Coordenada X esquina superior izq.
	if(INT(txcorner).NE.INT(xcorner)) goto 1000
!    
	read(100,*) dummy,tycorner      !Coordenada X esquina superior izq.
	if(INT(tycorner).NE.INT(ycorner)) goto 1000
!    
	read(100,*) dummy,tdx           !Delta X
	if(tdx.ne.dx) goto 1000
!
	read(100,*) dummy,tnodata        !nodata ESRI
    if(tnodata.ne.nodata) goto 1000
!
!   Header output
    write(6,'("rain_event.asc header info:")')
    write(6,'("mx =      ",I6)') imx
    write(6,'("my =      ",I6)') imy
    write(6,'("xcorner = ",F12.3)') txcorner
    write(6,'("ycorner = ",F12.3)') tycorner    
    write(6,'("dx =      ",F12.6)') tdx
    write(6,'("nodata =  ",F12.6,/)') nodata 
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
100  format('rain_event.asc grid and de.asc grid non corresponding',/)
!
!
2000 continue
!   
!
end subroutine
