!
!
!****************************************************************************
!
!  SUBROUTINE: LecZonesDat
!
!  PURPOSE:  Read soil data.
!
!****************************************************************************
subroutine LecZonesDat()
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
!   Variables
    character(len=1) :: hsg                     !Soil group
!
!	Abrimos ficheros de entrada de datos de control
	open(100,file='./data/soil.dat',status='old',form='formatted',err=1000)
!
!	first & second line
	read(100,*)	!index  Ksmax   Ksmin    Cmax    Cmin    phimax     phimin     hmax    hmin     densmax    densmin    porositymax   porositymin   hsg
	read(100,*) !()     (m/s)   (m/s)    (kPa)   (kPa)   (degree)   (degree)   (m)     (m)      (kg/m3)    (kg/m3)    (m3/m3)       (m3/m3)       ()
!
!
	do i=1, numberZones
!	
!		Entrada de parametros de input.dat:
		read(100,*) index,rKsmax,rKsmin,Cmax,Cmin,phimax,phimin,hmax,hmin,densmax,densmin,rporositymax,rporositymin,hsg
!
!		Corregimos unidades
		Soils(index)%Ksmin = rKsmin
		Soils(index)%Ksmax = rKsmax
		Soils(index)%Cmin = Cmin * 1000.d0
		Soils(index)%Cmax = Cmax * 1000.d0
		Soils(index)%phimin = phimin * R_D
		Soils(index)%phimax = phimax * R_D
		Soils(index)%hmin = hmin
		Soils(index)%hmax = hmax
		Soils(index)%densmin = densmin
		Soils(index)%densmax = densmax
        Soils(index)%porositymax = rporositymax
        Soils(index)%porositymin = rporositymin
        Soils(index)%hsg = hsg
!
	enddo
!
!	Cerramos el fichero
    close(100)
!
!
!
!	No error
	goto 2000
!
!	Error
!
!	Formats
1000 write(6,100) numberZones
100  format('Soils data file error. Number of zones: ',I5,/)
!
!
2000 continue
      
end subroutine
