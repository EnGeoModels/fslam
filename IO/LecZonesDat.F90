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
    TYPE(ReadSoilProperties) :: read_aux
!
!	Abrimos ficheros de entrada de datos de control
	open(100,file='./data/soil.dat',status='old',form='formatted',err=1000)
!
!	first & second line
	read(100,*)	!index,Ks,Cmax,Cmin,phimax,phimin,h,dens,porosity,hsg
	read(100,*) !(),(m/s),(kPa),(kPa),(degree),(degree),(m),(kg/m3),(m3/m3),()
!
!
	do i=1, numberZones
!	
!		Entrada de parametros de input.dat:
		read(100,*) read_aux
!
!		Corregimos unidades
		Soils(read_aux%index)%Ksmin = read_aux%rKs
		Soils(read_aux%index)%Ksmax = read_aux%rKs
		Soils(read_aux%index)%Cmin = read_aux%Cmin * 1000.d0
		Soils(read_aux%index)%Cmax = read_aux%Cmax * 1000.d0
		Soils(read_aux%index)%tanPhimin = DTAN(read_aux%phimin * R_D)
		Soils(read_aux%index)%tanPhimax = DTAN(read_aux%phimax * R_D)
		Soils(read_aux%index)%hmin = read_aux%h_soil
		Soils(read_aux%index)%hmax = read_aux%h_soil
		Soils(read_aux%index)%densmin = read_aux%dens
		Soils(read_aux%index)%densmax = read_aux%dens
        Soils(read_aux%index)%porositymax = read_aux%rporosity
        Soils(read_aux%index)%porositymin = read_aux%rporosity
        Soils(read_aux%index)%hsg = read_aux%hsg
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
