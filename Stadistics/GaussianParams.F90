!
!
!****************************************************************************
!
!  SUBROUTINE: GaussianParams
!
!  PURPOSE:  Generate random soil properties.
!
!****************************************************************************
subroutine GaussianParams()
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
!	Explicit variables
	real   :: rand
	real*8 :: InverseNormal
!
!
!	Gaussian properties calculation for soils
	do i = 1, numberZones
		GaussKs(i) = GaussianProperties( (Soils(i)%Ksmax+Soils(i)%Ksmin)/2.d0,(Soils(i)%Ksmax-Soils(i)%Ksmin)/4.d0 )
		GaussC(i) = GaussianProperties( (Soils(i)%Cmax+Soils(i)%Cmin)/2.d0,(Soils(i)%Cmax-Soils(i)%Cmin)/4.d0 )
		Gaussphi(i)= GaussianProperties( (Soils(i)%phimax+Soils(i)%phimin)/2.d0,(Soils(i)%phimax-Soils(i)%phimin)/4.d0 )
		Gaussh(i) = GaussianProperties( (Soils(i)%hmax+Soils(i)%hmin)/2.d0,(Soils(i)%hmax-Soils(i)%hmin)/4.d0 )
		GaussPor(i) = GaussianProperties( (Soils(i)%porositymax+Soils(i)%porositymin)/2.d0,(Soils(i)%porositymax-Soils(i)%porositymin)/4.d0 )
		GaussDens(i) = GaussianProperties( (Soils(i)%Densmax+Soils(i)%Densmin)/2.d0,(Soils(i)%Densmax-Soils(i)%Densmin)/4.d0 )
    enddo
!
!	Gaussian properties calculation for landuse
	do i = 1, numberLandUses
		GaussCr(i) = GaussianProperties( (LandUses(i)%Crmax+LandUses(i)%Crmin)/2.d0,(LandUses(i)%Crmax-LandUses(i)%Crmin)/4.d0 )
	enddo
!
end subroutine
