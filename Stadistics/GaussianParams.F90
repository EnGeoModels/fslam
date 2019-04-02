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
!	Gaussian properties calculation
	do i = 1, numberZones
		Gausskv(i) = GaussianProperties( (Soils(i)%kvmax+Soils(i)%kvmin)/2.d0,(Soils(i)%kvmax-Soils(i)%kvmin)/4.d0 )
		Gausskh(i) = GaussianProperties( (Soils(i)%khmax+Soils(i)%khmin)/2.d0,(Soils(i)%khmax-Soils(i)%khmin)/4.d0 )
		GaussC(i) = GaussianProperties( (Soils(i)%Cmax+Soils(i)%Cmin)/2.d0,(Soils(i)%Cmax-Soils(i)%Cmin)/4.d0 )
		Gaussphi(i)= GaussianProperties( (Soils(i)%phimax+Soils(i)%phimin)/2.d0,(Soils(i)%phimax-Soils(i)%phimin)/4.d0 )
		Gaussh(i) = GaussianProperties( (Soils(i)%hmax+Soils(i)%hmin)/2.d0,(Soils(i)%hmax-Soils(i)%hmin)/4.d0 )
		GaussDiff(i) = GaussianProperties( (Soils(i)%Diffmax+Soils(i)%Diffmin)/2.d0,(Soils(i)%Diffmax-Soils(i)%Diffmin)/4.d0 )
		GaussDens(i) = GaussianProperties( (Soils(i)%Densmax+Soils(i)%Densmin)/2.d0,(Soils(i)%Densmax-Soils(i)%Densmin)/4.d0 )
	enddo
!
end subroutine
