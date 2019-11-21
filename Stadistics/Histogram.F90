!
!
!****************************************************************************
!
!  SUBROUTINE: Histogram
!
!  PURPOSE:  Compute results histogram
!
!****************************************************************************
subroutine Histogram(Filename)
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
!   Variables
    integer :: i
    real*8  :: numNullCells
    real*8  :: numNotNullCells  
    CHARACTER Filename*(*)
!
!   Output file
    OPEN(20,FILE=Filename,ACTION="write",STATUS="replace",IOSTAT=istat)
!
!   Control file
    IF (istat .ne. 0) THEN
        !*****LOG ERROR
        CLOSE(20)
        RETURN
    END IF
!
!   Write CSV header
    WRITE(20,'("value,cdf")')
!
!   Not NULL cells
    numNullCells = COUNT(PFGrid .EQ. nodata)
    numNotNullCells = my*mx - numNullCells
!
!   Stable cells
    write(20, '(f10.3,",",f10.4)' ) 0.d0, (COUNT( PFGrid <= 0.d0 ) - numNullCells) / numNotNullCells
!
!   Main loop
    do i = 1,100
        write(20, '(f10.3,",",f10.4)' ) DBLE(i)/100.d0, (COUNT( PFGrid <= (DBLE(i)/100.d0) ) - numNullCells) / numNotNullCells
    enddo
!
    close(20)
!
end subroutine Histogram
