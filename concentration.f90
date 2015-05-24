! concentration.f90 - Module, supporting averaging of concentrations over a grid.
!
! Written by: Mauri Favaron
!
MODULE Concentration
	
	IMPLICIT NONE
	
	PRIVATE
	
	! Public interface
	PUBLIC	:: ConcType
	
	! Other useful constants (please do not change)
	INTEGER, PARAMETER	:: SVR_INFO    = 0
	INTEGER, PARAMETER	:: SVR_WARNING = 1
	INTEGER, PARAMETER	:: SVR_ERROR   = 2
	
	! Data types
	
	TYPE ConcType
		! Type management "standard" variables
		LOGICAL, PRIVATE					:: lDebug
		CHARACTER(LEN=256), PRIVATE			:: sErrorMsg
		! Data space
		INTEGER								:: iNx
		INTEGER								:: iNy
		INTEGER								:: iSize
		INTEGER								:: iNextValue
		REAL, DIMENSION(:,:), ALLOCATABLE	:: rmSumConc
		REAL, DIMENSION(:,:), ALLOCATABLE	:: rmSumConc2
		INTEGER								:: iNumFields
		! Results file management
		CHARACTER(LEN=256), PRIVATE			:: sResultsPath
	CONTAINS
		PROCEDURE	:: Initialize => ConcPoolClean
		PROCEDURE	:: Add        => ConcAdd
		PROCEDURE	:: Stats      => ComputeAverages
	END TYPE ConcType
	
CONTAINS
	
	FUNCTION ConcPoolClean(this, sResultsPath, iNx, iNy, lDebug) RESULT(iRetCode)
	
		! Routine arguments
		CLASS(ConcType)					:: this
		CHARACTER(LEN=*), INTENT(IN)	:: sResultsPath
		INTEGER, INTENT(IN)				:: iNx
		INTEGER, INTENT(IN)				:: iNy
		LOGICAL, INTENT(IN), OPTIONAL	:: lDebug
		INTEGER							:: iRetCode
		
		! Locals
		INTEGER	:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % sErrorMsg = " "
		
		! Get optional parameters
		IF(PRESENT(lDebug)) THEN
			this % lDebug = lDebug
		ELSE
			this % lDebug = .FALSE.
		END IF
		
		! Try reserving workspace
		IF(ALLOCATED(this % rmSumConc)) DEALLOCATE(this % rmSumConc)
		IF(ALLOCATED(this % rmSumConc2)) DEALLOCATE(this % rmSumConc2)
		ALLOCATE(this % rmSumConc(iNx, iNy), this % rmSumConc2(iNx, iNy), STAT=iErrCode)
		IF(iErrCode /= 0) THEN
			iRetCode = 1
			this % sErrorMsg = "Impossible to allocate concentration pool"
			CALL ErrorReport(SVR_ERROR, this % sErrorMsg)
			RETURN
		END IF
		
		! Assign values
		this % iNx          = iNx
		this % iNy          = iNy
		this % iSize        = 0
		this % iNextValue   = 0
		this % sResultsPath = sResultsPath
		this % rmSumConc    = 0.
		this % rmSumConc2   = 0.
		this % iNumFields   = 0
		
	END FUNCTION ConcPoolClean
	
	
	FUNCTION ConcAdd(this, rmConc) RESULT(iRetCode)
	
		! Routine arguments
		CLASS(ConcType)						:: this
		REAL, DIMENSION(:,:), INTENT(IN)	:: rmConc
		INTEGER								:: iRetCode
		
		! Locals
		INTEGER	:: iErrCode
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % sErrorMsg = " "
		
		! Check input matrix dimensions
		IF(SIZE(rmConc,DIM=1) /= this % iNx .OR. SIZE(rmConc,DIM=2) /= this % iNy) THEN
			iRetCode = 1
			this % sErrorMsg = "Concentration matrix dimension(s) not matching the ones desired"
			CALL ErrorReport(SVR_ERROR, this % sErrorMsg)
			RETURN
		END IF
		
		! Add matrix to proper position
		this % iNumFields = this % iNumFields + 1
		this % rmSumConc  = this % rmSumConc  + rmConc
		this % rmSumConc2 = this % rmSumConc2 + rmConc**2
	
	END FUNCTION ConcAdd
	
	
	FUNCTION ComputeAverages(this, rmAvg, rmStdDev) RESULT(iRetCode)
	
		! Routine arguments
		CLASS(ConcType)							:: this
		REAL, DIMENSION(:,:), INTENT(OUT)	:: rmAvg
		REAL, DIMENSION(:,:), INTENT(OUT)	:: rmStdDev
		INTEGER								:: iRetCode
		
		! Locals
		INTEGER	:: iErrCode
		INTEGER	:: iX
		INTEGER	:: iY
		
		! Assume success (will falsify on failure)
		iRetCode = 0
		this % sErrorMsg = " "
		
		! Check input matrix dimensions
		IF(SIZE(rmAvg,DIM=1) /= this % iNx .OR. SIZE(rmAvg,DIM=2) /= this % iNy) THEN
			iRetCode = 1
			this % sErrorMsg = "Averages matrix dimension(s) not matching the ones desired"
			CALL ErrorReport(SVR_ERROR, this % sErrorMsg)
			RETURN
		END IF
		IF(SIZE(rmStdDev,DIM=1) /= this % iNx .OR. SIZE(rmStdDev,DIM=2) /= this % iNy) THEN
			iRetCode = 2
			this % sErrorMsg = "Standard deviations matrix dimension(s) not matching the ones desired"
			CALL ErrorReport(SVR_ERROR, this % sErrorMsg)
			RETURN
		END IF
		
		! Compute desired values
		rmAvg    = this % rmSumConc / this % iNumFields
		rmStdDev = SQRT(this % rmSumConc2 / this % iNumFields - rmAvg**2)
		
		! Reset counters
		this % iNumFields = 0
		this % rmSumConc  = 0.
		this % rmSumConc2 = 0.
	
	END FUNCTION ComputeAverages
	
	! *********************
	! * Internal routines *
	! *********************
	
	SUBROUTINE ErrorReport(iSeverity, sErrMsg)
	
		! Routine arguments
		INTEGER, INTENT(IN)			:: iSeverity
		CHARACTER(LEN=*), INTENT(IN)	:: sErrMsg
		
		! Locals
		CHARACTER(LEN=8)	:: sSeverity
		
		! Print message
		SELECT CASE(iSeverity)
		CASE(SVR_INFO)
			sSeverity = "info"
		CASE(SVR_WARNING)
			sSeverity = "warning"
		CASE(SVR_ERROR)
			sSeverity = "error"
		END SELECT
		PRINT *,TRIM(sSeverity), "::", TRIM(sErrMsg)
		
		! Stop, if severity prevents to proceed
		IF(iSeverity >= SVR_ERROR) STOP
		
	END SUBROUTINE ErrorReport

END MODULE Concentration
