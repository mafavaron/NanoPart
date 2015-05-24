!------------------------------------------------------------------
! Calendar
!
! Module, supporting the treatment of date and time information, up to
! the resolution of 1s.
!
! Copyright (C) 2004 by Servizi Territorio srl
!
! Written by: M.Favaron
! e-mail:     mafavaron@tin.it
!
!------------------------------------------------------------------
! Statement of Licensing Conditions
!------------------------------------------------------------------
!
!   This library is free software; you can redistribute it and/or
!   modify it under the terms of the GNU Lesser General Public
!   License as published by the Free Software Foundation; either
!   version 2.1 of the License, or (at your option) any later version.
!
!   This library is distributed in the hope that it will be useful,
!   but WITHOUT ANY WARRANTY; without even the implied warranty of
!   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
!   Lesser General Public License for more details.
!
!   You should have received a copy of the GNU Lesser General Public
!   License along with this library; if not, write to the Free Software
!   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA  02111-1307  USA
!
!------------------------------------------------------------------

MODULE Calendar

    IMPLICIT NONE

    PRIVATE

   ! Public interface
    ! -1- Date oriented routines
    PUBLIC  :: JulianDay
    PUBLIC	:: DayInYear
    PUBLIC  :: UnpackDate
    PUBLIC  :: DayOfWeek
    PUBLIC  :: Season
    ! -1- Date and time oriented routines
    PUBLIC  :: PackTime
    PUBLIC  :: UnpackTime
    ! -1- Date and time formatting and decoding routines
    PUBLIC  :: FormatDate
    PUBLIC  :: FormatTime
    PUBLIC  :: FormatFileTime
    PUBLIC  :: DecodeDate
    PUBLIC  :: DecodeTime
    PUBLIC  :: DecodeHour
    ! -1- Auxiliary procedures
    PUBLIC	:: NumDays
    ! -1- Constants
    INTEGER, PARAMETER  :: HOUR_00_23 = 0
    PUBLIC              :: HOUR_00_23
    INTEGER, PARAMETER  :: HOUR_01_24 = 1
    PUBLIC              :: HOUR_01_24

    ! Internal parameters
    REAL, PARAMETER     :: YEAR_DURATION   = 365.25
    REAL, PARAMETER     :: MONTH_DURATION  = 30.6001
    INTEGER, PARAMETER  :: BASE_DAY        = 2440588    ! 01. 01. 1970
    
    ! Interfaces
    INTERFACE Season
        MODULE PROCEDURE SeasonA, SeasonB
    END INTERFACE Season

CONTAINS

    FUNCTION JulianDay(iYear, iMonth, iDay) RESULT(iJulianDay)

        ! Routine arguments
        INTEGER, INTENT(IN) :: iYear
        INTEGER, INTENT(IN) :: iMonth
        INTEGER, INTENT(IN) :: iDay
        INTEGER             :: iJulianDay

        ! Locals
        INTEGER     :: iAuxYear
        INTEGER     :: iAuxMonth
        INTEGER     :: iCentury
        INTEGER     :: iTryJulianDay
        INTEGER     :: iNumDays
        INTEGER, PARAMETER  :: DATE_REFORM_DAY = 588829 ! 15 October 1582, with 31-days months
        INTEGER, PARAMETER  :: BASE_DAYS       = 1720995

        ! Check year against invalid values. Only positive
        ! years are supported in this version. Year "0" does
        ! not exist.
        IF(iYear <= 0) THEN
            iJulianDay = -9999
            RETURN
        END IF

        ! Check month and day to look valid (a rough, non-month-aware
        ! test is intentionally adopted in sake of simplicity)
        IF((.NOT.(1<=iMonth .AND. iMonth<=12)) .OR. (.NOT.(1<=iDay .AND. iDay<=31))) THEN
            iJulianDay = -9999
            RETURN
        END IF

        ! Preliminary estimate the Julian day, based on
        ! the average duration of year and month in days.
        IF(iMonth > 2) THEN
            iAuxYear  = iYear
            iAuxMonth = iMonth + 1
        ELSE
            iAuxYear  = iYear - 1
            iAuxMonth = iMonth + 13
        END IF
        iTryJulianDay = FLOOR(YEAR_DURATION * iAuxYear) + &
                        FLOOR(MONTH_DURATION * iAuxMonth) + &
                        iDay + BASE_DAYS

        ! Correct estimate if later than the date reform day
        iNumDays = iDay + 31*iMonth + 372*iYear
        IF(iNumDays >= DATE_REFORM_DAY) THEN
            iCentury = 0.01*iAuxYear
            iJulianDay = iTryJulianDay - iCentury + iCentury/4 + 2
        ELSE
            iJulianDay = iTryJulianDay
        END IF

    END FUNCTION JulianDay


    FUNCTION DayInYear(iYear, iMonth, iDay) RESULT(iDayInYear)

        ! Routine arguments
        INTEGER, INTENT(IN) :: iYear
        INTEGER, INTENT(IN) :: iMonth
        INTEGER, INTENT(IN) :: iDay
        INTEGER             :: iDayInYear

        ! Locals
        INTEGER     :: iFirstJulianDay
        INTEGER     :: iCurrentJulianDay

        ! Compute the information desired
        iFirstJulianDay = JulianDay(iYear, 1, 1)
        iCurrentJulianDay = JulianDay(iYear, iMonth, iDay)
        iDayInYear = iCurrentJulianDay - iFirstJulianDay + 1

    END FUNCTION DayInYear


    SUBROUTINE UnpackDate(iJulianDay, iYear, iMonth, iDay)

        ! Routine arguments
        INTEGER, INTENT(IN)     :: iJulianDay
        INTEGER, INTENT(OUT)    :: iYear
        INTEGER, INTENT(OUT)    :: iMonth
        INTEGER, INTENT(OUT)    :: iDay

        ! Locals
        INTEGER :: iDeviation
        INTEGER :: iPreJulianDay
        INTEGER :: iPostJulianDay
        INTEGER :: iYearIndex
        INTEGER :: iMonthIndex
        INTEGER :: iDayIndex
        INTEGER, PARAMETER  :: LIMIT_JULIAN_DAY = 2299161
        INTEGER, PARAMETER  :: CORRECTION_DAYS  = 1524

        ! Unwind Pope Gregorius' day correction
        IF(iJulianDay >= LIMIT_JULIAN_DAY) THEN
            iDeviation = FLOOR(((iJulianDay-1867216)-0.25)/36524.25)
            iPreJulianDay = iJulianDay + iDeviation - iDeviation/4 + 1
        ELSE
            iPreJulianDay = iJulianDay
        END IF
        iPostJulianDay = iPreJulianDay + CORRECTION_DAYS

        ! Compute time indices
        iYearIndex  = FLOOR(6680+((iPostJulianDay-2439870)-122.1)/YEAR_DURATION)
        iDayIndex   = 365*iYearIndex + iYearIndex/4
        iMonthIndex = FLOOR((iPostJulianDay - iDayIndex)/MONTH_DURATION)

        ! Deduce preliminary date from time indices
        iDay = iPostJulianDay - FLOOR(MONTH_DURATION*iMonthIndex) - iDayIndex
        IF(iMonthIndex > 13) THEN
            iMonth = iMonthIndex - 13
        ELSE
            iMonth = iMonthIndex - 1
        END IF
        iYear = iYearIndex - 4715
        IF(iMonth > 2) iYear = iYear - 1

    END SUBROUTINE UnpackDate


    FUNCTION DayOfWeek(iJulianDay) RESULT(iDayOfWeek)

        ! Routine arguments
        INTEGER, INTENT(IN) :: iJulianDay
        INTEGER         :: iDayOfWeek

        ! Locals
        ! -none-

        ! Compute the desired quantity
        iDayOfWeek = MOD(iJulianDay, 7)

    END FUNCTION DayOfWeek
    
    
    FUNCTION SeasonA(iJulianDay) RESULT(iSeason)
    
        ! Routine arguments
        INTEGER, INTENT(IN) :: iJulianDay
        INTEGER             :: iSeason
        
        ! Locals
        INTEGER :: iYear
        INTEGER :: iMonth
        INTEGER :: iDay
        INTEGER :: iSpring
        INTEGER :: iSummer
        INTEGER :: iAutumn
        INTEGER :: iWinter
        
        ! Convert Julian day to date
        CALL UnpackDate(iJulianDay, iYear, iMonth, iDay)
        iSpring = JulianDay(iYear,  3, 21)
        iSummer = JulianDay(iYear,  6, 21)
        iAutumn = JulianDay(iYear,  9, 21)
        iWinter = JulianDay(iYear, 12, 21)
        
        ! Compute season
        IF(iSpring <= iJulianDay .AND. iJulianDay < iSummer) THEN
            iSeason = 2
        ELSEIF(iSummer <= iJulianDay .AND. iJulianDay < iAutumn) THEN
            iSeason = 3
        ELSEIF(iAutumn <= iJulianDay .AND. iJulianDay < iWinter) THEN
            iSeason = 4
        ELSE
            iSeason = 1
        END IF
        
    END FUNCTION SeasonA
    
    
    FUNCTION SeasonB(iYear, iMonth, iDay) RESULT(iSeason)
    
        ! Routine arguments
        INTEGER, INTENT(IN) :: iYear
        INTEGER, INTENT(IN) :: iMonth
        INTEGER, INTENT(IN) :: iDay
        INTEGER             :: iSeason
        
        ! Locals
        INTEGER :: iJulianDay
        INTEGER :: iSpring
        INTEGER :: iSummer
        INTEGER :: iAutumn
        INTEGER :: iWinter
        
        ! Convert date to Julian day
        iJulianDay = JulianDay(iYear, iMonth, iDay)
        iSpring = JulianDay(iYear,  3, 21)
        iSummer = JulianDay(iYear,  6, 21)
        iAutumn = JulianDay(iYear,  9, 21)
        iWinter = JulianDay(iYear, 12, 21)
        
        ! Compute season
        IF(iSpring <= iJulianDay .AND. iJulianDay < iSummer) THEN
            iSeason = 2
        ELSEIF(iSummer <= iJulianDay .AND. iJulianDay < iAutumn) THEN
            iSeason = 3
        ELSEIF(iAutumn <= iJulianDay .AND. iJulianDay < iWinter) THEN
            iSeason = 4
        ELSE
            iSeason = 1
        END IF
        
    END FUNCTION SeasonB
    
    
    SUBROUTINE PackTime(iTime, iYear, iMonth, iDay, iInHour, iInMinute, iInSecond)
    
        ! Routine arguments
        INTEGER, INTENT(OUT)            :: iTime
        INTEGER, INTENT(IN)             :: iYear
        INTEGER, INTENT(IN)             :: iMonth
        INTEGER, INTENT(IN)             :: iDay
        INTEGER, INTENT(IN), OPTIONAL   :: iInHour
        INTEGER, INTENT(IN), OPTIONAL   :: iInMinute
        INTEGER, INTENT(IN), OPTIONAL   :: iInSecond
        
        ! Locals
        INTEGER :: iHour
        INTEGER :: iMinute
        INTEGER :: iSecond
        INTEGER :: iJulianDay
        INTEGER :: iJulianSecond
        
        ! Check for optional parameters; assign defaults if necessary
        IF(PRESENT(iInHour)) THEN
            iHour = iInHour
        ELSE
            iHour = 0
        END IF
        IF(PRESENT(iInMinute)) THEN
            iMinute = iInMinute
        ELSE
            iMinute = 0
        END IF
        IF(PRESENT(iInSecond)) THEN
            iSecond = iInSecond
        ELSE
            iSecond = 0
        END IF
        
        ! Check input parameters for validity
        IF( &
            iHour   < 0 .OR. iHour   > 23 .OR. &
            iMinute < 0 .OR. iMinute > 59 .OR. &
            iSecond < 0 .OR. iSecond > 59 &
        ) THEN
            iTime = -1
            RETURN
        END IF
        
        ! Compute based Julian day
        iJulianDay = JulianDay(iYear, iMonth, iDay) - BASE_DAY
        
        ! Convert based Julian day to second, and add seconds from time,
        ! regardless of hour type.
        iJulianSecond = iJulianDay * 24 * 3600
        iTime = iJulianSecond + iSecond + 60*(iMinute + 60*iHour)
        
    END SUBROUTINE PackTime
    
    
    SUBROUTINE UnpackTime(iTime, iYear, iMonth, iDay, iHour, iMinute, iSecond)
    
        ! Routine arguments
        INTEGER, INTENT(IN)     :: iTime
        INTEGER, INTENT(OUT)    :: iYear
        INTEGER, INTENT(OUT)    :: iMonth
        INTEGER, INTENT(OUT)    :: iDay
        INTEGER, INTENT(OUT)    :: iHour
        INTEGER, INTENT(OUT)    :: iMinute
        INTEGER, INTENT(OUT)    :: iSecond
        
        ! Locals
        INTEGER :: iJulianDay
        INTEGER :: iTimeSeconds
        
        ! Check parameter
        IF(iTime < 0) THEN
            iYear   = 1970
            iMonth  = 1
            iDay    = 1
            iHour   = 0
            iMinute = 0
            iSecond = 0
            RETURN
        END IF
        
        ! Isolate the date and time parts
        iJulianDay = iTime/(24*3600) + BASE_DAY
        iTimeSeconds = MOD(iTime, 24*3600)
        
        ! Process the date part
        CALL UnpackDate(iJulianDay, iYear, iMonth, iDay)
        
        ! Extract time from the time part
        iSecond = MOD(iTimeSeconds,60)
        iTimeSeconds = iTimeSeconds/60
        iMinute = MOD(iTimeSeconds,60)
        iHour   = iTimeSeconds/60
        
    END SUBROUTINE UnpackTime
    
    
    FUNCTION FormatDate(iYear, iMonth, iDay, lShortFormIn) RESULT(sDate)
    
        ! Routine arguments
        INTEGER, INTENT(IN)             :: iYear
        INTEGER, INTENT(IN)             :: iMonth
        INTEGER, INTENT(IN)             :: iDay
        LOGICAL, INTENT(IN), OPTIONAL   :: lShortFormIn
        CHARACTER(LEN=10)               :: sDate
        
        ! Locals
        LOGICAL :: lShortForm
        
        ! Format date
        IF(PRESENT(lShortFormIn)) THEN
            lShortForm = lShortFormIn
        ELSE
            lShortForm = .FALSE.
        END IF
        IF(lShortForm) THEN
            WRITE(sDate,"(2(i2.2,'/'),i2.2)") iDay, iMonth, MOD(iYear,100)
        ELSE
            WRITE(sDate,"(2(i2.2,'/'),i4.4)") iDay, iMonth, iYear
        END IF
        
    END FUNCTION FormatDate

    
    FUNCTION FormatTime(iYear, iMonth, iDay, iHourIn, iMinuteIn, iSecondIn, lShortFormIn) RESULT(sDate)
    
        ! Routine arguments
        INTEGER, INTENT(IN)             :: iYear
        INTEGER, INTENT(IN)             :: iMonth
        INTEGER, INTENT(IN)             :: iDay
        INTEGER, INTENT(IN), OPTIONAL   :: iHourIn
        INTEGER, INTENT(IN), OPTIONAL   :: iMinuteIn
        INTEGER, INTENT(IN), OPTIONAL   :: iSecondIn
        LOGICAL, INTENT(IN), OPTIONAL   :: lShortFormIn
        CHARACTER(LEN=19)               :: sDate
        
        ! Locals
        INTEGER :: iHour
        INTEGER :: iMinute
        INTEGER :: iSecond
        LOGICAL :: lShortForm
        
        ! Format date
        IF(PRESENT(iHourIn)) THEN
            iHour = iHourIn
        ELSE
            iHour = 0
        END IF
        IF(PRESENT(iMinuteIn)) THEN
            iMinute = iMinuteIn
        ELSE
            iMinute = 0
        END IF
        IF(PRESENT(iSecondIn)) THEN
            iSecond = iSecondIn
        ELSE
            iSecond = 0
        END IF
        IF(PRESENT(lShortFormIn)) THEN
            lShortForm = lShortFormIn
        ELSE
            lShortForm = .FALSE.
        END IF
        IF(lShortForm) THEN
            WRITE(sDate,"(2(i2.2,'/'),i2.2,1x,2(i2.2,':'),i2.2)") iDay, iMonth, MOD(iYear,100), iHour, iMinute, iSecond
        ELSE
            WRITE(sDate,"(2(i2.2,'/'),i4.4,1x,2(i2.2,':'),i2.2)") iDay, iMonth, iYear, iHour, iMinute, iSecond
        END IF
        
    END FUNCTION FormatTime
    
    
    FUNCTION FormatFileTime(iYear, iMonth, iDay, iHour) RESULT(sDate)
    
        ! Routine arguments
        INTEGER, INTENT(IN) :: iYear
        INTEGER, INTENT(IN) :: iMonth
        INTEGER, INTENT(IN) :: iDay
        INTEGER, INTENT(IN) :: iHour
        CHARACTER(LEN=11)   :: sDate
        
        ! Locals
        INTEGER :: iTotalYear
        
        ! Format date
        IF(iYear > 100) THEN
            WRITE(sDate,"(i4.4,2i2.2,'.',i2.2)") iYear, iMonth, iDay, iHour
        ELSE
            IF(iYear < 70) THEN
                iTotalYear = iYear + 2000
            ELSE
                iTotalYear = iYear + 1900
            END IF
            WRITE(sDate,"(i4.4,2i2.2,'.',i2.2)") iTotalYear, iMonth, iDay, iHour
        END IF
        
    END FUNCTION FormatFileTime
    
    
    SUBROUTINE DecodeDate(sDate, iYear, iMonth, iDay)
    
        ! Routine arguments
        CHARACTER(LEN=*), INTENT(IN)    :: sDate
        INTEGER, INTENT(OUT)            :: iYear
        INTEGER, INTENT(OUT)            :: iMonth
        INTEGER, INTENT(OUT)            :: iDay
        
        ! Locals
        INTEGER :: iLength
        INTEGER :: iRetCode
        
        ! Infer type of date from string length
        iLength = LEN_TRIM(sDate)
        SELECT CASE(iLength)
        CASE(8)
            ! Short-form date
            READ(sDate,"(i2,1x,i2,1x,i2)",IOSTAT=iRetCode) iDay, iMonth, iYear
            IF(iRetCode == 0) THEN
                IF(iYear > 70) THEN
                    iYear = iYear + 1900
                ELSE
                    iYear = iYear + 2000
                END IF
            ELSE
                iYear  = -9999
                iMonth = -9999
                iDay   = -9999
            END IF
        CASE(10)
            ! Long-form date
            READ(sDate,"(i2,1x,i2,1x,i4)",IOSTAT=iRetCode) iDay, iMonth, iYear
            IF(iRetCode /= 0) THEN
                iYear  = -9999
                iMonth = -9999
                iDay   = -9999
            END IF
        CASE DEFAULT
            iYear  = -9999
            iMonth = -9999
            iDay   = -9999
        END SELECT
        
    END SUBROUTINE DecodeDate
    
    
    SUBROUTINE DecodeTime(sDate, iYear, iMonth, iDay, iHour, iMinute, iSecond)
    
        ! Routine arguments
        CHARACTER(LEN=*), INTENT(IN)    :: sDate
        INTEGER, INTENT(OUT)            :: iYear
        INTEGER, INTENT(OUT)            :: iMonth
        INTEGER, INTENT(OUT)            :: iDay
        INTEGER, INTENT(OUT)            :: iHour
        INTEGER, INTENT(OUT)            :: iMinute
        INTEGER, INTENT(OUT)            :: iSecond
        
        ! Locals
        INTEGER :: iLength
        INTEGER :: iRetCode
        
        ! Infer type of date from string length
        iLength = LEN_TRIM(sDate)
        SELECT CASE(iLength)
        CASE(8)
            ! Short-form date
            READ(sDate,"(i2,1x,i2,1x,i2)",IOSTAT=iRetCode) iDay, iMonth, iYear
            IF(iRetCode /= 0) THEN
                iYear   = -9999
                iMonth  = -9999
                iDay    = -9999
                iHour   = -9999
                iMinute = -9999
                iSecond = -9999
            ELSE
                iHour   = 0
                iMinute = 0
                iSecond = 0
                IF(iYear > 70) THEN
                    iYear = iYear + 1900
                ELSE
                    iYear = iYear + 2000
                END IF
            END IF
        CASE(10)
            ! Long-form date
            READ(sDate,"(i2,1x,i2,1x,i4)",IOSTAT=iRetCode) iDay, iMonth, iYear
            IF(iRetCode /= 0) THEN
                iYear   = -9999
                iMonth  = -9999
                iDay    = -9999
                iHour   = -9999
                iMinute = -9999
                iSecond = -9999
            ELSE
                iHour   = 0
                iMinute = 0
                iSecond = 0
            END IF
        CASE(17)
            ! Short-form time
            READ(sDate,"(i2,1x,i2,1x,i2,1x,i2,1x,i2,1x,i2)",IOSTAT=iRetCode) iDay, iMonth, iYear, iHour, iMinute, iSecond
            IF(iRetCode /= 0) THEN
                iYear   = -9999
                iMonth  = -9999
                iDay    = -9999
                iHour   = -9999
                iMinute = -9999
                iSecond = -9999
            ELSE
                IF(iYear < 70) THEN
                    iYear = 2000 + iYear
                ELSE
                    iYear = 1900 + iYear
                END IF
            END IF
        CASE(19)
            ! Long-form time
            READ(sDate,"(i2,1x,i2,1x,i4,1x,i2,1x,i2,1x,i2)",IOSTAT=iRetCode) iDay, iMonth, iYear, iHour, iMinute, iSecond
            IF(iRetCode /= 0) THEN
                iYear   = -9999
                iMonth  = -9999
                iDay    = -9999
                iHour   = -9999
                iMinute = -9999
                iSecond = -9999
            END IF
        CASE DEFAULT
            iYear   = -9999
            iMonth  = -9999
            iDay    = -9999
            iHour   = -9999
            iMinute = -9999
            iSecond = -9999
        END SELECT
        
    END SUBROUTINE DecodeTime

    
    SUBROUTINE DecodeHour(sTime, iHour, iMinute, iSecond)
    
        ! Routine arguments
        CHARACTER(LEN=*), INTENT(IN)    :: sTime
        INTEGER, INTENT(OUT)            :: iHour
        INTEGER, INTENT(OUT)            :: iMinute
        INTEGER, INTENT(OUT)            :: iSecond
        
        ! Locals
        INTEGER :: iLength
        INTEGER :: iRetCode
        
        ! Infer type of date from string length
        iLength = LEN_TRIM(sTime)
        SELECT CASE(iLength)
        CASE(8)
            ! hh:mm:ss
            READ(sTime,"(i2,1x,i2,1x,i2)",IOSTAT=iRetCode) iHour, iMinute, iSecond
            IF(iRetCode /= 0) THEN
                iHour   = -9999
                iMinute = -9999
                iSecond = -9999
            END IF
        CASE(5)
            ! hh:mm
            READ(sTime,"(i2,1x,i2)",IOSTAT=iRetCode) iHour, iMinute
            IF(iRetCode /= 0) THEN
                iHour   = -9999
                iMinute = -9999
                iSecond = -9999
            ELSE
                iSecond = 0
            END IF
        CASE(2)
            ! hh
            READ(sTime,"(i2)",IOSTAT=iRetCode) iHour
            IF(iRetCode /= 0) THEN
                iHour   = -9999
                iMinute = -9999
                iSecond = -9999
            ELSE
                iMinute = 0
                iSecond = 0
            END IF
        CASE DEFAULT
            iHour   = -9999
            iMinute = -9999
            iSecond = -9999
        END SELECT
        
    END SUBROUTINE DecodeHour
    
    
    FUNCTION NumDays(iYear, iMonthIn) RESULT(iDays)
    
    	! Routine arguments
    	INTEGER, INTENT(IN)	:: iYear
    	INTEGER, INTENT(IN)	:: iMonthIn
    	INTEGER				:: iDays
    	
    	! Locals
    	INTEGER								:: iMonth
    	INTEGER, DIMENSION(12), PARAMETER	:: ivDaysN = (/ &
    		31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 &
    	/)
    	INTEGER, DIMENSION(12), PARAMETER	:: ivDaysE = (/ &
    		31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 &
    	/)
    	
    	! Compute the number of days in current month
    	iMonth = MAX(MIN(iMonthIn,12),1)
    	IF(MOD(iYear,4)/=0) THEN
    		iDays = ivDaysN(iMonth)
    	ELSE
    		iDays = ivDaysE(iMonth)
    	END IF
    	
    END FUNCTION NumDays

END MODULE Calendar



