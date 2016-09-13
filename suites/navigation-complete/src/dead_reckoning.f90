PROGRAM extract_compass_log
IMPLICIT NONE
CHARACTER(31)    :: dt_hr_env    ! No. of hours, from environment
CHARACTER(255)   :: pos_fpath    ! File path to a position file with lat/long
CHARACTER(255)   :: task_log_root! File path to cylc output files
INTEGER:: code             ! iostat code
INTEGER:: timevalues(8)    ! time values
INTEGER, PARAMETER :: real64=SELECTED_REAL_KIND(15, 307)
REAL(KIND=real64) :: ang_distance ! Angular distance travelled across Earth
REAL(KIND=real64) :: dt_hr        ! No. of hours elapsed, from dt_hr_env
REAL(KIND=real64) :: heading      ! Compass heading in radians
REAL(KIND=real64) :: lat          ! Initial latitude
REAL(KIND=real64) :: long         ! Initial longitude
REAL(KIND=real64) :: new_lat      ! Final latitude
REAL(KIND=real64) :: new_long     ! Final longitude
REAL(KIND=real64) :: speed_kn=5.0 ! Speed in knots
REAL(KIND=real64), PARAMETER :: pi=3.141592654          ! pi
REAL(KIND=real64), PARAMETER :: radius_earth_nm=3443.89 ! Earth radius (nm)

! Get position file location via $POSITION_FILEPATH
CALL get_environment_variable("POSITION_FILEPATH",value=pos_fpath,status=code)
IF (code /= 0) THEN
  WRITE(0,*) "$POSITION_FILEPATH: not set."
  STOP 1
END IF

! Read in starting latitude and longitude
OPEN(1,file=pos_fpath,action="read",iostat=code)
IF (code /= 0) THEN
  WRITE(0,*) pos_fpath,": position file read failed."
  STOP 1
END IF
READ(1,*) lat,long
CLOSE(1)

! Convert to radians, where they belong
lat = (pi/180.0) * lat
long = (pi/180.0) * long

! Read in our duration input
CALL get_environment_variable("TIME_INTERVAL_HRS",value=dt_hr_env,status=code)
IF (code /= 0) THEN
  WRITE(0,*) "$TIME_INTERVAL_HRS: not set"
  STOP 1
END IF
READ(dt_hr_env,*) dt_hr

! Pretend to extract an average heading from the ship's compass
CALL date_and_time(VALUES=timevalues)
heading = mod(1000 * timevalues(7) + timevalues(8), 60) * 2 * pi / 60

! This is how far we went, in radians:
! (1 knot = 1 nautical mile / 1 hour)
ang_distance = (speed_kn*dt_hr) / radius_earth_nm

! Get the new latitude and longitude
new_lat = ASIN(SIN(lat) * COS(ang_distance) + &
               COS(lat) * SIN(ang_distance) * COS(heading))
new_long = long + &
           ATAN2(SIN(heading) * SIN(ang_distance) * COS(lat), &
                 COS(ang_distance) - SIN(lat) * SIN(new_lat))
new_lat = (180.0/pi) * new_lat
new_long = (180.0/pi) * new_long

lat = (180.0/pi) * lat
long = (180.0/pi) * long
CALL get_environment_variable("CYLC_TASK_LOG_ROOT",value=task_log_root,status=code)
OPEN(1,file=TRIM(task_log_root) // '-map.html',action='write')
WRITE(1,'(A, F7.4, A, F7.4, A, F7.4, A, F7.4, A, F7.4, A, F7.4, A)') &
"<img src='https://maps.googleapis.com/maps/api/staticmap?center=",&
lat,",",long,"&zoom=7&size=600x300&markers=color:blue|label:A|",&
lat,",",long,"&markers=color:red|label:B|",new_lat,",",new_long,&
"'/>"
CLOSE(1)

PRINT*, "New position, me hearties:",new_lat," ",new_long

! Overwrite position file with new lat and long
OPEN(1,file=pos_fpath,action='write')
WRITE(1,*) new_lat,new_long
CLOSE(1)
END PROGRAM

