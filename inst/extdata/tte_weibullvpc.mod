$PROBLEM    Time-to-event model example Weibull distribution
$INPUT      ID TIME DV EVID DOSE EXPO AGE
$DATA       tte_data1.dat
            IGNORE=@
$SUBROUTINE ADVAN=6 TOL=9
$MODEL      COMP=(HAZARD)
$ABB COMRES=7
$PK
IF (NEWIND.EQ.0) THEN      ; Only for the first record
  COM(5) = (IREP-1)*NINDR+1 ; Reset simulation ID counter
"  ! Initialize sim output file
"  OPEN (99, FILE = 'vpctabC:/PMX/Projects/Pharmetheus/PMXtte/inst/extdata/tte_weibull', POSITION='APPEND')
"  IF (IREP.EQ.1) THEN !Write header for 1st subproblem
"    WRITE (99,'(A,7(1XA))') 'ID','DV','TIME','RTTE','SUR','ICOUNT','ITER','RAND'
"  ENDIF
ENDIF
IF (NEWIND.EQ.1) THEN      ; For every new ind except first in dataset
  COM(5) = COM(5) + 1  ; Update individual counter over simulations
ENDIF

IF (ICALL.EQ.4) THEN 
 IF (NEWIND.NE.2) THEN      ; For every new individual
  CALL RANDOM(2,R)
  COM(4) = ENDTIME ; Maxtime per individual (in hours)
  COM(3) = -1          ; Variable for survival at event time
  COM(2) = R; Store the random number
  COM(1) = -1          ; Variable for the event time
  COM(6) = 0            ; Individual event counter
  COM(7) = 0            ; Cumulative hazard
 ENDIF
ENDIF

;Store iteration num (dataset sim num)
ITER = IREP

   LAM= THETA(1)*EXP(ETA(1))
   SHP=THETA(2)

;--------------Covariate relationships---------------
;--------------SCM compatible code-------------------
   TVRF =  (AGE-50)*THETA(3)
   TVEFF = THETA(4)*EXPO
   RFCOV = 0

   TVRF = RFCOV+TVRF
   RF   = 1*TVRF+TVEFF



;---------MTIME for increasing $DES precision --------
IF (TIME.EQ.0) TEMP=0
TEMP=TEMP+0.1
MTIME(1)=TEMP
MTDIFF=1


IF (COM(1).EQ.-1) THEN ; IF NO EVENT SIMULATED YET
 
ENDIF
$DES


   DEL= 1E-12

;----------hazard-----------------------------------

   DADT(1)=LAM*EXP(RF)*SHP*(LAM*T+DEL)**(SHP-1)

;----------TTE Model------------------------------

;---------- TTE Simulation specific
SUR = EXP(-A(1)) ; Survival at this T)
IF(COM(2).GT.SUR.AND.COM(1).EQ.-1) THEN ; If event, save event time in COM(1)
 COM(1)=T
 COM(3)=SUR
ENDIF
$ERROR

  CHZ = A(1)
  SURX = EXP(-CHZ)            ;survival probability

  DELX = 1E-12

  HAZNOW = LAM*SHP*EXP(RF)*(LAM*TIME+DELX)**(SHP-1)

  Y=SURX                      ;censored event (prob of survival)
  IF(DV.EQ.1)  Y=SURX*HAZNOW  ;prob density function of event

SURX2 = EXP(-(A(1)-COM(7))) ; Survival at last record
IF (LIREC.EQ.NDREC) THEN ;Last record per individual
 IF (COM(1).GT.COM(4)) THEN ;IF T > ENDTIME, T=ENDTIME
  IF (COM(2).GT.SURX) THEN
   COM(1) = COM(4)
  ELSE 
   COM(1) = -1 ;Integrated too far, reset event
  ENDIF
 ENDIF
  IF (COM(1).NE.-1) THEN ;If an EVENT 
  TDVX=1
  RTTE=1 
 ; Write event specific output
"      WRITE (99,'(E13.7,7(1XE13.7))') ID,TDVX,COM(1),RTTE,COM(3),COM(5),ITER,COM(2)
  ELSE
  IF (LIREC.EQ.NDREC) THEN ;Right Censoring
   TDVX=0
   COM(3)=SURX2; Survival at last record, not neccesarily censoring time
   COM(6)=COM(6)+1 ; Event counter
   
  " WRITE (99,'(E13.7,7(1XE13.7))') ID,TDVX,COM(4),COM(6),COM(3),COM(5),ITER,COM(2)
  ENDIF
  ENDIF
ENDIF
IF (NDREC.EQ.LIREC.AND.NIREC.EQ.NINDR) THEN ; Last record for last individual
  CLOSE(99) ; Close File pointer
ENDIF

$THETA 0.0109033 ; LAMBDA; 1
$THETA 1.31887 ; SHAPE; 2  1 FIX for exponential distr
$THETA 0.0134745 ; AGE EFFECT
$THETA -0.010509 ; EXPOSURE EFFECT
$OMEGA BLOCK(1) 
0 FIX
$SIMULATION (1137034) (6222994 UNIFORM) ONLYSIMULATION NOPREDICTION NSUB=100


