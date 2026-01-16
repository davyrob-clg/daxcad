C
C     SCCS id Keywords             @(#)  412.1 date 6/11/92 apotr81.f 
C
C     Practical Technology 1990 (c)
C
C     DAXCAD Source file
C
C     Functions and subroutines index:-
C
C     SUBROUTINE SETW2V
C
C
C     |-----------------------------------------------------------------|
C
      SUBROUTINE SETW2V
C     =================
C1    no arguments needed
C
C2       Subroutine SETW2V intialises the world to view
C2       coordinates transformation matix WVXY
C2
C2       The commom data block WTOV contians all
C2       variables associated with WORLD , SCREEN & VIEW
C2       coordinates.
C2
C2       World , Screen , View  limits
C
      include 'include/wtov.inc'
C
      REAL  A(3,3),WRATIO,VRATIO,WVSCAL,WVTRAX,WVTRAY,
     +      B(3,3),C(3,3),VAL
C
      EXTERNAL MULT3M,SCAL2D,TRAN2D,I3M,SC2WO,WO2SC
C
C        Aspect ratio of world,screen,viewport systems
C        determine the scaling conditions used
C
      WRATIO=(WXMAX-WXMIN)/(WYMAX-WYMIN)
      VRATIO=(VXMAX-VXMIN)/(VYMAX-VYMIN)
C
C        Test for ruling condition
C
      IF(WRATIO.LT.VRATIO) THEN
         WVSCAL=(VYMAX-VYMIN)/(WYMAX-WYMIN)
      ELSE
         WVSCAL=(VXMAX-VXMIN)/(WXMAX-WXMIN)
      END IF
C
C     Translate back to world origin
      CALL TRAN2D(-WXMIN,-WYMIN,A)
C
C     Scale aabout world origin by scaling factor
C     (note equal in each direction )
      CALL SCAL2D(WVSCAL,WVSCAL,B)
C     Concatenate result
      CALL MULT3M(A,B,C)
C     Mirror about the X-axis since origin is top,left
C     and Y direction is down not UP
      CALL SCAL2D(1.0,-1.0,B)
C     Concatenate result
      CALL MULT3M(C,B,A)
C     Translate back by Y limit difference to put old
C     Y max on the wrold origin
      CALL TRAN2D(0.0,VYMAX-VYMIN,B)
C     Concatenate result
      CALL MULT3M(A,B,C)
C     Trasnlate to position of window on screen
      CALL TRAN2D(VXMIN,VYMIN,B)
C     Finally concatenate result into World to View matrice
      CALL MULT3M(C,B,WVXY)
C
C        Matrix WVXY contains World to Viewport transformation
C
C        Find inverse of WVXY , call it VWXY
C        Matrix VWXY contains Viewport to World transformation
C
      CALL TRAN2D(-VXMIN,-VYMIN,A)
C
      CALL TRAN2D(0.0,VYMIN-VYMAX,B)
C
      CALL MULT3M(A,B,C)
C
      CALL SCAL2D(1.0,-1.0,B)
C
      CALL MULT3M(C,B,A)
C
      CALL SCAL2D(1.0/WVSCAL,1.0/WVSCAL,B)
C
      CALL MULT3M(A,B,C)
C
      CALL TRAN2D(WXMIN,WYMIN,B)
C
      CALL MULT3M(C,B,VWXY)
C
C       Set world window exactly on the screen window
C2     Note there is a change of VYMAX and VYMIN for
C2     screens which have their origin in the top left
C
      CALL SC2WO(VXMIN,VYMAX,WXMIN,WYMIN)
      CALL SC2WO(VXMAX,VYMIN,WXMAX,WYMAX)
C
C       Set screen window exactly on the world window
C      CALL WO2SC(WXMIN,WYMIN,VXMIN,VYMIN)
C      CALL WO2SC(WXMAX,WYMAX,VXMAX,VYMAX)
C
      END
 
