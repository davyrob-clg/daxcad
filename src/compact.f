C
C
C
C	@(#) Advent/Intelliscan	%A%
C	Release ID:		%I%
C	Last Change:		%G%-%U%
C	Date Retrieved:		%H%-%T%
C
C	Copyright (C) 1994 Advent Imaging Limited. (ALL RIGHTS RESERVED)
C
C     Intelliscan Limited 
C
C	Copyright in the whole and every part of this source file belongs to
C	The Company and may not be used,  sold, licenced,
C	transferred, copied or reproduced in whole or in
C	part in any manner or form or in or on any media to any person
C	other than in accordance with the terms of The Company's agreement
C	or otherwise without the prior written consent of The Company.  All
C	information contained in this source file is confidential information
C	belonging to The Company and as such may not be disclosed other
C	than in accordance with the terms of The Company's agreement, or
C	otherwise, without the prior written consent of The Company.  As
C	confidential information this source file must be kept fully and
C	effectively secure at all times.
C
C
C	DESCRIPTION:
C
C	New compact code for creating a compacted drawing
C

C
      SUBROUTINE CreateCompactDrg(FNAM,DBASE)
C     =======================================
C
C     vartype                     C*(*)  I4(7)
C     iostatus                      I     I
C
C     Create a drawing file by using the contents of SWINDU to
C     write out all non deleted entities.
C     
C     We must make multiple passes on the SWINDU
C
      include 'include/masti.inc'
      include 'include/movdat.inc'
      include 'include/fhead.inc'
      include 'include/dhead.inc'
      include 'include/nhead.inc'
      include 'include/filunit.inc'
      include 'include/params.inc'
      include 'include/props.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
C
      INTEGER*4 DBASE(7),NREC

      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TTPRP,TTPCP,II
      INTEGER*2 T2MIP,T2PDP,T2TXP,T2RLP,T2PRP,T2PCP,WUN
      INTEGER*2 NEWMIP
      INTEGER*2 NEWPDP
      INTEGER*2 NEWTXP
      INTEGER*2 NEWRLP
      INTEGER*2 NEWPP
      INTEGER*2 OLDIMBUFF(13)
      INTEGER*2 OLDIDBUFF(4)
      INTEGER*2 OLDICBUFF(2)
      INTEGER*2 OLDRLBUFF(10)
      INTEGER*2 TTXP
      INTEGER*2 TMIP
      INTEGER*2 TPDP
      INTEGER*2 TRLP
      INTEGER*2 I
      INTEGER*2 TRMIP
      INTEGER*2 RLPPP
      INTEGER*2 RLOOKUP(5000)
      INTEGER*2 RLPOS

      LOGICAL OK
      CHARACTER*(*) FNAM
      INTEGER*2 SPA
      INTEGER*4 POS
C
C     ensure writing starts at beginning of file
      REWIND(UNIT=PARFUN,ERR=99)
C
      RLPOS = 1
      WUN=1
      T2MIP=TTMIP-WUN
      T2PDP=TTPDP-WUN
      T2TXP=TTTXP-WUN
      T2RLP=TTRLP-WUN
      T2PRP=TTRLP-WUN
      T2PRP=TTPRP-WUN
      T2PCP=TTPCP-WUN

C     Create a new lookup table for relation lists
      DO 5000,II=1,NRLPOS-1
	  CALL DRR950(II,OK)
	  TMIP = RLBUFF(3)
	  CALL DIR500(TMIP,OK)
	  IF ( IMBUFF(13).GT.0) THEN
	      RLOOKUP(II) = RLPOS
	      RLPOS = RLPOS + 1
          ENDIF
5000  CONTINUE
C
C     create file header
      CALL MAKFHD()
      CFHEAD(1)='DRAWING'
      CFHEAD(2)=FNAM
C     write the file header
      CALL SAVFHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     create a drawing header
C
      CALL MAKDHD()
      CALL SAVDHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     Create viewing header block
C
      CALL MAKVHD()
      CALL SAVVHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     create drawing parameters header block
C
      CALL MAKPHD()
      CALL SAVPHD(OK)
      IF (.NOT.OK) GOTO 99
C
C     Create a section block
C
      CALL SAVSEC(OK)
      IF (.NOT.OK) GOTO 99
C
C
C     create a database header
C     set pointers in database header
C
      CALL MAKNHD()
C
C     We need to find out how many records we have so we must
C
      NHEADI(1)=DBASE(1)
      NHEADI(2)=DBASE(2)
      NHEADI(3)=DBASE(3)
      NHEADI(4)=DBASE(4)
C
C     write the database header
      CALL SAVNHD(OK)
      IF (.NOT.OK) GOTO 99
C
C
C     Reset various pointer here
C
      T2MIP = 1
      T2PDP = 1
      T2TXP = 1
      T2RLP = 1
      T2PRP = 1
C
C
C     now get down to writing the part data it's self
C     start with the MI data
      CALL CPRINT('COMPACT> Saving MI data')
      DO 30 II=1,NMIPOS-1

	  NEWPP = 0
	  NEWPDP = 0
	  NEWTXP = 0
	  NEWRLP = 0
	  CALL DIR500(II,OK)
C         Check for valid ent
          IF ( IMBUFF(13) .EQ.0 ) THEN
              GOTO 30
          ENDIF
C         Save buffer
          do 31 i=1,13
31             oldimbuff(i)=imbuff(i)
C
C         Resetall pointers
C
          IF ( IMBUFF(8).GT.0) THEN
C             Check his parent to get the new MIP
              NEWPP=IMBUFF(8)
              CALL DIR500(NEWPP,OK)
              NEWPP = IMBUFF(13)
          ENDIF

          CALL DIR500(II,OK)
	  IF ( IMBUFF(7).GT.0) THEN
	      NEWPDP = T2PDP
              CALL EntCount(2,II,NREC)
	      T2PDP = T2PDP + NREC
	  ENDIF

          CALL DIR500(II,OK)
	  IF (IMBUFF(9).GT.0) THEN
	      NEWTXP = T2TXP
              CALL EntCount(3,II,NREC)
	      T2TXP = T2TXP + NREC
          ENDIF

          CALL DIR500(II,OK)
          IF(IMBUFF(10).GT.0) THEN
C             Get the relation list pointer directly from the lookup table
	      NEWRLP = RLOOKUP(IMBUFF(10))
          ENDIF

          CALL DIR500(II,OK)
C         Set new pointers for this save
          IMBUFF(7)=NEWPDP
	  IMBUFF(8)=NEWPP
	  IMBUFF(9)=NEWTXP
	  IMBUFF(10)=NEWRLP
C         Crlear the control pointer for save
	  IMBUFF(13)=0
C         Save the ent as it will be read in the ne drawing
	  CALL DIM500(II,OK)
C         Save the current buffers
          CALL SAVMI0(II,OK)
C         Reset the imbuff and make it 
          do 32 i=1,13
32             imbuff(i)= oldimbuff(i)
C         save the new MIP for this ent
          IMBUFF(13)=T2MIP
C         Reset to the current value
          CALL DIM500(II,OK)

	  T2MIP = T2MIP + 1

 30   CONTINUE
C
C     =====================================================================
C
C     Save the PART DATA. Remember we need to include the
C     continuation pointers and we need to modify the back pointers
C     to the master index
C
C     We shoot throught the MI index fisrt to see what ents have been marked as
C     Deleted and which are to be saved
C
C     ======================================================================
C
      CALL CPRINT('COMPACT> Saving Part data')
      T2PDP = 1
      DO 40 II=1,NMIPOS-1

C         Read the master index pointer first
	  CALL DIR500(II,OK)

          IF ( IMBUFF(13).EQ.0.OR.IMBUFF(7).LE.0) THEN
C              No part data pointer is defined
               GOTO 40
          ENDIF

          TPDP = IMBUFF(7)

45        CONTINUE
          CALL DBR500(TPDP,OK)

C         Save the data before hand
          do 41 i=1,4
41             oldidbuff(i)=idbuff(i)

          IF ( IDBUFF(2).GT.0) THEN
C             Get hist old parent and thus his new
              IDBUFF(2) = IMBUFF(13)
          ENDIF
	  
          IF ( IDBUFF(3).GT.0) THEN
C             Adjust the continueatin pointer
              IDBUFF(3) = T2PDP + 1
              
          ENDIF

C         Modify the curewnt buffer
	  CALL DBM500(TPDP,OK)
C         Save the data
          CALL SAVPD0(TPDP,OK)

          T2PDP = T2PDP + 1

          do 42 i=1,4
42             idbuff(i)=oldidbuff(i)

	  CALL DBM500(TPDP,OK)
          IF ( IDBUFF(3).GT.0) THEN
C             Continuation pomter here
              TPDP = IDBUFF(3)
              GOTO 45
          ENDIF
C         Go back to master index pointer

40    CONTINUE
C     =====================================================================
C     Save All of the TEXT data,  Similar to that of the PART DATA
C     =====================================================================
C
C     Reset the text pointer
      CALL CPRINT('COMPACT> Saving Text data')
      T2TXP = 1
      DO 50 II=1,NMIPOS-1

C         First read the master index to get a valid ent
	  CALL DIR500(II,OK)

          IF ( IMBUFF(13).EQ.0.OR.IMBUFF(9).LE.0) THEN
C              Entity was not marked to be written or is not TEXT
               GOTO 50
          ENDIF

          TTXP = IMBUFF(9)

55        CONTINUE
          CALL DTR500(TTXP,OK)

C         Save the data before hand
          do 51 i=1,2
51             oldicbuff(i)=icbuff(i)

          IF ( ICBUFF(1).GT.0) THEN
C             Get hist old parent and thus his new
              ICBUFF(1) = IMBUFF(13)
          ENDIF
	  
          IF ( ICBUFF(2).GT.0) THEN
C             Adjust the continueatin pointer
              IDBUFF(2) = T2TXP + 1
              
          ENDIF

          CALL DTM500(TTXP,OK)
C         Save the data
          CALL SAVTX0(TTXP,OK)

          T2TXP = T2TXP + 1

          do 52 i=1,2
52             icbuff(i)=oldicbuff(i)

          CALL DTM500(TTXP,OK)

          IF ( ICBUFF(2).GT.0) THEN
C             Continuation pomter here
              TTXP = ICBUFF(2)
              GOTO 55
          ENDIF
C         Go back to master index pointer

50    CONTINUE
C
C     =============================================================
C
C     Write all of the relational data.  Similar again to PART DATA
C     in that we have continuation pointers
C
C     Just in case you had not worked it out !
C
C     T2RLP = Relation list pointer in new file
C     TRLP  = Releation pointer currently being read
C
C     =============================================================
C
C
C     Reset the relation pointer to the first one
      CALL CPRINT('COMPACT> Saving Relation data')
      T2RLP = 1
      DO 60 II=1,NRLPOS-1

C         First read the master index to get a valid ent
	  CALL DRR950(II,OK)

	  TMIP = RLBUFF(3)

	  CALL DIR500(TMIP,OK)

          IF ( IMBUFF(13).EQ.0) THEN
C              Entity was not marked to be written or is not Relational ent
               GOTO 60
          ENDIF

C         Save the data before hand
          do 61 i=1,10
61             oldrlbuff(i)=rlbuff(i)

          IF ( RLBUFF(3).GT.0) THEN
C             Get hist old parent and thus his new
              RLBUFF(3) = IMBUFF(13)
          ENDIF
	  
          IF ( RLBUFF(2).GT.0) THEN
C             Adjust the continueatin pointer
              RLBUFF(2) = T2RLP + 1
          ENDIF
C
C         Ajust the relation pointers in buffers 4-10 in the proceeding buffers
C
	  IF ( RLBUFF(1).EQ.200) THEN
              DO 63 i=4,10
	          TRMIP = RLBUFF(I)
	          IF ( TRMIP.GT.0 ) THEN
	              CALL DIR500(TRMIP,OK)
C                     This is the new master index pointer
	              RLBUFF(i)=IMBUFF(13)
                  ENDIF
63            CONTINUE
          ENDIF

          CALL DRM950(II,OK)
C         Save the data
          CALL SAVRL0(II,OK)

          T2RLP = T2RLP + 1

          do 62 i=1,10
62             rlbuff(i)=oldrlbuff(i)

          CALL DRM950(II,OK)

60    CONTINUE
C
C     =============================================================
C
C     Currently we write all of the property data out to file
C     as is. We only need to adjust the property back pointers
C     This is done as we write
C
C     =============================================================
C
      CALL CPRINT('COMPACT> Saving Property and other data')
C
C     now save all the property index data
      DO 70 I=1,NPRPOS-1
C        write the Prop record record
         TMIP = PRIFIL(3,I) 
         CALL DIR500(TMIP,OK)
         PRIFIL(3,I) = IMBUFF(13)
         CALL SAVPI0(I,OK)
         PRIFIL(3,I) = TMIP
         IF ( .NOT. OK ) GOTO 99
70    CONTINUE
C
C     now save all the property character data
      DO 80 I=1,NPCPOS-1
C        write the MI record
         CALL SAVPC0(I,OK)
         IF ( .NOT. OK ) GOTO 99
80    CONTINUE
C
C     now save all the name layer.
      DO 90 I=0,255
C        write the MI record
         IF ( NLEN(LNAME(I)).GT.0 ) THEN
            CALL SAVLY0(I,OK)
         END IF
         IF ( .NOT. OK ) GOTO 99
90    CONTINUE

      CALL CPRINT('COMPACT> Drawing Compact completed')
      RETURN
C
 99   CONTINUE
      CALL ERRORDIALOG('Error compacting the drawing')
      OK = .FALSE.
C
      END
C

C

      SUBROUTINE CMPACT(REGEN,OK)
C     ===========================
C
C1    vartype           L     L
C1    iostatus          I     O
C
C     New version of the DAXCAD compacting routine
C
C     Basically we create a brand new drawing file 
C     without the deleted ents.  See my documentaion for more details
C
      include  'include/masti.inc'
      include  'include/ndata.inc'
      include  'include/movdat.inc'
      include  'include/fhead.inc'
      include  'include/dhead.inc'
      include  'include/nhead.inc'
      include  'include/filunit.inc'
      include  'include/params.inc'
      include  'include/nbuff.inc'
      include  'include/swind.inc'
      include  'include/props.inc'
      include  'include/entity.inc'
      include  'include/product.inc'
      include  'include/ftypes.inc'
      include  'include/viewport.inc'
C
      REAL SX,SY
      LOGICAL OK,YES,IGNORE,REGEN,CompactToFile
      CHARACTER FILNM*128
      INTEGER*4 DBASE(7),FN
      INTEGER*4 DNUM,DRGU,NLEN
      INTEGER*2 TMIP,D1,D2,WUN,LT
      INTEGER*2 TTMIP,TTPDP,TTTXP,TTRLP,TLDFIL,II,PMIP,TTPRP,TTPCP
      INTEGER*2 NEWMIP,TP
C      CHARACTER INFO(3)*80

      INTEGER*4 POS
      INTEGER*2 SPA 
      REAL X,Y
C
      EXTERNAL OPENNM,DEPRNT,DIR500,SSFLAG,MOVMAS,CRTP05,INSP05
C
C SPB - 081294 - Give the user the option of compaction
      CALL CONFIRMATION('Do you wish to save as a compacted drawing ?',
     +                  .FALSE.,
     +                  YES)
      IF ( .NOT.YES ) THEN
         OK=.FALSE.
         RETURN
      ENDIF

C      INFO(1) = 'Compact Drawing Utility V1.0'
C      INFO(2) = ' Copyright Intelliscan 1994'
C      CALL INFODIALOG(INFO,2)
      
      WUN=1
      NEWMIP=1
      D1=0
      D2=0
C
C     Reset the new database size
C
      NDATA=0
      DBASE(1)=0
      DBASE(2)=0
      DBASE(3)=0
      DBASE(4)=0
C     the drawing file will be open at this time

      DRGNAM=DFNAM

      CALL CPRINT('DAXCAD Direct Drawing Compact ')
C      CALL CPRINT('  (C) Intelliscan Ltd 1994')
C 


C
      SX=0.0
      SY=0.0
C
C     save the current record pointers for now ?
C
      TTMIP=NMIPOS
      TTPDP=NPDPOS
      TTTXP=NTXPOS
      TTRLP=NRLPOS
      TTPRP=NPRPOS
      TTPCP=NPCPOS
C
C     cycle through the master index
C
      NEWMIP = 1
      TMIP=0
C
C     =======================================
C
C     Pass on the database first an mark all masters which are
C     available as inserted companents
C
C     =======================================
C
C
      CALL CPRINT('COMPACT> First Pass on deleted component data')
      DO 1000,TMIP=1,NMIPOS-1
C        read master index and create a scratch file of all ents to be saved
C
         CALL DIR500(TMIP,OK)
C
C        Ignore deleted ents
C
         IF (IMBUFF(1).EQ.100) THEN
              GOTO 1000
         ENDIF
C
C        collect this entity and set its new masterindex pointer in 13
         OK = .TRUE.
C
         IF ( IMBUFF(2) .EQ. COMPM .OR.IMBUFF(2).EQ.SYMBM) THEN
C
C             We have a comp master so check if its used
C
              IF ( IMBUFF(2).EQ.COMPM ) THEN
                  FN = 1
              ELSE
                  FN=2
              ENDIF

              CALL INSC10(CBUFF,FN,CMIP,OK)
	      CALL DIR500(TMIP,OK)

              IF ( OK ) THEN
C                 It is used so keep it
                  IMBUFF(13) = 1
	      ELSE
C                 The master is deleted
                  IMBUFF(13) = 0
              ENDIF

	      CALL DIM500(TMIP,OK)

         ELSE IF ( IMBUFF(2) .EQ. GROUP ) THEN
	      IMBUFF(13) = 1
	      CALL DIM500(TMIP,OK)
         ENDIF

1000  CONTINUE

      CALL CPRINT('COMPACT> Second pass mark new ents in master index')
      DO 2000,TMIP=1,NMIPOS-1
C
C     read master index and create a scratch file of all ents to be saved
C
      CALL DIR500(TMIP,OK)
C
C     Ignore deleted ents
C
      IF (IMBUFF(1).EQ.100) THEN
           GOTO 2000
      ENDIF
C
C     collect this entity and set its new masterindex pointer in 13
      OK = .TRUE.
C
      IF( IMBUFF(1).EQ.COMPM.OR.IMBUFF(1).EQ.SYMBM
     +        .OR.IMBUFF(1).EQ.GROUP) THEN
C
C          We have a sub master ent. Check its parent to see if its not deleted
C
	   TP = IMBUFF(8)
           CALL DIR500(TP,OK)
           IF ( IMBUFF(13).EQ.0) THEN
	       GOTO 2000
	   ENDIF
      ENDIF
C
      CALL DIR500(TMIP,OK)
C     Normal ent so keep it and flag it
      IMBUFF(13)=NEWMIP
      CALL DIM500(TMIP,OK)
C
C     Get the new database etents and flag 
      CALL GETENTCOUNT(TMIP,DBASE)
C     increment the new master count
      NEWMIP = NEWMIP+1
C
2000  CONTINUE
C
      CALL CreateCompactDrg(FILNM,DBASE)

C     Reset all index 13 cos it will cos trouble
      DO 4000,II=1,NMIPOS-1
	   CALL DIR500(II,OK)
	   IMBUFF(13)=0
	   CALL DIM500(II,OK)
4000  CONTINUE

      OK=.TRUE.
C
      RETURN
C
99    CONTINUE
C
C     Some kind of error
C
      OK=.FALSE.
C
      END

	SUBROUTINE ENTCOUNT(TYPE,TMIP,NREC)
C	===================================
C
C	Find all of the records to do with this element
C
c	TYPE NO		ACTION
c	1		Master index 
c	2		Part data records
c	3		Text records
c	4		Releation records
c	5		Propert data records
c	6		propert character data
c	7		number of layers
c	8		NUmber of elements in a group master
c
c
	include 'include/nbuff.inc'
	include 'include/entity.inc'
	include 'include/masti.inc'

	integer*4 type
	integer*4 nrec
	integer*2 tmip

	integer*2 ttmip
	integer*2 tp
	integer*2 entpnt
	integer*2 relp
	logical deleted
	logical ok
	logical finish
	real matrix(3,3)

	integer*2 ent
C
C	Main code start here
C
	nrec = 0
        call dir500(tmip,ok)
c
c	Master index records required
c
	if ( type .eq. 1 ) then
		nrec = 1
		return

	else if ( type.eq.2) then
c
c		Part data reecords for this mip
c

		if ( imbuff(7) .eq. 0 ) then
			return
		endif

		tp = imbuff(7)
		call dbr500(tp,ok)

10		continue

		nrec = nrec + 1

		if ( idbuff(3).eq.0 ) then
			return
		endif
	
		tp = idbuff(3)

		call dbr500(tp,ok)

		goto 10
c
	else if ( type .eq. 3 ) then
c
c		Check Text pointers
c
		if ( imbuff(9).eq.0)  then
			return
		endif

		tp = imbuff(9)
		call dtr500(tp,ok)

20		continue

		nrec = nrec + 1

		if ( icbuff(2).eq.0 ) then
			return
		endif

		tp = icbuff(2)

		call dtr500(tp,ok)

		goto 20

	elseif ( type.eq.4 ) then
c
c		Find number of relation list elements
c
c		Did you know that shared ents such as instances dont have thrir own rel lists
c
		if ( imbuff(2).eq.compi.or.imbuff(2).eq.SYMBI ) then
			return
		endif

		if ( imbuff(10).eq.0) then
			return
		endif

		relp = imbuff(10)

		call drr950(relp,ok)

40		continue

		nrec = nrec + 1

		if ( rlbuff(2).eq.0) then
			return
		endif

		relp = rlbuff(2)

		call drr950(relp,ok)

		goto 40

	endif

	return

	end

	subroutine GETENTCOUNT(TMIP,DBASE)
C       ===================================
C
C       Find all of the records to do with this element
C
c       TYPE NO         ACTION
c       1               Master index
c       2               Part data records
c       3               Text records
c       4               Releation records
c       5               Propert data records
c       6               propert character data
c       7               number of layers
c
c
        include 'include/nbuff.inc'
        include 'include/entity.inc'
        include 'include/masti.inc'

	integer*2 tmip
	integer*4 dbase(7),i,nrec
	logical ok

	call dir500(tmip,ok)

c	Normal entity so get all of its constuents

	do 100 i=1,4
		call EntCount(i,tmip,nrec)
		dbase(i) = dbase(i)+nrec
100	continue

	end


	
	subroutine dumpdb()

        include 'include/nbuff.inc'
        include 'include/entity.inc'
        include 'include/masti.inc'

	integer*4 i
	integer*2 ii
	logical ok

	print*,'NMIPOS= ',NMIPOS
	print*,'NPDPOS= ',NPDPOS
	print*,'NTXPOS= ',NTXPOS
	print*,'NRLPOS= ',NRLPOS

	do 10 ii=1,nmipos-1
		call dir500(ii,ok)
		print*, 'MASTER INDEX: ',ii,' ',(imbuff(i),i=1,13)
10	continue
	do 20 ii=1,npdpos-1
		call dbr500(ii,ok)
		print*, 'PART DATA: ',ii,' ',(idbuff(i),i=1,4)
20	continue
	do 30 ii=1,ntxpos-1
		call dtr500(ii,ok)
		print*, 'TEXT DATA: ',ii,' ',(icbuff(i),i=1,2),' >',CBUFF
30	continue
	do 40 ii=1,nrlpos-1
		call drr950(ii,ok)
		print*, 'RELATION DATA: ',ii,' ',(rlbuff(i),i=1,10)
40	continue
	print*,'<==============>'
	end

	subroutine dumpmi()

	include  'include/masti.inc'
	include  'include/nbuff.inc'
	include  'include/entity.inc'

	print*, 'MASTER INDEX: ',mip,' ',(imbuff(i),i=1,13)

	end


      SUBROUTINE DRM950(P,OK)
C     =======================
C
C1    VARTYPE           I2 L
C1    IOSTATUS          O  O
C
C     This routine modifies the releation buffer
C     stored at the pointer
C
      include  'include/masti.inc'
      include  'include/nbuff.inc'
      include  'include/entity.inc'
C
      INTEGER*2 P
      INTEGER*4 I,DICTN
      LOGICAL OK
C
      IF (P.GE.NRLPOS .OR. P.LE.0) THEN
C        corruption in pointer.
         OK=.FALSE.
         WRITE(UNIT=10,FMT=*)'[DRM950] POINTER OUT OF RANGE P=',P
         DICTN=61
         CALL DEPRNT(DICTN)
      ELSE
C        Modify the contents of the existing buffer
CARRAY
         DO 10 I=1,10
           RLFILE(I,P)=RLBUFF(I)
 10      CONTINUE
CARRAY
         OK=.TRUE.
      END IF
C
      END

