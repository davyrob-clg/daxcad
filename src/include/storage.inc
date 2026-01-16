C
C     @(#)  412.1 date 6/11/92 storage.inc 
C
C
C     Filename    : storage.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:46
C     Last change : 92/06/11 14:41:14
C
C     Copyright : Practical Technology Limited  
C     File :- storage.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
C	This include file contains the main 
C	Master Index and Parameter data declarations
C	required for normal workspace in the KSOLID
C	test program.
C
C	Set limits on data space
	INTEGER MIPLIM,PDILIM
	PARAMETER (MIPLIM=1000)
	PARAMETER (PDILIM=1000)
C	declare working data space
C	Master Index File
	INTEGER MIFILE(2,MIPLIM)
C	Parameter Data File
	INTEGER PDIFIL(5,PDILIM)
	REAL PDRFIL(6,PDILIM)
C
C	declare buffer space for data construction
	INTEGER MIBUFF(2),ENSTAT,ENTYPE,ENTPDP,IRBUFF(5,10),ENTMIP
	EQUIVALENCE (MIBUFF(2),ENTPDP)
C	set equivalences for body limits in model space
	REAL RBUFF(6,10),BDXMIN,BDYMIN,BDZMIN,BDXMAX,BDYMAX,BDZMAX
	EQUIVALENCE(RBUFF(1,1),BDXMIN)
	EQUIVALENCE(RBUFF(2,1),BDYMIN)
	EQUIVALENCE(RBUFF(3,1),BDZMIN)
	EQUIVALENCE(RBUFF(4,1),BDXMAX)
	EQUIVALENCE(RBUFF(5,1),BDYMAX)
	EQUIVALENCE(RBUFF(6,1),BDZMAX)
C	set equivalences for body centre position in model space
	REAL BODYXC,BODYYC,BODYZC
	EQUIVALENCE(RBUFF(4,2),BODYXC)
	EQUIVALENCE(RBUFF(5,2),BODYYC)
	EQUIVALENCE(RBUFF(6,2),BODYZC)
C	set equivalence for segment count
	INTEGER NSEGS
	EQUIVALENCE(RBUFF(6,5),NSEGS)
C	set equivalence for entity return MIP
	EQUIVALENCE(IRBUFF(2,1),ENTMIP)
C
C	declare control pointers for MI and PARAMETER data
	INTEGER NMIPOS,NPDPOS
C
C	declare COMMON for workspace
	COMMON /MIDAT1/MIFILE,PDIFIL,PDRFIL
	SAVE /MIDAT1/
C	declare COMMON for buffer space
	COMMON /MBUFF1/MIBUFF,ENSTAT,ENTYPE,RBUFF,IRBUFF
	SAVE /MBUFF1/
C	declare COMMON for pointers
	COMMON /POINTS/NMIPOS,NPDPOS
	SAVE /POINTS/
C
