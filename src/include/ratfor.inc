C
C     @(#)  412.1 date 6/11/92 ratfor.inc 
C
C
C     Filename    : ratfor.inc
C     Version     : 412.1
C     Retrieved   : 92/06/12 15:15:39
C     Last change : 92/06/11 14:39:22
C
C     Copyright : Practical Technology Limited  
C     File :- ratfor.inc
C
C     DAXCAD FORTRAN 77 Include file
C
C
C     |-----------------------------------------------------------------|
C
C
*
* Ratfor system definitions
*

        character ALPHA,DEFTYP,TAB,EOL,EOS,EOF

        parameter (ALPHA=1)
        parameter (DEFTYP=2)
        parameter (TAB=9)
        parameter (EOL=10)
        parameter (EOS=30)
        parameter (EOF=128)

        character LEXDIGITS,LEXIF,LEXELSE,LEXWHILE,LEXDO,LEXBREAK,
     +            LEXNEXT,LEXFOR,LEXREPEAT,LEXUNTIL,LEXOTHER

        parameter (LEXDIGITS=130)
        parameter (LEXIF=131)
        parameter (LEXELSE=132)
        parameter (LEXWHILE=133)
        parameter (LEXDO=134)
        parameter (LEXBREAK=135)
        parameter (LEXNEXT=136)
        parameter (LEXFOR=137)
        parameter (LEXREPEAT=138)
        parameter (LEXUNTIL=139)
        parameter (LEXOTHER=140)

        parameter (MAXCARD=80)
        parameter (MAXTOK=300)

        character DIGIT,LETTER

        parameter (LETTER=1)
        parameter (DIGIT=2)

        character incard*81,outcard*81,inbuf(200),outbuf*81
        integer icp,ocp,ibp,obp
        integer level,linect(5),infile(5)
        integer label
        character forstk*300
        integer fordep,forptr(100)
        integer lastp,lastt,namptr(200)
        character table(1500)

        common /ratfor/icp,ocp,ibp,obp,level,linect,infile,label,fordep,
     +                 forptr,lastp,lastt,namptr

        common /ratforc/incard,outcard,inbuf,outbuf,forstk,table
