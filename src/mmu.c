/*
     @(#)  412.1 date 6/11/92 mmu.c 


     Filename    : mmu.c
     Version     : 412.1
     Retrieved   : 92/06/12 15:48:54
     Last change : 92/06/11 14:35:44

     Copyright : Practical Technology Limited  
     File :- mmu.c

     Memory management unit for DAXCAD

     This section takes the place of the F77 arrays
     MIFILE PDFR etc and makes them functions with the
     the original arguments.

     These functions will have there own memory locations
     which will be increased on predefined chunks say 100
     records at a time. All data will be I4 to give virtually unlimited data
     space

     The memory grows upwards only and will be freed only when the drawing is closed
     There will be limits to stop any unessary growth.

     This system is kind of demand paging in the old 9.7 fashion

*/

#include <stdio.h>
#include <stdint.h>
#include <errno.h>
#include "xlang.h"


/* error codes */

#define NEGATIVE_POWER    -1		/* less than 0 */

#define MIFILE mifile

#define MIFILELIMIT  100000	/* define limit before spilling onto disk */
#define ALLOCLIMIT   100	/* Allocation limite of entities */

typedef int32_t INT4;
typedef short DAXINT;		/* current daxcad integer defintion */
typedef float DAXFLOAT;		/* current daxcad floating  defintion */

typedef union	{
			INT4 i4;	/* ether int or double */
			double dble;
		} DAXPD;

typedef struct {		/* master index file structure */

		char status;	/* status of entity */
		char type;	/* entity type */
		char color;	/* color */
		char layer;	/* layer */
		char form;	/* form ( polygon */
		char font;	/* font type */
		INT4 pdfp;	/* part data file pointer */
		INT4 parent;	/* parent pointer */
		INT4 text;	/* text pointer */
		INT4 relation;	/* relation pointer */
		INT4 prop;	/* property pointer */
		char thick;	/* thickness */
		INT4 reg;	/* registation */

       }  MifileRec;

typedef struct {		/* part data file structure */


		double buff1;
		double buff2;
		double buff3;
		double buff4;
		double buff5;
		double buff6;
		INT4 type;
		INT4 mip;
		INT4 cont;
		INT4 spare;
		

       }  PartdataRec;


static struct {			/* higest record pointer */

	INT4 mifile;
	INT4 mifilebytes;	/* bytes count */
	INT4 partd;
	INT4 partdbytes;	/* bytes count */
	INT4 textd;
	INT4 textdbytes;	/* bytes count */
       } HighRecord;

typedef  struct {
			DAXINT mip;	/* master index pointer */
			DAXINT cont;	/* continuation pointer */
			char *text;	/* text string */
			INT4 len;	/* length of text */
		} TextRec;


static MifileRec *MasterIndex;	/* Master index pointer */
static PartdataRec *PartData;	/* Part data index pointer */
static TextRec *TextData;	/* text data index pointer */


mainmmu()


{

static short imbuff[13]= { 10,3,7,4,5,0,1009,45,15678,200,29993,1,32};
static short outbuff[13];
short p;
DAXFLOAT rdbuff[6];
DAXINT idbuff[4];

INT4 rec;
INT4 pos;
INT4 set;
INT4 st;
int i;


	set = 1;

	DATABASEINIT(&st);

	for ( p=1;p<32000;p++)	{


		imbuff[6] = p;
		MIRSET(&p,imbuff);


	}
	for ( p=1;p<32000;p++)	{


		memset(outbuff,'\0',26);
		MIRGET(&p,outbuff);
		if ( outbuff[6] != p )	{
			printf("DIFF %d \n",p);
			exit(1);
		}


	}
	puts("Part data");

	for ( p=1;p<32000;p++)	{


		idbuff[0] = 1;
		idbuff[1] = 2;
		idbuff[2] = 3;
		idbuff[3] = p;
		rdbuff[0] = 1.123;
		rdbuff[1] = 7672.456;
		rdbuff[2] = 5453.567;
		rdbuff[3] = 884.567;
		rdbuff[4] = 98985.876;
		rdbuff[5] = 896.567;

		PDRSET(&p,rdbuff,idbuff);


	}
	for ( p=1;p<32000;p++)	{


		PDRGET(&p,rdbuff,idbuff);
		if ( idbuff[3] != p )	{
			printf("DIFF %d %d\n",idbuff[3],p);
			exit(1);
		}


	}



}




DATABASEINIT(Status)

      /* Description   :-Initialise the database pointer system
       *                 free anything taht is not set;
       *                
       *                
       * Return status :- 0 Success ( Should not fail )
       *                
       *                
       *                
       * Notes         :- 
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

INT4 *Status;	/* status return */

{

	*Status = 0;

	if ( MasterIndex )	{
		free(MasterIndex);
		MasterIndex =NULL;
	}
	if ( PartData )	{
		free(PartData);
		PartData = NULL;
	}

	HighRecord.mifile = -1;		/* set highest record counts */
	HighRecord.mifilebytes  = 0;
	HighRecord.partd = -1;		/* set highest record counts */
	HighRecord.partdbytes  = 0;

}



INT4
MIFILE(Pos,Recnum,Value,Set)

      /* Description   :-The master index function to set 
       *                 the master index record
       *                
       *                
       * Return status :- -1 Cannot set the record
       *                  1> The index pointer number
       *                
       *                
       * Notes         :- This is used to set the record and allocate when
       *                  nessesary
       *                  
       *                  
       *                  
       *                  
       *
       */

INT4 Pos;	/* <i> set the approriate function within the master index number   */
INT4 Recnum;	/* <i> Record number */
INT4 *Value;	/* <i/o> The integer value */
INT4 Set;	/* <i> If true then set value to recnum other wise recover and set value */


{

INT4 pos;
INT4 recnum;
MifileRec *rec;
MifileRec *temp;
INT4 bytes;
INT4 blocks;	/* the number of segments to page */

	pos = Pos -1 ;
	recnum = Recnum-1;

	if ( HighRecord.mifile < recnum && Set )	{	/* allocate new segment */


		blocks = (recnum - HighRecord.mifile)/ALLOCLIMIT + 1 ;

		bytes = sizeof(MifileRec) * ALLOCLIMIT * blocks;	/* number of bytes needed */

		if ( MasterIndex )	{
			bytes += HighRecord.mifilebytes;
			temp = MasterIndex;
			MasterIndex = (MifileRec*)realloc(MasterIndex,bytes);
		}
		else	{
			temp = NULL;
			MasterIndex = (MifileRec*)malloc(bytes);

		}
		if ( !MasterIndex )	{ 	/* error return with current allocation */

			MasterIndex = temp;
			return -1;
		}
		HighRecord.mifile += blocks*ALLOCLIMIT;
		HighRecord.mifilebytes = bytes;


	}

	rec = &MasterIndex[recnum];

	switch (pos)	{

	case 0:		/* status */

		if ( Set ) 
			rec->status = (char)*Value;
		else
			*Value = (INT4)rec->status;

		break;

	case 1 :	/* entity type */

		if ( Set ) 
			rec->type = (char)*Value;
		else
			*Value = (INT4)rec->type;

		break;

	case 2 :	/* color */

		if ( Set ) 
			rec->color = (char)*Value;
		else
			*Value = (INT4)rec->color;

		break;

	case 3 :	/* color */

		if ( Set ) 
			rec->layer = (char)*Value;
		else
			*Value = (INT4)rec->layer;

		break;
		
	case 4 :	/* color */

		if ( Set ) 
			rec->form = (char)*Value;
		else
			*Value = (INT4)rec->form;

		break;

	case 5 :	/* font */

		if ( Set ) 
			rec->font = (char)*Value;
		else
			*Value = (INT4)rec->font;

		break;
		
	case 6 :	/* part data file pointer */

		if ( Set ) 
			rec->pdfp = (INT4)*Value;
		else
			*Value = (INT4)rec->pdfp;

		break;

	case 7 :	/* parent pointer */

		if ( Set ) 
			rec->parent = (INT4)*Value;
		else
			*Value = (INT4)rec->parent;

		break;

	case 8 :	/* text pointer */

		if ( Set ) 
			rec->text = (INT4)*Value;
		else
			*Value = (INT4)rec->text;

		break;
		
	case 9 :	/* relation  */

		if ( Set ) 
			rec->relation = (INT4)*Value;
		else
			*Value = (INT4)rec->relation;

		break;
		
			
	case 10 :	/* property  */

		if ( Set ) 
			rec->prop = (INT4)*Value;
		else
			*Value = (INT4)rec->prop;

		break;

	case 11 :	/* thickness  */

		if ( Set ) 
			rec->thick = (INT4)*Value;
		else
			*Value = (INT4)rec->thick;

		break;
			
				
	case 12 :	/* registration  */

		if ( Set ) 
			rec->reg = (INT4)*Value;
		else
			*Value = (INT4)rec->reg;

		break;

	}

	return (INT4)*Value;

}



WordBoundary(Num,Word)        /* This routine calculates if the number supplied lies in
                                 the range also supplied. eg if the number does then
                                 it returns 1 otherwise 0
                               */

int Word;                     /* the word boundary value */
int Num;                      /* the number supplied */

{

   return Num -( Word*(Num/Word) ) ? 0 : 1;       /* do a quick calc */

}       



MIRGET(Recnum,Imbuff)

      /* Description   :- Master Index record decoder. Called from F77 it decodes
       *                  
       *                                                                             
       *                                                                             
       * Return status :- None                                                       
       *                                                                             
       *                                                                             
       *                                                                             
       * Notes         :- 
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

DAXINT *Recnum;                 /*  <i>     record number */
DAXINT Imbuff[13];         /*  <o>     filled record buffer */
{

INT4 recnum;
INT4 val;


	recnum = (INT4)*Recnum;		/* get record number */

	Imbuff[0] = (unsigned char) MIFILE(1,recnum,&val,0);
	Imbuff[1] = (unsigned char) MIFILE(2,recnum,&val,0);
	Imbuff[2] = (unsigned char) MIFILE(3,recnum,&val,0);
	Imbuff[3] = (unsigned char) MIFILE(4,recnum,&val,0);
	Imbuff[4] = (unsigned char) MIFILE(5,recnum,&val,0);
	Imbuff[5] = (unsigned char) MIFILE(6,recnum,&val,0);
	Imbuff[6] = (short) MIFILE(7,recnum,&val,0);
	Imbuff[7] = (short) MIFILE(8,recnum,&val,0);
	Imbuff[8] = (short) MIFILE(9,recnum,&val,0);
	Imbuff[9] = (short) MIFILE(10,recnum,&val,0);
	Imbuff[10] = (short) MIFILE(11,recnum,&val,0);
	Imbuff[11] = (short) MIFILE(12,recnum,&val,0);
	Imbuff[12] = (short) MIFILE(13,recnum,&val,0);
}

MIRSET(Recnum,Imbuff)

      /* Description   :- Master Index record encoder. Called from F77 it encodes
       *                  into new mmu
       *                                                                             
       *                                                                             
       * Return status :- None                                                       
       *                                                                             
       *                                                                             
       *                                                                             
       * Notes         :- 
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

DAXINT *Recnum;                 /*  <i>     record number */
DAXINT Imbuff[13];         /*  <o>     filled record buffer */
{

INT4 recnum;
INT4 val;
int pos;
INT4 imbuff[13];
INT4 ret;
INT4 i;

	for ( i=0;i<13;i++)
		imbuff[i] = (INT4)Imbuff[i];

	recnum = (INT4)*Recnum;		/* get record number */

	ret = MIFILE(1,recnum,&imbuff[0],1);
	ret = MIFILE(2,recnum,&imbuff[1],1);
	ret = MIFILE(3,recnum,&imbuff[2],1);
	ret = MIFILE(4,recnum,&imbuff[3],1);
	ret = MIFILE(5,recnum,&imbuff[4],1);
	ret = MIFILE(6,recnum,&imbuff[5],1);
	ret = MIFILE(7,recnum,&imbuff[6],1);
	ret = MIFILE(8,recnum,&imbuff[7],1);
	ret = MIFILE(9,recnum,&imbuff[8],1);
	ret = MIFILE(10,recnum,&imbuff[9],1);
	ret = MIFILE(11,recnum,&imbuff[10],1);
	ret = MIFILE(12,recnum,&imbuff[11],1);
	ret = MIFILE(13,recnum,&imbuff[12],1);
}

INT4
PDFILE(Pos,Recnum,Value,Set)

      /* Description   :-The master index function to set 
       *                 the master index record
       *                
       *                
       * Return status :- -1 Cannot set the record
       *                  1> The index pointer number
       *                
       *                
       * Notes         :- This is used to set the record and allocate when
       *                  nessesary
       *                  
       *                  
       *                  
       *                  
       *
       */

INT4 Pos;	/* <i> set the approriate function within the master index number   */
INT4 Recnum;	/* <i> Record number */
DAXPD *Value;	/* <i/o> The value to be set */
INT4 Set;	/* <i> If true then set value to recnum other wise recover and set value */


{

INT4 pos;
INT4 recnum;
PartdataRec *rec;
PartdataRec *temp;
INT4 bytes;
INT4 blocks;	/* the number of segments to page */

	pos = Pos -1 ;
	recnum = Recnum-1;

	if ( HighRecord.partd < recnum && Set )	{	/* allocate new segment */


		blocks = (recnum - HighRecord.partd)/ALLOCLIMIT + 1 ;

		bytes = sizeof(PartdataRec) * ALLOCLIMIT * blocks;	/* number of bytes needed */

		if ( PartData )	{
			bytes += HighRecord.partdbytes;
			temp = PartData;
			PartData = (PartdataRec*)realloc(PartData,bytes);
		}
		else	{
			temp = NULL;
			PartData = (PartdataRec*)malloc(bytes);

		}
		if ( !PartData )	{ 	/* error return with current allocation */

			PartData = temp;
			return -1;
		}
		HighRecord.partd += blocks*ALLOCLIMIT;
		HighRecord.partdbytes = bytes;


	}

	rec = &PartData[recnum];

	switch (pos)	{

	case 0:		/* entity type */

		if ( Set ) 
			rec->type = (double)(*Value).i4;
		else
			(*Value).i4 = (INT4)rec->type;

		break;

	case 1 :	/* mip*/

		if ( Set ) 
			rec->mip = (double)(*Value).i4;
		else
			(*Value).i4 = (INT4)rec->mip;

		break;

	case 2 :	/* continuation */

		if ( Set ) 
			rec->cont = (double)(*Value).i4;
		else
			(*Value).i4 = (INT4)rec->cont;
		break;

	case 3 :	/* spare */

		if ( Set ) 
			rec->spare = (double)(*Value).i4;
		else
			(*Value).i4 = (INT4)rec->spare;

		break;
		
	case 4 :	/* buff 1 */

		if ( Set ) 
			rec->buff1 = (double)(*Value).dble;
		else
			(*Value).dble = (DAXFLOAT)rec->buff1;

		break;

	case 5 :	/* buff 2 */

		if ( Set ) 
			rec->buff2 = (double)(*Value).dble;
		else
			(*Value).dble = (DAXFLOAT)rec->buff2;
		break;

	case 6 :	/* buff 3 */

		if ( Set ) 
			rec->buff3 = (double)(*Value).dble;
		else
			(*Value).dble = (DAXFLOAT)rec->buff3;

		break;
	case 7 :	/* buff 4 */

		if ( Set ) 
			rec->buff4 = (double)(*Value).dble;
		else
			(*Value).dble = (DAXFLOAT)rec->buff4;

		break;
	case 8 :	/* buff 5 */

		if ( Set ) 
			rec->buff5 = (double)(*Value).dble;
		else
			(*Value).dble = (DAXFLOAT)rec->buff5;
		break;

	case 9 :	/* buff 6 */

		if ( Set ) 
			rec->buff6 = (double)(*Value).dble;
		else
			(*Value).dble = (DAXFLOAT)rec->buff6;

		break;


	}

	return (INT4)0;

}


PDRGET(Recnum,Idbuff,Rdbuff)

      /* Description   :- Part data and part index recored decoder
       *                  
       *                                                                             
       *                                                                             
       * Return status :- None                                                       
       *                                                                             
       *                                                                             
       *                                                                             
       * Notes         :- 
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

DAXINT *Recnum;		/*  <i>     record number */
DAXINT Idbuff[4];	/*  <o>     filled record buffer */
DAXFLOAT Rdbuff[6];	/*  <o>     filled record buffer */
{

INT4 recnum;
DAXPD val;


	recnum = (INT4)*Recnum;		/* get record number */

	PDFILE(1,recnum,&val,0);Idbuff[0] = (DAXINT)val.i4;	/* load id and pd */
	PDFILE(2,recnum,&val,0);Idbuff[1] = (DAXINT)val.i4;
	PDFILE(3,recnum,&val,0);Idbuff[2] = (DAXINT)val.i4;
	PDFILE(4,recnum,&val,0);Idbuff[3] = (DAXINT)val.i4;
	PDFILE(5,recnum,&val,0);Rdbuff[0] = (DAXFLOAT)val.dble;
	PDFILE(6,recnum,&val,0);Rdbuff[1] = (DAXFLOAT)val.dble;
	PDFILE(7,recnum,&val,0);Rdbuff[2] = (DAXFLOAT)val.dble;
	PDFILE(8,recnum,&val,0);Rdbuff[3] = (DAXFLOAT)val.dble;
	PDFILE(9,recnum,&val,0);Rdbuff[4] = (DAXFLOAT)val.dble;
	PDFILE(10,recnum,&val,0);Rdbuff[5] = (DAXFLOAT)val.dble;

}

PDRSET(Recnum,Idbuff,Rdbuff)

      /* Description   :- Part data record encoder. Called from F77 it encodes
       *                  into new mmu
       *                                                                             
       *                                                                             
       * Return status :- None                                                      
       *                                                                             
       *                                                                             
       *                                                                             
       * Notes         :- 
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

DAXINT *Recnum;		/*  <i>     record number */
DAXINT Idbuff[4];	/*  <o>     filled record buffer */
DAXFLOAT Rdbuff[6];	/*  <o>     filled record buffer */
{

INT4 recnum;
int pos;
INT4 ret;
INT4 i;
DAXPD val;


	recnum = (INT4)*Recnum;		/* get record number */

	val.i4 = (INT4)Idbuff[0];ret = PDFILE(1,recnum,&val,1);
	val.i4 = (INT4)Idbuff[1];ret = PDFILE(2,recnum,&val,1);
	val.i4 = (INT4)Idbuff[2];ret = PDFILE(3,recnum,&val,1);
	val.i4 = (INT4)Idbuff[3];ret = PDFILE(4,recnum,&val,1);

	val.dble = (double)Rdbuff[0];ret = PDFILE(5,recnum,&val,1);
	val.dble = (double)Rdbuff[1];ret = PDFILE(6,recnum,&val,1);
	val.dble = (double)Rdbuff[2];ret = PDFILE(7,recnum,&val,1);
	val.dble = (double)Rdbuff[3];ret = PDFILE(8,recnum,&val,1);
	val.dble = (double)Rdbuff[4];ret = PDFILE(9,recnum,&val,1);
	val.dble = (double)Rdbuff[5];ret = PDFILE(10,recnum,&val,1);

	
}


CBUFFSET(Recnum,Icbuff,String,Length)

      /* Description   :- Text management system. 
       *                  Will malloc a structure
       * 
       *
       * Return status :- 
       *                  
       *   
       *                  
       * Notes         :- 
       *                  
       *                  
       *                  
       *                  
       *                  
       *
       */

DAXINT *Recnum;		/*	<i>     record number */
DAXINT Icbuff[2];	/*	<i>	Ic buffer for mip and continuation */
char *String;		/*	<i>	String of text*/
INT4 *Length;		/*	<i>	Length */


{





}



