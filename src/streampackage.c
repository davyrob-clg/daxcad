
/*      @(#)  412.1 date 6/11/92 streampackage.c 
 *
 *
 *        Filename    : streampackage.c
 *        Version     : 412.1
 *        Retrieved   : 92/06/12 15:49:25
 *        Last change : 92/06/11 14:41:16
 *
 *        Copyright : Practical Technology Intl Limited  1991
*/

#include<stdio.h>
#include<sys/file.h>
#include<fcntl.h>
#include<errno.h>

#include "daxcad_functions.h"
#include "xlang.h"

/*

	Yet another Apollo hack. A small stream package emulator

*/


#define STREAM_READ         0	/* Various open modes */
#define STREAM_WRITE        1
#define STREAM_OVERWRITE    2
#define STREAM_UPDATE       3
#define STREAM_APPEND       4
#define STREAM_MAKE_BACKUP  5
#define STREAM_CREATE       6	/* PTI new mode */

#define STREAM_NO_CONC_WRITE        0    /* Various control modes */
#define STREAM_CONTROLLED_SHARING   1
#define STREAM_UNREGULATED          2



STREAM_CLOSE(Streamid,st)

      /* Description   :- Closes a stream in the fashion on apollo streams
       * 
       * 
       *
       * Return status :- 0   Success
       *                 -1   Descipter not valid
       *                 
       *                 
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


short *Streamid;	/* <o> Stream id of opened file */
int *st;		/* <o> status */


{

	*st = 0;

	if ( *Streamid < 0 ) {

		*st = -1;
		return;
	}

	close(*Streamid);



}




STREAM_OPEN(Pathname,Plen,Wmode,Smode,Streamid,st)

      /* Description   :- Opens a stream in the fashion on apollo streams
       * 
       * 
       *
       * Return status :- 0   Success
       *                 -1   File length invalid
       *                 -2   Mode not supported
       *                 -3   File could not be opened  
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

char *Pathname;		/* <i> Pathname of file yow ant to open */
short *Plen;		/* <i> Length of pathname ( Ignore nulls ) */
short *Wmode;		/* <i> Append or owverwrite the stream */
short *Smode;		/* <i> Writing mode either sharing or uncontrolled ( IGNORED )*/
short *Streamid;	/* <o> Stream id of opened file */
int *st;		/* <o> status */

{


int mode;	/* c mode */
int dir;	/* access mode */
char *file;
int len;
int fd;

	*st = 0;	/* set as OK */

	dir = 0666;	/* set full open mode */

	if ( *Plen <= 0 ) {
		*st = -1;
		return;
	}

	switch ( *Wmode )	{	/* map into C open modes */

		case STREAM_WRITE :

			mode = O_RDWR;
			break;

		case STREAM_READ :

			mode = O_RDONLY;
			break;

		case STREAM_OVERWRITE :

			mode = O_RDWR | O_TRUNC;
			break;

		case STREAM_APPEND :

			mode = O_RDWR | O_APPEND;
			break;

		case STREAM_CREATE :

			mode = O_RDWR | O_TRUNC | O_CREAT;
			break;

		default:
			*st = -2;
			return;
	}


	file = (char*)malloc(*Plen+1);	/* copy incase of F77 */
	strncpy(file,Pathname,*Plen);

	file[*Plen] = '\0';
	fd = open(file,mode,dir);	/* try to open file */

	free(file);

	if ( fd < 0 )	{
		*st = -3;
		return;
	}	


	*Streamid = fd;


}


STREAM_GET_REC(Streamid,Buffer,Bytes,Retsize,st)

      /* Description   :- Gets a record from the buffer
       *                  of requested bytes
       *                  and returns the actual bytes read
       *
       * Return status :- 0   Success
       *                 -1   Invalid read 
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


short *Streamid;
char *Buffer;
int *Bytes;
int *Retsize;
int *st;

{

int ret;


	*st = 0;

	ret = read(*Streamid,Buffer,*Bytes);

	if ( ret < 0 )	{
		*st = -1;
		return;
	}
	*Retsize = ret;

	return;




}


STREAM_PUT_REC(Streamid,Buffer,Bytes,st)

      /* Description   :- Writes a number of byes to a buffer
       * 
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

short *Streamid;
char *Buffer;
int *Bytes;
int *st;

{


	write(*Streamid,Buffer,*Bytes);



}

STREAM_PUT_CHR(Streamid,Buffer,Bytes,st)

      /* Description   :- Writes a number of byes to a buffer
       * 
       * 
       *
       * Return status :- 0   Success
       *                  -1  Could not write
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

short *Streamid;
char *Buffer;
int *Bytes;
int *st;

{

int ret;

	*st = 0;

	ret = write(*Streamid,Buffer,*Bytes);

	if ( ret < 0) {
                perror("[ STREAM_PUT_CHR ] stream write failure");
		*st = -1;
		return;
	}


}


