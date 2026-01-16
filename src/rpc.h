/*
     @(#)  412.1 date 6/11/92 rpc.h 


     Filename    : rpc.h
     Version     : 412.1
     Retrieved   : 92/06/11 14:40:10
     Last change : 92/06/11 14:40:09

     Copyright : Practical Technology Int Limited  (c) 1991


     RPC codes numbers for PCNFS-UNIX comms


*/
#define PC_EOF 26	/* the end of file character for processing */

#define PCNFS_EXIT 0	/* exit function */
#define PCNFS_DIR  1	/* a directory inquiriy */
#define PCNFS_SHUT  2	/* shuts down DOS server */
#define PCNFS_RETCODE 3	/* We are reqested the status of the last exit code */
#define PCNFS_COMM_NOREDIR 99	/* Command function But output of pc command on screen */
#define PCNFS_COMM 100	/* Command function */

