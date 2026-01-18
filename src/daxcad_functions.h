

/* SCCS id Keywords             @(#)  412.1 date 6/11/92 daxcad_functions.h   */

/*
 *
 *
 *        Filename    : daxcad_functions.h
 *        Version     : 412.1
 *
 *        Copyright : Practical Technology Limited
 *
 *        Cross language lookup file
 *
 	Used primarily for DAXCAD functions lookup for C files.

*/

#ifdef USE_UNDERSCORE      /* Functions need underscore in X lang  (APOLLO F77 ) */

#define DAXCADFault			DAXCADFault_
#define DAXCAD				daxcad_
#define DAXCADREPAINT 		daxcadrepaint_
#define WRMX00				wrmx00_
#define PTOL				ptol_
#define VALLDRW				valldrw_
#define DEWC02				dewc02_
#define SSFLAG				ssflag_
#define CREATETEXT			createtext_
#define CREATEPROFILE		createprofile_
#define FINDP0				findp0_
#define WORKLAYER			worklayer_
#define RSCRF				rscrf_
#define ZSFLAG				zsflag_
#define DEPRNT				deprnt_
#define GETPROFILE			getprofile_
#define MNLPTS				mnlpts_
#define MNUPTS				mnupts_
#define GTDMEN				gtdmen_
#define GTDMHD				gtdmhd_
#define MAXDISPLAY			maxdisplay_
#define RDISPF				rdispf_
#define FULLREAD			fullread_
#define GTCLRM				gtclrm_
#define GTMCHI				gtmchi_
#define GTMCLO				gtmclo_
#define GETCELL				getcell_
#define TCURS				tcurs_
#define GETNEXTSEQ			getnextseq_
#define MISOLAYER			misolayer_
#define ALSRCH				alsrch_
#define NOSRCH				nosrch_
#define GETNMIPOS			getnmipos_
#define EPRINT				eprint_
#define FINDGROUP			findgroup_
#define MXPICKPROFILE		mxpickprofile_
#define MXWRITECHANGE		mxwritechange_
#define CONFRM				confrm_
#define MXDELETE			mxdelete_
#define ALLDRW				alldrw_
#define PENERS				peners_
#define PENDRW				pendrw_
#define MXMARKER			mxmarker_
#define HFONT				hfont_
#define LABFLG				labflg_
#define LABNFG				labnfg_
#define DCANG				dcang_
#define DCVL14				dcvl14_
#define DVV0L6				dvv0l6_
#define DVN0D6				dvn0d6_
#define DVC0P4				dvc0p4_
#define MENPOP				menpop_
#define MXMODMRK			mxmodmrk_
#define NCPROP				ncprop_
#define NCEDIT				ncedit_
#define NCATTACH			ncattach_
#define GETDRAWINGNAME			getdrawingname_
#define GETGROUP			getgroup_
#define MAKEOK				makeok_
#define NCWRITEPROPERTY			ncwriteproperty_
#define NCEXTRACTPROPERTY		ncextractproperty_
#define GETDRAWINGPARAMS		getdrawingparams_
#define NCTOL				nctol_
#define FAULTHANDLER			faulthandler_
#define GETMPFILE			getmpfile_
#define BUILDUSERINT			builduserint_
#define GETNODEID			getnodeid_
#define GET_DIRS_WILD			get_dirs_wild_
#define GETTMPFILE			gettmpfile_
#define TMPCLEANUP			tmpcleanup_
#define TOOLSGETCWD			toolsgetcwd_
#define TOOLSSETCWD			toolssetcwd_

#define STREAM_OPEN			stream_open_
#define STREAM_CLOSE			stream_close_
#define STREAM_GET_REC			stream_get_rec_
#define STREAM_PUT_REC			stream_put_rec_
#define STREAM_PUT_CHR			stream_put_chr_
#define STREAM_GET_CONDITIONAL		stream_get_conditional_

#define PROMPTOUT			promptout_

#endif


#ifdef NO_UNDERSCORE      /* Functions do not need underscore in X lang  (IBM 6000, APOLLO FTN ) */

#define DAXCAD                          daxcad
#define DAXCADREPAINT                   daxcadrepaint
#define WRMX00                          wrmx00
#define PTOL                            ptol
#define VALLDRW                         valldrw
#define DEWC02                          dewc02
#define SSFLAG                          ssflag
#define CREATETEXT                      createtext
#define CREATEPROFILE                   createprofile
#define FINDP0                          findp0
#define WORKLAYER                       worklayer
#define RSCRF                           rscrf
#define ZSFLAG                          zsflag
#define DEPRNT                          deprnt
#define GETPROFILE                      getprofile
#define MNLPTS                          mnlpts
#define MNUPTS                          mnupts
#define GTDMEN                          gtdmen
#define GTDMHD                          gtdmhd
#define MAXDISPLAY                      maxdisplay
#define RDISPF                          rdispf
#define FULLREAD                        fullread
#define GTCLRM                          gtclrm
#define GTMCHI                          gtmchi
#define GTMCLO                          gtmclo
#define GETCELL                         getcell
#define TCURS                           tcurs
#define GETNEXTSEQ                      getnextseq
#define MISOLAYER                       misolayer
#define ALSRCH                          alsrch
#define NOSRCH                          nosrch
#define GETNMIPOS			getnmipos
#define EPRINT				eprint
#define FINDGROUP			findgroup
#define MXPICKPROFILE			mxpickprofile
#define MXWRITECHANGE			mxwritechange
#define CONFRM				confrm
#define MXDELETE			mxdelete
#define ALLDRW				alldrw
#define PENERS				peners
#define PENDRW				pendrw
#define MXMARKER			mxmarker
#define HFONT				hfont
#define LABFLG				labflg
#define LABNFG				labnfg
#define DCANG				dcang
#define DCVL14				dcvl14
#define DVV0L6				dvv0l6
#define DVN0D6				dvn0d6
#define DVC0P4				dvc0p4
#define MENPOP				menpop
#define MXMODMRK			mxmodmrk
#define NCPROP				ncprop
#define NCEDIT				ncedit
#define NCATTACH	 		ncattach
#define GETDRAWINGNAME			getdrawingname
#define GETGROUP			getgroup
#define MAKEOK				makeok
#define NCWRITEPROPERTY			ncwriteproperty
#define NCEXTRACTPROPERTY		ncextractproperty
#define GETDRAWINGPARAMS		getdrawingparams
#define NCTOL				nctol
#define FAULTHANDLER			faulthandler
#define GETMPFILE			getmpfile
#define BUILDUSERID			builduserint
#define GETNODEID			getnodeid
#define GETTMPFILE			gettmpfile
#define TMPCLEANUP			tmpcleanup
#define TOOLSGETCWD			toolsgetcwd
#define TOOLSSETCWD			toolssetcwd

#define STREAM_OPEN			stream_open
#define STREAM_CLOSE			stream_close
#define STREAM_GET_REC			stream_get_rec
#define STREAM_PUT_REC			stream_put_rec
#define STREAM_PUT_CHR			stream_put_chr
#define STREAM_GET_CONDITIONAL		stream_get_conditional

#define PROMPTOUT			promptout


#endif


#ifdef GPR_UNDERSCORE      /* Underscores except at the end are requred ( SUN ) */

#define DAXCAD                          daxcad_
#define DAXCADREPAINT                   daxcadrepaint_
#define WRMX00                          wrmx00_
#define PTOL                            ptol_
#define VALLDRW                         valldrw_
#define DEWC02                          dewc02_
#define SSFLAG                          ssflag_
#define CREATETEXT                      createtext_
#define CREATEPROFILE                   createprofile_
#define FINDP0                          findp0_
#define WORKLAYER                       worklayer_
#define RSCRF                           rscrf_
#define ZSFLAG                          zsflag_
#define DEPRNT                          deprnt_
#define GETPROFILE                      getprofile_
#define MNLPTS                          mnlpts_
#define MNUPTS                          mnupts_
#define GTDMEN                          gtdmen_
#define GTDMHD                          gtdmhd_
#define MAXDISPLAY                      maxdisplay_
#define RDISPF                          rdispf_
#define FULLREAD                        fullread_
#define GTCLRM                          gtclrm_
#define GTMCHI                          gtmchi_
#define GTMCLO                          gtmclo_
#define GETCELL                         getcell_
#define TCURS                           tcurs_
#define GETNEXTSEQ                      getnextseq_
#define MISOLAYER                       misolayer_
#define ALSRCH				alsrch_
#define NOSRCH				nosrch_
#define GETNMIPOS			getnmipos_
#define EPRINT				eprint_
#define FINDGROUP			findgroup_
#define MXPICKPROFILE			mxpickprofile_
#define MXWRITECHANGE			mxwritechange_
#define CONFRM				confrm_
#define MXDELETE			mxdelete_
#define ALLDRW				alldrw_
#define PENERS				peners_
#define PENDRW				pendrw_
#define MXMARKER			mxmarker_
#define HFONT				hfont_
#define LABFLG				labflg_
#define LABNFG				labnfg_
#define DCANG				dcang_
#define DCVL14				dcvl14_
#define DVV0L6				dvv0l6_
#define DVN0D6				dvn0d6_
#define DVC0P4				dvc0p4_
#define MENPOP				menpop_
#define MXMODMRK			mxmodmrk_
#define NCPROP				ncprop_
#define NCEDIT				ncedit_
#define NCATTACH			ncattach_
#define GETDRAWINGNAME			getdrawingname_
#define GETGROUP			getgroup_
#define MAKEOK				makeok_
#define NCWRITEPROPERTY			ncwriteproperty_
#define NCEXTRACTPROPERTY		ncextractproperty_
#define GETDRAWINGPARAMS		getdrawingparams_
#define NCTOL				nctol_
#define FAULTHANDLER			faulthandler_
#define GETMPFILE			getmpfile_
#define BUILDUSERINT			builduserint_
#define GETNODEID			getnodeid_
#define GET_DIRS_WILD			get_dirs_wild_
#define GETTMPFILE			gettmpfile_
#define TMPCLEANUP			tmpcleanup_
#define TOOLSGETCWD			toolsgetcwd_
#define TOOLSSETCWD			toolssetcwd_

#define STREAM_OPEN			stream_open_
#define STREAM_CLOSE			stream_close_
#define STREAM_GET_REC			stream_get_rec_
#define STREAM_PUT_REC			stream_put_rec_
#define STREAM_PUT_CHR			stream_put_chr_
#define STREAM_GET_CONDITIONAL		stream_get_conditional_

#define PROMPTOUT			promptout_


#endif
