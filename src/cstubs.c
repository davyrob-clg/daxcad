/* Legacy no-op entry points expected by older Fortran/C link paths. */
#define STUB(name) void name(void) {}

STUB(locnam_)
STUB(dei0le_)
STUB(dei0ee_)
STUB(gtsrln_)
STUB(mntixt_)
STUB(gtpmencverfy_)
STUB(c2asc1_)
STUB(gtdmengtdmch_)
STUB(gtpmengtdmen_)
STUB(zsflagtyparr_)
STUB(crossdistxy_)
STUB(fndtokgtdmwt_)

STUB(getfd_)
STUB(strmfl_)
STUB(psback_)

STUB(wait_)

STUB(drotp2d_)
STUB(deprntmnlpn0_)
STUB(genngr_)
STUB(defbak_)
STUB(ldbak_)
STUB(pltbak_)
STUB(fhname_)
STUB(spcp00_)
STUB(spct00_)
STUB(spcs00_)
STUB(mnlsp1_)
STUB(mnlsp2_)
STUB(mnlsp3_)
STUB(mnlsp4_)
STUB(mnlsp5_)
STUB(spce02_)

/* STUB(wrmx00_) */

STUB(crebak_)

STUB(xword0_)
STUB(xwordg_)
/* STUB(ncwriteproperty_) */

STUB(window_paint_proc_)
STUB(window_refresh_)
STUB(fault_handler_)

STUB(plotc_)
STUB(fontc_)
STUB(newpec_)
STUB(plotsc_)
STUB(tparmc_)
STUB(symboc_)

#undef STUB
