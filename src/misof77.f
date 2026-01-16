c
c     Practical Technology 1990 (c)    
c         
c         
c     Source file for all F77 integeration on DAX-NC
c         
c     SCCS id Keywords             @(#)  412.1 date 6/11/92 misof77.f 
c

      subroutine worklayer(layer)
c     ===========================
c1    vartype               i4
c1    iostatus              o
c
c2    This routine will return the working layer being used in DAXCAD
c2
c2
c2
c2    Arguments:-
c2
c2
c2     Return Status:-
c2
c2
c2
c2
      include 'include/masti.inc'
c
      integer*4 layer
      layer = clayer
      end

      subroutine valldrw(ent,tmip)
c     ============================
c1    vartype                  i2   i2
c1    iostatus           i/o  i
c
c2    Does the same as ALLDRW but will allow the enetity to  be added into
c2    the viwport system. This must be done when inserting new entities into
c2    the database. See DAXCAD VIEWPORTING OPERATIONS by D. Robertson 
c2
c2    Arguments:-
c2
c2    ent         ->          Entity type actually redundant 
c2    tmip        ->          The master index pointer of the element to be drawn
c2
c2
c2
c2    Return Status:-
c2
c2    NONE
c2
c2
c2
      integer*2 ent
      integer*2 tmip
      include 'include/viewport.inc'
c
      vpadd = .true.
c
c     draw the entity
c
      call alldrw(ent,tmip)
      vpadd = .false.
      end

      subroutine misolayer(lay,code)
c     ==============================
c1    vartype               i4  i4
c1    iostatus              i   i
c
c2    Turns on the defined MISOMEX special interface layer.
c2    This routine also can switch off all other layers to view 
c2    just the misomex special layer defined by the variable lay.
c2
c2
c2    Arguments:-
c2
c2    lay         ->          The layer number to used. Should be soft coded.
c2    code        ->          The code needed to control layer ops
c2                                 
c2    1       ->       Bring misomex layer into view
c2    2       ->       exclude all else except misomex layer
c2    3       ->       Bring back the previous layers includeing miso layer.
c2
c2
c2
c2    Return Status:-
c2
c2    NONE
c2
c2
      include 'include/masti.inc'
      include 'include/viewport.inc'
      include 'include/misof77.inc'

      integer*4 lay
      integer*4 code
      integer*4 i
      integer*2 vpn
      logical ok
c
c
      vpn = cvpn
      if ( code .eq. 0 ) then
c
c         Initalisation code
c
          oldlay = clayer
          mxcontrollayer = lay
c
      elseif ( code .eq. 1 ) then
c         switch on special layer
          nlay = 1
          laydat(1) = lay
          vlayer(lay) = .true.
          call savlay(vpn,ok)
          call addlay(laydat,1)
      elseif(code.eq.2) then
c         Hide all layers except special
          nlay = 0
C         set construction layer 
          oldlay = clayer
          clayer = lay
          do 10 i=0,255
              if ( vlayer(i) .and.i.ne.lay ) then
c                 increment layer count to hide
                  nlay = nlay + 1
                  laydat(nlay) = i
              endif
 10       continue
          call dellay(laydat,nlay)
          do 30 i=0,255
              if ( vlayer(i) .and.i.ne.lay ) then
c                 increment layer count to hide
                  vlayer(i) = .false.
              endif
 30       continue
          call savlay(vpn,ok)

      elseif(code.eq.3 ) then
c         Bring back all layers that were hidden
          clayer = oldlay
          do 40 i=1,nlay
              vlayer(laydat(i)) = .true.
 40       continue
c         save viewport layers
          call savlay(vpn,ok)
          call regend()
      endif
c
      end

      subroutine fullread(mipp,mipbuff,pdpbuff,pdibuff,m,string,st)
c     =============================================================
c1    vartype             i4    i4(13)  i4(4)    d(6) d(9) c   i4
c1    iostatus            i     o       o        o    o    o   o
c
c2    Arguments:-
c2
c2    mipp        ->       The requsted MIP value
c2    mipbuff     ->       The values for the Master index buffer
c2    pdbuff      ->       Part data buffer
c2    pibuff      ->       Part data index buffer
c2    m           ->       Matrix if necessary
c2    string      ->       Text buffer
c2
c2    Return Status:-
c2
c2    -1          ->       Cannot read the element
c2
c2

      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/masti.inc'
c
      integer*2 mipp
      integer*2 mipbuff(13)
      integer*2 pdibuff(4)
      integer*4 st
      double precision pdpbuff(6)
      double precision m(9)
      character string*81
c
      integer*2 ent
      integer*2 tmip
      real matrix(9)
      integer*4 type
      integer*4 nlen
      integer*4 length
      integer*4 i
      logical delete
      logical ok
c
      external nlen
c
      if ( mipp.ge.nmipos.or.mipp.le.0 ) then
c              Master index is not in range
             st = -1
             return
      endif

c
c    Get all information for a full read 
c
      call allrd(mipp,ent,matrix,delete)

c
c    Master index buffer
c
      do 10 i=1,13
          mipbuff(i) = imbuff(i)
10    continue
c
c     Part data buffer
c
      do 20 i=1,6
          pdpbuff(i) = rdbuff(i)
20    continue
c       
c     Part index buffer
c
      do 30 i= i,4
          pdibuff(i) = idbuff(i)
30    continue
c
c     Matrix calculation
c
      do 40 i=1,9
          m(i) = matrix(i)
40    continue
c
c     Text string
c
      string= cbuff
c
c     Append null for c to use
c
      length = nlen(cbuff)+1
      string(length:length) = char(0)
      st = 0
      end
c
      subroutine maxdisplay(maxdfp,viewport)
c     ======================================
c1    vartype                 i4      i4
c1    iostatus                o       o
c
c2    Arguments:-
c2
c2    maxdfp              ->       The current maximum display file pointer.
c2    viewport       ->       The current viewport number.
c2
c2
c2    Return Status:-
c2
c2    NONE
c2
c2

      integer*2 viewport
      integer*2 maxdfp
c
      include 'include/masti.inc'
      include 'include/viewport.inc'
c
      maxdfp = ldfile(cvpn)
      viewport = cvpn
c       
      end
      

      function dtol(x,y)
c     ==================
c
c1    vartype       D,D
c1    iostatus      I,I
c
c2    DTOL returns a value of 0 if the tolerence between the 2 values
c2    is in effect non zero It will return a value of 1 if the 2 values
c2    are zero as defined by the current viewport size
c
      include 'include/wtov.inc'
c
      double precision x,y,rel,dif,dzero
      double precision ext
      logical dtol
c
      data rel/1e-6/
c
      ext = wxmax-wxmin

      dtol = dabs(x-y)/ext.lt.rel 
c
      end

      subroutine nctol(tolerance)
c     ============================
c1    vartype             d
c1    iostatus            o
c
c2    Reads the current viewport tolerance and uses that to get
c2    a tolerance for the values created in the profile. A bit
c2    messy but since we dont store working limits we must use
c2    the current viewport
c2
c2
c2
c2    Arguments:-
c2
c2       
c2    tolerance        ->       Tolerance value defined as the distance between the viewport extents
c2
c2
c2    Return Status:-
c2
c2       
c2
      include 'include/wtov.inc'
c
      double precision tolerance
c
      tolerance = wxmax-wxmin
c
      end


      function ptol(x1,y1,x2,y2)
c       ==========================
c1       vartype       d  d  d  d
c1       iostatus      i  i  d  d
c
c2       PTOL returns a value of 0 if the tolerence between the 2 values
c2       is in effect non zero It will return a value of 1 if the 2 values
c2       are zero as defined by the current viewport size
c
      logical ptol
      logical dtol
      double precision x1,y1,x2,y2
c
      external dtol
c
      ptol = dtol(x1,x2) .and. dtol(y1,y2)
c
      end

      subroutine getcell(daxcell,daxmenu,daxnum,popnoun)
c     ==================================================
c1    vartype              i4      i4      i4      i4
c1    iostatus             o       o       o       o
c
c2    Gets current values for current hit. Must be used after a call to TCURS
c2    or any routine which may use it.
c2
c2    Arguments:-
c2
c2    daxcell              ->              The current hit cell
c2    daxmenu              ->              The current menu number being used
c2    daxnum              ->              The actual cell number code for the hit cell
c2    popnoun              ->              The vnoun number of the last popup cell
c2
c2
c2    Return Status:-
c2
c2    NONE       
c2
c2

      include 'include/menun.inc'
      include 'include/menpop.inc'
c
      integer*4 daxcell
      integer*4 daxmenu
      integer*4 daxnum
      integer*4 popnoun
c
c     LOAD UP CURRENT CELL HIT VALUES
      daxcell = celln
      daxmenu = men
      daxnum = vnccmd
      popnoun = lstnou
c
      end
      

      subroutine getprofile(datan,st)
c     ===============================
c1    vartype                      i4   i4
c1    iostatus               o    o
c
c2    This routine gets a profile from the user and if all is succesful
c2    then the information is placed into SWINDU file for later use by the
c2    system. 
c2
c2
c2
c2    Arguments:-
c2
c2    DATAN       ->       Number of elements in SWINDU
c2
c2
c2
c2    Return Status:-
c2
c2    0       ->       Success. Profile is loaded into SWINDU.
c2
c2

      include 'include/menun.inc'
      include 'include/masti.inc'
      include 'include/hdata.inc'
      include 'include/swind.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/profile.inc'
      include 'include/vntable.inc'
      include 'include/viewport.inc'
c
      real buff1(6)
      real buff2(6)
      real x,y
      
      integer*4 datan
      integer*4 st
      integer*4 rec
      integer*4 entp
      integer*4 j
c
      integer*2 fn
      integer*2 tmip
      integer*2 ent
      integer*2 tdfp
      integer*2 p2
      integer*2 tpdp
c
      logical ok
c
      rec = 52
c     open scratch file for Hatching code.
      call ourscr(hunit,rec,ok)

c     get profile please
      fn = 2
      call pulent(fn)

      if ( vnccmd .eq. 126 ) then
c         accepted profile must use this only.
          ndata = 0
          do 10 entp = 1,no
              read(unit=hunit,rec=entp,err=99) tmip,ent,x,y,
     +             buff1(1),buff1(2),buff1(4),buff1(5),buff1(6),
     1             buff2(1),buff2(2),buff2(4),buff2(5),buff2(6)
c             read master index
              if ( no.gt.1.and.ent.eq.LINE) then
c                 modify lines only
                  call dir500(tmip,ok)
c                 read part data
                  tpdp = imbuff(7)
                  call dbr500(tpdp,ok)
                  do 20 j=1,6
                      rdbuff(j) = buff1(j)
20                continue
c                modify with new modified data
                 call dbm500(tpdp,ok)
              endif
c             write out to scratch file
              call ssflag(tmip,x,y,tdfp,p2,.FALSE.)
10        continue
          datan = ndata
          st = 0
c         clear all flags no need for them now.
          goto 999
      endif
99    continue
C     Error has occured
      call zsflag(.false.,ok)       
      st = -1
      datan = 0
999   continue
c     shut down hatching unit
      no = 0
      close(hunit)
      end


      FUNCTION   DDISTXY(X1,Y1,X2,Y2)
C     ==============================
C
C1       R             R, R, R, R
C1       O             I, I, I, I
C
C2      Function DISTXY returns the distance between the
C2      points (X1,Y1) and (X2,Y2)
C
      DOUBLE PRECISION DDISTXY,X1,Y1,X2,Y2
      INTRINSIC SQRT
C
      DDISTXY= SQRT((X1-X2)**2 + (Y1-Y2)**2)
C
      END
C
      subroutine createtext(x,y,string,p)
c     ===================================
c1    vartyp                d d   c*81 i2
c1    iostatus              i i    i   o
c
c2    Create a string of single text at the position based on current paramterer settings
c2
c2    Arguments:-
c2
c2    x       ->              X position for text
c2    y       ->              Y position for text
c2    string       ->              Character string ( must be null terminated )
c2    p       ->              The new pointer in the database
c2
c2
c2    Return Status:-
c2
c2    A value of p = o will indicate failure to assign new text in database
c2
c2
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/entity.inc'
      include 'include/style.inc'
c
      double precision x,y
      real xp,yp
      character*82 string
      character*82 inpt
      logical ok
      integer*2 p
      integer*2 ent
      integer*4 nlen
      integer*4 pos
c
      external nlen
c
      pos = index(string,char(0))
c
      if ( pos .eq. 0 ) then

          pos = nlen(string)

          if ( pos .eq. 0 ) then
              inpt = ' '
          else
              inpt = string(:pos)
          endif
             
      else
          pos = pos -1
          inpt = string(:pos)
      endif
c
      xp = x
      yp = y
      p = 0
c
c     Create text in DAXCAD database
c
      call dewc85(xp,yp,twidth,thigt,tangl,slant,just,clfont,
     +                  colour,clayer,inpt,p,ok)
      ent = TEXT
      call valldrw(ent,p)

      end

      subroutine createprofile(seq,layer)
c     ===================================
c1    vartype                        i4   i4
c1    iostatus                 i    i
c
c2    This routine creates a profile type entity for the MISOMEX interface
C2    All elements are in the current scratch file.
c2
c2
c2
c2    Arguments:-
c2
c2    seq              ->       Sequence number of profile.
c2    layer              ->       target layer for profile
c2
c2
c2    Return Status:-
c2
c2
c2
      include 'include/masti.inc'
      include 'include/ndata.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/swind.inc'
c
      integer*4 seq
      integer*4 layer
      integer*4 pos
      logical ok
      integer*2 tdfp
      integer*2 tmip
      integer*2 spa
      integer*2 pmip
      real x,y
c
      call dew050(pmip,ok)
      if ( ok ) then
          call dir500(pmip,ok)
          imbuff(5) = seq
          imbuff(13) = seq
          imbuff(4) = layer
          call dim500(pmip,ok)
      endif
c
c
c     Convert layers of the new group
c
      do 10 pos = 1,ndata

          call rscrf(pos,tmip,x,y,tdfp,spa)
          call dir500(tmip,ok)
          imbuff(13) = imbuff(4)
          imbuff(4) = layer
          call dim500(tmip,ok)

 10    continue

      end

      subroutine getnextseq(seed,lay,seq)
c     ===================================
c1    vartype                     i4   i4  i4
c1    iostatus              i    i   o
c
c2    Returns the next avaialble sequence number in the MISOMEX plotter interface
c2    driver. It looks at the layer defined and pulles out groups on that layer that
c2    have a value in slot 5 in the MIP buffer. This then works out the next available
c2    sequence number. The sequences cannot be out of order. Reseq will fix that 
c2    before this routine is called.
c2
c2
c2    Arguments:-
c2
c2    seed              ->              The sequence number to start looking from
c2    seq                     ->              Sequence number of profile.
c2    lay                     ->              target layer for profile
c2
c2    Return Status:-
c2
c2    NONE.
c2
c2

      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/style.inc'
c
      integer*2 pos
      integer*4 seq
      integer*4 seed
      integer*4 lay
      integer*4 maxseq
      integer*4 nextseq
      logical ok
c
c
c     Set first sequence number 
c
      maxseq = seed
      nextseq = seed
c
      do 10 pos = 1,nmipos-1

          call dir500(pos,ok)
c
          if ( imbuff(1) .eq.STATUS .and.
     +               imbuff(2).eq.GROUP.and.
     +               imbuff(4).eq.lay.and.
     +               imbuff(5).gt.0 ) then

                 seq = imbuff(5)
                 if ( seq .gt. maxseq ) then
                      maxseq = seq
                 endif
          endif
 10   continue
c
c     Get NEXT avaiable sequence number
c
      seq = maxseq + 1
      end


      subroutine getnmipos(pos)
c     ============================
c1    vartype               i4
c1    iostatus              o
c
c2    Gets the current NMIPOS of the daxcad database
c2    
c2    
c2
c2    Arguments:-
c2
c2    pos       ->       The next usable pointer 
c2    
c2
c2
c2
c2    Return Status:-
c2
c2    NONE
c2
c2
c2
      include 'include/masti.inc'
c
      integer*2 pos

c     set postion now
      pos = nmipos
c
      end

      subroutine findgroup(dnum,hpx,hpy,mipp,ent,option,quit)
c     =======================================================
c1    vartype               i4   r   r   i2  i2     l    l
c1    iostatus              i    o   o   o   o      o    o
c
c2    This routine will find a group only. It actuall calls FINDET
c2    with the global GSSTAT
c2    
c2
c2    Arguments:-
c2
c2    dnum              ->              DAXCAD prompt number
c2    hpx                     ->              Hit point X
c2    hpy                     ->              Hit point Y
c2    mipp              ->              returned master index pointer. ( indicates a miss )
c2    ent                     ->              returned DAXCAD entity type
c2    option              ->              Option menu has been hit
c2    quit              ->              Verb men has been hit
c2
c2
c2
c2    Return Status:-
c2
c2    mipp       contains 0 if a miss on the workspace
c2
c2
c2
c
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/apollo.inc'
      include 'include/movdat.inc'
      include 'include/misof77.inc'
c
      integer*4 dnum
      integer*4 c
      integer*2 mipp
      integer*2 ent
      real hpx
      real hpy
      logical option
      logical quit
      logical ok
c
c       initialise out parameters
c
      option = .false.
      quit = .false.
      mipp = 0
      ent = 0
c
c     start from here
 10   continue
c     prompt for entity
      call dcprnt(dnum)
 15   continue
c     get cursor input
c     show search zone on cursor
      npos=12
      call tcurs(c,hpx,hpy)
c     cancel search zone on cursor
      npos=4
c
      option=men.eq.3
c
      quit=men.eq.2 .or. ccmd.eq.'Q' .or. ccmd.EQ.'q'
c
c     return if quit or option hit
c
      if (quit.or.option) then
c         Quit or noun menu has been hit
          goto 999
      endif
c
c     search for any legal entity ( use GSSTAT for group searching )
c
      gsstat = 1
      call dse800(hpx,hpy,ok)
      gsstat = 0
c
c
      if ( ok ) then
          call zsflag(.false.,ok)       
          if ( imbuff(1).eq.GROUP.and.imbuff(4).eq.mxcontrollayer) then
c             Group selected set out paramaters
c             read parent pointer to get header
              mipp = imbuff(8)
c             read the real group header entity
              call dir500(mipp,ok)
              ent = imbuff(2)
          endif
      else
          mipp = 0
          ent = 0
      endif
c
c     return with pointer to entity
c
999   continue
c
      end
c
      subroutine mxpickprofile(mastermip,textmip,seqnum,option,quit)
c     =============================================================
c1    vartype                     i2       i2      i4     l     l
c1    iostatus                    o        o       o      o     o
c
c2    This routine asks the user to indicate a profile. The profile
c2    relates specifcaly to an NC sequence which has been used on 
c2    the misolayer. It also returns the mip of the text asociated 
c2    with the profile.
c2    
c2    Arguments:-
c2
c2    mastermip       ->              MIP of profile group set
c2    textmip         ->              MIP of text sequnce 
c2    seqnum          ->              Integer value of sequence number
c2    option          ->              Other option has been hit
c2    quit            ->              quit menu has been hit
c2
c2
c2    Return Status:-
c2
c2    If mastermip is 0 then no profile selected.
c2
c2
c2
c
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'
      include 'include/apollo.inc'
      include 'include/movdat.inc'
c
      integer*2 mastermip
      integer*2 textmip
      integer*2 mipp
      integer*2 ent
      integer*2 relation
      integer*4 seqnum
      integer*4 dnum
      integer*4 entpnt
      
      logical ok
      logical option
      logical quit

      real hpx
      real hpy
c
      option = .false.
      quit = .false.
      mastermip = 0
c     porfile prompt number for indicating a group
      dnum = 828
      call findgroup(dnum,hpx,hpy,mipp,ent,option,quit)
c
      if (option .or. quit ) then
          goto 999
      endif
c
c     check for a valid hit and set outgoung parameters
c
      if ( mipp .gt.0 ) then
          mastermip = mipp
          call dir500(mipp,ok)
c
c         set stored seq number 
c
          seqnum = imbuff(5)
c
c         Text mip is required next
c
          call mxtext(mipp,textmip)
      else
          goto 999
      endif
999   continue
      end
c
      subroutine mxtext(mipp,textmip)
c     ===============================
c1    vartype            i2     i2
c1    iostatus           i      o
c
c2    Reads the relation list of mipp to get the text pointer of the sequence number
c2    
c2    
c2
c2    Arguments:-
c2
c2    mipp            ->          Master index pointer of group 
c2    textmip         ->          Master index pointer of text sequence number
c2
c2
c2
c2    Return Status:-
c2
c2       
c2
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
c
      integer*2 mipp
      integer*2 tmip
      integer*2 textmip
      integer*2 relation
      integer*4 entpnt
      logical ok
c
c     read group relation list first
c
      call dir500(mipp,ok)
      relation = imbuff(10)
c
c     read first record
c
      call drr950(relation,ok)
c
c     start to read actual list
      relation = rlbuff(2)
      call drr950(relation,ok)
      
      entpnt = 4
 100  continue
c
c     begin reading all relation data
c
c
      if ( entpnt .eq. 11 ) then
c         read in a new relation record
          relation = rlbuff(2)
          if ( relation.eq.0) then 
c             Finished reading relation completely
              goto 999
          endif
c
          call drr950(relation,ok)
          entpnt = 4
      endif
c
      tmip = rlbuff(entpnt)
c
      call der500(tmip,ok)
c
c     find a text pointer
c
      if ( imbuff(2).eq.TEXT ) then
          textmip = tmip
          goto 999
      endif
c     increment loop and go backround for next relation record
      entpnt = entpnt + 1
      goto 100
999   continue
c       
      end
c
      subroutine mxwritechange(mipp,seqnum)
c     =====================================
c1    vartype                   i2    i4
c1    iostatus                  i     i
c
c2    Write single change to group text sequence number
c2    
c2    
c2
c2    Arguments:-
c2
c2    mipp              ->              Master index pointer of group
c2    seqnum              ->              new sequence number of group
c2
c2
c2
c2    Return Status:-
c2 
c2    NONE
c2
c2
c2
c
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
c
      integer*2 mipp
      integer*2 textmip
      integer*2 ent
      integer*2 p
      integer*2 pd
      integer*2 dummy
      integer*2 nchars
      integer*2 just
      integer*4 seqnum
      integer*4 nlen
      logical ok
      real compacted
      real sla
c
      external nlen
c
c
c
c     locate text master index for this sequence
c
      call mxtext(mipp,textmip)
c
c     Check sequence number is not the same
c
      call dir500(mipp,ok)
      if ( seqnum .eq. imbuff(5) ) then
          goto 999
      endif

c
c    Check mips 
c       
      if ( textmip.gt.0 ) then

c
c         Erase the old text
c
          call der500(textmip,ok)
          call peners()
          call alldrw(ent,textmip)
          call pendrw()
c
c         Write new seq number to buffer
c
          write(cbuff,fmt='(a,i3)',err=999) 'SEQ',seqnum
          p = imbuff(9)
          pd = imbuff(7)
c
c         modify lengths of text record first
c
          compacted = rdbuff(6)
          call ucodet(compacted,sla,just,nchars)
          nchars = nlen(cbuff)
          call codet(sla,just,nchars,compacted)
          rdbuff(6) = compacted
c
c         Modify text record and redraw it
c
          call dtm500(p,dummy,ok)
          call dbm500(pd,ok)
          call alldrw(ent,textmip)
c
c         Modify group to keep consistancy
c
          call dir500(mipp,ok)
          imbuff(5) = seqnum
          imbuff(13) = seqnum
          call dim500(mipp,ok)
c
      endif
c
999   continue
      end
c
c
c
      subroutine mxdelete(mipp)
c     =========================
c1    vartype              i2
c1    iostatus             i
c
c2    This routine will delete a group instance by exploding it 
c2    The marker and text are marked as deleted entities.
c2       
c2
c2    Arguments:-
c2
c2    mipp              ->              Master index pointer of group
c2       
c2
c2
c2
c2    Return Status:- NONE
c2
c2       
c2
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
c
      integer*2 mipp
      integer*2 relation
      integer*2 ent
      integer*2 tmip
      integer*4 entpnt
      logical ok
c
c
c     read group relation list first
c
      call dir500(mipp,ok)

c
c     Initally mark as deleted
c

      relation = imbuff(10)
c
c     read first record
c
      call drr950(relation,ok)
c
c     start to read actual list
      relation = rlbuff(2)
      call drr950(relation,ok)

      
      entpnt = 4
 100  continue
c
c     begin reading all relation data
c
c
      if ( entpnt .eq. 11 ) then
c         read in a new relation record
          relation = rlbuff(2)
          if ( relation.eq.0) then 
c             Finished reading relation completely
              goto 999
          endif
c
          call drr950(relation,ok)
          entpnt = 4
      endif
c
      
      tmip = rlbuff(entpnt)

      if ( tmip.le.0 ) then
c
c         Nothing left to do here
c
          entpnt = entpnt + 1
          goto 100
      endif
c
      call der500(tmip,ok)
c
c     find elements to delete or to explode
c
      if ( imbuff(2).eq.TEXT ) then
c
c         TEXT element to be deleted.
c
          call peners()
          call alldrw(ent,tmip)
          call pendrw()
c
          call dir500(tmip,ok)
          imbuff(1) = 100
          imbuff(8) = 0
          call dim500(tmip,ok)
c
      elseif ( imbuff(2).eq.MARKER ) then
c
c         MARKER element to be deleted.
c
          call peners()
          call alldrw(ent,tmip)
          call pendrw()
c
          call dir500(tmip,ok)
          imbuff(1) = 100
          imbuff(8) = 0
          call dim500(tmip,ok)
c
      elseif ( imbuff(2).eq.ARC.or.imbuff(2).eq.LINE ) then
c
c         LINE or ARC to be returned to original layer
c
          call dir500(tmip,ok)
          imbuff(4) = imbuff(13)
          imbuff(1) = 10
          imbuff(8) = 0
          call dim500(tmip,ok)
c
      endif
c     increment loop and go backround for next relation record
      entpnt = entpnt + 1
      goto 100
c
999   continue
c
c     Finally delete the GROUP Header
c
      call dir500(mipp,ok)
      imbuff(1) = 100
      call dim500(mipp,ok)
c
      end


      subroutine mxmarker(markermip,x,y)
c     ==================================
c1    vartype                i2     r r
c1    iostatus               i      i i
c
c2    Makes OR moves a marker. This marker must be at scales 1 for now
c2       
c2       
c2
c2    Arguments:-
c2
c2    markermip              ->              Marker mip. If 0 the create a new one.
c2    x                            ->              New or modifued X pos
c2    y                            ->              New or modifued Y pos
c2
c2
c2    Return Status:-
c2
c2       
c2
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/misof77.inc'
c
      integer*2 markermip
      integer*2 ent
      integer*2 tform
      integer*2 tfont
      integer*2 tlay
      integer*2 mp
      integer*2 pd
      logical ok
      real x
      real y
      real angle
      real sx
      real sy
      
c
      if ( markermip.gt.0 ) then
             
c
c         Erase first
c
          call peners()
          call alldrw(ent,markermip)
          call pendrw()
c
c         Modify Points now
c
          call der500(markermip,ok)
          rdbuff(1) = x
          rdbuff(2) = y
          pd = imbuff(7)
          call dbm500(pd,ok)
          call alldrw(ent,markermip)
c
      else
c
c         Set paramters form origin marker
c
          markermip = 0
          mp = 0
          angle = 0.0
          sx = 1.0
          sy = 1.0
          tform = mxoriginmarker
          tfont = 1
          tlay = mxcontrollayer
          call dewc02(x,y,angle,sx,sy,tform,tfont,tlay,mp,ok)
          if ( mp.gt.0 ) then
c
c             draw new marker into postion
c
              call valldrw(ent,mp)
              markermip = mp
          endif                     
      endif
      end

      subroutine mxmodmrk(mipp,direction)
c     ===================================
c     vartype              i2     i4
c     iostatus             i      i
c
c     Modifies marker to contain direction of arc for PP
c2       
c2       
c2
c     Arguments:-
c2
c2       
c2    mipp              ->              Marker master index pointer
c2    direction       ->              Direction 0 CCW 1 CW
c2       
c2       
c2
c2
c2
c2    Return Status:-
c2
c2       
c2
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/entity.inc'
      include 'include/misof77.inc'
c
      integer*2 mipp
      integer*4 direction
      logical ok
c
      call dir500(mipp,ok)
      imbuff(13) = direction
      call dim500(mipp,ok)
      
      end

      subroutine ncprop(propname,prompt,type,mipp,status)
c     ===================================================
c1    vartype              c*80   c*80   i4   i2    i4
c1    iostatus              i      i     i    o     o
c
c2
c2    Defines standard propeties for NC interface.
c2       
c2
c2    Arguments:-
c2
c2    propname       ->       Property name to be set
c2    prompt              ->       The prompt for entering data
c2    type              ->       The DAXCAD property type
c2    mipp              ->       The property master index
c2       
c2
c2
c2    Return Status:-
c2
c2    -1              ->       Property name allready exists
c2       
c2
      include 'include/menun.inc'
      include 'include/props.inc'
C
      integer*4 status
      integer*4 type
      integer*4 c
      integer*4 gtcell
      integer*4 nlen
      integer*4 i
      integer*4 length
      integer*2 prcpnt
      integer*2 pripnt
      integer*2 prpnt
      integer*2 i2
      integer*2 mipp
      logical found
      logical ok
      character*80 propname
      character*80 prompt
      character*80 pname
      character*80 ppromt
c
      external nlen
c
      mipp = 0
      status = 0
c
c
c     Modify names if coming for C
c
      length = index(propname,char(0))
      if ( length .gt. 1 ) then
          pname = propname(1:length-1)
      else
          pname = propname
      endif
      length = index(prompt,char(0))
      if ( length .gt. 1 ) then
          ppromt = prompt(1:length-1)
      else
          ppromt = prompt
      endif
      
c
c       Clear buffers first
c
      do 500 i=1,8
          pribuf(i)=0
 500  continue
c
c     Set type of property ( INT REAL TEXT )
c
      pdatyp=type
c
c     set default action code to KB input
c
      pacode=1
c
c     Foldup property names always
c
      call foldup(pname)
c
c     Check for existance of prop first 
c
      call dspr01(pname,prpnt,found)
c
      if (.not.found) then
c
c        Ok to create it
c
          pribuf(1)=1
c         set property type to current status
          pribuf(2)=pdatyp*256+pacode
c         write property name to file
          i2=5
c
c         Create the property
c
          call dwprc0(nprpos,i2,pname,prcpnt,ok)
c         store pointer in control block
          pribuf(5)=prcpnt
c         write prompt to file
          if (nlen(ppromt).gt.0) then
              i2=6
              call dwprc0(nprpos,i2,ppromt,prcpnt,ok)
              pribuf(6)=prcpnt
          endif
          call dwpri0(pripnt,ok)
          mipp = pripnt
          status = 0
      else
          status = 1
          mipp = prpnt
      endif
c     write the control block to file
      end


      subroutine ncattach(mode,name,get,mipp,pmip,st)
c     ===============================================
c     vartype              i4  c*80  l   i2   i2  i4
c1    iostatus             i    i    i   i    i   o
c
c2    Attach a property onto the supplied group mipp
c2    This will ask for a property name. If it exists
c2    then it will promt the user for data and add it to the database
c2
c2    Arguments:-
c2
c2    mode              ->              Either measure or attach prop 0 attach 1 measure
c2    name              ->              Propert name if required
c2    get               ->              Get name of other property
c2    mipp              ->              Group master index pointer
c2    pmip              ->              Property master index
c2       
c2
c2
c2
c2    Return Status:-
c2
c2    -1       ->       No prop has been attacjed
c2    -2       ->       No props for measure
c2
c2
      include 'include/menun.inc'
      include 'include/props.inc'
      include 'include/nbuff.inc'
      include 'include/masti.inc'
      include 'include/product.inc'
c
      integer*2 mipp
      integer*2 prpnt
      integer*2 pmip
      integer*4 st
      integer*4 length
      integer*4 nlen
      integer*4 mode
      integer*4 np
      character*80 name
      character*80 pname
      logical ok
      logical get
c
      external nlen
c
      st = -1
c
c     Convert form C
c
      if ( mode .eq. 1 ) then
c         get the properties into a buffer
          call expr03(mipp,np,ok)
          if (.not.ok) then
c             cannot extract list of properties
c             "No properties attached"
              call deprnt(343)
              st = -2
              return
          else
              call expr93(np,prnam(1:nlen(prnam))//'.pro',ok)
              if (ok) then
c                 pop to screen for display
                  call dcprnt(206)
                  call poppd1(prnam(1:nlen(prnam))//
     +                       '.pro',774,120,250,668)
              else
                  call deprnt(149)
                    endif
          endif
          return
      endif

      length = index(name,char(0))
      if ( length .gt. 1 ) then
          pname = name(1:length-1)
      else
          pname = name
      endif

      if ( get ) then
          call attp11(pname,pmip,ok)
      else
          propok = .true.
      endif
      
      if(propok) then
c
c         Attach the defined property
c
          call dir500(mipp,ok)
          call attp03(pname,pmip,mipp,ok)
          st = 0
      endif

      end

      subroutine getdrawingname(name,length)
c     ======================================
c1    vartype                   c*1024  o
c1    iostatus                   o      o
c
c2    Gets the current drawing name in the new unix sizes
c2       
c2       
c2
c2    Arguments:-
c2
c2    name       ->       Name of the drawing
c2    length       ->        the length in bytes since its F77
c2
c2
c2
c2    Return Status:-NONE
c2
c2       
c2
      include 'include/params.inc'
c
      character*1024 name
      integer*4 length
      integer*4 nlen
c
      external nlen
c
      length = nlen(drgnam)
      
      name(1:length) = drgnam

c     Conversion for C 
      name(length+1:) = char(0)

      end

      subroutine getgroup(rlp,pntr,omip)
c     ==================================
c1    vartype              i2  i2   i2
c1    iostatus             i   i/o  o
c
c2    This routine interfaces to either f77 or C 
c2    It reads a relation list rlp 
c2       
c2       
c2    Arguments:-
c2
c2    rlp              ->              Relation list pointer
c2    pntr             ->              If initaliy 0 then this sets the list at the start
c2                                   it will be incremented automatically for the next time reading
c2                                   You know your at teh cos it will be set to zero on exit
c2
c2    omip             ->              The mip of the element in the list
c2       
c2       
c2
c2
c2
c2    Return Status:-
c2
c2       
c2
      include 'include/masti.inc'
      include 'include/nbuff.inc'
      include 'include/menun.inc'
      include 'include/entity.inc'

      integer*2 rlp
      integer*2 omip
      integer*2 relation
      integer*2 tmip
      integer*4 pntr
      integer*4 entpnt
      integer*4 count
      logical ok
c
c
      relation = rlp
c       
c     read first record
c
      call drr950(relation,ok)
c
c     start to read actual list
      relation = rlbuff(2)
      call drr950(relation,ok)
c       
      count = 0
      entpnt = 4
 100  continue
c
c     begin reading all relation data
c
c
      if ( entpnt .eq. 11 ) then
c         read in a new relation record
          relation = rlbuff(2)
          if ( relation.eq.0) then 
c             Finished reading relation completely
              pntr = 0
              omip = 0
              goto 999
          endif
c
          call drr950(relation,ok)
          entpnt = 4
      endif
c
      tmip = rlbuff(entpnt)
c
      if ( pntr .eq. 0 .or. pntr .eq. count) then
          pntr = pntr+1 
          omip = tmip
          goto 999
      else
          count = count+1
      endif
c     increment loop and go backround for next relation record
      entpnt = entpnt + 1
      goto 100
c
999   continue
c
      end


      subroutine getdrawingparams(dscale,fac,units)
c     ==============================================
c1    vartype                       d     d   c*3
c1    iostatus                      o     o   o
c
c2    This routine will return certain drawing parameters from the DAXCAD drawing
c2       
c2       
c2
c2    Arguments:-
c2
c2
c2    dscale       ->              The drawing scale
c2    fac          ->              database unit factor
c2    units        ->              DAXCAD unit code eg MM or IN etc
c2
c2
c2
c2    Return Status:-
c2    NONE
c2       
c2

      include 'include/masti.inc'
      include 'include/params.inc'
c
      double precision dscale
      double precision fac
      character*3 units
c
c
      units = dbunit
      units(3:3) = char(0)
c
      dscale = drwscl
      fac = dbufac
c
      end

      subroutine wrts00()
c
c     Stub to be taken OUT !!!!!
c

      end

      
      
