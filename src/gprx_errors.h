
/* SCCS id Keywords             @(#)  412.1 date 6/11/92 gprx_errors.h   */

/*


           GPR X lib error codes 

*/

#ifndef GPRX_ERRORS_H_
#define GPRX_ERRORS_H_

#define GPR_ColorOutOfRange             1       /* color index not in range */
#define GPR_Size                        2       /* Bitmap is out of bounds  */
#define GPR_Planes                      3       /* PLanes are out of bounds */
#define GPR_NoBitmap                    4       /* Cannot allocate any more bitmaps */
#define GPR_CannotOpenDisplay           5       /* unablke to connect */
#define GPR_BadDescripter               6       /* Bad bitmap descipter */
#define GPR_ModeNotSupported            7       /* the mode is not supported */
#define GPR_EventNotSupported           8       /* Cannot get this event */
#define GPR_NotInputWindow              9       /* requset does not allow pixmap */
#define GPR_XdisplayNotOpen            10       /* the display is not open */
#define GPR_XRasterOpInvalid           11       /* raster op out of range */
#define GPR_LengthNotValid             12       /* Length of string is 0 */
#define GPR_CannotLoadFont             13       /* cannot load the font */
#define GPR_FontAllreadyLoaded         14       /* the font allready used */
#define GPR_FontidOutOfRange           15       /* The fontid is out of range */
#define GPR_FontNotLoaded              16       /* not loaded */
#define GPR_BitmapIsNotWindow          17       /* This bitmap is not a main window */
#define GPR_NoRegion                   18       /* region cannot be defined */
#define GPR_NoCursor                   19       /* cursor cannot be defined */
#define GPR_PolyNotStarted             20       /* the poly gon has not been started */
#define GPR_PolyPoints                 21       /* too mnay points */
#define GPR_NotPixmap                  22       /* the bitmap descripter is not a valid pixmap */
#define GPR_DisplayNotOpen             23       /* the X server has been connect */
#define GPR_NoEnvironment              24       /* No env has been saved */
#define GPR_CannotAllocateRefresh      25       /* Cannot allocate bitmap for automatic refresh */ 
#define GPR_NoAutoRefresh              26       /* Auto refresh mode not set for this bitmap */
#define GPR_ColormapLoaded             27       /* GPRX colormap allready loaded */


#endif // GPRX_ERRORS_H_
