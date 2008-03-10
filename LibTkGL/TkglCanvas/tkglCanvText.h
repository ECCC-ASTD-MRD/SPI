#include "tkInt.h"
#include "tkPort.h"
#include "default.h"
#include "tkglCanvas.h"

typedef struct LayoutChunk {
    CONST char *start;     /* Pointer to simple string to be displayed.
             * This is a pointer into the TkTextLayout's
             * string. */
    int numBytes;    /* The number of bytes in this chunk. */
    int numChars;    /* The number of characters in this chunk. */
    int numDisplayChars;   /* The number of characters to display when
             * this chunk is displayed.  Can be less than
             * numChars if extra space characters were
             * absorbed by the end of the chunk.  This
             * will be < 0 if this is a chunk that is
             * holding a tab or newline. */
    int x, y;        /* The origin of the first character in this
             * chunk with respect to the upper-left hand
             * corner of the TextLayout. */
    int totalWidth;     /* Width in pixels of this chunk.  Used
             * when hit testing the invisible spaces at
             * the end of a chunk. */
    int displayWidth;      /* Width in pixels of the displayable
             * characters in this chunk.  Can be less than
             * width if extra space characters were
             * absorbed by the end of the chunk. */
} LayoutChunk;

typedef struct TextLayout {
    Tk_Font tkfont;     /* The font used when laying out the text. */
    CONST char *string;    /* The string that was layed out. */
    int width;       /* The maximum width of all lines in the
             * text layout. */
    int numChunks;      /* Number of chunks actually used in
             * following array. */
    LayoutChunk chunks[1]; /* Array of chunks.  The actual size will
             * be maxChunks.  THIS FIELD MUST BE THE LAST
             * IN THE STRUCTURE. */
} TextLayout;

/* The structure below defines the record for each text item. */

typedef struct glTextItem  {
   Tk_Item header;     /* Generic stuff that's the same for all types.  MUST BE FIRST IN STRUCTURE. */
   Tk_CanvasTextInfo *textInfoPtr;
            /* Pointer to a structure containing
             * information about the selection and
             * insertion cursor.  The structure is owned
             * by (and shared with) the generic canvas
             * code. */
   /* Fields that are set by widget commands other than "configure". */

   double x, y;     /* Positioning point for text. */
   double dx, dy;     /* Positioning point for text. */
   int insertPos;   /* Character index of character just before which the insertion cursor is displayed. */

   /* Configuration settings that are updated by Tk_ConfigureWidget. */

   Tk_Anchor anchor;        /* Where to anchor text relative to (x,y). */
   Tk_TSOffset tsoffset;
   XColor *color;          /* Color for text. */
   XColor *activeColor;    /* Color for text. */
   XColor *disabledColor;  /* Color for text. */
   Tk_Font tkfont;         /* Font for drawing text. */
   Tk_Justify justify;     /* Justification mode for text. */
   char *text;                     /* Text for item (malloc-ed). */
   int width;                      /* Width of lines for word-wrap, pixels. Zero means no word-wrap. */
   int alpha;                      /* Text alpha value */
   int angle;                      /* Angle of text */

   /* Fields whose values are derived from the current values of the configuration settings above. */

   int numChars;     /* Length of text in characters. */
   int numBytes;     /* Length of text in bytes. */
   Tk_TextLayout textLayout;     /* Cached text layout information. */
   int leftEdge;                 /* Pixel location of the left edge of the text item; where the left border of the text layout is drawn. */
   int rightEdge;                /* Pixel just to right of right edge of  area of text item.  Used for selecting up to end of line. */

   T_glBitmap *stipple;              /* Stipple bitmap for filling item. */
   T_glBitmap *activeStipple;        /* Stipple bitmap for filling item if state is active. */
   T_glBitmap *disabledStipple;      /* Stipple bitmap for filling item if state is disabled. */
} glTextItem;

