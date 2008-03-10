
#ifndef _GLFONT_H
#define _GLFONT_H

#define FONTMAP_SHIFT       10
#define FONTMAP_PAGES       (1 << (sizeof(Tcl_UniChar)*8 - FONTMAP_SHIFT))
#define FONTMAP_BITSPERPAGE (1 << FONTMAP_SHIFT)
#define SUBFONT_SPACE       3
#define BASE_CHARS          128

typedef struct TkFontAttributes {
   Tk_Uid family;		/* Font family, or NULL to represent plaform-specific default system font. */
   int size;			/* Pointsize of font, 0 for default size, or negative number meaning pixel size. */
   int weight;			/* Weight flag; see below for def'n. */
   int slant;			/* Slant flag; see below for def'n. */
   int underline;		/* Non-zero for underline font. */
   int overstrike;		/* Non-zero for overstrike font. */
} TkFontAttributes;

typedef struct TkFontMetrics {
    int	ascent;			/* From baseline to top of font. */
    int	descent;		/* From baseline to bottom of font. */
    int maxWidth;		/* Width of widest character in font. */
    int fixed;			/* Non-zero if this is a fixed-width font, 0 otherwise. */
} TkFontMetrics;

typedef struct TkFont {
   int resourceRefCount;	/* Number of active uses of this font (each
				 * active use corresponds to a call to
				 * Tk_AllocFontFromTable or Tk_GetFont).
				 * If this count is 0, then this TkFont
				 * structure is no longer valid and it isn't
				 * present in a hash table: it is being
				 * kept around only because there are objects
				 * referring to it.  The structure is freed
				 * when resourceRefCount and objRefCount
				 * are both 0. */
    int objRefCount;		/* The number of Tcl objects that reference
				 * this structure. */
    Tcl_HashEntry *cacheHashPtr;/* Entry in font cache for this structure,
				 * used when deleting it. */
    Tcl_HashEntry *namedHashPtr;/* Pointer to hash table entry that
				 * corresponds to the named font that the
				 * tkfont was based on, or NULL if the tkfont
				 * was not based on a named font. */
    Screen *screen;		/* The screen where this font is valid. */
    int tabWidth;		/* Width of tabs in this font (pixels). */
    int	underlinePos;		/* Offset from baseline to origin of
				 * underline bar (used for drawing underlines
				 * on a non-underlined font). */
    int underlineHeight;	/* Height of underline bar (used for drawing
				 * underlines on a non-underlined font). */

    /*
     * Fields used in the generic code that are filled in by
     * platform-specific code.
     */

    Font fid;			/* For backwards compatibility with XGCValues
				 * structures.  Remove when TkGCValues is
				 * implemented.  */
    TkFontAttributes fa;	/* Actual font attributes obtained when the
				 * the font was created, as opposed to the
				 * desired attributes passed in to
				 * TkpGetFontFromAttributes().  The desired
				 * metrics can be determined from the string
				 * that was used to create this font. */
    TkFontMetrics fm;		/* Font metrics determined when font was
				 * created. */
    struct TkFont *nextPtr;	/* Points to the next TkFont structure with
				 * the same name.  All fonts with the
				 * same name (but different displays) are
				 * chained together off a single entry in
				 * a hash table. */
} TkFont;

typedef struct FontFamily {
    struct FontFamily *nextPtr;	/* Next in list of all known font families. */
    int refCount;		/* How many SubFonts are referring to this
				 * FontFamily.  When the refCount drops to
				 * zero, this FontFamily may be freed. */
    /*
     * Key.
     */
     
    Tk_Uid faceName;		/* Face name key for this FontFamily. */

    /*
     * Derived properties.
     */
     
    Tcl_Encoding encoding;	/* Encoding for this font family. */
    int isSymbolFont;		/* Non-zero if this is a symbol font. */
    int isWideFont;		/* 1 if this is a double-byte font, 0 
				 * otherwise. */
    BOOL (WINAPI *textOutProc)(HDC, int, int, TCHAR *, int);
				/* The procedure to use to draw text after
				 * it has been converted from UTF-8 to the 
				 * encoding of this font. */
    BOOL (WINAPI *getTextExtentPoint32Proc)(HDC, TCHAR *, int, LPSIZE);
				/* The procedure to use to measure text after
				 * it has been converted from UTF-8 to the 
				 * encoding of this font. */

    char *fontMap[FONTMAP_PAGES];
				/* Two-level sparse table used to determine
				 * quickly if the specified character exists.
				 * As characters are encountered, more pages
				 * in this table are dynamically added.  The
				 * contents of each page is a bitmask
				 * consisting of FONTMAP_BITSPERPAGE bits,
				 * representing whether this font can be used
				 * to display the given character at the
				 * corresponding bit position.  The high bits
				 * of the character are used to pick which
				 * page of the table is used. */

    /*
     * Cached Truetype font info.
     */
     
    int segCount;		/* The length of the following arrays. */
    USHORT *startCount;		/* Truetype information about the font, */
    USHORT *endCount;		/* indicating which characters this font
				 * can display (malloced).  The format of
				 * this information is (relatively) compact,
				 * but would take longer to search than 
				 * indexing into the fontMap[][] table. */
} FontFamily;

typedef struct SubFont {
    char **fontMap;		/* Pointer to font map from the FontFamily, 
				 * cached here to save a dereference. */
    HFONT hFont;		/* The specific screen font that will be
				 * used when displaying/measuring chars
				 * belonging to the FontFamily. */
    FontFamily *familyPtr;	/* The FontFamily for this SubFont. */
} SubFont;

typedef struct WinFont {
    TkFont font;/* Stuff used by generic font package.  Must
				 * be first in structure. */
    SubFont staticSubFonts[SUBFONT_SPACE];
				/* Builtin space for a limited number of
				 * SubFonts. */
    int numSubFonts;		/* Length of following array. */
    SubFont *subFontArray;	/* Array of SubFonts that have been loaded
				 * in order to draw/measure all the characters
				 * encountered by this font so far.  All fonts
				 * start off with one SubFont initialized by
				 * AllocFont() from the original set of font
				 * attributes.  Usually points to
				 * staticSubFonts, but may point to malloced
				 * space if there are lots of SubFonts. */

    HWND hwnd;	/* Toplevel window of application that owns
				 * this font, used for getting HDC for
				 * offscreen measurements. */
    int pixelSize;		/* Original pixel size used when font was
				 * constructed. */
    int widths[BASE_CHARS];	/* Widths of first 128 chars in the base
				 * font, for handling common case.  The base
				 * font is always used to draw characters
				 * between 0x0000 and 0x007f. */
} WinFont;

#endif
