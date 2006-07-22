#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <assert.h>
#include <math.h>
#include <limits.h>

#include <ncurses.h>

#include <sys/time.h>
#include <sys/types.h>
#include <unistd.h>

#include "putty.h"
#include "terminal.h"
#include "misc.h"

int curses_active;

/*
 * We're only _replaying_ terminal data, so we never need to send
 * back to a real application. Thus, ldisc_send as called from
 * terminal.c is a stub function.
 */
void ldisc_send(void *handle, char *buf, int len, int interactive)
{
}

void cleanup_exit(int code)
{
    exit(code);
}
void fatalbox(char *p, ...)
{
    va_list ap;

    if (curses_active)
	endwin();

    fprintf(stderr, "FATAL ERROR: ");
    va_start(ap, p);
    vfprintf(stderr, p, ap);
    va_end(ap);
    fputc('\n', stderr);
    cleanup_exit(1);
}
void modalfatalbox(char *p, ...)
{
    va_list ap;

    if (curses_active)
	endwin();

    fprintf(stderr, "FATAL ERROR: ");
    va_start(ap, p);
    vfprintf(stderr, p, ap);
    va_end(ap);
    fputc('\n', stderr);
    cleanup_exit(1);
}

int char_width(Context ctx, int uc)
{
    /*
     * I don't expect this to come up very often.
     */
    return 1;
}

/*
 * All the terminal report functions, and many of the front end
 * terminal functions, are pointless stubs in this implementation.
 */
void set_iconic(void *frontend, int iconic) {}
void move_window(void *frontend, int x, int y) {}
void set_zorder(void *frontend, int top) {}
void refresh_window(void *frontend) {}
void set_zoomed(void *frontend, int zoomed) {}
int is_iconic(void *frontend) { return 0; }
void get_window_pos(void *frontend, int *x, int *y) { *x = *y = 0; }
void get_window_pixels(void *frontend, int *x, int *y) { *x = *y = 0; }
char *get_window_title(void *frontend, int icon) { return ""; }
void set_title(void *frontend, char *title) {}
void set_icon(void *frontend, char *title) {}
void set_sbar(void *frontend, int total, int start, int page) {}
void get_clip(void *frontend, wchar_t ** p, int *len) { *p = NULL; *len = 0; }
void write_clip(void *frontend, wchar_t * data, int *attr, int len,
		int must_deselect) {}
void request_paste(void *frontend) {}
void request_resize(void *frontend, int w, int h) {}
void palette_reset(void *frontend) {}
void palette_set(void *frontend, int n, int r, int g, int b) {}
void set_raw_mouse_mode(void *frontend, int activate) {}
void logflush(void *handle) {}
void logtraffic(void *handle, unsigned char c, int logmode) {}
void do_beep(void *frontend, int mode) {}

/*
 * We don't save and load PuTTY configuration data, so these are
 * all stubs too.
 */
void *open_settings_w(const char *sessionname, char **errmsg)
{ return NULL; }
void write_setting_s(void *handle, const char *key, const char *value) {}
void write_setting_i(void *handle, const char *key, int value) {}
void close_settings_w(void *handle) {}
void *open_settings_r(const char *sessionname)
{ return NULL; }
char *read_setting_s(void *handle, const char *key, char *buffer, int buflen)
{ return NULL; }
int read_setting_i(void *handle, const char *key, int defvalue)
{ return defvalue; }
int read_setting_fontspec(void *handle, const char *name, FontSpec *result)
{ return 0; }
int read_setting_filename(void *handle, const char *name, Filename *result)
{ return 0; }
void write_setting_fontspec(void *handle, const char *name, FontSpec result) {}
void write_setting_filename(void *handle, const char *name, Filename result) {}
void close_settings_r(void *handle) {}
void *enum_settings_start(void) { return NULL; }
char *enum_settings_next(void *handle, char *buffer, int buflen) {return NULL;}
FontSpec platform_default_fontspec(const char *name)
{
    FontSpec ret;
    *ret.name = '\0';
    return ret;
}
Filename platform_default_filename(const char *name)
{
    Filename ret;
    *ret.path = '\0';
    return ret;
}
char *platform_default_s(const char *name) { return NULL; }
int platform_default_i(const char *name, int def) { return def; }
void enum_settings_finish(void *handle) {}
int default_port = -1, default_protocol = -1;

#define FG     0x000F0000
#define BG     0x00F00000
#define FGBG   0x00FF0000
#define BOLD   0x01000000
#define UNDER  0x02000000
#define REV    0x04000000
#define BLINK  0x08000000

#define FGSHIFT 16
#define BGSHIFT 20
#define FGBGSHIFT FGSHIFT

#define NODEFAULT -1
#define TTYREC 0
#define NHRECORDER 1
#define NTYPES 2
static const char *const typenames[] = { "ttyrec", "nh-recorder" };
struct filename {
    char *name;
    int type;
};

struct parray;

struct inst {
    Terminal *term;
    struct unicode_data ucsdata;
    Config cfg;
    int *screen, *oldscreen, w, h, screenlen;
    struct parray **parrays;
    int frames;
    unsigned long long movietime;

    struct filename *filenames;
    int nfiles, filesize;

    int cpairs[(FGBG >> FGSHIFT) + 1];
    int pairsused, nines;
    int number, osd;
    int playing;
    int logmod;
    double speedmod;
};

/*
 * Our notional screen data structure is simply an array of 32-bit
 * integers: W*H screen positions, plus a magic one for the cursor
 * position.
 * 
 * This rather simplistic and flat architecture is because a lot of
 * the time we won't be directly storing these. Instead, we'll be
 * storing a list of how each integer changed over time. Our
 * _movie_ data structure will be a collection of pseudo-`arrays',
 * one for each of the integers in our screen array, containing
 * elements of the form (frame number in which the integer changed,
 * what it changed to). This means that we can determine the value
 * of integer I at frame F by a binary search of the array. This
 * enables us to play back and forth through the entire movie with
 * arbitrary rewind. Hence the need to have the entire terminal
 * state encoded as an unstructured list of integers: if I had to
 * give separate treatment to the cursor position and any other
 * future enhancements such as line attributes, it would all get
 * more complicated.
 * 
 * To prevent memory wastage by repeatedly reallocing several
 * actual arrays, we instead use the concept of a `pseudo-array',
 * which is structured much like an ext2fs file: initially the
 * array is a single block of memory in the obvious format, but
 * once it overflows that block we move to a two-layer structure
 * containing an index block with (frame, sub-block) records each
 * indexing a block of real array. When the index block overflows,
 * we move to a three-layer structure with a second-level index
 * block indexing the first-level ones, and so on.
 */

struct parray_block;

struct parray_level0 {
    int frame;
    int data;
};

struct parray_level1 {
    int frame;
    struct parray_block *subblock;
};

#ifdef PARRAY_TEST
#define PARRAY_L0COUNT 5
#define PARRAY_L1COUNT 3
#else
#define PARRAY_BLKSIZE 16384
#define PARRAY_L0COUNT (PARRAY_BLKSIZE / sizeof(struct parray_level0))
#define PARRAY_L1COUNT (PARRAY_BLKSIZE / sizeof(struct parray_level1))
#endif

struct parray {
    int toplevel;
    int items;
    struct parray_block *root;
};

struct parray_block {
    union {
	struct parray_level0 level0[PARRAY_L0COUNT];
	struct parray_level1 level1[PARRAY_L1COUNT];
    };
};

struct parray *parray_new(void)
{
    struct parray *pa = snew(struct parray);

    pa->toplevel = -1;
    pa->items = 0;
    pa->root = NULL;

    return pa;
}

void parray_append(struct parray *pa, int frame, int data)
{
    struct parray_block *pb, *pb2;
    int i, n, index, count;

    /*
     * Special case: the very first item.
     */
    if (!pa->items) {
	pb = snew(struct parray_block);

	for (i = 0; i < PARRAY_L0COUNT; i++) {
	    pb->level0[i].frame = INT_MAX;
	    pb->level0[i].data = 0;
	}
	pb->level0[0].frame = frame;
	pb->level0[0].data = data;

	pa->items++;
	pa->toplevel = 0;
	pa->root = pb;

	return;
    }

    /*
     * Figure out how many items are covered by a single block at
     * the parray's current top level.
     */
    count = PARRAY_L0COUNT;
    for (i = 1; i <= pa->toplevel; i++)
	count *= PARRAY_L1COUNT;

    /*
     * If this is equal to the parray's current total item count,
     * we must create a new top-level block.
     */
    assert(pa->items <= count);
    if (pa->items == count) {
	pb = snew(struct parray_block);

	/*
	 * pa->root->level0[0].frame and pa->root->level1[0].frame
	 * overlap exactly (guaranteed by the C standard), so we
	 * don't need to worry about which one to access through.
	 */
	pb->level1[0].frame = pa->root->level1[0].frame;
	pb->level1[0].subblock = pa->root;

	pa->toplevel++;
	pa->root = pb;

	count *= PARRAY_L1COUNT;       /* we've moved up a level */
    }

    /*
     * Now work down the tree. At each level, create a new block
     * and descend to it if necessary, otherwise descend to the
     * last existing block if it's not completely full.
     */
    pb = pa->root;
    index = pa->items;
    for (i = pa->toplevel; i-- > 0 ;) {
	count /= PARRAY_L1COUNT;

	n = index / count;
	assert(n < PARRAY_L1COUNT);
	index %= count;

	if (!index) {
	    /*
	     * Create a new empty block at the next level down.
	     */
	    pb2 = snew(struct parray_block);
	    pb->level1[n].frame = frame;
	    pb->level1[n].subblock = pb2;
	}

	/*
	 * Descend to the partially filled end block, whether or
	 * not we just had to create it.
	 */
	pb = pb->level1[n].subblock;
    }

    /*
     * Now we're sitting on a level-0 block which is known to have
     * spare space. Add our entry.
     */
    pb->level0[index].frame = frame;
    pb->level0[index].data = data;

    pa->items++;
}

int parray_retrieve(struct parray *pa, int frame)
{
    struct parray_block *pb;
    int count, total, i, n, top, bot, mid;

    assert(pa->root);
    assert(pa->items > 0);
    assert(frame >= pa->root->level1[0].frame);

    /*
     * Figure out how many items are covered by a single block at
     * the parray's current top level. This will tell us how many
     * blocks to check at each level of the parray.
     */
    count = PARRAY_L0COUNT;
    for (i = 1; i <= pa->toplevel; i++)
	count *= PARRAY_L1COUNT;

    /*
     * Binary search each block on the way down.
     */
    pb = pa->root;
    total = pa->items;
    for (i = pa->toplevel; i-- > 0 ;) {
	count /= PARRAY_L1COUNT;

	n = (total + count - 1) / count;

	bot = 0;
	top = n;
	while (top - bot > 1) {
	    mid = (top + bot) / 2;
	    if (pb->level1[mid].frame > frame)
		top = mid;
	    else
		bot = mid;
	}

	total -= bot * count;
	if (total > count)
	    total = count;

	pb = pb->level1[bot].subblock;
    }

    /*
     * And binary search the bottom block.
     */
    bot = 0;
    top = total;
    while (top - bot > 1) {
	mid = (top + bot) / 2;
	if (pb->level0[mid].frame > frame)
	    top = mid;
	else
	    bot = mid;
    }

    return pb->level0[bot].data;
}

#define CURSOR (inst->w * inst->h)
#define TIMETOP (inst->w * inst->h + 1)
#define TIMEBOT (inst->w * inst->h + 2)
#define FILENO (inst->w * inst->h + 3)
#define OFFSET (inst->w * inst->h + 4)
#define TOTAL (inst->w * inst->h + 5)

void sys_cursor(void *frontend, int x, int y)
{
    struct inst *inst = (struct inst *)frontend;
    inst->screen[CURSOR] = y * inst->w + x;
}

Context get_ctx(void *frontend)
{
    return (Context)frontend;
}

void do_text(Context ctx, int x, int y, wchar_t *text, int len,
	     unsigned long attr, int lattr)
{
    struct inst *inst = (struct inst *)ctx;
    int i, index;
    unsigned int fg, bg, val;

    for (i = 0; i < len; i++) {
	assert(y >= 0 && y < inst->h);
	assert(x+i >= 0 && x+i < inst->w);
	index = y * inst->w + (x+i);

	val = text[i] & 0xFF;
	if (text[i] >= 0xD95F && text[i] < 0xD97F)
	    val += 0x100;
	if (attr & ATTR_BOLD)
	    val |= BOLD;
	if (attr & ATTR_UNDER)
	    val |= UNDER;
	if (attr & ATTR_REVERSE)
	    val |= REV;
	if (attr & ATTR_BLINK)
	    val |= BLINK;
	fg = (attr & ATTR_FGMASK) >> ATTR_FGSHIFT;
	bg = (attr & ATTR_BGMASK) >> ATTR_BGSHIFT;
	if (fg >= 8)
	    fg = 9;
	if (bg >= 8)
	    bg = 9;
	val |= (fg << FGSHIFT) | (bg << BGSHIFT);
	inst->screen[index] = val;
    }
}

void do_cursor(Context ctx, int x, int y, wchar_t *text, int len,
	       unsigned long attr, int lattr)
{
    do_text(ctx, x, y, text, len, attr, lattr);
}

void free_ctx(Context ctx)
{
}

void store_frame(struct inst *inst, unsigned long long delay,
		 int fileno, long fileoff)
{
    int i, n;

    /*
     * Force the terminal to refresh, so that our data is up to
     * date.
     */
    term_invalidate(inst->term);
    term_update(inst->term);

    /*
     * Now see which terminal integers have changed, and write
     * movie records for the ones that have.
     */
    inst->movietime += delay;
    inst->frames++;

    inst->screen[TIMETOP] = (unsigned long long)inst->movietime >> 32;
    inst->screen[TIMEBOT] = (unsigned long long)inst->movietime & 0xFFFFFFFF;

    inst->screen[FILENO] = fileno;
    inst->screen[OFFSET] = fileoff;

    n = 0;
    for (i = 0; i < inst->screenlen; i++) {
	/*
	 * 0xFFFFFFFF is an invalid value for _any_ integer, which
	 * enables us to use it as the `not initialised yet'
	 * setting for oldscreen.
	 */
	assert(inst->screen[i] != 0xFFFFFFFF);
	if (inst->screen[i] != inst->oldscreen[i])
	    n++;
    }

    for (i = 0; i < inst->screenlen; i++) {
	if (inst->screen[i] != inst->oldscreen[i]) {
	    parray_append(inst->parrays[i], inst->frames-1, inst->screen[i]);
	    inst->oldscreen[i] = inst->screen[i];
	}
    }
}

void start_player(struct inst *inst)
{
    int i;

    initscr();
    noecho();
    move(0,0);
    refresh();
    if (has_colors()) {
	start_color();
	for (i = 0; i < lenof(inst->cpairs); i++)
	    inst->cpairs[i] = -1;
	inst->pairsused = 1;
	inst->nines = (use_default_colors() == OK);
    } else {
	inst->pairsused = -1;
    }
    curses_active = TRUE;
}

void end_player(struct inst *inst)
{
    int my, mx;

    if (!curses_active)
	return;
    endwin();
    curses_active = FALSE;
}

unsigned int_for_frame(struct inst *inst, int i, int f)
{
    return parray_retrieve(inst->parrays[i], f);
}

void set_cpair(struct inst *inst, int col)
{
    int fg, bg;

    if (!inst->nines) {
	/*
	 * If default fg and bg are not supported, fall back to
	 * white on black as a default.
	 */
	fg = ((col << FGBGSHIFT) & FG) >> FGSHIFT;
	bg = ((col << FGBGSHIFT) & BG) >> BGSHIFT;
	if (fg == 9)
	    fg = 7;
	if (bg == 9)
	    bg = 0;
	col = ((fg << FGSHIFT) | (bg << BGSHIFT)) >> FGBGSHIFT;
    }

    if (col != 0x99) {
	if (inst->cpairs[col] == -1) {
	    inst->cpairs[col] = inst->pairsused++;
	    fg = ((col << FGBGSHIFT) & FG) >> FGSHIFT;
	    bg = ((col << FGBGSHIFT) & BG) >> BGSHIFT;
	    init_pair(inst->cpairs[col],
		      (fg < 8 ? fg : -1),
		      (bg < 8 ? bg : -1));
	}
	wattron(stdscr, COLOR_PAIR(inst->cpairs[col]));
    }
}

void display_frame(struct inst *inst, int f)
{
    int i, x, y;

    /*
     * Fetch the screen state in this frame.
     */

    for (i = 0; i < inst->screenlen; i++)
	inst->screen[i] = int_for_frame(inst, i, f);

    /*
     * Now display it.
     */
    for (y = 0; y < inst->h; y++)
	for (x = 0; x < inst->w; x++) {
	    unsigned val = inst->screen[y*inst->w + x];
	    int col, ch;

	    wattrset(stdscr, A_NORMAL);
	    if (val & BOLD)
		wattron(stdscr, A_BOLD);
	    if (val & UNDER)
		wattron(stdscr, A_UNDERLINE);
	    if (val & REV)
		wattron(stdscr, A_REVERSE);
	    if (val & BLINK)
		wattron(stdscr, A_BLINK);
	    if (inst->pairsused >= 0) {
		col = (val & FGBG) >> FGBGSHIFT;
		set_cpair(inst, col);
	    }
	    wmove(stdscr, y, x);
	    if (val & 0x100) {
		switch (val & 0xFF) {
		    /*
		     * Use the ncurses codes for the VT100 line
		     * drawing characters where available. We can't
		     * do all of them: the control character
		     * representations such as HT and VT are not
		     * encoded by ncurses. We replace missing
		     * characters with ACS_BLOCK, on the grounds
		     * that they've got to be _something_.
		     */
		  case 0x5f:
		    ch = ' ';
		    break;
		  case 0x60:
		    ch = ACS_DIAMOND;
		    break;
		  case 0x61:
		    ch = ACS_CKBOARD;
		    break;
		  case 0x66:
		    ch = ACS_DEGREE;
		    break;
		  case 0x67:
		    ch = ACS_PLMINUS;
		    break;
		  case 0x6a:
		    ch = ACS_LRCORNER;
		    break;
		  case 0x6b:
		    ch = ACS_URCORNER;
		    break;
		  case 0x6c:
		    ch = ACS_ULCORNER;
		    break;
		  case 0x6d:
		    ch = ACS_LLCORNER;
		    break;
		  case 0x6e:
		    ch = ACS_PLUS;
		    break;
		  case 0x6f:
		    ch = ACS_S1;
		    break;
		  case 0x70:
		    ch = ACS_S3;
		    break;
		  case 0x71:
		    ch = ACS_HLINE;
		    break;
		  case 0x72:
		    ch = ACS_S7;
		    break;
		  case 0x73:
		    ch = ACS_S9;
		    break;
		  case 0x74:
		    ch = ACS_LTEE;
		    break;
		  case 0x75:
		    ch = ACS_RTEE;
		    break;
		  case 0x76:
		    ch = ACS_BTEE;
		    break;
		  case 0x77:
		    ch = ACS_TTEE;
		    break;
		  case 0x78:
		    ch = ACS_VLINE;
		    break;
		  case 0x79:
		    ch = ACS_LEQUAL;
		    break;
		  case 0x7a:
		    ch = ACS_GEQUAL;
		    break;
		  case 0x7b:
		    ch = ACS_PI;
		    break;
		  case 0x7c:
		    ch = ACS_NEQUAL;
		    break;
		  case 0x7d:
		    ch = ACS_STERLING;
		    break;
		  case 0x7e:
		    ch = ACS_BULLET;
		    break;
		  default:
		    ch = ACS_BLOCK;
		    break;
		}
	    } else {
		ch = val & 0xFF;
	    }
	    waddch(stdscr, ch);
	}

    /*
     * Draw the OSD and the numeric count, if any.
     */
    if (inst->number) {
	char buf[40];
	int len = sprintf(buf, " %d ", inst->number);
	wmove(stdscr, 1, inst->w - len - 1);
	wattrset(stdscr, A_NORMAL);
	wattron(stdscr, A_BOLD);
	set_cpair(inst, 0x47);	       /* white on blue */
	waddstr(stdscr, buf);
    }
    if (inst->osd) {
	char buf1[80], buf2[80], buf3[80], buf4[80];
	long long t;

	t = int_for_frame(inst, TIMETOP, f);
	t = (t << 32) + int_for_frame(inst, TIMEBOT, f);

	sprintf(buf2, "%s x %g", inst->logmod ? "LOG" : "", inst->speedmod);
	if (inst->logmod || inst->speedmod != 1.0)
	    sprintf(buf4, " Speed:%20s ", buf2);
	else
	    buf4[0] = '\0';
	sprintf(buf2, "%d / %d", f, inst->frames);
	sprintf(buf1, " Frame:%20s ", buf2);
	sprintf(buf2, " Time:%21.3f ", t / 1000000.0);
	sprintf(buf3, " Mode:%21s ",
		(inst->playing ? "PLAY" : "PAUSE"));

	wattrset(stdscr, A_NORMAL);
	wattron(stdscr, A_BOLD);
	set_cpair(inst, 0x47);	       /* white on blue */
	wmove(stdscr, 1, 1);
	waddstr(stdscr, buf1);
	wmove(stdscr, 2, 1);
	waddstr(stdscr, buf2);
	wmove(stdscr, 3, 1);
	waddstr(stdscr, buf3);
	wmove(stdscr, 4, 1);
	waddstr(stdscr, buf4);
    }

    /*
     * Position the cursor.
     */
    x = inst->screen[CURSOR];
    y = x / inst->w;
    x %= inst->w;
    wmove(stdscr, y, x);
}

long long time_after_frame(struct inst *inst, int f)
{
    unsigned long long t1, t2;

    if (f+1 >= inst->frames)
	return -1;

    t1 = int_for_frame(inst, TIMETOP, f);
    t1 = (t1 << 32) + int_for_frame(inst, TIMEBOT, f);

    t2 = int_for_frame(inst, TIMETOP, f+1);
    t2 = (t2 << 32) + int_for_frame(inst, TIMEBOT, f+1);

    return t2 - t1;
}

int main(int argc, char **argv)
{
    struct inst tinst, *inst = &tinst;
    char *pname;
    int i, totalsize;
    time_t start, end;
    int doing_opts;
    int iw, ih, startframe;
    int deftype = NODEFAULT;
    int prepareonly = FALSE;
    /* FILE *debugfp = fopen("/home/simon/.f", "w"); setvbuf(debugfp, NULL, _IONBF, 0); */

#ifdef PARRAY_TEST
    {
	struct parray *pa;
	int i, j, k;

	pa = parray_new();
	for (i = 0; i < 5*3*3*3; i++) {
	    parray_append(pa, i, i*i);

	    for (j = 0; j <= i; j++) {
		k = parray_retrieve(pa, j);
		if (k != j*j) {
		    printf("FAIL: i=%d j=%d wrong=%d right=%d\n",
			   i, j, k, j*j);
		}
	    }
	}

	exit(0);
    }
#endif

    pname = argv[0];

    do_defaults(NULL, &inst->cfg);
    strcpy(inst->cfg.line_codepage, "");   /* disable UCS */
    inst->cfg.utf8_override = FALSE;

    init_ucs(&inst->ucsdata, inst->cfg.line_codepage, FALSE, CS_NONE,
	     inst->cfg.vtmode);
    /*
     * Fix up ucsdata so that it encodes the VT100 line drawing
     * characters in the D9xx page, for simplicity of
     * implementation in do_text().
     */
    for (i = 0; i < 256; i++) {
	if (i >= 0x5F && i < 0x7F)
	    inst->ucsdata.unitab_xterm[i] = 0xD900 + i;
	else
	    inst->ucsdata.unitab_xterm[i] = inst->ucsdata.unitab_line[i];
    }

    inst->filenames = NULL;
    inst->nfiles = inst->filesize = 0;

    doing_opts = TRUE;
    iw = 80;
    ih = 24;
    while (--argc) {
	char *p = *++argv;
	if (doing_opts && *p == '-') {
	    char optbuf[3], *optstr, *optval;
	    int optchr;

	    /*
	     * Special case "--" inhibits further option
	     * processing.
	     */
	    if (!strcmp(p, "--")) {
		doing_opts = FALSE;
		continue;
	    }

	    /*
	     * All other "--" long options are translated into
	     * short ones.
	     */
	    if (p[1] == '-') {
		optval = strchr(p, '=');
		if (optval)
		    *optval++ = '\0';
		optstr = p;
		p += 2;
		if (!strcmp(p, "width"))
		    optchr = 'w';
		else if (!strcmp(p, "height"))
		    optchr = 'h';
		else if (!strcmp(p, "frame"))
		    optchr = 'f';
		else if (!strcmp(p, "ttyrec"))
		    optchr = 'T';
		else if (!strcmp(p, "prepare-only"))
		    optchr = 'P';
		else if (!strcmp(p, "nhrecorder") ||
			 !strcmp(p, "nh-recorder") ||
			 !strcmp(p, "nh_recorder") ||
			 !strcmp(p, "nhrecording") ||
			 !strcmp(p, "nh-recording") ||
			 !strcmp(p, "nh_recording"))
		    optchr = 'N';
		else
		    optchr = '\1';     /* definitely not defined */
	    } else {
		optbuf[0] = '-';
		optbuf[1] = optchr = p[1];
		optbuf[2] = '\0';
		optstr = optbuf;
		if (p[2])
		    optval = p+2;
		else
		    optval = NULL;
	    }

	    switch (optchr) {
	      case 'w':
	      case 'h':
	      case 'f':
		/*
		 * these options all require an argument
		 */
		if (!optval) {
		    if (--argc)
			optval = *++argv;
		    else {
			fprintf(stderr, "%s: option '%s' expects an"
				" argument\n", pname, optstr);
			return 1;
		    }
		}
		break;
	    }

	    switch (optchr) {
	      case 'w':
		assert(optval);
		iw = atoi(optval);
		if (iw <= 0) {
		    fprintf(stderr, "%s: argument to '%s' must be positive\n",
			    pname, optstr);
		    return 1;
		}
		break;
	      case 'h':
		assert(optval);
		ih = atoi(optval);
		if (ih <= 0) {
		    fprintf(stderr, "%s: argument to '%s' must be positive\n",
			    pname, optstr);
		    return 1;
		}
		break;
	      case 'f':
		assert(optval);
		startframe = atoi(optval);
		if (startframe < 0) {
		    fprintf(stderr, "%s: argument to '%s' must be"
			    " non-negative\n", pname, optstr);
		    return 1;
		}
		break;
	      case 'T':
		deftype = TTYREC;
		break;
	      case 'N':
		deftype = NHRECORDER;
		break;
	      case 'P':
		prepareonly = TRUE;
		break;
	      default:
		fprintf(stderr, "%s: unrecognised option '%s'\n",
			pname, optstr);
		return 1;
	    }
	} else {
	    if (inst->nfiles >= inst->filesize) {
		inst->filesize = inst->nfiles + 32;
		inst->filenames = sresize(inst->filenames, inst->filesize,
					  struct filename);
	    }
	    inst->filenames[inst->nfiles].name = dupstr(p);
	    inst->filenames[inst->nfiles].type = deftype;
	    inst->nfiles++;
	}
    }

    inst->w = iw;
    inst->h = ih;

    inst->screenlen = TOTAL;
    inst->screen = snewn(inst->screenlen, unsigned int);
    inst->oldscreen = snewn(inst->screenlen, unsigned int);
    for (i = 0; i < inst->screenlen; i++)
	inst->oldscreen[i] = 0xFFFFFFFF;

    inst->term = term_init(&inst->cfg, &inst->ucsdata, inst);
    term_size(inst->term, inst->h, inst->w, 0);

    inst->parrays = snewn(TOTAL, struct parray *);
    for (i = 0; i < TOTAL; i++)
	inst->parrays[i] = parray_new();
    inst->movietime = 0LL;
    inst->frames = 0;

    term_pwron(inst->term, TRUE);

    start = time(NULL);
    totalsize = 0;

    for (i = 0; i < inst->nfiles; i++) {
	char *p = inst->filenames[i].name;
	FILE *fp;
	unsigned char hdrbuf[12];
	unsigned char nhrbuf[4096];
	char *termdata = NULL;
	int termdatasize = 0, termdatalen, ret, nframes = 0;
	unsigned long long timestamp, oldtimestamp = 0LL;
	unsigned long long frametime, totaltime = 0LL;
	int typemask, type, nhrstate;
	long fileoff, filelen;

	fp = fopen(p, "rb");
	if (!fp) {
	    fprintf(stderr, "%s: unable to open '%s': %s\n",
		    pname, p, strerror(errno));
	    return 1;
	}

	if (deftype == NODEFAULT) {
	    /*
	     * First pass: try to identify the file type. We do
	     * this by looking through the entire file to see which
	     * formats it satisfies.
	     */
	    typemask = 0;
	    oldtimestamp = 0;
	    fseek(fp, 0, SEEK_END);
	    filelen = ftell(fp);
	    rewind(fp);
	    while (1) {
		/*
		 * Try to parse the file as a ttyrec.
		 */
		long offset, newoffset;

		ret = fread(hdrbuf, 1, 12, fp);
		if (ret == 0) {
		    typemask |= 1 << TTYREC;
		    break;
		} else if (ret != 12) {
		    break;
		}

		timestamp = GET_32BIT_LSB_FIRST(hdrbuf);
		timestamp = timestamp*1000000 + GET_32BIT_LSB_FIRST(hdrbuf+4);
		if (timestamp < oldtimestamp)
		    break;
		oldtimestamp = timestamp;

		termdatalen = GET_32BIT_LSB_FIRST(hdrbuf + 8);
		offset = ftell(fp);
		ret = fseek(fp, termdatalen, SEEK_CUR);
		if (ret < 0)
		    break;
		newoffset = ftell(fp);
		if (newoffset != offset + termdatalen ||
		    newoffset < 0 || newoffset > filelen)
		    break;
	    }
	    rewind(fp);

	    oldtimestamp = timestamp = 0;
	    nhrstate = 0;
	    while (1) {
		/*
		 * Try to parse the file as a nh-recording.
		 */
		int i;

		ret = fread(nhrbuf, 1, 4096, fp);
		if (ret == 0) {
		    if (nhrstate == 0 || nhrstate == 1)
			typemask |= 1 << NHRECORDER;
		    break;
		}
		for (i = 0; i < ret; i++) {
		    switch (nhrstate) {
		      case 0:
			if (nhrbuf[i] == 0) {
			    nhrstate = 1;
			    timestamp = 0;
			}
			break;
		      case 1:
			timestamp |= (unsigned char)nhrbuf[i];
			nhrstate = 2;
			break;
		      case 2:
			timestamp |= (unsigned char)nhrbuf[i] << 8;
			nhrstate = 3;
			break;
		      case 3:
			timestamp |= (unsigned char)nhrbuf[i] << 16;
			nhrstate = 4;
			break;
		      case 4:
			timestamp |= (unsigned char)nhrbuf[i] << 24;
			nhrstate = 0;
			if (oldtimestamp > timestamp)
			    goto done_nhr_loop;   /* goto as multi-level break */
			oldtimestamp = timestamp;
			break;
		    }
		}
	    } done_nhr_loop:
	    rewind(fp);

	    if (!typemask) {
		/*
		 * No file type matched.
		 */
		fprintf(stderr, "%s: '%s' is not a valid input file\n",
			pname, p);
		return 1;
	    } else {
		for (type = 0; type < NTYPES; type++)
		    if (typemask & (1 << type))
			break;
		assert(type < NTYPES);

		if (typemask & (typemask-1)) {   /* test for power of two */
		    /*
		     * More than one file type matched.
		     */
		    printf("%s matched more than one file type, assuming %s\n",
			   p, typenames[type]);
		}
	    }
	} else
	    type = deftype;

	term_pwron(inst->term, TRUE);

	printf("Reading %s (%s) ... ", p, typenames[type]);
	fflush(stdout);

	switch (type) {
	  case TTYREC:
	    while (1) {
		ret = fread(hdrbuf, 1, 12, fp);
		fileoff = ftell(fp);
		if (ret == 0) {
		    break;
		} else if (ret < 0) {
		    fprintf(stderr, "%s: error reading '%s': %s\n",
			    pname, p, strerror(errno));
		    return 1;
		} else if (ret < 12) {
		    fprintf(stderr, "%s: unexpected EOF reading '%s'\n",
			    pname, p);
		    return 1;
		}

		termdatalen = GET_32BIT_LSB_FIRST(hdrbuf + 8);
		if (termdatasize < termdatalen) {
		    termdatasize = termdatalen;
		    termdata = sresize(termdata, termdatasize, char);
		}

		ret = fread(termdata, 1, termdatalen, fp);
		if (ret == 0) {
		    break;
		} else if (ret < 0) {
		    fprintf(stderr, "%s: error reading '%s': %s\n",
			    pname, p, strerror(errno));
		    return 1;
		} else if (ret < termdatalen) {
		    fprintf(stderr, "%s: unexpected EOF reading '%s'\n",
			    pname, p);
		    return 1;
		}

		totalsize += 12 + termdatalen;

		timestamp = GET_32BIT_LSB_FIRST(hdrbuf);
		timestamp = timestamp*1000000 + GET_32BIT_LSB_FIRST(hdrbuf+4);
		if (oldtimestamp)
		    frametime = timestamp - oldtimestamp;
		else
		    frametime = (inst->movietime == 0 ? 0 : 1000000);
		oldtimestamp = timestamp;

		term_data(inst->term, FALSE, termdata, termdatalen);
		store_frame(inst, frametime, i, fileoff);

		nframes++;
		totaltime += frametime;
	    }
	    break;
	  case NHRECORDER:
	    fileoff = 0;
	    frametime = (inst->movietime == 0 ? 0 : 1000000);
	    oldtimestamp = 0;
	    while (1) {
		int i;
		long thisoff = ftell(fp);

		ret = fread(nhrbuf, 1, 4096, fp);
		if (ret == 0)
		    break;

		totalsize += ret;

		for (i = 0; i < ret; i++) {
		    switch (nhrstate) {
		      case 0:
			if (nhrbuf[i] == 0) {
			    nhrstate = 1;
			    timestamp = 0;
			} else {
			    term_data(inst->term, FALSE, nhrbuf+i, 1);
			}
			break;
		      case 1:
			timestamp |= (unsigned char)nhrbuf[i];
			nhrstate = 2;
			break;
		      case 2:
			timestamp |= (unsigned char)nhrbuf[i] << 8;
			nhrstate = 3;
			break;
		      case 3:
			timestamp |= (unsigned char)nhrbuf[i] << 16;
			nhrstate = 4;
			break;
		      case 4:
			timestamp |= (unsigned char)nhrbuf[i] << 24;
			nhrstate = 0;

			store_frame(inst, frametime, i, fileoff);
			nframes++;
			totaltime += frametime;

			frametime = (timestamp - oldtimestamp) * 10000;
			oldtimestamp = timestamp;

			fileoff = thisoff + i + 1;
			break;
		    }
		}
	    }
	    break;
	}

	sfree(termdata);

	printf("%d frames\n", nframes);

	fclose(fp);
    }

    if (!inst->frames) {
	fprintf(stderr, "usage: %s <file> [<file...]\n", pname);
	return 0;
    }

    end = time(NULL);

    printf("Total %d frames" /* FIXME: ", %d bytes of memory used" */ "\n",
	   inst->frames);
    printf("Total loading time: %d seconds (%.3g sec/Mb)\n",
	   (int)difftime(end, start),
	   difftime(end, start) * 1048576 / totalsize);

    if (prepareonly) {
	printf("Not starting player due to -P option.\n");
	return 0;
    }

    {
	int f = startframe, fb = -1;
	long long t = -1;
	long long tsince = 0;
	int changed = TRUE;

	inst->number = 0;
	inst->osd = FALSE;
	inst->playing = FALSE;
	inst->logmod = FALSE;
	inst->speedmod = 1.0;

	start_player(inst);
	while (1) {
	    int c;

	    if (f < 0)
		f = 0;
	    if (f >= inst->frames)
		f = inst->frames - 1;

	    display_frame(inst, f);
	    if (changed) {
		changed = FALSE;
		if (inst->playing) {
		    t = time_after_frame(inst, f);
		    if (inst->logmod) {
			/*
			 * Logarithmic time compression: we replace
			 * a time t seconds with log(1+t) seconds.
			 * This starts off with gradient 1 at t=0,
			 * so that short times still work normally;
			 * but times compress gradually as you go
			 * up the scale, so that the person you're
			 * watching doesn't tediously stop and
			 * think all the time.
			 */
			t = 1000000 * log(1.0 + t / 1000000.0);
		    }
		    t /= inst->speedmod;
		} else
		    t = -1;
	    }

	    if (t >= 0) {
		struct timeval tv;
		fd_set r;
		int ret;
		long long tused;

		FD_ZERO(&r);
		FD_SET(0, &r);
		tv.tv_sec = t / 1000000;
		tv.tv_usec = t % 1000000;
		tused = t;

		wrefresh(stdscr);

		ret = select(1, &r, NULL, NULL, &tv);
		if (ret == 0) {
		    c = -1;
		    t = -1;
		} else {
		    c = getch();
		    t = tv.tv_sec;
		    t = t * 1000000 + tv.tv_usec;
		    tused -= t;
		}
		tsince += tused;
		if (tsince > 500000)
		    fb = -1;
	    } else {
		c = getch();
		fb = -1;
	    }

	    if (c == 'q' || c == 'Q' || c == '\003')
		break;

	    if (c == 'b' || c == '<') {
		/*
		 * When moving backwards, we move relative to the
		 * last frame we moved backwards _to_ rather than
		 * the current frame, provided it's been only a
		 * short time since the last press of 'b'. This
		 * enables the user to hold down 'b' to move
		 * backwards in playing mode, without a very short
		 * frame interval acting as a barrier.
		 */
		if (fb >= 0)
		    f = fb;
		f -= (inst->number ? inst->number : 1);
		inst->number = 0;
		tsince = 0;
		fb = f;
		changed = TRUE;
	    } else if (c >= '0' && c <= '9') {
		inst->number = inst->number * 10 + (c - '0');
	    } else if (c == 'o' || c == 'O') {
		inst->osd = !inst->osd;
		inst->number = 0;
	    } else if (c == 'l' || c == 'L') {
		inst->logmod = !inst->logmod;
		inst->number = 0;
	    } else if (c == 'x') {
		t *= inst->speedmod;
		inst->speedmod = (inst->number ? inst->number : 1);
		t /= inst->speedmod;
		inst->number = 0;
	    } else if (c == 'X') {
		t *= inst->speedmod;
		inst->speedmod = 1.0 / (inst->number ? inst->number : 1);
		t /= inst->speedmod;
		inst->number = 0;
	    } else if (c == 'g') {
		f = inst->number;
		inst->number = 0;
		changed = TRUE;
	    } else if (c == 'G') {
		f = inst->frames - 1 - inst->number;
		inst->number = 0;
		changed = TRUE;
	    } else if (c == ' ' || c == '>') {
		f += (inst->number ? inst->number : 1);
		inst->number = 0;
		changed = TRUE;
	    } else if (c == -1 && inst->playing) {
		f++;
		changed = TRUE;
	    } else if (c == 'p' || c == 'P' || c == 's' || c == 'S') {
		inst->playing = !inst->playing;
		if (inst->playing && f+1 < inst->frames)
		    f++;
		inst->number = 0;
		changed = TRUE;
	    }
	}
	end_player(inst);
	printf("\nPlayback finished.\nLast frame reached was %d\n", f);
    }

    return 0;
}
