/*
 * TODO:
 * 
 *  - cmdline: specify terminal size?
 *  - cmdline: control frame time between ttyrec files?
 *  - cmdline: cap maximum frame time?
 *  - player: h for a help screen would be nice
 */

#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <errno.h>
#include <assert.h>
#include <math.h>

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
void write_clip(void *frontend, wchar_t * data, int len, int must_deselect) {}
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

struct movie {
    int frame;
    int index;
    int data;
};

struct inst {
    Terminal *term;
    struct unicode_data ucsdata;
    Config cfg;
    int *screen, *oldscreen, w, h, screenlen;
    struct movie *movie;
    int movielen, moviesize;
    int frames;
    unsigned long long movietime;

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
 * _movie_ data structure will be a sequence of records of the form
 * (frame number, which integer changed, what did it change to). We
 * initially build this structure up in order of frame (because
 * we're reading through the file from the start); then we sort it
 * so that it's indexed primarily by integer, which means that we
 * can determine the value of integer I at frame F by a binary
 * search of the array. This enables us to play back and forth
 * through the entire movie with arbitrary rewind. Hence the need
 * to have the entire terminal state encoded as an unstructured
 * list of integers: if I had to give separate treatment to the
 * cursor position and any other future enhancements such as line
 * attributes, it would all get more complicated.
 */

#define CURSOR (inst->w * inst->h)
#define TIMETOP (inst->w * inst->h + 1)
#define TIMEBOT (inst->w * inst->h + 2)
#define TOTAL (inst->w * inst->h + 3)

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

void store_frame(struct inst *inst, unsigned long long delay)
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

    if (inst->movielen + n > inst->moviesize) {
	inst->moviesize = (inst->movielen + n) * 5 / 4;
	inst->movie = sresize(inst->movie, inst->moviesize, struct movie);
    }

     for (i = 0; i < inst->screenlen; i++) {
	if (inst->screen[i] != inst->oldscreen[i]) {
	    inst->movie[inst->movielen].frame = inst->frames - 1;
	    inst->movie[inst->movielen].index = i;
	    inst->movie[inst->movielen].data = inst->screen[i];
	    inst->movielen++;
	    inst->oldscreen[i] = inst->screen[i];
	}
    }
}

int moviecmp(const void *av, const void *bv)
{
    const struct movie *a = (const struct movie *)av;
    const struct movie *b = (const struct movie *)bv;

    if (a->index < b->index)
	return -1;
    else if (a->index > b->index)
	return +1;

    if (a->frame < b->frame)
	return -1;
    else if (a->frame > b->frame)
	return +1;

    return 0;
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
    if (!curses_active)
	return;
    endwin();
    curses_active = FALSE;
}

unsigned int_for_frame(struct inst *inst, int i, int f)
{
    int bot, top, mid, cmp;
    struct movie mtmp;

    /*
     * Binary search to find the movie record which set integer
     * i, as late as possible before or equal to frame f.
     */
    mtmp.frame = f;
    mtmp.index = i;

    bot = -1;
    top = inst->movielen;

    while (top - bot > 1) {
	mid = (bot + top) / 2;
	cmp = moviecmp(inst->movie + mid, &mtmp);
	if (cmp < 0)
	    bot = mid;
	else if (cmp > 0)
	    top = mid;
	else {
	    bot = mid;
	    break;		       /* found it exactly! */
	}
    }

    /*
     * Now bot is the frame number we want. It could in theory
     * still be -1, but only if we were searching for an
     * integer less than zero, or integer zero at a frame less
     * than zero.
     */
    assert(bot >= 0 && bot < inst->movielen);
    assert(inst->movie[bot].index == i);
    assert(inst->movie[bot].frame <= f);
    return inst->movie[bot].data;
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
    int i;
    /* FILE *debugfp = fopen("/home/simon/.f", "w"); setvbuf(debugfp, NULL, _IONBF, 0); */

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

    inst->w = 80;
    inst->h = 24;

    inst->screenlen = TOTAL;
    inst->screen = snewn(inst->screenlen, unsigned int);
    inst->oldscreen = snewn(inst->screenlen, unsigned int);
    for (i = 0; i < inst->screenlen; i++)
	inst->oldscreen[i] = 0xFFFFFFFF;

    inst->term = term_init(&inst->cfg, &inst->ucsdata, inst);
    term_size(inst->term, inst->h, inst->w, 0);

    inst->movie = NULL;
    inst->movielen = inst->moviesize = 0;
    inst->movietime = 0LL;
    inst->frames = 0;

    term_pwron(inst->term);

    /*
     * Read the ttyrec file(s) supplied on input.
     */
    pname = argv[0];
    while (--argc) {
	char *p = *++argv;

	if (*p == '-') {
	    fprintf(stderr, "%s: unrecognised command-line option '%s'\n",
		    pname, p);
	    return 1;
	} else {
	    FILE *fp;
	    unsigned char hdrbuf[12];
	    char *termdata = NULL;
	    int termdatasize = 0, termdatalen, ret, nframes = 0;
	    unsigned long long timestamp, oldtimestamp = 0LL;
	    unsigned long long frametime, totaltime = 0LL;

	    fp = fopen(p, "rb");
	    if (!fp) {
		fprintf(stderr, "%s: unable to open '%s': %s\n",
			pname, p, strerror(errno));
		return 1;
	    }

	    term_pwron(inst->term);

	    printf("Reading %s...", p);
	    fflush(stdout);

	    while (1) {
		ret = fread(hdrbuf, 1, 12, fp);
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

		timestamp = GET_32BIT_LSB_FIRST(hdrbuf);
		timestamp = timestamp*1000000 + GET_32BIT_LSB_FIRST(hdrbuf+4);
		if (oldtimestamp)
		    frametime = timestamp - oldtimestamp;
		else
		    frametime = (inst->movietime == 0 ? 0 : 1000000);
		oldtimestamp = timestamp;

		term_data(inst->term, FALSE, termdata, termdatalen);
		store_frame(inst, frametime);

		nframes++;
		totaltime += frametime;
	    }

	    sfree(termdata);

	    printf("%d frames, %g seconds\n", nframes, totaltime / 1000000.0);

	    fclose(fp);
	}
    }

    if (!inst->frames) {
	fprintf(stderr, "usage: %s <ttyrec> [<ttyrec...]\n", pname);
	return 0;
    }

    printf("Total %d frames, %g seconds, %d bytes of memory used\n",
	   inst->frames, inst->movietime / 1000000.0,
	   inst->movielen * sizeof(struct movie));

    qsort(inst->movie, inst->movielen, sizeof(struct movie), moviecmp);

    printf("Sorted and ready to go.\n");

    {
	int f = 0, fb = -1;
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

	    if (c == 'b') {
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
	    } else if (c == 'o') {
		inst->osd = !inst->osd;
		inst->number = 0;
	    } else if (c == 'L') {
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
	    } else if (c == ' ') {
		f += (inst->number ? inst->number : 1);
		inst->number = 0;
		changed = TRUE;
	    } else if (c == -1 && inst->playing) {
		f++;
		changed = TRUE;
	    } else if (c == 'p' || c == 's') {
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
