/*
 * stubs.c: ipbt empty implementations of putty stuff
 * In a separate file to confine the warnings
 */

#include "putty.h"
#include "unix.h"

/*
 * We're only _replaying_ terminal data, so we never need to send
 * back to a real application. Thus, ldisc_send as called from
 * terminal.c is a stub function.
 */
void ldisc_send(void *handle, char *buf, int len, int interactive)
{
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

void free_ctx(Context ctx)
{
}
