.c.o:
	$(CC) $(CFLAGS) $(XFLAGS) -c $*.c

IPBT = be_none.o fromucs.o ipbt.o ldiscucs.o localenc.o macenc.o mimeenc.o \
       minibidi.o misc.o noprint.o notiming.o sbcs.o sbcsdat.o settings.o \
       slookup.o terminal.o time.o toucs.o tree234.o utf8.o uxmisc.o uxucs.o \
       wcwidth.o xenc.o

ipbt: $(IPBT)
	$(CC) $(LFLAGS) -o ipbt $(IPBT) -lm -lncursesw

be_none.o: be_none.c putty.h puttyps.h unix.h charset.h network.h misc.h \
  puttymem.h
fromucs.o: fromucs.c charset.h internal.h
ipbt.o: ipbt.c putty.h puttyps.h unix.h charset.h network.h misc.h \
  puttymem.h terminal.h tree234.h
ldiscucs.o: ldiscucs.c putty.h puttyps.h unix.h charset.h network.h \
  misc.h puttymem.h terminal.h tree234.h ldisc.h
localenc.o: localenc.c charset.h internal.h
macenc.o: macenc.c charset.h internal.h
mimeenc.o: mimeenc.c charset.h internal.h
minibidi.o: minibidi.c misc.h puttymem.h
misc.o: misc.c putty.h puttyps.h unix.h charset.h network.h misc.h \
  puttymem.h
noprint.o: noprint.c putty.h puttyps.h unix.h charset.h network.h misc.h \
  puttymem.h
notiming.o: notiming.c putty.h puttyps.h unix.h charset.h network.h \
  misc.h puttymem.h
sbcs.o: sbcs.c charset.h internal.h
sbcsdat.o: sbcsdat.c charset.h internal.h
settings.o: settings.c putty.h puttyps.h unix.h charset.h network.h \
  misc.h puttymem.h storage.h
slookup.o: slookup.c charset.h internal.h enum.c sbcsdat.c utf8.c
terminal.o: terminal.c putty.h puttyps.h unix.h charset.h network.h \
  misc.h puttymem.h terminal.h tree234.h
time.o: time.c
toucs.o: toucs.c charset.h internal.h
tree234.o: tree234.c puttymem.h tree234.h
utf8.o: utf8.c charset.h internal.h
uxmisc.o: uxmisc.c putty.h puttyps.h unix.h charset.h network.h misc.h \
  puttymem.h
uxucs.o: uxucs.c putty.h puttyps.h unix.h charset.h network.h misc.h \
  puttymem.h terminal.h tree234.h
wcwidth.o: wcwidth.c putty.h puttyps.h unix.h charset.h network.h misc.h \
  puttymem.h
xenc.o: xenc.c charset.h internal.h

sbcsdat.c: sbcsgen.pl sbcs.dat
	perl sbcsgen.pl

ipbt.1 ipbt.html: ipbt.but
	halibut --man=ipbt.1 --html=ipbt.html ipbt.but

clean:
	rm -f *.o ipbt sbcsdat.c
	rm -f *.da *.bbg *.bb *.gcov gmon.out
	rm -f ipbt.1
