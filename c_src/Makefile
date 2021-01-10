#@BEGIN-C_SRC-DEFAULT-RULES@
ERL=$(shell readlink -f `which erl`)
ERL_TOP=$(ERL:%/bin/erl=%)
OS=$(shell uname -s)
CFLAGS=-MMD -MP -MF .$<.d -I$(ERL_TOP)/usr/include
#@END-C_SRC-DEFAULT-RULES@

APP=udev
PRIVDIR=../priv
CFLAGS+=-std=c99 -Wall -O3
LDFLAGS+=-shared

OBJS = $(APP)_nif.o
LIBS = -ludev

all: $(PRIVDIR)/$(APP)_nif.so

$(PRIVDIR)/$(APP)_nif.so: $(OBJS)
	$(CC) $(LDFLAGS) $(OBJS) $(LIBS) -o $(PRIVDIR)/$(APP)_nif.so

%.o:	%.c
	$(CC) $(CFLAGS) -c -fPIC $<

.%.d:	;

-include .*.d
