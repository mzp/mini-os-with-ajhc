HASKELL_SRC = Main.hs Time.hs Util.hs Xen.hs Struct.hs Console.hs XenconsRing.hs Gnttab.hs Sched.hs List.hs Xenbus.hs Fbfront.hs
STUB = TimeShadowInfoStub.hs XenconsoleStub.hs GnttabStub.hs SchedStub.hs XsWireStub.hs XenbusStub.hs FbfrontStub.hs

all: libjhcrts.a

# ---------------------
#  Haskell to C
# ---------------------
jhc_custom/main_code.c: $(HASKELL_SRC) $(STUB)
	ajhc -funboxed-values -fffi -fcpp -C --tdir rts -o $@ $^

$(STUB) : $(addprefix ../stub/stub/, $(STUB))
	cp -v $^ .

# ----------------------
#  C to static library
# ----------------------
JHCRTS_SRC = jhc_custom
vpath %.c $(JHCRTS_SRC)/rts
vpath %.c $(JHCRTS_SRC)
SRCS = gc_jgc.c jhc_rts.c stableptr.c rts_support.c main_code.c stub.c conc.c
OBJS = $(SRCS:.c=.o)

CFLAGS = -fno-builtin -g -DGNT_DEBUG -DGNTMAP_DEBUG -D__INSIDE_MINIOS__ -m64 -mno-red-zone -fno-reorder-blocks -fno-asynchronous-unwind-tables -isystem ../mini-os/include -D__MINIOS__ -D__XEN_INTERFACE_VERSION__=0x00030205  -isystem ../mini-os/include/x86 -isystem ../mini-os/include/x86/x86_64
CFLAGS += -I$(JHCRTS_SRC) -std=gnu99 -D_GNU_SOURCE
CFLAGS += -I../stub/stub
CFLAGS += -falign-functions=4 -ffast-math -fno-strict-aliasing
CFLAGS += -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_STANDALONE=0
CFLAGS += -D_JHC_JGC_STACKGROW=128 -D_JHC_JGC_LIMITED_NUM_MEGABLOCK=32
CFLAGS += -D_JHC_JGC_BLOCK_SHIFT=9 -D_JHC_JGC_MEGABLOCK_SHIFT=14
CFLAGS += -D_JHC_JGC_GC_STACK_SHIFT=8 -D_JHC_JGC_LIMITED_NUM_GC_STACK=32
CFLAGS += -D_JHC_JGC_NAIVEGC

libjhcrts.a: $(OBJS)
	    $(AR) -r $@ $(OBJS)

%.o : %.c
	    $(CC) $(CFLAGS) -c -o $@ $^

# -----------------------------
.PHONY: clean
clean:
	rm -rf rts jhc_custom/main_code.c *.o

