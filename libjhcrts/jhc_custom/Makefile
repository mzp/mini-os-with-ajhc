JHCRTS_SRC = .
vpath %.c $(JHCRTS_SRC)/rts
SRCS = gc_jgc.c jhc_rts.c stableptr.c rts_support.c conc.c
OBJS = $(SRCS:.c=.o)

CFLAGS = -fno-builtin -g -DGNT_DEBUG -DGNTMAP_DEBUG -D__INSIDE_MINIOS__ -m64 -mno-red-zone -fno-reorder-blocks -fno-asynchronous-unwind-tables -isystem ../mini-os/include -D__MINIOS__ -D__XEN_INTERFACE_VERSION__=0x00030205  -isystem ../mini-os/include/x86 -isystem ../mini-os/include/x86/x86_64
CFLAGS += -I$(JHCRTS_SRC) -std=gnu99
CFLAGS += -DNDEBUG -D_JHC_GC=_JHC_GC_JGC -D_JHC_STANDALONE=0
CFLAGS += -D_JHC_ARM_STAY_IN_THUMB_MODE
CFLAGS += -D_JHC_JGC_STACKGROW=128 -D_JHC_JGC_LIMITED_NUM_MEGABLOCK=1
CFLAGS += -D_JHC_JGC_BLOCK_SHIFT=9 -D_JHC_JGC_MEGABLOCK_SHIFT=14
CFLAGS += -D_JHC_JGC_GC_STACK_SHIFT=8 -D_JHC_JGC_LIMITED_NUM_GC_STACK=1
CFLAGS += -D_JHC_JGC_NAIVEGC

all: libjhcrts.a

libjhcrts.a: $(OBJS)
	    $(AR) -r $@ $(OBJS)

%.o : %.c
	    $(CC) $(CFLAGS) -c -o $@ $^
