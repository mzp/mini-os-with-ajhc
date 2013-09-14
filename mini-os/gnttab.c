/*
 ****************************************************************************
 * (C) 2006 - Cambridge University
 ****************************************************************************
 *
 *        File: gnttab.c
 *      Author: Steven Smith (sos22@cam.ac.uk)
 *     Changes: Grzegorz Milos (gm281@cam.ac.uk)
 *
 *        Date: July 2006
 *
 * Environment: Xen Minimal OS
 * Description: Simple grant tables implementation. About as stupid as it's
 *  possible to be and still work.
 *
 ****************************************************************************
 */
#include <mini-os/os.h>
#include <mini-os/mm.h>
#include <mini-os/gnttab.h>
#include <mini-os/semaphore.h>

#define NR_RESERVED_ENTRIES 8

/* NR_GRANT_FRAMES must be less than or equal to that configured in Xen */
#ifdef __ia64__
#define NR_GRANT_FRAMES 1
#else
#define NR_GRANT_FRAMES 4
#endif
#define NR_GRANT_ENTRIES (NR_GRANT_FRAMES * PAGE_SIZE / sizeof(grant_entry_t))

static grant_entry_t *gnttab_table;
static grant_ref_t gnttab_list[NR_GRANT_ENTRIES];
#ifdef GNT_DEBUG
static char inuse[NR_GRANT_ENTRIES];
#endif
static __DECLARE_SEMAPHORE_GENERIC(gnttab_sem, 0);

void put_free_entry(grant_ref_t ref);
void abort(void);
static grant_ref_t
get_free_entry(void)
{
    unsigned int ref;
    unsigned long flags;
    printk("get_free_entry[%d]\n", gnttab_sem.count);
    down(&gnttab_sem);
    printk("down\n");
    local_irq_save(flags);
    ref = gnttab_list[0];
    printk("bug\n");
    BUG_ON(ref < NR_RESERVED_ENTRIES || ref >= NR_GRANT_ENTRIES);
    if(ref < NR_RESERVED_ENTRIES || ref >= NR_GRANT_ENTRIES){
      abort();
      }
    printk("buged\n");
    gnttab_list[0] = gnttab_list[ref];
#ifdef GNT_DEBUG
    BUG_ON(inuse[ref]);
    inuse[ref] = 1;
#endif
    local_irq_restore(flags);
    return ref;
}

grant_ref_t
gnttab_grant_access(domid_t domid, unsigned long frame, int readonly)
{
    grant_ref_t ref;

    ref = get_free_entry();
    gnttab_table[ref].frame = frame;
    gnttab_table[ref].domid = domid;
    wmb();
    readonly *= GTF_readonly;
    gnttab_table[ref].flags = GTF_permit_access | readonly;

    return ref;
}

grant_ref_t
gnttab_grant_transfer(domid_t domid, unsigned long pfn)
{
    grant_ref_t ref;

    ref = get_free_entry();
    gnttab_table[ref].frame = pfn;
    gnttab_table[ref].domid = domid;
    wmb();
    gnttab_table[ref].flags = GTF_accept_transfer;

    return ref;
}

int
gnttab_end_access(grant_ref_t ref)
{
    uint16_t flags, nflags;

    BUG_ON(ref >= NR_GRANT_ENTRIES || ref < NR_RESERVED_ENTRIES);

    nflags = gnttab_table[ref].flags;
    do {
        if ((flags = nflags) & (GTF_reading|GTF_writing)) {
            printk("WARNING: g.e. still in use! (%x)\n", flags);
            return 0;
        }
    } while ((nflags = synch_cmpxchg(&gnttab_table[ref].flags, flags, 0)) !=
            flags);

    put_free_entry(ref);
    return 1;
}

unsigned long
gnttab_end_transfer(grant_ref_t ref)
{
    unsigned long frame;
    uint16_t flags;

    BUG_ON(ref >= NR_GRANT_ENTRIES || ref < NR_RESERVED_ENTRIES);

    while (!((flags = gnttab_table[ref].flags) & GTF_transfer_committed)) {
        if (synch_cmpxchg(&gnttab_table[ref].flags, flags, 0) == flags) {
            printk("Release unused transfer grant.\n");
            put_free_entry(ref);
            return 0;
        }
    }

    /* If a transfer is in progress then wait until it is completed. */
    while (!(flags & GTF_transfer_completed)) {
        flags = gnttab_table[ref].flags;
    }

    /* Read the frame number /after/ reading completion status. */
    rmb();
    frame = gnttab_table[ref].frame;

    put_free_entry(ref);

    return frame;
}

grant_ref_t
gnttab_alloc_and_grant(void **map)
{
    unsigned long mfn;
    grant_ref_t gref;

    *map = (void *)alloc_page();
    mfn = virt_to_mfn(*map);
    gref = gnttab_grant_access(0, mfn, 0);
    return gref;
}

static const char * const gnttabop_error_msgs[] = GNTTABOP_error_msgs;

const char *
gnttabop_error(int16_t status)
{
    status = -status;
    if (status < 0 || status >= ARRAY_SIZE(gnttabop_error_msgs))
	return "bad status";
    else
        return gnttabop_error_msgs[status];
}

void
fini_gnttab(void)
{
    struct gnttab_setup_table setup;

    setup.dom = DOMID_SELF;
    setup.nr_frames = 0;

    HYPERVISOR_grant_table_op(GNTTABOP_setup_table, &setup, 1);
}

// -------------
void abort(void);
void hs_set_xen_guest_handle(struct gnttab_setup_table* ptr, unsigned long* frames) {
  set_xen_guest_handle(ptr->frame_list, frames);
}
void hs_set_gnttab_table(grant_entry_t* p) {
  gnttab_table = p;
}

unsigned long hs_local_irq_save(unsigned long flags) {
  local_irq_save(flags);
  return flags;
}

unsigned long hs_local_irq_restore(unsigned long flags) {
  local_irq_restore(flags);
  return flags;
}

grant_ref_t* hs_get_gnttab_list(void) {
  return gnttab_list;
}
void* hs_get_gnttab_sem(void) {
//      printk("%d - %d\n", NR_RESERVED_ENTRIES, NR_GRANT_ENTRIES);
 // printk("[%d]\n", gnttab_sem.count);
  return &gnttab_sem;
}
#include <xmalloc.h>
#include "../stub/stub/gnttab_c_stub.h"
