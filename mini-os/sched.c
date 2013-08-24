/* 
 ****************************************************************************
 * (C) 2005 - Grzegorz Milos - Intel Research Cambridge
 ****************************************************************************
 *
 *        File: sched.c
 *      Author: Grzegorz Milos
 *     Changes: Robert Kaiser
 *              
 *        Date: Aug 2005
 * 
 * Environment: Xen Minimal OS
 * Description: simple scheduler for Mini-Os
 *
 * The scheduler is non-preemptive (cooperative), and schedules according 
 * to Round Robin algorithm.
 *
 ****************************************************************************
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to
 * deal in the Software without restriction, including without limitation the
 * rights to use, copy, modify, merge, publish, distribute, sublicense, and/or
 * sell copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 * 
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 * 
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR 
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, 
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE 
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER 
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING 
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER 
 * DEALINGS IN THE SOFTWARE.
 */

#include <mini-os/os.h>
#include <mini-os/hypervisor.h>
#include <mini-os/time.h>
#include <mini-os/mm.h>
#include <mini-os/types.h>
#include <mini-os/lib.h>
#include <mini-os/xmalloc.h>
#include <mini-os/list.h>
#include <mini-os/sched.h>
#include <mini-os/semaphore.h>


#ifdef SCHED_DEBUG
#define DEBUG(_f, _a...) \
    printk("MINI_OS(file=sched.c, line=%d) " _f "\n", __LINE__, ## _a)
#else
#define DEBUG(_f, _a...)    ((void)0)
#endif

struct thread *idle_thread = NULL;
MINIOS_LIST_HEAD(exited_threads);
static int threads_started;

struct thread *main_thread;

void inline print_runqueue(void)
{
    struct minios_list_head *it;
    struct thread *th;
    minios_list_for_each(it, &idle_thread->thread_list)
    {
        th = minios_list_entry(it, struct thread, thread_list);
        printk("   Thread \"%s\", runnable=%d\n", th->name, is_runnable(th));
    }
    printk("\n");
}
void hs_dump(struct thread* p) {
  printk("dump Thread \"%s\", runnable=%d prev=%p next=%p\n", p->name, is_runnable(p), p->thread_list.prev, p->thread_list.next);
}
void schedule(void);

struct thread* hs_create_thread(char *name, void (*function)(void *), void *data);
struct thread* create_thread(char *name, void (*function)(void *), void *data)
{
  return hs_create_thread(name, function, data);
}

#ifdef HAVE_LIBC
static struct _reent callback_reent;
struct _reent *__getreent(void)
{
    struct _reent *_reent;

    if (!threads_started)
	_reent = _impure_ptr;
    else if (in_callback)
	_reent = &callback_reent;
    else
	_reent = &get_current()->reent;

#ifndef NDEBUG
#if defined(__x86_64__) || defined(__x86__)
    {
#ifdef __x86_64__
	register unsigned long sp asm ("rsp");
#else
	register unsigned long sp asm ("esp");
#endif
	if ((sp & (STACK_SIZE-1)) < STACK_SIZE / 16) {
	    static int overflowing;
	    if (!overflowing) {
		overflowing = 1;
		printk("stack overflow\n");
		BUG();
	    }
	}
    }
#endif
#endif
    return _reent;
}
#endif

void exit_thread(void)
{
    unsigned long flags;
    struct thread *thread = current;
    printk("Thread \"%s\" exited.\n", thread->name);
    local_irq_save(flags);
    /* Remove from the thread list */
    minios_list_del(&thread->thread_list);
    clear_runnable(thread);
    /* Put onto exited list */
    minios_list_add(&thread->thread_list, &exited_threads);
    local_irq_restore(flags);
    /* Schedule will free the resources */
    while(1)
    {
        schedule();
        printk("schedule() returned!  Trying again\n");
    }
}

void hs_block(struct thread *thread);
void block(struct thread *thread) {
  hs_block(thread);
}

void msleep(uint32_t millisecs)
{
    struct thread *thread = get_current();
    thread->wakeup_time = NOW()  + MILLISECS(millisecs);
    clear_runnable(thread);
    schedule();
}

void wake(struct thread *thread)
{
    thread->wakeup_time = 0LL;
    set_runnable(thread);
}

void idle_thread_fn(void *unused)
{
    threads_started = 1;
    while (1) {
        block(current);
        schedule();
    }
}

DECLARE_MUTEX(mutex);

void th_f1(void *data)
{
    struct timeval tv1, tv2;

    for(;;)
    {
        down(&mutex);
        printk("Thread \"%s\" got semaphore, runnable %d\n", current->name, is_runnable(current));
        schedule();
        printk("Thread \"%s\" releases the semaphore\n", current->name);
        up(&mutex);
        
        
        gettimeofday(&tv1, NULL);
        for(;;)
        {
            gettimeofday(&tv2, NULL);
            if(tv2.tv_sec - tv1.tv_sec > 2) break;
        }
                
        
        schedule(); 
    }
}

void th_f2(void *data)
{
    for(;;)
    {
        printk("Thread OTHER executing, data 0x%lx\n", data);
        schedule();
    }
}

// ------
void hs_set_idle_thread(struct thread* p) {
  idle_thread = p;
}
void hs_minios_init_list_head(struct minios_list_head* p) {
  MINIOS_INIT_LIST_HEAD(p);
}
struct minios_list_head* hs_list_next(struct minios_list_head* p) {
  return p->next;
}
struct minios_list_head* hs_get_thread_list(struct thread* p) {
  return &p->thread_list;
}
struct thread* hs_get_idle_thread(void) {

  return idle_thread;
}
void hs_set_runnable(struct thread* p) {
  set_runnable(p);
}
void hs_set_threads_started(int n) {
  threads_started = n;
}
struct thread* hs_get_entry(struct minios_list_head* p) {
  return minios_list_entry(p, struct thread, thread_list);
}

void abort(void);
void hs_switch_threads(struct thread* prev, struct thread* next) {
  switch_threads(prev, next);
}
struct minios_list_head* hs_exited_threads(void) {
  return &exited_threads;
}
void hs_bug(void) {
  abort();
  BUG();
}

int hs_get_in_callback(void) {
  return in_callback;
}

s_time_t hs_now(void) {
  printk("RAW_NOW:[[%ld]]\n", (s_time_t)NOW());
  return NOW();
}
#include "../stub/stub/sched_c_stub.h"
