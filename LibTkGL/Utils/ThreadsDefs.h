

#ifndef _ThreadsDefs_h
#define _ThreadsDefs_h

#include <semaphore.h>
#include <pthread.h>
#include <sched.h>
#include <limits.h>
#include <unistd.h>
#include <stdio.h>
#include <errno.h>

/*#define _THREAD_DEBUG_
*/

void Thread_SingleWait(sem_t *Sem,int Count);
void Thread_SingleSend(sem_t *Sem);
void Thread_SemaphoreIgnore(sem_t *Sem);

#endif
