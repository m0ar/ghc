/*
 * (c)2014 Tweag I/O
 */

#include "PosixSource.h"
#include "Rts.h"
#include "Hash.h"

/*
#include <stdio.h>
#include <ctype.h>
#include <string.h>
#include <assert.h>
*/

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

#ifdef HAVE_SYS_STAT_H
#include <sys/stat.h>
#endif

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif


static HashTable * spt = NULL;

void
hs_spt_module(void *spe[])
{
  if (spt == NULL) {
    spt = allocStrHashTable();
  }

  size_t i;
  for (i=0; spe[i]; i+=2) {
	printf("save %s: %p\n", spe[i], spe[i+1]);
    getStablePtr(spe[i+1]);
    insertHashTable(spt, (StgWord)spe[i], spe[i+1]);
  }
}

StgPtr hs_spt_lookup(char* key) {
    StgPtr res = lookupHashTable(spt, (StgWord)key);
	printf("lookup %s: %p\n", key, res);
	return res;
}