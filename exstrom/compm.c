#include <math.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 *                            COPYRIGHT
 *
 *  compm.c
 *  Copyright (C) 2014 Exstrom Laboratories LLC
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  A copy of the GNU General Public License is available on the internet at:
 *  http://www.gnu.org/copyleft/gpl.html
 *
 *  or you can write to:
 *
 *  The Free Software Foundation, Inc.
 *  675 Mass Ave
 *  Cambridge, MA 02139, USA
 *
 *  Exstrom Laboratories LLC contact:
 *  stefan(AT)exstrom.com
 *
 *  Exstrom Laboratories LLC
 *  Longmont, CO 80503, USA
 *
 */

// Compile: gcc -lm -o compm compm.c

int *parts;
int mp;

void compose(int n, int p, int m)
{
  if (n == 0) {
    if (m == mp) {
      for (; n < m; ++n)
        printf("%d ", parts[n]);
      printf("%d\n", p);
    }
    return;
  }

  if (m < mp) {
    parts[m] = p;
    compose(n - 1, 1, m + 1);
  }
  compose(n - 1, p + 1, m);
}

/*******************************************************************/

int main(int argc, char *argv[])
{
  if (argc < 3) {
    printf("usage: %s n m\n", argv[0]);
    printf("  Generates all compositions of n into m parts\n");
    exit(-1);
  }

  int n = atoi(argv[1]);
  parts = (int *)malloc(n * sizeof(int));
  mp = atoi(argv[2]) - 1;

  compose(n - 1, 1, 0);
  free(parts);
  return (0);
}
