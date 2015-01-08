#include <stdlib.h>

typedef struct cell
{
  unsigned long int car;
  unsigned long int cdr;
} *cell;

unsigned long int
tt2(unsigned long int item, unsigned long int list, unsigned long int length)
{
  unsigned long int *table = alloca(sizeof(unsigned long int) * length);
  unsigned long int pointer;
  unsigned long int result = 0;
  int i;
  for (i = 0, pointer = list;
       i < length;
       i++, pointer = ((cell) (list - 7)) -> cdr)
    table[i] = ((cell) (list - 7)) -> car;
  for (i = length - 1; i >= 0; i--)
    if (table[i] == item)
      result++;
  return result;
}
