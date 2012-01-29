#include <stdlib.h>
#include <assert.h>
#include <stdio.h>

/**********************************************************************
 **
 ** Registers of the virtual machine
 **/

/* This one should not need to be very large at all. */
#define OPERAND_STACK_SIZE 100

lispobj operand_stack[OPERAND_STACK_SIZE];

int operand_stack_pointer = 0;

/* Maximum number of arguments and return values allowed */
#define MAX_NUMBER_OF_ARGUMENTS 100

/* The vector of arguments and return values */
lispobj args[MAX_NUMBER_OF_ARGUMENTS];

/* The number of arguments or values that are valid */
int arg_count;

/**********************************************************************
 **
 ** A FASL reader for 32-bit little-endian FASL files. 
 ** 
 ** A FASL file contains a sequence of 32-bit numbers.  The first
 ** such number indicates how many more there are in the file. 
 ** The remaining numbers come in two parts, but there is no way
 ** to distinguish between the two parts.  The first part, starting
 ** at index 0, is an instruction vector to be executed after the
 ** contents has been loaded into memory.  The second part is the
 ** vector of constants to be used by the first part.  
 **
 ** Here, the constants can only be unsigned machine integers.  The
 ** code in the first part is responsible for converting those numbers
 ** into Lisp objects.  
 **
 ** To construct a string, the code uses some numbers, each of which
 ** is converted to a character, creates a vector and fills it from
 ** those characters.
 **
 ** From string, other objects such as symbols, packages, etc, can 
 ** be created through the use of standard constructor functions. 
 **/

unsigned int
read_int(FILE *fp)
{
  unsigned char byte0 = getc(fp) & 0xff;
  unsigned char byte1 = getc(fp) & 0xff;
  unsigned char byte2 = getc(fp) & 0xff;
  int byte3 = getc(fp);
  return (byte3 << 24) | (byte2 << 16) | (byte1 << 8) | byte0;
}

int *
read_fasl_file(char *filename)
{
  FILE *fp = fopen(filename, "r");
  int size = read_int(fp);
  int *contents = malloc(size * sizeof(int));
  int i;
  for(i = 0; i < size; i++)
    contents[i] = read_int(fp);
  return contents;
}

static void
load_fasl_file(char *filename)
{
  int *contents = read_fasl_file(filename);
  dynamic_frame frame = malloc(sizeof(struct dynamic_frame));
  frame -> next = call_stack;
  frame -> static_environment = 0;
  frame -> instructions = contents;
  frame -> constants = 0;
  frame -> program_counter = 0;
  call_stack = frame;
}

/**********************************************************************
 **
 ** Operations of the virtual machine
 **/

typedef void (*operation)(void);

#define MAX_NUMBER_OF_OPCODES 1000
operation operations[MAX_NUMBER_OF_OPCODES];

/* Operations. */

/* Only used for error reporting. */
static int opcode;

int trace_flag = 0;

int
next_op(void)
{
  return call_stack -> instructions[call_stack -> program_counter++];
}

static void
check_operand_stack_depth(int i, int j)
{
  assert(operand_stack_pointer >= i);
  assert(operand_stack_pointer <= j);
}

static void
push_value(lispobj value)
{
  check_operand_stack_depth(0, OPERAND_STACK_SIZE - 1);
  operand_stack[operand_stack_pointer++] = value;
}

int
pop_value(void)
{
  check_operand_stack_depth(1, OPERAND_STACK_SIZE);
  return operand_stack[--operand_stack_pointer];
}

static void
op_error(void)
{
  printf("illegal operation %d\n", opcode);
}

static void
op_pop(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(1, OPERAND_STACK_SIZE);
  {
    if(trace_flag)
      printf("pop");
    operand_stack_pointer--;
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_push_immediate(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(0, OPERAND_STACK_SIZE - 1);
  {
    lispobj operand = next_op();
    if(trace_flag)
      printf("push immediate %d", operand);
    push_value(operand);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_dup(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(1, OPERAND_STACK_SIZE - 1);
  {
    lispobj operand = operand_stack[operand_stack_pointer - 1];
    if(trace_flag)
      printf("dup %d", operand);
    push_value(operand);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_swap(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(2, OPERAND_STACK_SIZE);
  {
    lispobj temp = operand_stack[operand_stack_pointer - 1];
    if(trace_flag)
      printf("swap");
    operand_stack[operand_stack_pointer - 1] = operand_stack[operand_stack_pointer - 2];
    operand_stack[operand_stack_pointer - 2] = temp;
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_swap2(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(3, OPERAND_STACK_SIZE);
  {
    lispobj temp = operand_stack[operand_stack_pointer - 1];
    if(trace_flag)
      printf("swap2");
    operand_stack[operand_stack_pointer - 1] = operand_stack[operand_stack_pointer - 3];
    operand_stack[operand_stack_pointer - 3] = temp;
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_jump(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(1, OPERAND_STACK_SIZE);
  {
    lispobj jump_amount = next_op();
    if(trace_flag)
      printf("jump %d", jump_amount);
    call_stack -> program_counter += jump_amount;
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_jump_zero(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(2, OPERAND_STACK_SIZE);
  {
    lispobj jump_amount = next_op();
    lispobj value_to_test = pop_value();
    if(trace_flag)
      printf("jump %d if %d is zero", jump_amount, value_to_test);
    if(value_to_test == 0)
      call_stack -> program_counter += jump_amount;
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_jump_positive(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(2, OPERAND_STACK_SIZE);
  {
    lispobj jump_amount = next_op();
    lispobj value_to_test = pop_value();
    if(trace_flag)
      printf("jump %d if %d is positive", jump_amount, value_to_test);
    if(value_to_test > 0)
      call_stack -> program_counter += jump_amount;
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_jump_negative(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(2, OPERAND_STACK_SIZE);
  {
    lispobj jump_amount = next_op();
    lispobj value_to_test = pop_value();
    if(trace_flag)
      printf("jump %d if %d is negative", jump_amount, value_to_test);
    if(value_to_test < 0)
      call_stack -> program_counter += jump_amount;
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_add(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(2, OPERAND_STACK_SIZE);
  {
    lispobj operand2 = pop_value();
    lispobj operand1 = pop_value();
    if(trace_flag)
      printf("add %d and %d", operand1, operand2);
    push_value(operand1 + operand2);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_sub(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(2, OPERAND_STACK_SIZE);
  {
    lispobj operand2 = pop_value();
    lispobj operand1 = pop_value();
    if(trace_flag)
      printf("subtract %d and %d", operand1, operand2);
    if(trace_flag)
      printf("subtract %d and %d", operand1, operand2);
    push_value(operand1 - operand2);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_neg(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(1, OPERAND_STACK_SIZE);
  {
    lispobj operand = pop_value();
    if(trace_flag)
      printf("negate %d", operand);
    push_value(-operand);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_put_char(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(1, OPERAND_STACK_SIZE);
  {
    lispobj operand = pop_value();
    if(trace_flag)
      printf("put_char %d", operand);
    putchar(operand);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_halt(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  if(trace_flag)
    printf("halt");
  exit(0);
}

static void
op_memalloc(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(1, OPERAND_STACK_SIZE);
  {
    lispobj operand = pop_value();
    if(trace_flag)
      printf("memalloc %d", operand);
    push_value((lispobj)malloc(sizeof(lispobj) * operand));
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_memref(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(1, OPERAND_STACK_SIZE);
  {
    lispobj operand = pop_value();
    if(trace_flag)
      printf("memref %d", operand);
    push_value(*((lispobj *)operand));
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_memset(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(2, OPERAND_STACK_SIZE);
  {
    lispobj operand2 = pop_value();
    lispobj operand1 = pop_value();
    if(trace_flag)
      printf("memset...");
    *((lispobj *)operand1) = operand2;
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_and(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(2, OPERAND_STACK_SIZE);
  {
    lispobj operand2 = pop_value();
    lispobj operand1 = pop_value();
    if(trace_flag)
      printf("and %d and %d", operand1, operand2);
    push_value(operand1 & operand2);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_or(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(2, OPERAND_STACK_SIZE);
  {
    lispobj operand2 = pop_value();
    lispobj operand1 = pop_value();
    if(trace_flag)
      printf("or %d and %d", operand1, operand2);
    push_value(operand1 | operand2);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_and_not(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(2, OPERAND_STACK_SIZE);
  {
    lispobj operand2 = pop_value();
    lispobj operand1 = pop_value();
    if(trace_flag)
      printf("and_not %d and %d", operand1, operand2);
    push_value(operand1 & ~operand2);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_not(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(1, OPERAND_STACK_SIZE);
  {
    lispobj operand = pop_value();
    if(trace_flag)
      printf("not %d", operand);
    push_value(~operand);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_push_from_arg_count(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(0, OPERAND_STACK_SIZE - 1);
  {
    if(trace_flag)
      printf("push from arg_count...");
    push_value(arg_count);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_pop_to_arg_count(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(1, OPERAND_STACK_SIZE);
  {
    if(trace_flag)
      printf("pop to arg_count...");
    arg_count = pop_value();
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_push_from_arg(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(0, OPERAND_STACK_SIZE - 1);
  {
    lispobj arg = pop_value();
    if(trace_flag)
      printf("push from arg...");
    push_value(args[arg]);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_pop_to_arg(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  check_operand_stack_depth(2, OPERAND_STACK_SIZE);
  {
    lispobj arg = pop_value();
    lispobj val = pop_value();
    if(trace_flag)
      printf("pop to arg...");
    args[arg] = val;
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
op_push_constant(void)
{
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
  {
    int operand = next_op();
    if(trace_flag)
      printf("push constant...");
    push_value(call_stack -> constants[operand]);
  }
  if(trace_flag) printf("[%2d] ", operand_stack_pointer);
}

static void
execute_next_instruction(void)
{
  if(trace_flag)
    printf("pc: %4d  ", call_stack -> program_counter);
  opcode = next_op();
  if(opcode < 0 || opcode >= MAX_NUMBER_OF_OPCODES)
    op_error();
  else
    operations[opcode]();
  if(trace_flag)
    printf("\n");
}

static void
machine_loop(void)
{
  for(;;)
    execute_next_instruction();
}

static void
initialize_ops(void)
{
  /* Initialize operations to error operation in
     case we forget to assign some element later. */
  {
    int i;
    for(i = 0; i < MAX_NUMBER_OF_OPCODES; i++)
      operations[i] = op_error;
  }
#include "init.h"
}

int
main(void)
{
  initialize_ops();

  load_fasl_file("boot.sfl");

  machine_loop();

  return 0;
}
