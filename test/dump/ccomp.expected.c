#include "stdbool.h"

struct opt_bool;

struct opt_bool some_opt_bool (bool);

struct opt_bool none_opt_bool ();

struct tup_opt_bool_bool;

struct tup_int_int;

struct tup_bool_bool_bool;

struct tup_bool_bool;

enum opt_tag
{
  None,
  Some
};

struct opt_bool;

struct opt_bool some_opt_bool (bool);

struct opt_bool none_opt_bool ();

struct opt_bool
{
  enum opt_tag tag;
  bool value;
};

struct opt_bool some_opt_bool (bool value)
{
  struct opt_bool ret;
  ret.tag = Some;
  ret.value = value;
  return ret;
}

struct opt_bool none_opt_bool ()
{
  struct opt_bool ret;
  ret.tag = None;
  return ret;
}

struct tup_opt_bool_bool
{
  struct opt_bool val0;
  bool val1;
};

struct tup_int_int
{
  int val0;
  int val1;
};

struct tup_bool_bool_bool
{
  bool val0;
  bool val1;
  bool val2;
};

struct tup_bool_bool
{
  bool val0;
  bool val1;
};

struct ccomp__init___state_t
{
  bool first_8;
};

void ccomp__init___reset (struct ccomp__init___state_t * self_11)
{
  self_11->first_8 = true;
}

bool ccomp__init___step (struct tup_bool_bool input_0,
  struct ccomp__init___state_t * self_11)
{
  bool return_val_10;
  bool r_3;
  bool tmp_9;
  bool tmp_6;
  bool tmp_5;
  bool tmp_4;
  bool init_1;
  bool signal_2;
  struct tup_bool_bool tmp_7;
  tmp_7 = input_0;
  init_1 = tmp_7.val0;
  signal_2 = tmp_7.val1;
  tmp_4 = init_1;
  tmp_5 = signal_2;
  tmp_9 = self_11->first_8;
  if (tmp_9)
  {
    tmp_6 = tmp_4;
  } else
  {
    tmp_6 = tmp_5;
  }
  self_11->first_8 = false;
  r_3 = tmp_6;
  return_val_10 = r_3;
  return return_val_10;
}

struct ccomp__if___state_t
{
  
};

void ccomp__if___reset (struct ccomp__if___state_t * self_9)
{
  
}

bool ccomp__if___step (struct tup_bool_bool_bool input_0,
  struct ccomp__if___state_t * self_9)
{
  bool return_val_8;
  bool r_4;
  bool tmp_6;
  bool tmp_5;
  bool c_1;
  bool t_2;
  bool e_3;
  struct tup_bool_bool_bool tmp_7;
  tmp_7 = input_0;
  c_1 = tmp_7.val0;
  t_2 = tmp_7.val1;
  e_3 = tmp_7.val2;
  tmp_5 = c_1;
  if (tmp_5)
  {
    tmp_6 = t_2;
  } else
  {
    tmp_6 = e_3;
  }
  r_4 = tmp_6;
  return_val_8 = r_4;
  return return_val_8;
}

struct ccomp__fby___state_t
{
  bool first_7;
};

void ccomp__fby___reset (struct ccomp__fby___state_t * self_10)
{
  self_10->first_7 = true;
}

bool ccomp__fby___step (struct tup_bool_bool input_0,
  struct ccomp__fby___state_t * self_10)
{
  bool return_val_9;
  bool r_3;
  bool tmp_8;
  bool tmp_5;
  bool tmp_4;
  bool init_1;
  bool signal_2;
  struct tup_bool_bool tmp_6;
  tmp_6 = input_0;
  init_1 = tmp_6.val0;
  signal_2 = tmp_6.val1;
  tmp_4 = init_1;
  tmp_8 = self_10->first_7;
  if (tmp_8)
  {
    tmp_5 = tmp_4;
  } else
  {
    tmp_5 = signal_2;
  }
  self_10->first_7 = false;
  r_3 = tmp_5;
  return_val_9 = r_3;
  return return_val_9;
}

struct ccomp__opt_output__state_t
{
  
};

void ccomp__opt_output__reset (struct ccomp__opt_output__state_t * self_9)
{
  
}

struct opt_bool ccomp__opt_output__step (struct tup_bool_bool input_0,
  struct ccomp__opt_output__state_t * self_9)
{
  struct opt_bool return_val_8;
  struct opt_bool r_3;
  struct opt_bool tmp_6;
  bool tmp_5;
  bool tmp_4;
  bool c_1;
  bool signal_2;
  struct tup_bool_bool tmp_7;
  tmp_7 = input_0;
  c_1 = tmp_7.val0;
  signal_2 = tmp_7.val1;
  tmp_4 = c_1;
  if (tmp_4)
  {
    tmp_5 = signal_2;
    tmp_6 = some_opt_bool(tmp_5);
  } else
  {
    tmp_6 = none_opt_bool();
  }
  r_3 = tmp_6;
  return_val_8 = r_3;
  return return_val_8;
}

struct ccomp__init_pre__state_t
{
  bool first_13;
  bool first_11;
};

void ccomp__init_pre__reset (struct ccomp__init_pre__state_t * self_16)
{
  self_16->first_13 = true;
  self_16->first_11 = true;
}

bool ccomp__init_pre__step (struct tup_bool_bool input_0,
  struct ccomp__init_pre__state_t * self_16)
{
  bool return_val_15;
  bool r_3;
  bool tmp_14;
  bool tmp_9;
  bool tmp_8;
  bool tmp_7;
  bool tmp_12;
  bool tmp_6;
  bool tmp_5;
  bool tmp_4;
  bool init_1;
  bool signal_2;
  struct tup_bool_bool tmp_10;
  tmp_10 = input_0;
  init_1 = tmp_10.val0;
  signal_2 = tmp_10.val1;
  tmp_4 = false;
  tmp_5 = signal_2;
  tmp_12 = self_16->first_11;
  if (tmp_12)
  {
    tmp_6 = tmp_4;
  } else
  {
    tmp_6 = tmp_5;
  }
  self_16->first_11 = false;
  tmp_7 = init_1;
  tmp_8 = tmp_6;
  tmp_14 = self_16->first_13;
  if (tmp_14)
  {
    tmp_9 = tmp_7;
  } else
  {
    tmp_9 = tmp_8;
  }
  self_16->first_13 = false;
  r_3 = tmp_9;
  return_val_15 = r_3;
  return return_val_15;
}

struct ccomp__add__state_t
{
  
};

void ccomp__add__reset (struct ccomp__add__state_t * self_8)
{
  
}

int ccomp__add__step (struct tup_int_int input_0,
  struct ccomp__add__state_t * self_8)
{
  int return_val_7;
  int c_3;
  int tmp_5;
  int tmp_4;
  int a_1;
  int b_2;
  struct tup_int_int tmp_6;
  tmp_6 = input_0;
  a_1 = tmp_6.val0;
  b_2 = tmp_6.val1;
  tmp_4 = a_1;
  tmp_5 = b_2;
  c_3 = (tmp_4 + tmp_5);
  return_val_7 = c_3;
  return return_val_7;
}

struct ccomp__opt_unpack__state_t
{
  
};

void ccomp__opt_unpack__reset (struct ccomp__opt_unpack__state_t * self_8)
{
  
}

bool ccomp__opt_unpack__step (struct tup_opt_bool_bool input_0,
  struct ccomp__opt_unpack__state_t * self_8)
{
  bool return_val_7;
  bool c_3;
  bool tmp_5;
  struct opt_bool tmp_4;
  struct opt_bool a_1;
  bool b_2;
  struct tup_opt_bool_bool tmp_6;
  tmp_6 = input_0;
  a_1 = tmp_6.val0;
  b_2 = tmp_6.val1;
  tmp_4 = a_1;
  switch (tmp_4.tag)
  {
    case Some:
      tmp_5 = tmp_4.value;
      break;
    case None:
      tmp_5 = b_2;
      break;
  }
  c_3 = tmp_5;
  return_val_7 = c_3;
  return return_val_7;
}

struct ccomp__fst__state_t
{
  
};

void ccomp__fst__reset (struct ccomp__fst__state_t * self_6)
{
  
}

bool ccomp__fst__step (struct tup_bool_bool input_0,
  struct ccomp__fst__state_t * self_6)
{
  bool return_val_5;
  bool b_2;
  bool unused_3;
  struct tup_bool_bool tmp_4;
  struct tup_bool_bool a_1;
  a_1 = input_0;
  tmp_4 = a_1;
  b_2 = tmp_4.val0;
  unused_3 = tmp_4.val1;
  return_val_5 = b_2;
  return return_val_5;
}
