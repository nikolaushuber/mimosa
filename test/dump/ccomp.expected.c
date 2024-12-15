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
  bool first_8;
};

void ccomp__fby___reset (struct ccomp__fby___state_t * self_10)
{
  self_10->first_8 = true;
}

bool ccomp__fby___step (struct tup_bool_bool input_0,
  struct ccomp__fby___state_t * self_10)
{
  bool return_val_9;
  bool r_3;
  bool tmp_6;
  bool tmp_5;
  bool tmp_4;
  struct tup_bool_bool tmp_7;
  tmp_7 = input_0;
  init_1 = tmp_7.val0;
  signal_2 = tmp_7.val1;
  tmp_4 = init_1;
  tmp_5 = signal_2;
  if (first_8)
  {
    tmp_6 = tmp_4;
  } else
  {
    tmp_6 = tmp_5;
  }
  self_10->first_8 = false;
  r_3 = tmp_6;
  return_val_9 = r_3;
  return return_val_9;
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
  struct tup_bool_bool tmp_4;
  struct tup_bool_bool a_1;
  a_1 = input_0;
  tmp_4 = a_1;
  b_2 = tmp_4.val0;
  unused_3 = tmp_4.val1;
  return_val_5 = b_2;
  return return_val_5;
}
