#include <stdlib.h>
#include <stdio.h>

typedef struct {
  int ipreg, ip;
  int rs[6];
} State;

void op_0(State *s) {
  s->rs[5] = s->rs[5] + 16;
}

void op_1(State *s) {
  s->rs[2] = 1;
}

void op_2(State *s) {
  s->rs[1] = 1;
}

void op_3(State *s) {
  s->rs[4] = s->rs[2] * s->rs[1];
}

void op_4(State *s) {
  if (s->rs[4] == s->rs[3]) {
    s->rs[4] = 1;
  } else {
    s->rs[4] = 0;
  }
}

void op_5(State *s) {
  s->rs[5] = s->rs[4] + s->rs[5];
}

void op_6(State *s) {
  s->rs[5] = s->rs[5] + 1;
}

void op_7(State *s) {
  s->rs[0] = s->rs[2] + s->rs[0];
}

void op_8(State *s) {
  s->rs[1] = s->rs[1] + 1;
}

void op_9(State *s) {
  if (s->rs[1] > s->rs[3]) {
    s->rs[4] = 1;
  } else {
    s->rs[4] = 0;
  }
}

void op_10(State *s) {
  s->rs[5] = s->rs[5] + s->rs[4];
}

void op_11(State *s) {
  s->rs[5] = 2;
}

void op_12(State *s) {
  s->rs[2] = s->rs[2] + 1;
}

void op_13(State *s) {
  if (s->rs[2] > s->rs[3]) {
    s->rs[4] = 1;
  } else {
    s->rs[4] = 0;
  }
}

void op_14(State *s) {
  s->rs[5] = s->rs[4] + s->rs[5];
}

void op_15(State *s) {
  s->rs[5] = 1;
}

void op_16(State *s) {
  s->rs[5] = s->rs[5] * s->rs[5];
}

void op_17(State *s) {
  s->rs[3] = s->rs[3] + 2;
}

void op_18(State *s) {
  s->rs[3] = s->rs[3] * s->rs[3];
}

void op_19(State *s) {
  s->rs[3] = s->rs[5] * s->rs[3];
}

void op_20(State *s) {
  s->rs[3] = s->rs[3] * 11;
}

void op_21(State *s) {
  s->rs[4] = s->rs[4] + 6;
}

void op_22(State *s) {
  s->rs[4] = s->rs[4] * s->rs[5];
}

void op_23(State *s) {
  s->rs[4] = s->rs[4] + 5;
}

void op_24(State *s) {
  s->rs[3] = s->rs[3] + s->rs[4];
}

void op_25(State *s) {
  s->rs[5] = s->rs[5] + s->rs[0];
}

void op_26(State *s) {
  s->rs[5] = 0;
}

void op_27(State *s) {
  s->rs[4] = s->rs[5];
}

void op_28(State *s) {
  s->rs[4] = s->rs[4] * s->rs[5];
}

void op_29(State *s) {
  s->rs[4] = s->rs[5] + s->rs[4];
}

void op_30(State *s) {
  s->rs[4] = s->rs[5] * s->rs[4];
}

void op_31(State *s) {
  s->rs[4] = s->rs[4] * 14;
}

void op_32(State *s) {
  s->rs[4] = s->rs[4] * s->rs[5];
}

void op_33(State *s) {
  s->rs[3] = s->rs[3] + s->rs[4];
}

void op_34(State *s) {
  s->rs[0] = 0;
}

void op_35(State *s) {
  s->rs[5] = 0;
}

void run_op(State *s) {
  switch (s->ip) {
    case 0:
      op_0(s);
      break;
    case 1:
      op_1(s);
      break;
    case 2:
      op_2(s);
      break;
    case 3:
      op_3(s);
      break;
    case 4:
      op_4(s);
      break;
    case 5:
      op_5(s);
      break;
    case 6:
      op_6(s);
      break;
    case 7:
      op_7(s);
      break;
    case 8:
      op_8(s);
      break;
    case 9:
      op_9(s);
      break;
    case 10:
      op_10(s);
      break;
    case 11:
      op_11(s);
      break;
    case 12:
      op_12(s);
      break;
    case 13:
      op_13(s);
      break;
    case 14:
      op_14(s);
      break;
    case 15:
      op_15(s);
      break;
    case 16:
      op_16(s);
      break;
    case 17:
      op_17(s);
      break;
    case 18:
      op_18(s);
      break;
    case 19:
      op_19(s);
      break;
    case 20:
      op_20(s);
      break;
    case 21:
      op_21(s);
      break;
    case 22:
      op_22(s);
      break;
    case 23:
      op_23(s);
      break;
    case 24:
      op_24(s);
      break;
    case 25:
      op_25(s);
      break;
    case 26:
      op_26(s);
      break;
    case 27:
      op_27(s);
      break;
    case 28:
      op_28(s);
      break;
    case 29:
      op_29(s);
      break;
    case 30:
      op_30(s);
      break;
    case 31:
      op_31(s);
      break;
    case 32:
      op_32(s);
      break;
    case 33:
      op_33(s);
      break;
    case 34:
      op_34(s);
      break;
    case 35:
      op_35(s);
      break;
  }

}

int main() {
  // init State
  State *s = malloc(sizeof(State));
  s->ipreg = 5;
  s->ip = 0;
  s->rs[0] = 1;
  s->rs[1] = 0;
  s->rs[2] = 0;
  s->rs[3] = 0;
  s->rs[4] = 0;
  s->rs[5] = 0;

  // run machine
  while(s->ip >= 0 && s->ip < 36) {
    s->rs[s->ipreg] = s->ip;
    run_op(s);
    s->ip = s->rs[s->ipreg] + 1;
  }

  // print result
  printf("reg 0 is %d\n", s->rs[0]);
  return 0;
}
