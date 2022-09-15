#include <istream>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>

using namespace std;

bool input_is_valid(unsigned long n) {
  if (n > 99999999999999) {
    return false;
  }

  if (n < 11111111111111) {
    return false;
  }

  while (n > 0) {
    if ((n % 10) == 0) { return false; }
    n = n / 10;
  }

  return true;
}

bool run_monad(unsigned long n) {
  // w,x,y,z are the registers.
  int w = 0, x = 0, y = 0, z = 0;

  // logic for each digit is the same, just different constants (moduli,
  // dividing). digit 1 can omit a lot of logic since everything is 0 to start.
  //
  // digit 1: z = w + 13 (z in 14..22)
  // digit 2: check if w != z + 10, y = 25 or 26, z = z * y + (w + 16)

  // digit 1
  w = n % 10;
  z = w + 13;

  // digit 2
  n = n / 10;
  w = n % 10;
  x = z + 10; //(z % 26) + 10;, but z must be < 26 here, so mod is pointless
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 16) * x;
  z += y;

  // digit 3
  n = n / 10;
  w = n % 10;
  x = (z % 26) + 10;
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 2) * x;
  z += y;

  // digit 4
  n = n / 10;
  w = n % 10;
  x = (z % 26) + 10;
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 8) * x;
  z += y;

  // digit 5
  n = n / 10;
  w = n % 10;
  x = (z % 26) + 14;
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 11) * x;
  z += y;

  // digit 6
  n = n / 10;
  w = n % 10;
  x = (z % 26) - 11;
  z = z / 26; // first time this isn't 1
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 11) * x;
  z += y;

  // digit 7
  n = n / 10;
  w = n % 10;
  x = (z % 26) + 10;
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 12) * x;
  z += y;

  // digit 8
  n = n / 10;
  w = n % 10;
  x = (z % 26) - 16;
  z = z / 26;
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 2) * x;
  z += y;

  // digit 9
  n = n / 10;
  w = n % 10;
  x = (z % 26) - 9;
  z = z / 26;
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 2) * x;
  z += y;

  // digit 10
  n = n / 10;
  w = n % 10;
  x = (z % 26) + 11;
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 15) * x;
  z += y;

  // digit 11
  n = n / 10;
  w = n % 10;
  x = (z % 26) - 8;
  z = z / 26;
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 1) * x;
  z += y;

  // digit 12
  n = n / 10;
  w = n % 10;
  x = (z % 26) - 8;
  z = z / 26;
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 10) * x;
  z += y;

  // digit 13
  n = n / 10;
  w = n % 10;
  x = (z % 26) - 10;
  z = z / 26;
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 14) * x;
  z += y;

  // digit 14
  n = n / 10;
  w = n % 10;
  x = (z % 26) - 9;
  z = z / 26;
  x = (int)(x != w);
  y = (25 * x) + 1;
  z = z * y;
  y = (w + 10) * x;
  z += y;

  return z == 0;
}

unsigned long p1() {
  unsigned long it = 0;

  for(unsigned long x = 99999999999999; x >= 11111111111111; x--) {
    it++;
    if (it % 1000000 == 0) { cout << '.'; } // track progress every 1 million numbers
    if (input_is_valid(x) && run_monad(x)) {
      return x;
    }
  }

  return 0;
}

#ifndef IS_TEST
int main(int, char** ) {
  cout << "p1: " << p1() << " is the largest valid model number" << endl;
}
#endif
