#include <math.h>
#include <algorithm>
#include <istream>
#include <string>
#include <iostream>
#include <fstream>
#include <sstream>
#include <vector>
#include <optional>

using namespace std;

string trim(const string& str)
{
    size_t first = str.find_first_not_of(' ');
    if (string::npos == first) { return str; }
    size_t last = str.find_last_not_of(" \t\n");
    return str.substr(first, (last - first + 1));
}

// coefficients for a digit
// every digit follows the same basic pattern:
// w = <input-digit>
// x = (z % 26) + a
// z = z / b
// x = (int)(x != w)
// y = (25 * x) + 1
// z = z * y
// z += (w + c) * x
struct Coef {
  int a, b, c;

  bool operator==(const Coef& other) const = default;
  bool operator!=(const Coef& other) const = default;

  operator std::string() const {
    ostringstream s;
    s << "Coef{" << a << ',' << b << ',' << c << '}';
    return s.str();
  }
};

// read coefficients from input program
// I transcribed this by hand at first, but after 14 digits I wasn't really
// trusting I got it right. Better to just automate it.
vector<Coef> read_coefficients(istream&& input) {
  vector<Coef> rv{};
  string line;
  Coef c;

  while (input.good()) {
    getline(input, line);
    if (trim(line) == "inp w") {
      c = Coef{0, 0, 0};
      for (auto i = 0; i < 4; i++) { getline(input, line); } // skip 4 lines to get to mod x b
      c.b = stoi(trim(line).substr(6));
      getline(input, line); // next line is coefficient a
      c.a = stoi(trim(line).substr(6));
      for (auto i = 0; i < 10; i++) { getline(input, line); } // skip 10 lines to get to add y c
      c.c = stoi(trim(line).substr(6));
      rv.push_back(c);
    }
  }

  return rv;
}

// apply the algorithm for 1 digit, return the value of z
int apply_coefficients(const int& z0, const int& w, const Coef& c) {
  int x = (z0 % 26) + c.a; // given range of c.a, x is always -26..26
  int z = z0 / c.b;
  x = (int)(x != w);       // if w != (z % 26) + c.a
  int y = (25 * x) + 1;    // y here is always 1 or 26
  z = z * y;               // z stays the same or gets multiplied by 26
  z += (w + c.c) * x;      // z increasing by at most 26ish

  return z;
}

// given the z we want after applying the algorithm to a current digit,
// return the value z could have been previously.
optional<int> find_prev_z(const int& z_after, const int& w, const Coef& c) {
  // z can't be less than 0 since an early instruction per-digit is z % 26, and that's illegal according to spec
  // the z * 26 or z * 1 stuff, plus the fact that the increment is always
  // in -26..26 means that although z can get quite large (26**7 ish),
  // we can bound the possibilities

  int z_min = 0, z_max = 0;
  if (c.a < 0) {
    z_min = (z_after * 26) - 26;
    z_max = (z_after * 26) + 26;
  } else {
    z_min = (z_after / 26) - 26;
    z_max = (z_after / 26) + 26;
  }

  for (int z = max(z_min, 0); z < z_max; z++) {
    if (apply_coefficients(z, w, c) == z_after) {
      return optional<int>{z};
    }
  }

  return optional<int>{};
}

// Depth first search, working backwards through the digits
// We know we want to finish the entire algorithm with z == 0. So we want to
// find the highest last digit (call it d14) with a potential z at the beginning
// of the step (call it z13) that results in z == 0. If you find that, then you
// step backwards to find a d13 & z12 that results in z13, and so on, until
// you've found all the digits.
optional<vector<unsigned int>> search(vector<Coef> coefs, const int& z_after, bool part2=false) {
  auto c = coefs.back(); // pop the last coeffient for use
  coefs.pop_back(); // erase the last coefficient for descent

  for (unsigned int d = (part2 ? 1 : 9); (part2 ? d <= 9 : d >= 1); (part2 ? d++ : d--)) {
    if (coefs.empty()) {
      if (apply_coefficients(0, d, c) == z_after) {
        return optional<vector<unsigned int>>{{d}};
      }
    } else {
      auto z_before = find_prev_z(z_after, d, c);
      if (z_before) {
        auto rest_of_the_owl = search(coefs, *z_before, part2);
        if (rest_of_the_owl) {
          (*rest_of_the_owl).push_back(d);
          return rest_of_the_owl;
        }
      }
    }
  }

  return optional<vector<unsigned int>>{};
}

unsigned long vec_to_ul(vector<unsigned int> digits) {
  unsigned long r = 0;
  for (auto d : digits) {
    r *= 10;
    r += d;
  }
  return r;
}

#ifndef IS_TEST
int main(int argc, char** argv) {
  if (argc < 2) {
    std::cerr << "Provide input filename as argument" << std::endl;
    return 1;
  }
  auto coefs = read_coefficients(ifstream{argv[1]});

  auto p1_digits = search(coefs, 0);
  if (p1_digits) {
    auto p1 = vec_to_ul(*p1_digits);
    cout << "p1: " << p1 << " is the largest valid model number" << endl;
  } else {
    cout << "p1 didn't find an answer" << endl;
  }

  auto p2_digits = search(coefs, 0, true);
  if (p2_digits) {
    auto p2 = vec_to_ul(*p2_digits);
    cout << "p2: " << p2 << " is the smallest valid model number" << endl;
  } else {
    cout << "p2 didn't find an answer" << endl;
  }
}
#endif
