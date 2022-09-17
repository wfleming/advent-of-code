#include <cassert>
#include "d24.cpp"
#include "alu.h"

void test_read_coefficients() {
  auto i = R"(inp w
mul x 0
add x z
mod x 26
div z 1
add x 15
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 13
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 10
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 16
mul y x
add z y)";

  auto cs = read_coefficients(istringstream{i});
  assert(cs.size() == 2);
  assert((cs[0] == Coef{15,1,13}));
  assert((cs[1] == Coef{10,1,16}));
}

void test_find_prev_z() {
  // The actual coefficients for my last digit
  auto d14_coef = Coef{-9, 26, 10};

  // last digit we want 0 out for valid serials
  auto z13 = find_prev_z(0, 9, d14_coef);
  assert(z13.has_value());
  assert((*z13) == 18);
}

void test_vec_to_ul() {
  assert((vec_to_ul(vector<unsigned int>{1,2,3,4,5}) == 12345));
}

void test_d14_result() {
  // sanity-check my results from search & test_find_prev_z
  // parse the last digit step of my program, set z to what I got above for
  // "before", confirm after running z is what I expect.

  auto p = R"(inp w
mul x 0
add x z
mod x 26
div z 26
add x -9
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 10
mul y x
add z y)";
  auto prog = parse_program(istringstream{p});
  auto alu = ALU{prog, vector<int>{9}};
  alu.z = 18;
  assert(!alu.run_until_halt_or_error().has_value());
  assert(alu.z == 0);
}

void test_run_forward_p1() {
  auto p = R"(inp w
mul x 0
add x z
mod x 26
div z 1
add x 15
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 13
mul y x
add z y
inp w
mul x 0
add x z
mod x 26
div z 1
add x 10
eql x w
eql x 0
mul y 0
add y 25
mul y x
add y 1
mul z y
mul y 0
add y w
add y 16
mul y x
add z y)";
  auto prog = parse_program(istringstream{p});
  auto alu = ALU{prog, vector<int>{9, 9}};
  for (auto i = 0; i < 18; i++) { alu.step(); } // process digit 1
  assert(alu.z == 22); // 9 + 13 = 22
  for (auto i = 0; i < 18; i++) { alu.step(); } // process digit 2
  assert(alu.z == 597); // (22 * 26) + (9 * 16) = 597

  // ok, now, backwards!
  auto z1 = find_prev_z(597, 9, Coef{10, 1, 16});
  assert(z1.has_value());
  assert(*z1 == 22);

  // search should get the answer
  auto coefs = read_coefficients(istringstream{p});
  assert(apply_coefficients(0, 9, coefs[0]) == 22);
  assert(apply_coefficients(22, 9, coefs[1]) == 597);
  auto r = search(coefs, 597);
  assert(r.has_value());
  assert((*r == vector<unsigned int>{9,9}));
}

// parse & run the machine with the p1 answer I got
void test_verify_p1() {
  auto prog = parse_program(ifstream{"input.txt"});
  auto alu = ALU{prog, vector<int>{5,3,9,9,9,9,9,5,8,2,9,3,9,9}};
  assert(!alu.run_until_halt_or_error().has_value());
  assert(alu.z == 0);
}

void test_verify_p2() {
  auto prog = parse_program(ifstream{"input.txt"});
  auto alu = ALU{prog, vector<int>{1,3,9,9,9,9,9,5,8,2,9,3,9,5}};
  assert(!alu.run_until_halt_or_error().has_value());
  assert(alu.z == 0);
}

int main(int, char**) {
  test_read_coefficients();
  test_find_prev_z();
  test_vec_to_ul();
  test_d14_result();
  test_run_forward_p1();
  test_verify_p1();
  test_verify_p2();

  cout << "Tests complete." << endl;
}
