#include <cassert>
#include "d24.cpp"

void test_parse_instr() {
  auto s = string{"inp a"};
  auto i = Instruction{s};
  assert(i.instr == "inp");
  assert(i.arg0 == 'a');

  s = string{"mul x b"};
  i = Instruction{s};
  assert(i.instr == "mul");
  char* arg1_c = get_if<char>(&i.arg1);
  assert(i.arg0 == 'x');
  assert(arg1_c != NULL && *arg1_c == 'b');

  s = string{"zed w -300"};
  i = Instruction{s};
  assert(i.instr == "zed");
  int* arg1_i = get_if<int>(&i.arg1);
  assert(i.arg0 == 'w');
  assert(arg1_i != NULL && *arg1_i == -300);
}

void test_parse_program() {
  const char* sample_input = R"(inp z
inp x
mul z 3
eql z x)";
  auto prog = parse_program(istringstream{sample_input});

  /* vector<Instruction> expected; */
  assert((prog == vector<Instruction>{
      Instruction{(string{"inp"}), 'z', instr_arg_t{}},
      Instruction{(string{"inp"}), 'x', instr_arg_t{}},
      Instruction{(string{"mul"}), 'z', 3},
      Instruction{(string{"eql"}), 'z', 'x'},
  }));
}

void test_input() {
  vector<int> xs = split_input(12345678912345);
  assert((xs == vector<int>{1,2,3,4,5,6,7,8,9,1,2,3,4,5}));
  assert(input_is_valid(xs));

  xs = split_input(12340678912345);
  assert((xs == vector<int>{1,2,3,4,0,6,7,8,9,1,2,3,4,5}));
  assert(!input_is_valid(xs));

  xs = split_input(3);
  assert(!input_is_valid(xs));
}

void test_run_program() {
  // the example program that converts input to binary
  const char* sample_input = R"(inp w
add z w
mod z 2
div w 2
add y w
mod y 2
div w 2
add x w
mod x 2
div w 2
mod w 2)";
  auto prog = parse_program(istringstream{sample_input});
  auto alu = ALU(prog, vector<int>{10});
  assert(!alu.run_until_halt_or_error().has_value());
  assert(alu.z == 0); // 1s place
  assert(alu.y == 1); // 2s place
  assert(alu.x == 0); // 4s place
  assert(alu.w == 1); // 8s place
}

void test_program_multiple_input() {
  const char* sample_input = R"(inp w
inp x
mul w x)";
  auto prog = parse_program(istringstream{sample_input});
  auto alu = ALU(prog, vector<int>{3,4});
  assert(!alu.run_until_halt_or_error().has_value());
  assert(alu.w == 12);
}

void test_program_eql() {
  const char* sample_input = R"(inp w
inp x
mul w x
eql w 12)";
  auto prog = parse_program(istringstream{sample_input});
  auto alu = ALU(prog, vector<int>{3,4});
  assert(!alu.run_until_halt_or_error().has_value());
  assert(alu.w == 1);
}

int main(int, char**) {
  test_parse_instr();
  test_parse_program();
  test_input();
  test_run_program();
  test_program_multiple_input();
  test_program_eql();

  cout << "Tests complete." << endl;
}
