#import <functional>
#import <variant>
#include <sstream>

typedef variant<char, int> instr_arg_t;

struct Instruction {
  string instr;
  char arg0;
  instr_arg_t arg1;

  Instruction(string& line) {
    // no error handling, YOLO
    auto instr_end_idx = line.find(" ");
    instr = line.substr(0, instr_end_idx);
    auto arg0_end_idx = line.find(" ", instr_end_idx + 1);
    string arg0_str;
    if (arg0_end_idx == string::npos) {
      arg0 = line.substr(instr_end_idx + 1).front();
    } else {
      arg0 = line.substr(instr_end_idx + 1, arg0_end_idx - instr_end_idx + 1).front();
    }

    if (arg0_end_idx != string::npos) {
      auto arg1_str = line.substr(arg0_end_idx + 1);
      try {
        arg1 = stoi(arg1_str);
      } catch(invalid_argument const& ex) {
        arg1 = arg1_str.front();
      }
    }
  }

  Instruction(string instr, char arg0, instr_arg_t arg1):
    instr{instr}, arg0{arg0}, arg1{arg1} {}

  bool operator ==(const Instruction& other) const = default;
};

vector<Instruction> parse_program(istream&& in) {
  vector<Instruction> rv;

  string line;
  while (in.good()) {
    getline(in, line);
    if(line.size() > 0) {
      rv.push_back(Instruction{line});
    }
  }

  return rv;
}

struct AluErr {
  string msg;

  AluErr(const char* m): msg{m} {};
};

struct ALU {
  int w = 0, x = 0, y = 0, z = 0; // registers
  size_t ip = 0; // instruction pointer
  vector<Instruction> program;
  vector<int> inputs;

  ALU(vector<Instruction> program): program{program} {}
  ALU(vector<Instruction> program, vector<int> inputs): program{program}, inputs{inputs} {}

  void set_reg(char name, int val) {
    switch (name) {
      case 'w': w = val; break;
      case 'x': x = val; break;
      case 'y': y = val; break;
      case 'z': z = val; break;
    }
  }

  int get_reg(char name) {
    switch (name) {
      case 'w': return w;
      case 'x': return x;
      case 'y': return y;
      case 'z': return z;
    }
    return -1;
  }

  int get_lit_or_reg(instr_arg_t a) {
    if (a.index() == 0) {
      return get_reg(get<char>(a));
    } else {
      return get<int>(a);
    }
  }

  void math_op(Instruction i, function<int(int, int)> op) {
    int lhs = get_reg(i.arg0), rhs = get_lit_or_reg(i.arg1);
    int r = op(lhs, rhs);
    set_reg(i.arg0, r);
  }

  void step() {
    auto instr = program[ip];

    if (instr.instr == "inp") {
      if (inputs.empty()) {
        throw AluErr{"requested input but there is no input"};
      } else {
        set_reg(instr.arg0, inputs.front());
        inputs.erase(inputs.begin());
      }
    } else if (instr.instr == "add") {
      math_op(instr, [](auto a, auto b) { return a + b; } );
    } else if (instr.instr == "mul") {
      math_op(instr, [](auto a, auto b) { return a * b; } );
    } else if (instr.instr == "div") {
      math_op(instr, [](auto a, auto b) {
        if ( b == 0) { throw AluErr{"Illegal divide by zero"}; }
        return a / b;
      } );
    } else if (instr.instr == "mod") {
      math_op(instr, [](auto a, auto b) {
        if ( a < 0 || b <= 0) { throw AluErr{"Illegal mod operation"}; }
        return a % b;
      } );
    } else if (instr.instr == "eql") {
      math_op(instr, [](auto a, auto b) { return a == b; } );
    } else {
      throw AluErr{"unknown instruction"};
    }

    ip++;
  }

  optional<AluErr> run_until_halt_or_error() {
    while (ip < program.size()) {
      try {
        step();
      } catch (const AluErr& e) {
        return e;
      }
    }

    return optional<AluErr>{};
  }
};
