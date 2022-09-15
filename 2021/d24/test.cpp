#include <cassert>
#include "d24.cpp"

void test_input_is_valid() {
  assert(input_is_valid(12345678912345));

  assert(!input_is_valid(12340678912345));

  assert(!input_is_valid(3));
}

void test_run_monad() {
}

int main(int, char**) {
  test_input_is_valid();
  test_run_monad();

  cout << "Tests complete." << endl;
}
