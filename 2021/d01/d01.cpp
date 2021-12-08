/* Advent of code 2021 day 01
 */

#include <iostream>
#include <vector>
#include <functional>
#include <string>
#include <fstream>

std::vector<int> input_nums(char *filename) {
  std::ifstream fin(filename);
  std::vector<int> nums;
  std::string line;

  while(fin.good()) {
    std::getline(fin, line);
    if (line.size() > 0) {
      nums.push_back(std::stoi(line));
    }
  }

  return nums;
}

int count_increases(std::vector<int>& nums) {
  int count = 0;
  for (unsigned int i = 0; i < nums.size() - 1; i++) {
    if (nums[i] < nums[i + 1]) {
      count++;
    }
  }
  return count;
}

std::vector<int> window(std::vector<int>& nums) {
  std::vector<int> windowed_nums;
  for (unsigned int i = 0; i < nums.size() - 2; i++) {
    windowed_nums.push_back(nums[i] + nums[i + 1] + nums[i + 2]);
  }
  return windowed_nums;
}

int main(int argc, char *argv[]) {
  if (argc < 2) {
    std::cerr << "Provide input filename as argument" << std::endl;
    return 1;
  }
  auto nums = input_nums(argv[1]);
  std::cout << "p1: depth increased " << count_increases(nums) << " times" << std::endl;
  auto windowed_nums = window(nums);
  std::cout << "p2: depth increased " << count_increases(windowed_nums) << " times" << std::endl;
}
