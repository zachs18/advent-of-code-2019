#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <sstream>
#include <fstream>

std::istream &operator>>(std::istream &str, const char &&c) {
	char given{};
	str >> given;
	if (given != c) {
		if (str) str.putback(given);
		str.setstate(str.failbit);
	}
	return str;
}

std::ostream &operator<<(std::ostream &str, const std::vector<auto> &vec) {
	str << '{';
	std::copy(
		vec.begin(), vec.end(),
		std::ostream_iterator<decltype(vec[0])>{str, ","}
	);
	return str << '}';
}

struct instruction {
	unsigned opcode;
	unsigned modes[3];

	instruction() = default;
	instruction(unsigned val)
	 : opcode(val % 100),
	   modes{(val / 100) % 10, (val / 1000) % 10, (val / 10000) % 10}
	{}
	instruction(int value)
	 : instruction(value < 0 ? 0u : static_cast<unsigned>(value))
	{}
};


int main(int argc, char **argv) {
	std::vector<int> numbers;
	int num;
	{
		std::fstream file{"input"};
		while (file >> num) {
			numbers.push_back(num);
			file >> ',';
		}
	}

	if (numbers.size() < 1) return EXIT_FAILURE;
	for (auto arg = 1; arg < argc; ++arg) {
		unsigned index, value;
		if (!(std::istringstream{argv[arg]} >> index >> '=' >> value)) return EXIT_FAILURE;
		numbers[index] = value;
	}

	auto get_parameter = [&numbers](int &parameter, unsigned parameter_mode) -> int& {
		if (parameter_mode == 0) { return numbers[parameter]; }
		//if (parameter_mode == 1) {
		return parameter;
	};


	for (unsigned index = 0; index < numbers.size()-3; ) {
		std::cerr << numbers << std::endl;
		instruction inst{numbers[index]};
		if (inst.opcode == 1) {
			get_parameter(numbers[index+3], inst.modes[2])
			 = get_parameter(numbers[index+1], inst.modes[0])
			 + get_parameter(numbers[index+2], inst.modes[1]);
			index += 4;
		} else if (inst.opcode == 2) {
			get_parameter(numbers[index+3], inst.modes[2])
			 = get_parameter(numbers[index+1], inst.modes[0])
			 * get_parameter(numbers[index+2], inst.modes[1]);
			index += 4;
		} else if (inst.opcode == 3) {
			std::cin >> get_parameter(numbers[index+1], inst.modes[0]);
			index += 2;
		} else if (inst.opcode == 4) {
			std::cout << get_parameter(numbers[index+1], inst.modes[0]) << '\n';
			index += 2;
		} else if (inst.opcode == 99) {
			index += 1;
			break;
		}
	}
	std::cerr << numbers << std::endl;

//	std::cout << numbers[0] << '\n';
}