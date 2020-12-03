#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <sstream>
#include <optional>

std::istream &operator>>(std::istream &str, const char &&c) {
	char given{};
	str >> given;
	if (given != c) str.setstate(str.failbit);
	return str;
}

std::ostream &operator<<(std::ostream &str, const std::vector<auto> &vec) {
	str << '{';
//	std::copy(
//		vec.begin(), vec.end(),
//		std::ostream_iterator<decltype(vec[0])>{str, ","}
//	);
	unsigned count = 0;
	for (auto i : vec) {
		str << i << ',';
		if (++count % 4 == 0) str << '\t';
	}
	return str << '}';
}

std::ostream &operator<<(std::ostream &str, const std::optional<auto> &val) {
	str << '{';
	if (val) str << *val;
	else str << "nullopt";
	return str << '}';
}

std::optional<long> run(std::vector<long> &&memory) {
	for (unsigned index = 0; index < memory.size()-3; index += 4) {
//		std::cout << memory << std::endl;
		if (memory[index] == 1) {
			memory[memory[index+3]] = memory[memory[index+1]] + memory[memory[index+2]];
		} else if (memory[index] == 2) {
			memory[memory[index+3]] = memory[memory[index+1]] * memory[memory[index+2]];
		} else if (memory[index] == 99) {
			break;
		} else {
			return std::nullopt;
		}
	}
	return memory[0];
}

int main(int argc, char **argv) {
	const std::vector<long> numbers = []{
		std::vector<long> numbers;
		long num;
		while (std::cin >> num) {
			numbers.push_back(num);
			std::cin >> ',';
		}
		return numbers;
	}();

	long correct_result;
	std::istringstream{argv[1]} >> correct_result;

	if (numbers.size() < 1) return EXIT_FAILURE;

	std::vector<long> memory;
	for (auto arg = 0; arg < 10000; ++arg) {
		memory = numbers;
		memory[1] = arg / 100;
		memory[2] = arg % 100;
		auto result = run(std::move(memory));
		std::cout << arg << ':' << result << '\n';
		if (result == correct_result) {
			std::cout << '\n' << arg << '\n';
			break;
		}
	}

}
