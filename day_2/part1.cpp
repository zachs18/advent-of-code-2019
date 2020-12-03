#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <sstream>

std::istream &operator>>(std::istream &str, const char &&c) {
	char given{};
	str >> given;
	if (given != c) str.setstate(str.failbit);
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

int main(int argc, char **argv) {
	std::vector<int> numbers;
	int num;
	while (std::cin >> num) {
		numbers.push_back(num);
		std::cin >> ',';
	}

	if (numbers.size() < 1) return EXIT_FAILURE;
	for (auto arg = 1; arg < argc; ++arg) {
		unsigned index, value;
		if (!(std::istringstream{argv[arg]} >> index >> '=' >> value)) return EXIT_FAILURE;
		numbers[index] = value;
	}

	for (unsigned index = 0; index < numbers.size()-3; index += 4) {
		std::cerr << numbers << std::endl;
		if (numbers[index] == 1) {
			numbers[numbers[index+3]] = numbers[numbers[index+1]] + numbers[numbers[index+2]];
		} else if (numbers[index] == 2) {
			numbers[numbers[index+3]] = numbers[numbers[index+1]] * numbers[numbers[index+2]];
		} else if (numbers[index] == 99) {
			break;
		}
	}
		std::cerr << numbers << std::endl;

	std::cout << numbers[0] << '\n';
}
