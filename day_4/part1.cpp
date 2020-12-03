#include <cstdlib>
#include <iostream>
#include <string>
#include <optional>
#include <regex>

bool has_nontriple_double_digits(auto i) {
	const std::string s = std::to_string(i);

	if (s.size() < 2) return false;
	if (s.size() == 2) return s[0] == s[1];

	bool has_nontriple_double = false;
	for (unsigned long i = 1; i < s.size()-2; ++i) {
		if (s[i-1] != s[i] && s[i] == s[i+1] && s[i] != s[i+2]) {
			has_nontriple_double = true;
		}
	}
	if (s[0] == s[1] && s[0] != s[2]) {
		has_nontriple_double = true;
	}
	if (auto i = s.size()-2; s[i-1] != s[i] && s[i] == s[i+1]) {
		has_nontriple_double = true;
	}
	return has_nontriple_double;
}

bool has_nondecreasing_digits(auto i) {
	int prev = i % 10;
	i /= 10;
	while (i) {
		int current = i % 10;
		i /= 10;
		if (current > prev) return false;
		prev = current;
	}
	return true;
}


int main(int argc, char **argv) {
	if (argc != 3) return EXIT_FAILURE;

	int first = std::atoi(argv[1]);
	int last = std::atoi(argv[2]);

	unsigned count = 0;

	for (auto i = first; i <= last; ++i) {
		if (has_nontriple_double_digits(i) && has_nondecreasing_digits(i)) ++count;
	}

	std::cout << count << '\n';

	return EXIT_SUCCESS;
	return EXIT_FAILURE;
}
