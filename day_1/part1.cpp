#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
//#include <algorithm>
#include <numeric>
#include <ranges>

constexpr int module_fuel_requirement(int mass) {
	return mass / 3 - 2;
}


int main(int argc, char **argv) {
	std::ranges::subrange module_masses{std::istream_iterator<int>{std::cin}, std::istream_iterator<int>{}};
	auto module_fuel_requirements = std::views::transform(module_masses, module_fuel_requirement);

//	std::cout << std::reduce(module_fuel_requirements.begin(), module_fuel_requirements.end()) << '\n';
	int sum = 0;
	for (auto fuel : module_fuel_requirements) sum += fuel;
	std::cout << sum << std::endl;
/*
	auto lower = numbers.begin();
	auto higher = numbers.end()-1;
	while (lower < higher) {
		auto sum = *lower + *higher;
		if (sum == 2020) {
			std::cout << *lower << " + " << *higher << " = " << sum << '\n';
			std::cout << *lower << " * " << *higher << " = " << (*lower * *higher) << '\n';
			return EXIT_SUCCESS;
		} else if (sum > 2020) {
			--higher;
		} else {
			++lower;
		}
	}
	return EXIT_FAILURE;*/
}
