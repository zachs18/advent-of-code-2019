#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <algorithm>
#include <numeric>
#include <ranges>

constexpr int module_fuel_requirement(int mass) {
	auto fuel = mass / 3 - 2;
	return std::max(0, fuel);
}

constexpr int full_module_fuel_requirement(int mass) {
	auto total_fuel = module_fuel_requirement(mass);
	auto partial_fuel = total_fuel;
	do {
		partial_fuel = module_fuel_requirement(partial_fuel);
//		std::cout << partial_fuel << '\n';
		total_fuel += partial_fuel;
	} while (partial_fuel);
	return total_fuel;
}

int main(int argc, char **argv) {
	std::ranges::subrange module_masses{std::istream_iterator<int>{std::cin}, std::istream_iterator<int>{}};
	auto module_fuel_requirements = std::views::transform(module_masses, full_module_fuel_requirement);

//	std::cout << std::reduce(module_fuel_requirements.begin(), module_fuel_requirements.end()) << '\n';
	int total_sum = 0;
	for (auto fuel : module_fuel_requirements) total_sum += fuel;
	std::cout << total_sum << std::endl;
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
