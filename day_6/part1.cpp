#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <set>
#include <map>

struct connection {
	std::string m_body;
	std::string m_satellite;
};

int main(int argc, char **argv) {
	std::map<std::string, std::string> orbits;
	std::map<std::string, std::optional<unsigned>> bodies;
	{
		std::string line;
		while (std::cin >> line) {
			auto paren_index = line.find(')');
			auto body = line.substr(0, paren_index);
			auto satellite = line.substr(paren_index+1);
			orbits[satellite] = body;
			bodies[body];
			bodies[satellite];
		}
	}

	auto find_orbits = [&](std::string const &body, auto &find_orbits) -> unsigned {
		if (bodies[body].has_value()) return *bodies[body];
		if (auto it = orbits.find(body); it != orbits.end()) {
			bodies[body] = 1 + find_orbits(it->second, find_orbits);
		} else { // no parent
			bodies[body] = 0;
		}
		return bodies[body].value();
	};

//	for (auto [a,b] : orbits) std::cout << a << ") " << b << '\n';

	unsigned orbit_count = 0;
	for (auto const &[body, _] : bodies) orbit_count += find_orbits(body, find_orbits);

	std::cout << orbit_count << '\n';

	return EXIT_SUCCESS;
//	return EXIT_FAILURE;
}
