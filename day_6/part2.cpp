#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <set>
#include <map>

std::ostream &operator<<(std::ostream &str, const std::optional<auto> &op) {
	if (op.has_value()) return str << '{' << op.value() << '}';
	else return str << "{}";
}

int main(int argc, char **argv) {
	std::map<std::string, std::set<std::string>> adjacencies;
	{
		std::string line;
		while (std::cin >> line) {
			auto paren_index = line.find(')');
			auto body = line.substr(0, paren_index);
			auto satellite = line.substr(paren_index+1);
			adjacencies[satellite].insert(body);
			adjacencies[body].insert(satellite);
		}
	}

	auto find_depth = [&adjacencies](auto &find_depth, std::string const &src, std::string const &dst, auto &depths, std::set<std::string> &visited) -> std::optional<unsigned> {
		if (depths[src].has_value()) return depths[src].value();
		if (src == dst) {
			depths[src] = 0;
			return 0;
		}
		visited.insert(src);
		std::optional<unsigned> best;
		for (auto const &body : adjacencies[src]) {
			if (visited.find(body) != visited.end()) continue;
			if (
				auto depth = find_depth(find_depth, body, dst, depths, visited);
				depth.has_value() && (!best.has_value() || 1 + depth.value() < best.value())
			) {
				best = 1 + depth.value();
			}
		}
		visited.erase(src);
		if (best.has_value()) depths[src] = best;
		return depths[src];
	};

//	for (auto [a,b] : adjacencies) std::cout << a << ") " << b << '\n';

	std::set<std::string> visited;
	std::map<std::string, std::optional<unsigned>> depths;
	std::cout << find_depth(find_depth, "YOU", "SAN", depths, visited) << "-2\n";

//	for (auto [a,b] : depths) std::cout << a << ": " << b << '\n';

	return EXIT_SUCCESS;
//	return EXIT_FAILURE;
}
