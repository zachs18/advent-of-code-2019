#include <cstdlib>
#include <iostream>
#include <iterator>
#include <vector>
#include <set>
#include <memory>

struct vec2d {
	int x, y;

	constexpr vec2d operator-(void) const {
		return {-x, -y};
	}

	constexpr unsigned abs(void) const {
		auto absll = [](int i) -> long long { return i < 0 ? -static_cast<long long>(i) : i; };
		return absll(x) + absll(y);
	}
};

constexpr auto operator<=>(const vec2d &a, const vec2d &b) {
	if (auto cmp = a.abs() <=> b.abs(); cmp != 0) return cmp;
	if (auto cmp = a.x <=> b.x; cmp != 0) return cmp;
	return a.y <=> b.y;
}

constexpr vec2d operator+(const vec2d &a, const vec2d &b) {
	return {a.x + b.x, a.y + b.y};
}

constexpr vec2d operator-(const vec2d &a, const vec2d &b) {
	return a + (-b);
}

std::istream &operator>>(std::istream &str, const char &&c) {
	char given;
	str >> given;
	if (given != c) {
		str.putback(given);
		str.setstate(str.failbit);
	}
	return str;
}

std::istream &operator>>(std::istream &str, vec2d &movement) {
	char c;
	int d;
	if (str >> c >> d) {
		if (c == 'R')
			movement = {+d, 0};
		else if (c == 'L')
			movement = {-d, 0};
		else if (c == 'U')
			movement = {0, +d};
		else // if (c == 'D')
			movement = {0, -d};
	}
	return str;
}

class wire {
	std::vector<vec2d> m_vertices;
public:
	wire(std::vector<vec2d> const &movements) {
		m_vertices.reserve(movements.size()+1);
		vec2d position = {0,0};
		m_vertices.push_back(position);
		for (auto movement : movements) {
			position = position + movement;
			m_vertices.push_back(position);
		}
	}

	void test(void) const {
		for (unsigned long i = 1; i < m_vertices.size(); ++i) {
			auto diff = m_vertices[i] - m_vertices[i-1];
			std::cout << '\t' << diff.x << ',' << diff.y << '\n';
		}
	}

	bool contains(const vec2d point) const {
		const auto [px, py] = point;
		for (unsigned long index = 0; index < m_vertices.size()-1; ++index) {
			const auto [ox1, oy1] = m_vertices[index];
			const auto [ox2, oy2] = m_vertices[index+1];
			if (ox1 == px && ox1 == ox2) {
				if (oy1 <= py && py <= oy2) return true;
				if (oy1 >= py && py >= oy2) return true;
			} else if (oy1 == py && oy1 == oy2) {
				if (ox1 <= px && px <= ox2) return true;
				if (ox1 >= px && px >= ox2) return true;
			}
		}
		return false;
	}

	std::set<vec2d> intersect(wire const &w) const {
		std::set<vec2d> result;
		for (unsigned long index = 0; index < m_vertices.size()-1; ++index) {
			w.intersect(m_vertices[index], m_vertices[index+1], result);
		}
		return result;
	}

	void intersect(vec2d p1, vec2d p2, std::set<vec2d> &result) const {
		// std::set<vec2d> result;
		for (unsigned long index = 0; index < m_vertices.size()-1; ++index) {
			auto o1 = m_vertices[index];
			auto o2 = m_vertices[index+1];
			if (o1.x == o2.x) { // our line is vertical
				if (o1.y > o2.y) std::swap(o1, o2); // direction: up

				if (p1.x == p2.x && p1.x == o1.x) { // argument line is vertical && coincident
					if (p1.y > p2.y) std::swap(p1, p2); // direction: up
					if (o1.y <= p1.y && p2.y <= o2.y) { // entire arg line is intersection
						for (auto y = p1.y; y <= p2.y; ++y) {
							result.insert(vec2d{p1.x, y});
						}
					} else if (p1.y <= o1.y && o2.y <= p2.y) { // entire our line is intersection
						for (auto y = o1.y; y <= o2.y; ++y) {
							result.insert(vec2d{p1.x, y});
						}
					} else if (o1.y <= p1.y && p1.y <= o2.y) { // start p1, end o2
						for (auto y = p1.y; y <= o2.y; ++y) {
							result.insert(vec2d{p1.x, y});
						}
					} else if (p1.y <= o1.y && o1.y <= p2.y) { // start o1, end p2
						for (auto y = o1.y; y <= p2.y; ++y) {
							result.insert(vec2d{p1.x, y});
						}
					}

				} else if (p1.y == p2.y) { // argument line is horizontal
					if (p1.x > p2.x) std::swap(p1, p2); // direction: right
					if (p1.x <= o1.x && o1.x <= p2.x && o1.y <= p1.y && p1.y <= o2.y) {
						result.insert(vec2d{o1.x, p1.y});
					}
				}
			} else if (o1.y == o2.y) { // our line is horizontal
				if (o1.x > o2.x) std::swap(o1, o2); // direction: up

				if (p1.y == p2.y && p1.y == o1.y) { // argument line is horizontal && coincident
					if (p1.x > p2.x) std::swap(p1, p2); // direction: up
					if (o1.x <= p1.x && p2.x <= o2.x) { // entire arg line is intersection
						for (auto x = p1.x; x <= p2.x; ++x) {
							result.insert(vec2d{x, p1.y});
						}
					} else if (p1.x <= o1.x && o2.x <= p2.x) { // entire our line is intersection
						for (auto x = o1.x; x <= o2.x; ++x) {
							result.insert(vec2d{x, p1.y});
						}
					} else if (o1.x <= p1.x && p1.x <= o2.x) { // start p1, end o2
						for (auto x = p1.x; x <= o2.x; ++x) {
							result.insert(vec2d{x, p1.y});
						}
					} else if (p1.x <= o1.x && o1.x <= p2.x) { // start o1, end p2
						for (auto x = o1.x; x <= p2.x; ++x) {
							result.insert(vec2d{x, p1.y});
						}
					}

				} else if (p1.x == p2.x) { // argument line is vertical
					if (p1.y > p2.y) std::swap(p1, p2); // direction: right
					if (p1.y <= o1.y && o1.y <= p2.y && o1.x <= p1.x && p1.x <= o2.x) {
						result.insert(vec2d{o1.y, p1.x});
					}
				}
			}
		}
		//return result;
	}

};

int main(int argc, char **argv) {
	auto read_movements = []{
		std::vector<vec2d> movements;
		vec2d m;
		while (std::cin >> std::ws >> m) {
			movements.push_back(m);
			std::cin >> ',';
		}
		std::cin.clear();
		return movements;
	};
	auto wire1 = wire{read_movements()};
//	wire1.test();
//	std::cout << '\n';
	auto wire2 = wire{read_movements()};
//	wire2.test();
//	std::cout << '\n';

	auto intersections = wire1.intersect(wire2);

	for (auto [x, y] : intersections) {
		std::cout << x << ',' << y << '\n';
	}

	return EXIT_SUCCESS;
	return EXIT_FAILURE;
}
