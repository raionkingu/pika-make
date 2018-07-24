#include "common-header.hpp"

std::ostream &operator<<(std::ostream &o, const dep::CommonDep &cd)
{
	o << "dep-test1.cpp says \"" << cd.getContent() << "\"";

	return o;
}
