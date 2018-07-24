#include "common-dep.hpp"

dep::CommonDep::CommonDep(const std::string &content): m_content(content)
{
}

const std::string &dep::CommonDep::getContent() const
{
	return m_content;
}
