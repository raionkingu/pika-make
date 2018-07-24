#pragma once

#include <string>

namespace dep
{
	class CommonDep
	{
	public:
		explicit CommonDep(const std::string &content);

		const std::string &getContent() const;

	private:
		std::string m_content;
	};
}
