#include <boost/algorithm/string.hpp>
#include "FindOption.h"

namespace cppfind {
    FindOption::FindOption(const std::string* sa, const std::string& la, const std::string& desc) {
        m_shortarg = sa;
        m_longarg = la;
        m_description = desc;
        if (m_shortarg != nullptr && !m_shortarg->empty()) {
            std::string so = boost::to_lower_copy(*m_shortarg);
            so.append("@");
            so.append(m_longarg);
            m_sortarg = so;
        } else {
            m_sortarg = m_longarg;
        }
    }

    const std::string* FindOption::shortarg() const {
        return m_shortarg;
    }

    std::string FindOption::longarg() const {
        return m_longarg;
    }

    std::string FindOption::description() const {
        return m_description;
    }

    std::string FindOption::sortarg() const {
        return m_sortarg;
    }
}
