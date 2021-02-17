#ifndef CPPFIND_FINDOPTION_H
#define CPPFIND_FINDOPTION_H

namespace cppfind {
    class FindOption {
    private:
        const std::string* m_shortarg;
        std::string m_longarg;
        std::string m_description;
        std::string m_sortarg;

    public:
        FindOption(const std::string* shortarg, const std::string& longarg, const std::string& description);
        const std::string* shortarg() const;
        std::string longarg() const;
        std::string description() const;
        std::string sortarg() const;
    };
}

#endif //CPPFIND_FINDOPTION_H
