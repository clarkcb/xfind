namespace CsFindLib;

public class ArgToken(string name, ArgTokenType type, object value)
{
    public string Name { get; } = name;
    public ArgTokenType Type { get; } = type;
    public object Value { get; } = value;
}
