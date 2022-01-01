using System;

namespace CsFindLib;

public class FindException : Exception
{
	public FindException(string message) : base(message)
	{
	}
}
