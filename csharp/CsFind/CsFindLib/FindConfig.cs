using System;

namespace CsFindLib;

public static class FindConfig
{
    private static string _home = Environment.GetEnvironmentVariable("HOME")
                                  ?? Environment.GetEnvironmentVariable("USERPROFILE")
                                  ?? "~";
    public static string XfindPath = Environment.GetEnvironmentVariable("XFIND_PATH")
                                     ?? _home + "/src/xfind";
    public static string XfindDb = XfindPath + "/shared/xfind.db";
}
