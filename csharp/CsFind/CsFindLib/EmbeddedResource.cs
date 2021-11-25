using System;
using System.IO;
using System.Reflection;
using System.Text;

namespace CsFindLib
{
    public static class EmbeddedResource
    {
        public static string GetResourceFileContents(string namespaceAndFileName)
        {
            try
            {
                using var stream = Assembly.GetAssembly(typeof(EmbeddedResource))!.GetManifestResourceStream(namespaceAndFileName);
                using var reader = new StreamReader(stream!, Encoding.UTF8);
                return reader.ReadToEnd();
            }
            catch(FileNotFoundException)
            {
                throw new Exception($"Failed to read Embedded Resource {namespaceAndFileName}");
            }
        }
    }
}
