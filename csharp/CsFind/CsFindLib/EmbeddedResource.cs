using System;
using System.IO;
using System.Reflection;
using System.Text;

namespace CsFind
{
    public static class EmbeddedResource
    {
        public static string GetResourceFileContents(string namespaceAndFileName)
        {
            try
            {
                var assembly = Assembly.GetAssembly(typeof(EmbeddedResource));

                // using var stream = Assembly.GetExecutingAssembly().GetManifestResourceStream(namespaceAndFileName);
                // using var stream = typeof(EmbeddedResource).GetTypeInfo().Assembly
                //     .GetManifestResourceStream(namespaceAndFileName);
                using var stream = assembly.GetManifestResourceStream(namespaceAndFileName);

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
