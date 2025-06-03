namespace FsFindLib

open System.IO
open System.Reflection

module EmbeddedResource = 

    type T = interface end

    let GetResourceFileContents (namespaceAndFileName : string) : string = 
        let contents =
            try
                use stream = Assembly.GetAssembly(typeof<T>).GetManifestResourceStream(namespaceAndFileName)
                use reader = new StreamReader(stream)
                reader.ReadToEnd()
            with
            | :? IOException as e -> printfn $"%s{e.Message}"; ""
        contents
