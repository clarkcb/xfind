package gofind

func GetFileTypes() *FileTypes {
	return &FileTypes{
		map[string]set{
			"archive":  makeSet([]string{"7z", "Z", "ane", "apk", "arj", "bz2", "cpio", "ear", "egg", "gz", "hqx", "jar", "pax", "pyz", "pyzw", "rar", "sit", "sitx", "swc", "tar", "tgz", "war", "zip", "zipx"}),
			"binary":   makeSet([]string{"a", "ai", "beam", "bin", "chm", "class", "com", "dat", "dbmdl", "dcr", "dir", "dll", "dms", "doc", "dot", "dxr", "dylib", "epub", "exe", "fm", "hi", "hlp", "indd", "lib", "lnk", "mdb", "mo", "mobi", "mpp", "nib", "o", "obj", "odm", "odt", "ott", "pages", "pdb", "ppt", "psd", "pub", "pyc", "pyo", "qxd", "rpt", "so", "swf", "sys", "vsd", "wpd", "wps", "wpt", "wri", "xcuserstate", "xls", "xlt"}),
			"code":     makeSet([]string{"ada", "adb", "ads", "applejs", "as", "asm", "au3", "bas", "bash", "bat", "boo", "bsh", "c", "c++", "cbl", "cc", "cfm", "cgi", "clj", "cls", "cmd", "cob", "coffee", "cpp", "cs", "csh", "cxx", "d", "e", "el", "elm", "erl", "es", "es6", "ex", "exs", "frm", "fs", "fth", "fx", "go", "groovy", "h", "h++", "hh", "hpp", "hs", "hxx", "ipp", "ipynb", "java", "js", "js2", "jsf", "json", "jsp", "jspf", "jsx", "kt", "kts", "lhs", "lisp", "lua", "m", "ml", "p", "pas", "pbxproj", "php", "php3", "php4", "php5", "pl", "pm", "ps1", "psc1", "psd1", "psm1", "pxd", "pxi", "py", "pyw", "pyx", "r", "rb", "rexx", "rkt", "rs", "s", "sass", "sbt", "sc", "scala", "scm", "scss", "sh", "swift", "tcl", "tk", "ts", "vb", "vbs"}),
			"nosearch": makeSet([]string{"aif", "aifc", "aiff", "au", "avi", "bmp", "cab", "cur", "db", "dib", "dmg", "eot", "gif", "icns", "ico", "idlk", "ief", "iso", "jpe", "jpeg", "jpg", "m3u", "m4a", "m4p", "mov", "movie", "mp3", "mp4", "mpe", "mpeg", "mpg", "mxu", "ogg", "otf", "pdf", "pict", "png", "qt", "ra", "ram", "rm", "rpm", "snd", "suo", "tif", "tiff", "tte", "ttf", "wav", "woff"}),
			"text":     makeSet([]string{"1", "10", "11", "12", "13", "14", "15", "16", "17", "18", "19", "2", "20", "3", "323", "4", "5", "6", "7", "8", "9", "am", "app", "asc", "ascx", "asm", "asmx", "asp", "aspx", "bib", "brf", "cabal", "cfg", "cls", "cmake", "cmd", "cnt", "conf", "css", "csv", "ctl", "d", "dbml", "dbschema", "ddl", "dep", "dfm", "diff", "disco", "dlg", "dof", "dpr", "drl", "dsp", "dsw", "dtd", "elt", "ent", "env", "eps", "etx", "exp", "feature", "fls", "gcd", "gradle", "hql", "hs", "htaccess", "htc", "htm", "html", "hxx", "ics", "icz", "iml", "in", "inc", "ini", "ipr", "iws", "jad", "jam", "jql", "layout", "lhs", "log", "ltx", "mak", "mako", "manifest", "map", "markdown", "master", "md", "mf", "mht", "mml", "moc", "mod", "mxml", "org", "p", "patch", "pm", "po", "pod", "policy", "pot", "properties", "ps", "pt", "rc", "rc2", "rdf", "rex", "rst", "rtf", "rtx", "scc", "sct", "sfv", "sgm", "sgml", "sht", "shtm", "shtml", "sln", "smi", "smil", "spec", "sqc", "sql", "st", "str", "strings", "sty", "suml", "sxw", "t", "tex", "text", "tk", "tld", "tm", "tmx", "tsv", "txt", "ui", "uls", "uml", "url", "user", "vbs", "vcf", "vcs", "vm", "vrml", "vssscc", "vxml", "wbxml", "webinfo", "wml", "wmls", "wrl", "wsc", "wsd", "wsdd", "xlf", "xsp", "yaml", "yml"}),
			"unknown":  makeSet([]string{"adm", "aps", "cli", "clw", "def", "df2", "ncb", "nt", "nt2", "orig", "pc", "plg", "roff", "sun", "texinfo", "tr", "xwd"}),
			"xml":      makeSet([]string{"ant", "atom", "atomcat", "atomsrv", "bdsproj", "config", "csproj", "davmount", "dbproj", "docx", "dotx", "fsproj", "fxml", "iml", "jhm", "jnlp", "kml", "mm", "plist", "pom", "potx", "ppsx", "pptx", "qrc", "rdf", "resx", "rng", "rss", "settings", "sldx", "stc", "std", "sti", "stw", "svg", "svgz", "sxc", "sxd", "sxg", "sxi", "sxm", "sxw", "tld", "vbproj", "vcproj", "vdproj", "wadl", "wsdd", "wsdl", "x3d", "xaml", "xcbkptlist", "xcscheme", "xcworkspacedata", "xht", "xhtml", "xjb", "xlsx", "xltx", "xml", "xps", "xsd", "xsl", "xslt", "xspf", "xul"}),
		},
		map[string]set{
			"archive":  makeSet([]string{}),
			"binary":   makeSet([]string{}),
			"code":     makeSet([]string{".bashrc", ".bash_profile", ".editorconfig", ".env", ".gitattributes", ".gitignore", ".gitkeep", ".profile", "Dockerfile", "Gemfile", "Jenkinsfile", "Makefile", "MANIFEST", "mime.types", "PKG-INFO", "Rakefile"}),
			"nosearch": makeSet([]string{}),
			"text":     makeSet([]string{"LICENSE", "README"}),
			"unknown":  makeSet([]string{}),
			"xml":      makeSet([]string{}),
		},
	}
}
