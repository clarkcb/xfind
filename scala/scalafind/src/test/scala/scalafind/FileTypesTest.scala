package scalafind

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Paths

class FileTypesTest extends AnyFunSuite {
  private val whitespaceRegex = "\\s+"

  test("test archive extensions") {
    val fileTypes = new FileTypes()
    """7z arj bin bz2 cab cpio dmg ear gz hqx iso jar pax rar sit sitx tar tgz war zip zipx Z""".
      split(whitespaceRegex).foreach { ext =>
        val archiveFile = Paths.get("archive." + ext)
        // println("archiveFile: " + archiveFile)
        assert(fileTypes.isArchiveFile(archiveFile))
        assert(!fileTypes.isAudioFile(archiveFile))
        // assert(!FileTypes.isBinaryFile(archiveFile))
        assert(!fileTypes.isCodeFile(archiveFile))
        assert(!fileTypes.isImageFile(archiveFile))
        assert(!fileTypes.isTextFile(archiveFile))
        assert(!fileTypes.isUnknownFile(archiveFile))
        assert(!fileTypes.isVideoFile(archiveFile))
        val fileType = fileTypes.getFileType(archiveFile)
        assert(fileType == FileType.Archive)
    }
  }

  test("test audio extensions") {
    val fileTypes = new FileTypes()
    """aac cda mid midi mp3 oga opus wav weba""".
      split(whitespaceRegex).foreach { ext =>
        val audioFile = Paths.get("audio." + ext)
        assert(!fileTypes.isArchiveFile(audioFile))
        assert(fileTypes.isAudioFile(audioFile))
        // assert(!fileTypes.isBinaryFile(audioFile))
        assert(!fileTypes.isCodeFile(audioFile))
        assert(!fileTypes.isFontFile(audioFile))
        assert(!fileTypes.isImageFile(audioFile))
        assert(!fileTypes.isTextFile(audioFile))
        assert(!fileTypes.isUnknownFile(audioFile))
        assert(!fileTypes.isVideoFile(audioFile))
        val fileType = fileTypes.getFileType(audioFile)
        assert(fileType == FileType.Audio)
    }
  }

  test("test binary extensions") {
    val fileTypes = new FileTypes()
    """a beam chm class com dat dbmdl dcr dir dll dms doc dot dxr dylib
      |epub exe fm hi hlp lib lnk mdb mo mobi mpp nib o obj odm odt ott
      |pages pdb ppt pub pyc pyo qxd rpt so swf sys vsd wpd wps wpt wri
      |xls xlt""".stripMargin.split(whitespaceRegex).foreach { ext =>
        val binFile = Paths.get("binfile." + ext)
        // println("binFile: " + binFile)
        assert(!fileTypes.isArchiveFile(binFile))
        assert(!fileTypes.isAudioFile(binFile))
        assert(fileTypes.isBinaryFile(binFile))
        assert(!fileTypes.isCodeFile(binFile))
        assert(!fileTypes.isFontFile(binFile))
        assert(!fileTypes.isImageFile(binFile))
        assert(!fileTypes.isTextFile(binFile))
        assert(!fileTypes.isUnknownFile(binFile))
        assert(!fileTypes.isVideoFile(binFile))
        val fileType = fileTypes.getFileType(binFile)
        assert(fileType == FileType.Binary)
    }
  }

  test("test code extensions") {
    val fileTypes = new FileTypes()
    """adb ads applejs as asm au3 bas bash bat boo bsh c c++ cbl cc cfm cgi
      |clj cls cmd cob coffee cpp cs csh cxx d dart e el elm erl es ex exs frm
      |fs fth fx go groovy h h++ hh hpp hs java js js2 jsf json jsp jspf kt lhs
      |lisp lua m ml pas php php3 php4 php5 pl pm ps1 psc1 psd1 psm1 pxd pxi py
      |pyw pyx r rb rkt rs s sass sbt sc scm scss scala sh swift tcl ts vb vbs""".
      stripMargin.split(whitespaceRegex).foreach { ext =>
        val codeFile = Paths.get("codefile." + ext)
        // println("codeFile: " + codeFile)
        assert(!fileTypes.isArchiveFile(codeFile))
        assert(!fileTypes.isAudioFile(codeFile))
        assert(!fileTypes.isBinaryFile(codeFile))
        assert(fileTypes.isCodeFile(codeFile))
        assert(!fileTypes.isFontFile(codeFile))
        assert(!fileTypes.isImageFile(codeFile))
        assert(fileTypes.isTextFile(codeFile))
        assert(!fileTypes.isUnknownFile(codeFile))
        assert(!fileTypes.isVideoFile(codeFile))
        val fileType = fileTypes.getFileType(codeFile)
        assert(fileType == FileType.Code)
    }
  }

  test("test font extensions") {
    val fileTypes = new FileTypes()
    """eot otf ttf woff woff2""".
      stripMargin.split(whitespaceRegex).foreach { ext =>
        val fontFile = Paths.get("font." + ext)
        assert(!fileTypes.isArchiveFile(fontFile))
        assert(!fileTypes.isAudioFile(fontFile))
        // assert(!fileTypes.isBinaryFile(imageFile))
        assert(!fileTypes.isCodeFile(fontFile))
        assert(fileTypes.isFontFile(fontFile))
        assert(!fileTypes.isImageFile(fontFile))
        assert(!fileTypes.isTextFile(fontFile))
        assert(!fileTypes.isUnknownFile(fontFile))
        assert(!fileTypes.isVideoFile(fontFile))
        val fileType = fileTypes.getFileType(fontFile)
        assert(fileType == FileType.Font)
    }
  }

  test("test image extensions") {
    val fileTypes = new FileTypes()
//    """ai bmp heif indd jpeg jpg png tif tiff""".
    """bmp heif jpeg jpg png tif tiff""".
      stripMargin.split(whitespaceRegex).foreach { ext =>
        val imageFile = Paths.get("image." + ext)
//         println("imageFile: " + imageFile)
        assert(!fileTypes.isArchiveFile(imageFile))
        assert(!fileTypes.isAudioFile(imageFile))
        // assert(!fileTypes.isBinaryFile(imageFile))
        assert(!fileTypes.isCodeFile(imageFile))
        assert(!fileTypes.isFontFile(imageFile))
        assert(fileTypes.isImageFile(imageFile))
        assert(!fileTypes.isTextFile(imageFile))
        assert(!fileTypes.isUnknownFile(imageFile))
        assert(!fileTypes.isVideoFile(imageFile))
        val fileType = fileTypes.getFileType(imageFile)
        assert(fileType == FileType.Image)
    }
  }

  test("test text extensions") {
    val fileTypes = new FileTypes()
    """asc ascx asm asmx asp aspx bib brf cabal cfg cls cmake cmd cnt conf css
      |csv ctl d dbml dbschema ddl dep dfm diff disco dlg dof dpr drl dsp dsw
      |dtd elt ent env etx exp feature fls gcd hql hs htc htm html hxx ics
      |icz iml in inc ini ipr iws jad jql layout lhs log ltx mak mako
      |manifest map markdown master md mf mht mml moc mod mxml p patch plist pm
      |po properties pt rc rc2 rdf rex rtx scc sct sfv sgm sgml sht
      |shtm shtml sln smi smil spec sqc strings sty suml sxw t
      |text tk tld tm tmx tsv txt ui uls uml url user vbs vcf vcs vm vssscc
      |vxml webinfo wml wmls wsc wsd wsdd xlf xsp yaml
      |yml""".stripMargin.split(whitespaceRegex).foreach { ext =>
        val textFile = Paths.get("textFile." + ext)
        // println("textFile: " + textFile)
        assert(!fileTypes.isArchiveFile(textFile))
        assert(!fileTypes.isAudioFile(textFile))
        assert(!fileTypes.isBinaryFile(textFile))
        assert(!fileTypes.isFontFile(textFile))
        assert(!fileTypes.isImageFile(textFile))
        assert(fileTypes.isTextFile(textFile))
        assert(!fileTypes.isUnknownFile(textFile))
        assert(!fileTypes.isVideoFile(textFile))
        val fileType = fileTypes.getFileType(textFile)
        assert(Set(FileType.Code, FileType.Text, FileType.Xml).contains(fileType))
    }
  }

  test("test video extensions") {
    val fileTypes = new FileTypes()
    """avi mov mp4 mpeg webm""".
      stripMargin.split(whitespaceRegex).foreach { ext =>
        val videoFile = Paths.get("video." + ext)
        assert(!fileTypes.isArchiveFile(videoFile))
        assert(!fileTypes.isAudioFile(videoFile))
        // assert(!fileTypes.isBinaryFile(imageFile))
        assert(!fileTypes.isCodeFile(videoFile))
        assert(!fileTypes.isFontFile(videoFile))
        assert(!fileTypes.isImageFile(videoFile))
        assert(!fileTypes.isTextFile(videoFile))
        assert(!fileTypes.isUnknownFile(videoFile))
        assert(fileTypes.isVideoFile(videoFile))
        val fileType = fileTypes.getFileType(videoFile)
        assert(fileType == FileType.Video)
      }
  }

  test("test xml extensions") {
    val fileTypes = new FileTypes()
    """atom atomcat atomsrv bdsproj config csproj davmount dbproj docx dotx
      |fsproj fxml jhm jnlp kml mm pom potx ppsx pptx qrc rdf resx rng rss
      |settings sldx stc std sti stw svgz sxc sxd sxg sxi stw sxm sxw tld
      |vbproj vcproj vdproj wadl wsdd wsdl x3d xaml xjb xlsx xltx xml
      |xps xsd xsl xslt xspf xul""".stripMargin.split(whitespaceRegex).foreach { ext =>
        val xmlFile = Paths.get("xmlfile." + ext)
        // println("xmlFile: " + xmlFile)
        assert(!fileTypes.isArchiveFile(xmlFile))
        assert(!fileTypes.isAudioFile(xmlFile))
        assert(!fileTypes.isBinaryFile(xmlFile))
        assert(!fileTypes.isCodeFile(xmlFile))
        assert(!fileTypes.isFontFile(xmlFile))
        assert(!fileTypes.isImageFile(xmlFile))
        assert(fileTypes.isTextFile(xmlFile))
        assert(!fileTypes.isUnknownFile(xmlFile))
        assert(!fileTypes.isVideoFile(xmlFile))
        val fileType = fileTypes.getFileType(xmlFile)
        assert(fileType == FileType.Xml)
    }
  }
}
