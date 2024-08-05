package scalafind

import org.scalatest.funsuite.AnyFunSuite

import java.nio.file.Paths

class FileTypesTest extends AnyFunSuite {
  private val whitespaceRegex = "\\s+"

  test("test archive extensions") {
    """7z arj bin bz2 cab cpio dmg ear gz hqx iso jar pax rar sit sitx tar tgz war zip zipx Z""".
      split(whitespaceRegex).foreach { ext =>
        val archiveFile = Paths.get("archive." + ext)
        // println("archiveFile: " + archiveFile)
        assert(FileTypes.isArchiveFile(archiveFile))
        assert(!FileTypes.isAudioFile(archiveFile))
        // assert(!FileTypes.isBinaryFile(archiveFile))
        assert(!FileTypes.isCodeFile(archiveFile))
        assert(!FileTypes.isImageFile(archiveFile))
        assert(!FileTypes.isTextFile(archiveFile))
        assert(!FileTypes.isUnknownFile(archiveFile))
        assert(!FileTypes.isVideoFile(archiveFile))
        val fileType = FileTypes.getFileType(archiveFile)
        assert(fileType == FileType.Archive)
    }
  }

  test("test audio extensions") {
    """aac cda mid midi mp3 oga opus wav weba""".
      split(whitespaceRegex).foreach { ext =>
        val audioFile = Paths.get("audio." + ext)
        assert(!FileTypes.isArchiveFile(audioFile))
        assert(FileTypes.isAudioFile(audioFile))
        // assert(!FileTypes.isBinaryFile(audioFile))
        assert(!FileTypes.isCodeFile(audioFile))
        assert(!FileTypes.isFontFile(audioFile))
        assert(!FileTypes.isImageFile(audioFile))
        assert(!FileTypes.isTextFile(audioFile))
        assert(!FileTypes.isUnknownFile(audioFile))
        assert(!FileTypes.isVideoFile(audioFile))
        val fileType = FileTypes.getFileType(audioFile)
        assert(fileType == FileType.Audio)
    }
  }

  test("test binary extensions") {
    """a beam chm class com dat dbmdl dcr dir dll dms doc dot dxr dylib
      |epub exe fm hi hlp lib lnk mdb mo mobi mpp nib o obj odm odt ott
      |pages pdb ppt pub pyc pyo qxd rpt so swf sys vsd wpd wps wpt wri
      |xls xlt""".stripMargin.split(whitespaceRegex).foreach { ext =>
        val binFile = Paths.get("binfile." + ext)
        // println("binFile: " + binFile)
        assert(!FileTypes.isArchiveFile(binFile))
        assert(!FileTypes.isAudioFile(binFile))
        assert(FileTypes.isBinaryFile(binFile))
        assert(!FileTypes.isCodeFile(binFile))
        assert(!FileTypes.isFontFile(binFile))
        assert(!FileTypes.isImageFile(binFile))
        assert(!FileTypes.isTextFile(binFile))
        assert(!FileTypes.isUnknownFile(binFile))
        assert(!FileTypes.isVideoFile(binFile))
        assert(!FileTypes.isXmlFile(binFile))
        val fileType = FileTypes.getFileType(binFile)
        assert(fileType == FileType.Binary)
    }
  }

  test("test code extensions") {
    """adb ads applejs as asm au3 bas bash bat boo bsh c c++ cbl cc cfm cgi
      |clj cls cmd cob coffee cpp cs csh cxx d dart e el elm erl es ex exs frm
      |fs fth fx go groovy h h++ hh hpp hs java js js2 jsf json jsp jspf kt lhs
      |lisp lua m ml pas php php3 php4 php5 pl pm ps1 psc1 psd1 psm1 pxd pxi py
      |pyw pyx r rb rkt rs s sass sbt sc scm scss scala sh swift tcl ts vb vbs""".
      stripMargin.split(whitespaceRegex).foreach { ext =>
        val codeFile = Paths.get("codefile." + ext)
        // println("codeFile: " + codeFile)
        assert(!FileTypes.isArchiveFile(codeFile))
        assert(!FileTypes.isAudioFile(codeFile))
        assert(!FileTypes.isBinaryFile(codeFile))
        assert(FileTypes.isCodeFile(codeFile))
        assert(!FileTypes.isFontFile(codeFile))
        assert(!FileTypes.isImageFile(codeFile))
        assert(FileTypes.isTextFile(codeFile))
        assert(!FileTypes.isUnknownFile(codeFile))
        assert(!FileTypes.isVideoFile(codeFile))
        //assert(!FileTypes.isXmlFile(codeFile))
        val fileType = FileTypes.getFileType(codeFile)
        assert(fileType == FileType.Code)
    }
  }

  test("test font extensions") {
    """eot otf ttf woff woff2""".
      stripMargin.split(whitespaceRegex).foreach { ext =>
        val fontFile = Paths.get("font." + ext)
        assert(!FileTypes.isArchiveFile(fontFile))
        assert(!FileTypes.isAudioFile(fontFile))
        // assert(!FileTypes.isBinaryFile(imageFile))
        assert(!FileTypes.isCodeFile(fontFile))
        assert(FileTypes.isFontFile(fontFile))
        assert(!FileTypes.isImageFile(fontFile))
        assert(!FileTypes.isTextFile(fontFile))
        assert(!FileTypes.isUnknownFile(fontFile))
        assert(!FileTypes.isVideoFile(fontFile))
        val fileType = FileTypes.getFileType(fontFile)
        assert(fileType == FileType.Font)
    }
  }

  test("test image extensions") {
    """ai bmp heif indd jpeg jpg png tif tiff""".
      stripMargin.split(whitespaceRegex).foreach { ext =>
        val imageFile = Paths.get("image." + ext)
        // println("imageFile: " + imageFile)
        assert(!FileTypes.isArchiveFile(imageFile))
        assert(!FileTypes.isAudioFile(imageFile))
        // assert(!FileTypes.isBinaryFile(imageFile))
        assert(!FileTypes.isCodeFile(imageFile))
        assert(!FileTypes.isFontFile(imageFile))
        assert(FileTypes.isImageFile(imageFile))
        assert(!FileTypes.isTextFile(imageFile))
        assert(!FileTypes.isUnknownFile(imageFile))
        assert(!FileTypes.isVideoFile(imageFile))
        val fileType = FileTypes.getFileType(imageFile)
        assert(fileType == FileType.Image)
    }
  }

  test("test text extensions") {
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
        assert(!FileTypes.isArchiveFile(textFile))
        assert(!FileTypes.isAudioFile(textFile))
        assert(!FileTypes.isBinaryFile(textFile))
        assert(!FileTypes.isFontFile(textFile))
        assert(!FileTypes.isImageFile(textFile))
        assert(FileTypes.isTextFile(textFile))
        assert(!FileTypes.isUnknownFile(textFile))
        assert(!FileTypes.isVideoFile(textFile))
        val fileType = FileTypes.getFileType(textFile)
        assert(Set(FileType.Code, FileType.Text, FileType.Xml).contains(fileType))
    }
  }

  test("test video extensions") {
    """avi mov mp4 mpeg webm""".
      stripMargin.split(whitespaceRegex).foreach { ext =>
        val videoFile = Paths.get("video." + ext)
        assert(!FileTypes.isArchiveFile(videoFile))
        assert(!FileTypes.isAudioFile(videoFile))
        // assert(!FileTypes.isBinaryFile(imageFile))
        assert(!FileTypes.isCodeFile(videoFile))
        assert(!FileTypes.isFontFile(videoFile))
        assert(!FileTypes.isImageFile(videoFile))
        assert(!FileTypes.isTextFile(videoFile))
        assert(!FileTypes.isUnknownFile(videoFile))
        assert(FileTypes.isVideoFile(videoFile))
        assert(!FileTypes.isXmlFile(videoFile))
        val fileType = FileTypes.getFileType(videoFile)
        assert(fileType == FileType.Video)
      }
  }

  test("test xml extensions") {
    """atom atomcat atomsrv bdsproj config csproj davmount dbproj docx dotx
      |fsproj fxml jhm jnlp kml mm pom potx ppsx pptx qrc rdf resx rng rss
      |settings sldx stc std sti stw svgz sxc sxd sxg sxi stw sxm sxw tld
      |vbproj vcproj vdproj wadl wsdd wsdl x3d xaml xjb xlsx xltx xml
      |xps xsd xsl xslt xspf xul""".stripMargin.split(whitespaceRegex).foreach { ext =>
        val xmlFile = Paths.get("xmlfile." + ext)
        // println("xmlFile: " + xmlFile)
        assert(!FileTypes.isArchiveFile(xmlFile))
        assert(!FileTypes.isAudioFile(xmlFile))
        assert(!FileTypes.isBinaryFile(xmlFile))
        assert(!FileTypes.isCodeFile(xmlFile))
        assert(!FileTypes.isFontFile(xmlFile))
        assert(!FileTypes.isImageFile(xmlFile))
        assert(FileTypes.isTextFile(xmlFile))
        assert(!FileTypes.isUnknownFile(xmlFile))
        assert(!FileTypes.isVideoFile(xmlFile))
        assert(FileTypes.isXmlFile(xmlFile))
        val fileType = FileTypes.getFileType(xmlFile)
        assert(fileType == FileType.Xml)
    }
  }
}
