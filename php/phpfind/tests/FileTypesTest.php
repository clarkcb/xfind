<?php declare(strict_types=1);

use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpfind\FileType;
use phpfind\FileTypes;

class FileTypesTest extends TestCase
{
    /**
     * @var FileTypes
     */
    private FileTypes $filetypes;

    public function __construct()
    {
        parent::__construct();
        $this->filetypes = new FileTypes();
    }

    public function test_getfiletype_archive_file(): void
    {
        $filename = 'archive.zip';
        $this->assertEquals(true, $this->filetypes->is_archive($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Archive, $filetype);
    }

    public function test_getfiletype_binary_file(): void
    {
        $filename = 'binary.exe';
        $this->assertEquals(true, $this->filetypes->is_binary($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Binary, $filetype);
    }

    public function test_getfiletype_code_file(): void
    {
        $filename = 'code.php';
        $this->assertEquals(true, $this->filetypes->is_code($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Code, $filetype);
    }

    public function test_getfiletype_text_file(): void
    {
        $filename = 'text.txt';
        $this->assertEquals(true, $this->filetypes->is_text($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Text, $filetype);
    }

    public function test_getfiletype_xml_file(): void
    {
        $filename = 'content.xml';
        $this->assertEquals(true, $this->filetypes->is_xml($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Xml, $filetype);
    }

    public function test_getfiletype_unknown_file(): void
    {
        $filename = 'unknown.xyz';
        $this->assertEquals(true, $this->filetypes->is_unknown($filename));
        $filetype = $this->filetypes->get_filetype($filename);
        $this->assertEquals(FileType::Unknown, $filetype);
    }
}
