<?php

declare(strict_types=1);

use PHPUnit\Framework\TestCase;

use phpfind\FileType;
use phpfind\FileTypes;

class FileTypesTest extends TestCase
{
    /**
     * @var FileTypes
     */
    private FileTypes $file_types;

    public function __construct()
    {
        parent::__construct();
        $this->file_types = new FileTypes();
    }

    public function test_get_file_type_archive_file(): void
    {
        $file_name = 'archive.zip';
        $this->assertTrue($this->file_types->is_archive($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Archive, $file_type);
    }

    public function test_get_file_type_audio_file(): void
    {
        $file_name = 'music.mp3';
        $this->assertTrue($this->file_types->is_audio($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Audio, $file_type);
    }

    public function test_get_file_type_binary_file(): void
    {
        $file_name = 'binary.exe';
        $this->assertTrue($this->file_types->is_binary($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Binary, $file_type);
    }

    public function test_get_file_type_code_file(): void
    {
        $file_name = 'code.php';
        $this->assertTrue($this->file_types->is_code($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Code, $file_type);
    }

    public function test_get_file_type_font_file(): void
    {
        $file_name = 'font.ttf';
        $this->assertTrue($this->file_types->is_font($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Font, $file_type);
    }

    public function test_get_file_type_image_file(): void
    {
        $file_name = 'image.png';
        $this->assertTrue($this->file_types->is_image($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Image, $file_type);
    }

    public function test_get_file_type_text_file(): void
    {
        $file_name = 'text.txt';
        $this->assertTrue($this->file_types->is_text($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Text, $file_type);
    }

    public function test_get_file_type_video_file(): void
    {
        $file_name = 'movie.mp4';
        $this->assertTrue($this->file_types->is_video($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Video, $file_type);
    }

    public function test_get_file_type_xml_file(): void
    {
        $file_name = 'content.xml';
        $this->assertTrue($this->file_types->is_xml($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Xml, $file_type);
    }

    public function test_get_file_type_unknown_file(): void
    {
        $file_name = 'unknown.xyz';
        $this->assertTrue($this->file_types->is_unknown($file_name));
        $file_type = $this->file_types->get_file_type($file_name);
        $this->assertEquals(FileType::Unknown, $file_type);
    }
}
