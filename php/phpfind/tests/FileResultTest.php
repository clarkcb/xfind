<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpfind\FileResult;
use phpfind\FileType;

class FileResultTest extends TestCase
{
    public function test_fileresult_abs_path(): void
    {
        $home = getenv('HOME');
        $path = "$home/src/xfind/php/phpfind/src/phpfind";
        $filename = 'FileResult.php';
        $fileresult = new FileResult($path, $filename, FileType::Code, false);
        $this->assertEquals("$home/src/xfind/php/phpfind/src/phpfind/FileResult.php",
            $fileresult->filepath());
    }

    public function test_fileresult_rel_path1(): void
    {
        $path = '.';
        $filename = 'FileResult.php';
        $fileresult = new FileResult($path, $filename, FileType::Code, false);
        $this->assertEquals('./FileResult.php', $fileresult->filepath());
    }

    public function test_fileresult_rel_path2(): void
    {
        $path = './';
        $filename = 'FileResult.php';
        $fileresult = new FileResult($path, $filename, FileType::Code, false);
        $this->assertEquals('./FileResult.php', $fileresult->filepath());
    }

    public function test_fileresult_rel_path3(): void
    {
        $path = '..';
        $filename = 'FileResult.php';
        $fileresult = new FileResult($path, $filename, FileType::Code, false);
        $this->assertEquals('../FileResult.php', $fileresult->filepath());
    }
}
