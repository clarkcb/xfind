<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpfind\FileResult;
use phpfind\FileType;

class FileResultTest extends TestCase
{
    public function test_file_result_abs_path(): void
    {
        $home = getenv('HOME');
        $path = "$home/src/xfind/php/phpfind/src/phpfind";
        $file_name = 'FileResult.php';
        $file_type = FileType::Code;
        $file_result = new FileResult($path, $file_name, $file_type, 0, 0);
        $this->assertEquals("$home/src/xfind/php/phpfind/src/phpfind/FileResult.php",
            $file_result->file_path());
    }

    public function test_file_result_rel_path1(): void
    {
        $path = '.';
        $file_name = 'FileResult.php';
        $file_type = FileType::Code;
        $file_result = new FileResult($path, $file_name, $file_type, 0, 0);
        $this->assertEquals('./FileResult.php', $file_result->file_path());
    }

    public function test_file_result_rel_path2(): void
    {
        $path = './';
        $file_name = 'FileResult.php';
        $file_type = FileType::Code;
        $file_result = new FileResult($path, $file_name, $file_type, 0, 0);
        $this->assertEquals('./FileResult.php', $file_result->file_path());
    }

    public function test_file_result_rel_path3(): void
    {
        $path = '..';
        $file_name = 'FileResult.php';
        $file_type = FileType::Code;
        $file_result = new FileResult($path, $file_name, $file_type, 0, 0);
        $this->assertEquals('../FileResult.php', $file_result->file_path());
    }
}
