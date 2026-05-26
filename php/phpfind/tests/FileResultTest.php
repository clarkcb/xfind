<?php

declare(strict_types=1);

use PHPUnit\Framework\TestCase;

use phpfind\FileResult;
use phpfind\FileType;

class FileResultTest extends TestCase
{
    public function test_file_result_abs_path(): void
    {
        $home = getenv('HOME');
        $file_path = "$home/src/xfind/php/phpfind/src/phpfind/FileResult.php";
        $file_type = FileType::Code;
        $file_result = new FileResult($file_path, $file_type, 0, 0);
        $this->assertEquals("$home/src/xfind/php/phpfind/src/phpfind/FileResult.php",
            $file_result->file_path);
    }

    public function test_file_result_rel_path1(): void
    {
        $file_path = './FileResult.php';
        $file_type = FileType::Code;
        $file_result = new FileResult($file_path, $file_type, 0, 0);
        $this->assertEquals('./FileResult.php', $file_result->file_path);
    }

    public function test_file_result_rel_path2(): void
    {
        $file_path = '../FileResult.php';
        $file_type = FileType::Code;
        $file_result = new FileResult($file_path, $file_type, 0, 0);
        $this->assertEquals('../FileResult.php', $file_result->file_path);
    }
}
