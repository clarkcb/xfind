<?php declare(strict_types=1);
use PHPUnit\Framework\TestCase;

require_once __DIR__ . '/../src/autoload.php';

use phpfind\FileType;
use phpfind\FindFile;

class FindFileTest extends TestCase
{
    public function test_findfile_abs_path()
    {
        $path = '/Users/cary/src/xfind/php/phpfind/src';
        $filename = 'findfile.php';
        $findfile = new FindFile($path, $filename, FileType::Code);
        $this->assertEquals('/Users/cary/src/xfind/php/phpfind/src/findfile.php',
            $findfile->filepath());
    }

    public function test_findfile_rel_path1()
    {
        $path = '.';
        $filename = 'findfile.php';
        $findfile = new FindFile($path, $filename, FileType::Code);
        $this->assertEquals('./findfile.php', $findfile->filepath());
    }

    public function test_findfile_rel_path2()
    {
        $path = './';
        $filename = 'findfile.php';
        $findfile = new FindFile($path, $filename, FileType::Code);
        $this->assertEquals('./findfile.php', $findfile->filepath());
    }

    public function test_findfile_rel_path3()
    {
        $path = '..';
        $filename = 'findfile.php';
        $findfile = new FindFile($path, $filename, FileType::Code);
        $this->assertEquals('../findfile.php', $findfile->filepath());
    }
}
