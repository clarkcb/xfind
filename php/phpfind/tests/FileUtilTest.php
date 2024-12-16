<?php

declare(strict_types=1);

use PHPUnit\Framework\TestCase;

use phpfind\FileUtil;

class FileUtilTest extends TestCase
{
    /***************************************************************************
     * expand_path tests
     **************************************************************************/
    public function test_expand_path_tilde(): void
    {
        $path = '~';
        $expected = $_SERVER['HOME'];
        $this->assertEquals($expected, FileUtil::expand_path($path));
    }

    public function test_expand_path_with_tilde(): void
    {
        $path = '~/src/xfind';
        $expected = FileUtil::join_paths($_SERVER['HOME'], 'src/xfind');
        $this->assertEquals($expected, FileUtil::expand_path($path));
    }

    public function test_expand_path_with_tilde_and_name(): void
    {
        $path = '~cary/src/xfind';
        $expected = FileUtil::join_paths($_SERVER['HOME'], 'src/xfind');
        $this->assertEquals($expected, FileUtil::expand_path($path));
    }

    /***************************************************************************
     * get_extension tests
     **************************************************************************/
    public function test_get_extension_has_txt_extension(): void
    {
        $file_name = 'filename.txt';
        $this->assertEquals("txt", FileUtil::get_extension($file_name));
    }

    public function test_get_extension_missing_extension(): void
    {
        $file_name = 'filename.';
        $this->assertEquals("", FileUtil::get_extension($file_name));
    }

    public function test_get_extension_no_extension(): void
    {
        $file_name = 'filename';
        $this->assertEquals("", FileUtil::get_extension($file_name));
    }

    public function test_get_extension_hidden_txt_extension(): void
    {
        $file_name = '.filename.txt';
        $this->assertEquals("txt", FileUtil::get_extension($file_name));
    }

    public function test_get_extension_hidden_missing_extension(): void
    {
        $file_name = '.filename.';
        $this->assertEquals("", FileUtil::get_extension($file_name));
    }

    public function test_get_extension_hidden_no_extension(): void
    {
        $file_name = '.filename';
        $this->assertEquals("", FileUtil::get_extension($file_name));
    }

    /***************************************************************************
     * is_dot_dir tests
     **************************************************************************/
    public function test_is_dot_dir_single_dot(): void
    {
        $file_name = '.';
        $this->assertTrue(FileUtil::is_dot_dir($file_name));
    }

    public function test_is_dot_dir_double_dot(): void
    {
        $file_name = '..';
        $this->assertTrue(FileUtil::is_dot_dir($file_name));
    }

    public function test_is_dot_dir_non_dot_dir(): void
    {
        $file_name = '.git';
        $this->assertFalse(FileUtil::is_dot_dir($file_name));
    }

    /***************************************************************************
     * is_hidden tests
     **************************************************************************/
    public function test_is_hidden_hidden_file(): void
    {
        $file_name = '.filename.txt';
        $this->assertTrue(FileUtil::is_hidden($file_name));
    }

    public function test_is_hidden_not_hidden_file(): void
    {
        $file_name = 'filename.txt';
        $this->assertFalse(FileUtil::is_hidden($file_name));
    }

    public function test_is_hidden_single_dot(): void
    {
        $file_name = '.';
        $this->assertFalse(FileUtil::is_hidden($file_name));
    }

    public function test_is_hidden_double_dot(): void
    {
        $file_name = '..';
        $this->assertFalse(FileUtil::is_hidden($file_name));
    }

    /***************************************************************************
     * join_path tests
     **************************************************************************/
    public function test_join_path_forward_slashes(): void
    {
        // $path = '/path/to/nowhere';
        $path = DIRECTORY_SEPARATOR . implode(DIRECTORY_SEPARATOR, ['path', 'to', 'nowhere']);
        $file = 'nowhere.txt';
        $joined = FileUtil::join_paths($path, $file);
        $this->assertEquals('/path/to/nowhere/nowhere.txt', $joined);
    }

    public function test_join_path_no_slashes(): void
    {
        $path = 'nowhere';
        $file = 'nowhere.txt';
        $expected = implode(DIRECTORY_SEPARATOR, ['nowhere', 'nowhere.txt']);
        $joined = FileUtil::join_paths($path, $file);
        $this->assertEquals($expected, $joined);
    }

    /***************************************************************************
     * normalize_path tests
     **************************************************************************/
    public function test_normalize_path_no_trailing_slash(): void
    {
        // $path = '/path/to/nowhere';
        $path = DIRECTORY_SEPARATOR . implode(DIRECTORY_SEPARATOR, ['path', 'to', 'nowhere']);
        $this->assertEquals(FileUtil::normalize_path($path), $path);
    }

    public function test_normalize_path_trailing_slash(): void
    {
        // $path = '/path/to/nowhere/';
        $path = DIRECTORY_SEPARATOR . implode(DIRECTORY_SEPARATOR, ['path', 'to', 'nowhere']);
        $this->assertEquals(
            $path,
            FileUtil::normalize_path($path . DIRECTORY_SEPARATOR)
        );
    }

    /***************************************************************************
     * split_path tests
     **************************************************************************/
    public function test_split_path_no_sep(): void
    {
        $path = 'nowhere';
        $split = FileUtil::split_path($path);
        $this->assertCount(1, $split);
        $this->assertEquals('nowhere', $split[0]);
    }

    public function test_split_path_slashes(): void
    {
        // $path = '/path/to/nowhere';
        $path = DIRECTORY_SEPARATOR . implode(DIRECTORY_SEPARATOR, ['path', 'to', 'nowhere']);
        $split = FileUtil::split_path($path);
        $this->assertCount(4, $split);
        $this->assertEquals('', $split[0]);
        $this->assertEquals('path', $split[1]);
        $this->assertEquals('to', $split[2]);
        $this->assertEquals('nowhere', $split[3]);
    }
}
