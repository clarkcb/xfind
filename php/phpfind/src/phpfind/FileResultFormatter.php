<?php

declare(strict_types=1);

namespace phpfind;

class FileResultFormatter
{
    private FindSettings $settings;
    private \Closure $cl_format_dir;
    private \Closure $cl_format_file_name;

    public function __construct(FindSettings $settings)
    {
        $this->settings = $settings;
        # Define dir and file_name formatting closures depending on colorize and matches
        if ($settings->colorize && count($settings->in_dir_patterns) > 0) {
            $this->cl_format_dir = function (string $dir): string { return $this->format_dir_with_color($dir); };
        } else {
            $this->cl_format_dir = function (string $dir): string { return $dir; };
        }
        if ($settings->colorize && (count($settings->in_extensions) > 0 || count($settings->in_file_patterns) > 0)) {
            $this->cl_format_file_name = function (string $file_name): string { return $this->format_file_name_with_color($file_name); };
        } else {
            $this->cl_format_file_name = function (string $file_name): string { return $file_name; };
        }
    }

    public function colorize(string $s, int $match_start_index, int $match_end_index): string
    {
        $prefix = '';
        if ($match_start_index > 0) {
            $prefix = substr($s, 0, $match_start_index);
        }
        $suffix = '';
        if ($match_end_index < strlen($s)) {
            $suffix = substr($s, $match_end_index);
        }
        $match_length = $match_end_index - $match_start_index;
        return $prefix .
            Color::Green->value .
            substr($s, $match_start_index, $match_length) .
            Color::Reset->value .
            $suffix;
    }

    private function format_dir_with_color(string $dir): string
    {
        $formatted_dir = '.';
        if ($dir) {
            $formatted_dir = $dir;
            foreach ($this->settings->in_dir_patterns as $p) {
                $pattern = '/' . $p . '/';
                preg_match($pattern, $formatted_dir, $matches, PREG_OFFSET_CAPTURE);
                if (!empty($matches)) {
                    $start_index = (int)$matches[0][1];
                    $end_index = $start_index + strlen($matches[0][0]);
                    $formatted_dir = $this->colorize($formatted_dir, $start_index, $end_index);
                    break;
                }
            }
        }
        return $formatted_dir;
    }

    public function format_dir(string $dir): string
    {
        // call the closure as defined in constructor
        return ($this->cl_format_dir)($dir);
    }

    private function format_file_name_with_color(string $file_name): string
    {
        $formatted_file_name = $file_name;
        foreach ($this->settings->in_file_patterns as $p) {
            $pattern = '/' . $p . '/';
            preg_match($pattern, $formatted_file_name, $matches, PREG_OFFSET_CAPTURE);
            if (!empty($matches)) {
                $start_index = (int)$matches[0][1];
                $end_index = $start_index + strlen($matches[0][0]);
                $formatted_file_name = $this->colorize($formatted_file_name, $start_index, $end_index);
                break;
            }
        }
        if ($this->settings->in_extensions) {
            $dot_idx = strrpos($formatted_file_name, '.');
            $file_name_length = strlen($formatted_file_name);
            if ($dot_idx !== false && $dot_idx > 0 && $dot_idx < $file_name_length - 1) {
                $formatted_file_name = $this->colorize($formatted_file_name, $dot_idx + 1, $file_name_length);
            }
        }
        return $formatted_file_name;
    }

    public function format_file_name(string $file_name): string
    {
        // call the closure as defined in constructor
        return ($this->cl_format_file_name)($file_name);
    }

    public function format_file_result(FileResult $result): string
    {
        $formatted_dir = $this->format_dir($result->path);
        $formatted_file_name = $this->format_file_name($result->file_name);
        return FileUtil::join_paths($formatted_dir, $formatted_file_name);
    }
}
