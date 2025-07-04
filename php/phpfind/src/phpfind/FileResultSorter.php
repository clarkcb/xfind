<?php

declare(strict_types=1);

namespace phpfind;

class FileResultSorter
{
    private FindSettings $settings;

    public function __construct(FindSettings $settings)
    {
        $this->settings = $settings;
    }

    /**
     * @param FileResult $fr1
     * @param FileResult $fr2
     * @return int
     */
    public function cmp_file_result_path(FileResult $fr1, FileResult $fr2): int
    {
        [$path1, $path2] = $this->settings->sort_case_insensitive ?
            array(strtolower($fr1->path), strtolower($fr2->path)) :
            array($fr1->path, $fr2->path);
        if ($path1 == $path2) {
            [$file_name1, $file_name2] = $this->settings->sort_case_insensitive ?
                array(strtolower($fr1->file_name), strtolower($fr2->file_name)) :
                array($fr1->file_name, $fr2->file_name);
            if ($file_name1 == $file_name2) return 0;
            return ($file_name1 < $file_name2) ? -1 : 1;
        }
        return ($path1 < $path2) ? -1 : 1;
    }

    /**
     * @param FileResult $fr1
     * @param FileResult $fr2
     * @return int
     */
    public function cmp_file_result_file_name(FileResult $fr1, FileResult $fr2): int
    {
        [$file_name1, $file_name2] = $this->settings->sort_case_insensitive ?
            array(strtolower($fr1->file_name), strtolower($fr2->file_name)) :
            array($fr1->file_name, $fr2->file_name);
        if ($file_name1 == $file_name2) {
            [$path1, $path2] = $this->settings->sort_case_insensitive ?
                array(strtolower($fr1->path), strtolower($fr2->path)) :
                array($fr1->path, $fr2->path);
            if ($path1 == $path2) return 0;
            return ($path1 < $path2) ? -1 : 1;
        }
        return ($file_name1 < $file_name2) ? -1 : 1;
    }

    /**
     * @param FileResult $fr1
     * @param FileResult $fr2
     * @return int
     */
    public function cmp_file_result_file_size(FileResult $fr1, FileResult $fr2): int
    {
        if ($fr1->file_size == $fr2->file_size) {
            return $this->cmp_file_result_path($fr1, $fr2);
        }
        return ($fr1->file_size < $fr2->file_size) ? -1 : 1;
    }

    /**
     * @param FileType $ft1
     * @param FileType $ft2
     * @return int
     */
    private function cmp_file_type(FileType $ft1, FileType $ft2): int
    {
        $file_type_cases = FileType::cases();
        return array_search($ft1, $file_type_cases) - array_search($ft2, $file_type_cases);
    }

    /**
     * @param FileResult $fr1
     * @param FileResult $fr2
     * @return int
     */
    public function cmp_file_result_file_type(FileResult $fr1, FileResult $fr2): int
    {
        $ftcmp = $this->cmp_file_type($fr1->file_type, $fr2->file_type);
        if ($ftcmp == 0) {
            return $this->cmp_file_result_path($fr1, $fr2);
        }
        return $ftcmp;
    }

    /**
     * @param FileResult $fr1
     * @param FileResult $fr2
     * @return int
     */
    public function cmp_file_result_last_mod(FileResult $fr1, FileResult $fr2): int
    {
        if ($fr1->last_mod == $fr2->last_mod) {
            return $this->cmp_file_result_path($fr1, $fr2);
        }
        return ($fr1->last_mod < $fr2->last_mod) ? -1 : 1;
    }

    public function get_cmp_function(): \Closure
    {
        if ($this->settings->sort_descending) {
            return match ($this->settings->sort_by) {
                SortBy::Filename => fn(FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_file_name(
                    $fr2,
                    $fr1
                ),
                SortBy::Filesize => fn(FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_file_size(
                    $fr2,
                    $fr1
                ),
                SortBy::Filetype => fn(FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_file_type(
                    $fr2,
                    $fr1
                ),
                SortBy::LastMod => fn(FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_last_mod($fr2, $fr1),
                default => fn(FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_path($fr2, $fr1),
            };
        }
        return match ($this->settings->sort_by) {
            SortBy::Filename => fn(FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_file_name($fr1, $fr2),
            SortBy::Filesize => fn(FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_file_size($fr1, $fr2),
            SortBy::Filetype => fn(FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_file_type($fr1, $fr2),
            SortBy::LastMod => fn(FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_last_mod($fr1, $fr2),
            default => fn(FileResult $fr1, FileResult $fr2) => $this->cmp_file_result_path($fr1, $fr2),
        };
    }

    /**
     * @param FileResult[] $file_results
     * @return FileResult[]
     */
    public function sort(array $file_results): array
    {
        $cmp_function = $this->get_cmp_function();
        usort($file_results, $cmp_function);
        return $file_results;
    }
}
