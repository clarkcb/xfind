<?php

class SearchResultTest extends PHPUnit_Framework_TestCase {
    const CSSEARCHPATH = '~/src/xsearch/csharp/CsSearch/CsSearch';
    public function test_singleline_searchresult() {
        $pattern = "Search";
        $file = self::CSSEARCHPATH . "/Searcher.cs";
        $linenum = 10;
        $match_start_index = 15;
        $match_end_index = 23;
        $line = "\tpublic class Searcher\n";
        $linesbefore = [];
        $linesafter = [];
        $searchresult = new SearchResult($pattern, $file, $linenum,
            $match_start_index, $match_end_index, $line, $linesbefore, $linesafter);
        $expectedoutput = sprintf("%s: %d: [%d:%d]: %s", $file,
            $linenum, $match_start_index, $match_end_index, trim($line));
        $this->assertEquals($expectedoutput, "$searchresult");
    }

    public function test_binaryfile_searchresult() {
        $pattern = "Search";
        $file = self::CSSEARCHPATH . "/Searcher.exe";
        $linenum = 0;
        $match_start_index = 0;
        $match_end_index = 0;
        $line = null;
        $linesbefore = [];
        $linesafter = [];
        $searchresult = new SearchResult($pattern, $file, $linenum,
            $match_start_index, $match_end_index, $line, $linesbefore, $linesafter);
        $expectedoutput = sprintf("%s matches at [%d:%d]", $file, $match_start_index, $match_end_index);
        $this->assertEquals($expectedoutput, "$searchresult");
    }

    public function test_multiline_searchresult() {
        $pattern = "Search";
        $file = self::CSSEARCHPATH . "/Searcher.cs";
        $linenum = 10;
        $match_start_index = 15;
        $match_end_index = 23;
        $line = "\tpublic class Searcher\n";
        $linesbefore = ["namespace CsSearch\n", "{\n"];
        $linesafter = ["\t{\n", "\t\tprivate readonly FileTypes _fileTypes;\n"];
        $searchresult = new SearchResult($pattern, $file, $linenum,
            $match_start_index, $match_end_index, $line, $linesbefore, $linesafter);
        $outputtemplate = 
            "================================================================================\n" .
            "%s: %d: [%d:%d]\n" .
            "--------------------------------------------------------------------------------\n" .
            "   8 | namespace CsSearch\n" .
            "   9 | {\n" .
            "> 10 | \tpublic class Searcher\n" .
            "  11 | \t{\n" .
            "  12 | \t\tprivate readonly FileTypes _fileTypes;\n";
        $expectedoutput = sprintf($outputtemplate, $file, $linenum,
            $match_start_index, $match_end_index);
        $this->assertEquals($expectedoutput, "$searchresult");
    }
}

?>