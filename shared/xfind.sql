PRAGMA foreign_keys=OFF;
BEGIN TRANSACTION;

--
-- Table: file_type
--
CREATE TABLE IF NOT EXISTS file_type (
    id INTEGER PRIMARY KEY,
    name TEXT NOT NULL UNIQUE
);

INSERT INTO file_type
VALUES
    (1,  'Unknown'),
    (2,  'Archive'),
    (3,  'Audio'),
    (4,  'Binary'),
    (5,  'Code'),
    (6,  'Font'),
    (7,  'Image'),
    (8,  'Text'),
    (9,  'Video'),
    (10, 'Xml')
ON CONFLICT DO NOTHING;


--
-- Table: file_name
--
CREATE TABLE IF NOT EXISTS file_name (
    id INTEGER PRIMARY KEY,
    file_type_id INTEGER NOT NULL,
    name TEXT NOT NULL UNIQUE,
    description TEXT,
    FOREIGN KEY(file_type_id) REFERENCES file_type(id)
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_file_name_name ON file_name(name);

INSERT INTO file_name (file_type_id, name)
VALUES
    (5, '.bash_history'),
    (5, '.bash_profile'),
    (5, '.bashrc'),
    (5, '.bzrignore'),
    (5, '.dockerignore'),
    (5, '.editorconfig'),
    (5, '.env'),
    (5, '.gitattributes'),
    (5, '.gitconfig'),
    (5, '.gitignore'),
    (5, '.gitkeep'),
    (5, '.profile'),
    (5, '.viminfo'),
    (5, '.zprofile'),
    (5, '.zsh_history'),
    (5, '.zshenv'),
    (5, '.zshrc'),
    (5, 'CMakeLists.txt'),
    (5, 'Dockerfile'),
    (5, 'Gemfile'),
    (5, 'Jenkinsfile'),
    (5, 'Makefile'),
    (5, 'MANIFEST'),
    (5, 'mime.types'),
    (5, 'Pipfile'),
    (5, 'PKG-INFO'),
    (5, 'Rakefile'),
    (5, 'Vagrantfile'),
    (8, 'AUTHORS'),
    (8, 'LICENSE'),
    (8, 'README');


--
-- Table: file_extension
--
CREATE TABLE IF NOT EXISTS file_extension (
    id INTEGER PRIMARY KEY,
    file_type_id INTEGER NOT NULL,
    extension TEXT NOT NULL UNIQUE,
    description TEXT,
    FOREIGN KEY(file_type_id) REFERENCES file_type(id)
);

CREATE UNIQUE INDEX IF NOT EXISTS idx_file_extension_extension ON file_extension(extension);

INSERT INTO file_extension (file_type_id, extension, description)
VALUES
    (2, '7z', '7-Zip format'),
    (2, 'ace', NULL),
    (2, 'air', NULL),
    (2, 'ane', NULL),
    (2, 'apk', 'Android package'),
    (2, 'appx', NULL),
    (2, 'arc', NULL),
    (2, 'arj', NULL),
    (2, 'ark', NULL),
    (2, 'ass', NULL),
    (2, 'b1', NULL),
    (2, 'bcpio', NULL),
    (2, 'bin', NULL),
    (2, 'bke', NULL),
    (2, 'bkf', NULL),
    (2, 'boz', NULL),
    (2, 'bz', NULL),
    (2, 'bz2', 'Bzip2 format'),
    (2, 'cab', 'Windows Cabinet file'),
    (2, 'cfs', NULL),
    (2, 'cpio', NULL),
    (2, 'deb', NULL),
    (2, 'dgc', NULL),
    (2, 'dmg', NULL),
    (2, 'ear', NULL),
    (2, 'egg', NULL),
    (2, 'esd', NULL),
    (2, 'ez3', NULL),
    (2, 'gca', NULL),
    (2, 'gem', NULL),
    (2, 'gtar', NULL),
    (2, 'gz', NULL),
    (2, 'hqx', NULL),
    (2, 'ipa', NULL),
    (2, 'ipg', NULL),
    (2, 'ipk', NULL),
    (2, 'iso', 'ISO-9660 disc image'),
    (2, 'jar', 'Java archive file'),
    (2, 'joda', NULL),
    (2, 'lbr', NULL),
    (2, 'lha', NULL),
    (2, 'lqr', NULL),
    (2, 'lz', NULL),
    (2, 'lzh', NULL),
    (2, 'lzma', NULL),
    (2, 'lzop', NULL),
    (2, 'lzr', NULL),
    (2, 'maff', NULL),
    (2, 'mbw', NULL),
    (2, 'mgp', NULL),
    (2, 'mpq', NULL),
    (2, 'msi', 'Microsoft installer file'),
    (2, 'oar', NULL),
    (2, 'obr', NULL),
    (2, 'pax', NULL),
    (2, 'phar', NULL),
    (2, 'pyz', NULL),
    (2, 'pyzw', NULL),
    (2, 'rar', NULL),
    (2, 'rpm', NULL),
    (2, 'shar', NULL),
    (2, 'sit', 'StuffIt archive file'),
    (2, 'sitx', 'StuffIt archive file'),
    (2, 'smzip', NULL),
    (2, 'sq', NULL),
    (2, 'sqx', NULL),
    (2, 'ssa', NULL),
    (2, 'swc', NULL),
    (2, 'tao', NULL),
    (2, 'tar', NULL),
    (2, 'tgz', NULL),
    (2, 'udeb', NULL),
    (2, 'war', 'Java Web Archive file'),
    (2, 'whl', NULL),
    (2, 'wim', NULL),
    (2, 'xap', NULL),
    (2, 'xz', NULL),
    (2, 'Z', NULL),
    (2, 'zip', 'Zip archive file'),
    (2, 'zipx', 'Zip archive file'),
    (2, 'zoo', NULL),
    (3, 'aac', 'Windows audio file'),
    (3, 'adt', 'Windows audio file'),
    (3, 'adts', 'Windows audio file'),
    (3, 'aif', 'Audio Interchange File format file'),
    (3, 'aifc', 'Audio Interchange File format file'),
    (3, 'aiff', 'Audio Interchange File format file'),
    (3, 'cda', 'CD Audio Track'),
    (3, 'm4a', 'MPEG-4 audio file'),
    (3, 'mid', 'MIDI audio file'),
    (3, 'midi', 'MIDI audio file'),
    (3, 'mp3', 'MPEG layer 3 audio file'),
    (3, 'oga', NULL),
    (3, 'opus', NULL),
    (3, 'wav', 'Wave audio file'),
    (3, 'weba', NULL),
    -- (4, '123', NULL),
    (4, 'a', NULL),
    (4, 'aab', NULL),
    (4, 'aam', NULL),
    (4, 'aas', NULL),
    (4, 'abw', NULL),
    (4, 'ac', NULL),
    (4, 'acc', NULL),
    (4, 'accdb', 'Microsoft Access database file'),
    (4, 'accde', 'Microsoft Access execute-only file'),
    (4, 'accdr', 'Microsoft Access runtime database'),
    (4, 'accdt', 'Microsoft Access database template'),
    (4, 'acu', NULL),
    (4, 'acutc', NULL),
    (4, 'aep', NULL),
    (4, 'afp', NULL),
    (4, 'ahead', NULL),
    (4, 'ai', NULL),
    (4, 'ait', NULL),
    (4, 'ami', NULL),
    (4, 'application', NULL),
    (4, 'apr', NULL),
    (4, 'aso', NULL),
    (4, 'atc', NULL),
    (4, 'atx', NULL),
    (4, 'aw', NULL),
    (4, 'azf', NULL),
    (4, 'azs', NULL),
    (4, 'azw', NULL),
    (4, 'bdm', NULL),
    (4, 'beam', NULL),
    (4, 'bed', NULL),
    (4, 'bh2', NULL),
    (4, 'bin', NULL),
    (4, 'blb', NULL),
    (4, 'blorb', NULL),
    (4, 'bmi', NULL),
    (4, 'book', NULL),
    (4, 'box', NULL),
    (4, 'bpk', NULL),
    (4, 'c11amc', NULL),
    (4, 'c11amz', NULL),
    (4, 'c4d', NULL),
    (4, 'c4f', NULL),
    (4, 'c4g', NULL),
    (4, 'c4p', NULL),
    (4, 'c4u', NULL),
    (4, 'cap', NULL),
    (4, 'car', NULL),
    (4, 'cat', NULL),
    (4, 'cb7', NULL),
    (4, 'cba', NULL),
    (4, 'cbr', NULL),
    (4, 'cbt', NULL),
    (4, 'cbz', NULL),
    (4, 'cct', NULL),
    (4, 'cdbcmsg', NULL),
    (4, 'cdf', NULL),
    (4, 'cdkey', NULL),
    (4, 'cdmia', NULL),
    (4, 'cdmic', NULL),
    (4, 'cdmid', NULL),
    (4, 'cdmio', NULL),
    (4, 'cdmiq', NULL),
    (4, 'cdy', NULL),
    (4, 'cer', NULL),
    (4, 'chat', NULL),
    (4, 'chm', NULL),
    (4, 'chrt', NULL),
    (4, 'cii', NULL),
    (4, 'cil', NULL),
    (4, 'cla', NULL),
    (4, 'class', 'Java class file'),
    (4, 'clkk', NULL),
    (4, 'clkp', NULL),
    (4, 'clkt', NULL),
    (4, 'clkw', NULL),
    (4, 'clkx', NULL),
    (4, 'clp', NULL),
    (4, 'cmc', NULL),
    (4, 'cmp', NULL),
    (4, 'cod', NULL),
    (4, 'com', NULL),
    (4, 'cpt', NULL),
    (4, 'crd', NULL),
    (4, 'crl', NULL),
    (4, 'crt', NULL),
    (4, 'cryptonote', NULL),
    (4, 'csp', NULL),
    (4, 'cst', NULL),
    (4, 'cu', NULL),
    (4, 'cww', NULL),
    (4, 'cxt', NULL),
    (4, 'daf', NULL),
    (4, 'dat', NULL),
    (4, 'dataless', NULL),
    (4, 'dbmdl', NULL),
    (4, 'dcr', NULL),
    (4, 'ddd', NULL),
    (4, 'deploy', NULL),
    (4, 'der', NULL),
    (4, 'dfac', NULL),
    (4, 'dir', NULL),
    (4, 'dis', NULL),
    (4, 'dist', NULL),
    (4, 'distz', NULL),
    (4, 'dll', 'Dynamic Link Library file'),
    (4, 'dmp', NULL),
    (4, 'dms', NULL),
    (4, 'dna', NULL),
    (4, 'doc', NULL),
    (4, 'docm', NULL),
    (4, 'dot', NULL),
    (4, 'dotm', NULL),
    (4, 'dp', NULL),
    (4, 'dpg', NULL),
    (4, 'dssc', NULL),
    (4, 'dump', NULL),
    (4, 'dvi', NULL),
    (4, 'dxp', NULL),
    (4, 'dxr', NULL),
    (4, 'dylib', NULL),
    (4, 'edm', NULL),
    (4, 'edx', NULL),
    (4, 'efif', NULL),
    (4, 'ei6', NULL),
    (4, 'elc', NULL),
    (4, 'emf', NULL),
    (4, 'eml', NULL),
    (4, 'emz', NULL),
    (4, 'epub', NULL),
    (4, 'esa', NULL),
    (4, 'esf', NULL),
    (4, 'eva', NULL),
    (4, 'evy', NULL),
    (4, 'exe', 'Executable program file'),
    (4, 'exi', NULL),
    (4, 'ext', NULL),
    (4, 'ez', NULL),
    (4, 'ez2', NULL),
    (4, 'fcdt', NULL),
    (4, 'fcs', NULL),
    (4, 'fdf', NULL),
    (4, 'fe_launch', NULL),
    (4, 'fg5', NULL),
    (4, 'fgd', NULL),
    (4, 'fig', NULL),
    (4, 'flo', NULL),
    (4, 'flw', NULL),
    (4, 'fm', NULL),
    (4, 'fnc', NULL),
    (4, 'frame', NULL),
    (4, 'fsc', NULL),
    (4, 'ftc', NULL),
    (4, 'fti', NULL),
    (4, 'fxp', NULL),
    (4, 'fxpl', NULL),
    (4, 'fzs', NULL),
    (4, 'g2w', NULL),
    (4, 'g3w', NULL),
    (4, 'gac', NULL),
    (4, 'gam', NULL),
    (4, 'gbr', NULL),
    (4, 'geo', NULL),
    (4, 'gex', NULL),
    (4, 'ggb', NULL),
    (4, 'ggt', NULL),
    (4, 'ghf', NULL),
    (4, 'gim', NULL),
    (4, 'gmx', NULL),
    (4, 'gnumeric', NULL),
    (4, 'gph', NULL),
    (4, 'gqf', NULL),
    (4, 'gqs', NULL),
    (4, 'gram', NULL),
    (4, 'gramps', NULL),
    (4, 'gre', NULL),
    (4, 'grv', NULL),
    (4, 'gtm', NULL),
    (4, 'gxf', NULL),
    (4, 'gxt', NULL),
    (4, 'hbci', NULL),
    (4, 'hdf', NULL),
    (4, 'hi', NULL),
    (4, 'hlp', NULL),
    (4, 'hpgl', NULL),
    (4, 'hpid', NULL),
    (4, 'hps', NULL),
    (4, 'htke', NULL),
    (4, 'hvd', NULL),
    (4, 'hvp', NULL),
    (4, 'hvs', NULL),
    (4, 'i2g', NULL),
    (4, 'icc', NULL),
    (4, 'icm', NULL),
    (4, 'idx', NULL),
    (4, 'ifm', NULL),
    (4, 'igl', NULL),
    (4, 'igm', NULL),
    (4, 'igx', NULL),
    (4, 'iif', NULL),
    (4, 'imp', NULL),
    (4, 'ims', NULL),
    (4, 'indd', NULL),
    (4, 'install', NULL),
    (4, 'iota', NULL),
    (4, 'ipfix', NULL),
    (4, 'irm', NULL),
    (4, 'itp', NULL),
    (4, 'ivp', NULL),
    (4, 'ivu', NULL),
    (4, 'jam', NULL),
    (4, 'jisp', NULL),
    (4, 'jlt', NULL),
    (4, 'jsonml', NULL),
    (4, 'karbon', NULL),
    (4, 'kfo', NULL),
    (4, 'kia', NULL),
    (4, 'kmz', NULL),
    (4, 'kne', NULL),
    (4, 'knp', NULL),
    (4, 'kon', NULL),
    (4, 'kpr', NULL),
    (4, 'kpt', NULL),
    (4, 'kpxx', NULL),
    (4, 'ksp', NULL),
    (4, 'ktr', NULL),
    (4, 'ktz', NULL),
    (4, 'kwd', NULL),
    (4, 'kwt', NULL),
    (4, 'latex', NULL),
    (4, 'lbd', NULL),
    (4, 'les', NULL),
    (4, 'lib', NULL),
    (4, 'list3820', NULL),
    (4, 'listafp', NULL),
    (4, 'lnk', NULL),
    (4, 'lrf', NULL),
    (4, 'lrm', NULL),
    (4, 'ltf', NULL),
    (4, 'lwp', NULL),
    (4, 'm13', NULL),
    (4, 'm14', NULL),
    (4, 'm21', NULL),
    (4, 'm3u8', NULL),
    (4, 'ma', NULL),
    (4, 'mag', NULL),
    (4, 'maker', NULL),
    (4, 'mar', NULL),
    (4, 'mb', NULL),
    (4, 'mbk', NULL),
    (4, 'mbox', NULL),
    (4, 'mc1', NULL),
    (4, 'mcd', NULL),
    (4, 'mdb', 'Microsoft Access database before Access 2007'),
    (4, 'mfm', NULL),
    (4, 'mft', NULL),
    (4, 'mgc', NULL),
    (4, 'mgz', NULL),
    (4, 'mie', NULL),
    (4, 'mif', NULL),
    (4, 'mjs', NULL),
    (4, 'mlp', NULL),
    (4, 'mmd', NULL),
    (4, 'mmf', NULL),
    (4, 'mny', NULL),
    (4, 'mo', NULL),
    (4, 'mobi', NULL),
    (4, 'mobipocket-ebook', NULL),
    (4, 'mp21', NULL),
    (4, 'mp4s', NULL),
    (4, 'mpc', NULL),
    (4, 'mpm', NULL),
    (4, 'mpn', NULL),
    (4, 'mpp', NULL),
    (4, 'mpt', NULL),
    (4, 'mpy', NULL),
    (4, 'mqy', NULL),
    (4, 'mrc', NULL),
    (4, 'mseed', NULL),
    (4, 'mseq', NULL),
    (4, 'msf', NULL),
    (4, 'msl', NULL),
    (4, 'msty', NULL),
    (4, 'mus', NULL),
    (4, 'mvb', NULL),
    (4, 'mwf', NULL),
    (4, 'mxf', NULL),
    (4, 'mxl', NULL),
    (4, 'mxs', NULL),
    (4, 'n-gage', NULL),
    (4, 'nb', NULL),
    (4, 'nbp', NULL),
    (4, 'nc', NULL),
    (4, 'ngdat', NULL),
    (4, 'nib', NULL),
    (4, 'nitf', NULL),
    (4, 'nlu', NULL),
    (4, 'nml', NULL),
    (4, 'nnd', NULL),
    (4, 'nns', NULL),
    (4, 'nnw', NULL),
    (4, 'nsc', NULL),
    (4, 'nsf', NULL),
    (4, 'ntf', NULL),
    (4, 'nzb', NULL),
    (4, 'o', NULL),
    (4, 'oa2', NULL),
    (4, 'oa3', NULL),
    (4, 'oas', NULL),
    (4, 'obd', NULL),
    (4, 'obj', NULL),
    (4, 'oda', NULL),
    (4, 'odb', NULL),
    (4, 'odc', NULL),
    (4, 'odf', NULL),
    (4, 'odft', NULL),
    (4, 'odg', NULL),
    (4, 'odi', NULL),
    (4, 'odm', NULL),
    (4, 'odp', NULL),
    (4, 'ods', NULL),
    (4, 'odt', NULL),
    (4, 'ogx', NULL),
    (4, 'onepkg', NULL),
    (4, 'onetmp', NULL),
    (4, 'onetoc', NULL),
    (4, 'onetoc2', NULL),
    (4, 'oprc', NULL),
    (4, 'org', NULL),
    (4, 'osf', NULL),
    (4, 'otc', NULL),
    (4, 'otg', NULL),
    (4, 'oth', NULL),
    (4, 'oti', NULL),
    (4, 'otp', NULL),
    (4, 'ots', NULL),
    (4, 'ott', NULL),
    (4, 'oxps', NULL),
    (4, 'oxt', NULL),
    (4, 'p10', NULL),
    (4, 'p12', NULL),
    (4, 'p7b', NULL),
    (4, 'p7c', NULL),
    (4, 'p7m', NULL),
    (4, 'p7r', NULL),
    (4, 'p7s', NULL),
    (4, 'p8', NULL),
    (4, 'pack', NULL),
    (4, 'pages', NULL),
    (4, 'paw', NULL),
    (4, 'pbd', NULL),
    (4, 'pcap', NULL),
    (4, 'pcl', NULL),
    (4, 'pclxl', NULL),
    (4, 'pcurl', NULL),
    (4, 'pdb', NULL),
    (4, 'pdf', 'Portable Document Format file'),
    (4, 'pfx', NULL),
    (4, 'pgn', NULL),
    (4, 'pgp', NULL),
    (4, 'pkg', NULL),
    (4, 'pki', NULL),
    (4, 'pkipath', NULL),
    (4, 'plb', NULL),
    (4, 'plc', NULL),
    (4, 'plf', NULL),
    (4, 'pml', NULL),
    (4, 'portpkg', NULL),
    (4, 'pot', NULL),
    (4, 'potm', NULL),
    (4, 'ppa', NULL),
    (4, 'ppam', NULL),
    (4, 'ppd', NULL),
    (4, 'pps', NULL),
    (4, 'ppsm', NULL),
    (4, 'ppt', NULL),
    (4, 'pptm', NULL),
    (4, 'pqa', NULL),
    (4, 'prc', NULL),
    (4, 'pre', NULL),
    (4, 'prf', NULL),
    (4, 'ps', NULL),
    (4, 'psb', NULL),
    (4, 'psd', 'Adobe Photoshop file'),
    (4, 'ptid', NULL),
    (4, 'pub', NULL),
    (4, 'pvb', NULL),
    (4, 'pwn', NULL),
    (4, 'pwz', NULL),
    (4, 'pyc', 'Python compiled file'),
    (4, 'pyo', 'Python compiled/optimized file'),
    (4, 'qam', NULL),
    (4, 'qbo', NULL),
    (4, 'qfx', NULL),
    (4, 'qps', NULL),
    (4, 'qwd', NULL),
    (4, 'qwt', NULL),
    (4, 'qxb', NULL),
    (4, 'qxd', NULL),
    (4, 'qxl', NULL),
    (4, 'qxt', NULL),
    (4, 'rcprofile', NULL),
    (4, 'rdz', NULL),
    (4, 'rep', NULL),
    (4, 'resources', NULL),
    (4, 'ris', NULL),
    (4, 'rlib', NULL),
    (4, 'rm', NULL),
    (4, 'rms', NULL),
    (4, 'rmvb', NULL),
    (4, 'rnc', NULL),
    (4, 'roa', NULL),
    (4, 'rp9', NULL),
    (4, 'rpss', NULL),
    (4, 'rpst', NULL),
    (4, 'rpt', NULL),
    (4, 'rq', NULL),
    (4, 'saf', NULL),
    (4, 'scd', NULL),
    (4, 'scq', NULL),
    (4, 'scs', NULL),
    (4, 'sda', NULL),
    (4, 'sdc', NULL),
    (4, 'sdd', NULL),
    (4, 'sdp', NULL),
    (4, 'sdw', NULL),
    (4, 'see', NULL),
    (4, 'seed', NULL),
    (4, 'sema', NULL),
    (4, 'semd', NULL),
    (4, 'semf', NULL),
    (4, 'ser', NULL),
    (4, 'setpay', NULL),
    (4, 'setreg', NULL),
    (4, 'sfd-hdstx', NULL),
    (4, 'sfs', NULL),
    (4, 'sgl', NULL),
    (4, 'sig', NULL),
    (4, 'sis', NULL),
    (4, 'sisx', NULL),
    (4, 'skd', NULL),
    (4, 'skm', NULL),
    (4, 'skp', NULL),
    (4, 'skt', NULL),
    (4, 'sldm', NULL),
    (4, 'slt', NULL),
    (4, 'sm', NULL),
    (4, 'smf', NULL),
    (4, 'so', NULL),
    (4, 'spc', NULL),
    (4, 'spf', NULL),
    (4, 'spl', NULL),
    (4, 'spp', NULL),
    (4, 'spq', NULL),
    (4, 'src', NULL),
    (4, 'srt', NULL),
    (4, 'sse', NULL),
    (4, 'ssf', NULL),
    (4, 'st', NULL),
    (4, 'stf', NULL),
    (4, 'stk', NULL),
    (4, 'stl', NULL),
    (4, 'str', NULL),
    (4, 'sus', NULL),
    (4, 'susp', NULL),
    (4, 'sv4cpio', NULL),
    (4, 'sv4crc', NULL),
    (4, 'svc', NULL),
    (4, 'svd', NULL),
    (4, 'swa', NULL),
    (4, 'swf', NULL),
    (4, 'swi', NULL),
    (4, 'sys', NULL),
    (4, 't3', NULL),
    (4, 'taglet', NULL),
    (4, 'tcap', NULL),
    (4, 'teacher', NULL),
    (4, 'tex', NULL),
    (4, 'texi', NULL),
    (4, 'texinfo', NULL),
    (4, 'tfm', NULL),
    (4, 'thmx', NULL),
    (4, 'tmo', NULL),
    (4, 'torrent', NULL),
    (4, 'tpl', NULL),
    (4, 'tpt', NULL),
    (4, 'tra', NULL),
    (4, 'trm', NULL),
    (4, 'tsd', NULL),
    (4, 'twd', NULL),
    (4, 'twds', NULL),
    (4, 'txd', NULL),
    (4, 'txf', NULL),
    (4, 'u32', NULL),
    (4, 'ufd', NULL),
    (4, 'ufdl', NULL),
    (4, 'ulx', NULL),
    (4, 'umj', NULL),
    (4, 'unityweb', NULL),
    (4, 'ustar', NULL),
    (4, 'utz', NULL),
    (4, 'uvd', NULL),
    (4, 'uvf', NULL),
    (4, 'uvvd', NULL),
    (4, 'uvvf', NULL),
    (4, 'uvvx', NULL),
    (4, 'uvvz', NULL),
    (4, 'uvx', NULL),
    (4, 'uvz', NULL),
    (4, 'vcd', NULL),
    (4, 'vcg', NULL),
    (4, 'vcx', NULL),
    (4, 'vis', NULL),
    (4, 'vor', NULL),
    (4, 'vox', NULL),
    (4, 'vsd', NULL),
    (4, 'vsf', NULL),
    (4, 'vss', NULL),
    (4, 'vst', NULL),
    (4, 'vsw', NULL),
    (4, 'w3d', NULL),
    (4, 'wad', NULL),
    (4, 'wasm', NULL),
    (4, 'wbxml', NULL),
    (4, 'wcm', NULL),
    (4, 'wdb', NULL),
    (4, 'webmanifest', NULL),
    (4, 'wg', NULL),
    (4, 'wgt', NULL),
    (4, 'wiz', NULL),
    (4, 'wks', NULL),
    (4, 'wmd', NULL),
    (4, 'wmf', NULL),
    (4, 'wmlc', NULL),
    (4, 'wmlsc', NULL),
    (4, 'wmz', NULL),
    (4, 'wpd', NULL),
    (4, 'wpl', NULL),
    (4, 'wps', NULL),
    (4, 'wpt', NULL),
    (4, 'wqd', NULL),
    (4, 'wri', NULL),
    (4, 'wtb', NULL),
    (4, 'x32', NULL),
    (4, 'xar', NULL),
    (4, 'xbap', NULL),
    (4, 'xbd', NULL),
    (4, 'xcuserstate', NULL),
    (4, 'xdw', NULL),
    (4, 'xfdf', NULL),
    (4, 'xfdl', NULL),
    (4, 'xla', NULL),
    (4, 'xlam', NULL),
    (4, 'xlb', NULL),
    (4, 'xlc', NULL),
    (4, 'xlm', NULL),
    (4, 'xls', NULL),
    (4, 'xlsb', NULL),
    (4, 'xlsm', NULL),
    (4, 'xlt', NULL),
    (4, 'xltm', NULL),
    (4, 'xlw', NULL),
    (4, 'xo', NULL),
    (4, 'xpi', NULL),
    (4, 'xpr', NULL),
    (4, 'xpw', NULL),
    (4, 'xpx', NULL),
    (4, 'yang', NULL),
    (4, 'z1', NULL),
    (4, 'z2', NULL),
    (4, 'z3', NULL),
    (4, 'z4', NULL),
    (4, 'z5', NULL),
    (4, 'z6', NULL),
    (4, 'z7', NULL),
    (4, 'z8', NULL),
    (4, 'zir', NULL),
    (4, 'zirz', NULL),
    (5, 'adb', NULL),
    (5, 'ads', NULL),
    (5, 'ahk', NULL),
    (5, 'applejs', NULL),
    (5, 'applescript', 'AppleScript language source file'),
    (5, 'as', NULL),
    (5, 'asc', NULL),
    (5, 'ascx', NULL),
    (5, 'asm', NULL),
    (5, 'asmx', NULL),
    (5, 'asp', NULL),
    (5, 'aspx', NULL),
    (5, 'au3', NULL),
    (5, 'bas', 'Basic language source file'),
    (5, 'bash', 'Bash shell script file'),
    (5, 'bat', 'PC batch file'),
    (5, 'boo', 'Boo language source file'),
    (5, 'bsh', NULL),
    (5, 'btm', NULL),
    (5, 'c', 'C language source file'),
    (5, 'c++', 'C++ language source file'),
    (5, 'cbl', NULL),
    (5, 'cc', NULL),
    (5, 'cfm', NULL),
    (5, 'cgi', NULL),
    (5, 'cjs', NULL),
    (5, 'clj', 'Clojure language source file'),
    (5, 'cljc', NULL),
    (5, 'cljs', NULL),
    (5, 'cls', NULL),
    (5, 'cmake', NULL),
    (5, 'cmd', NULL),
    (5, 'cob', NULL),
    (5, 'coffee', 'CoffeeScript language script file'),
    (5, 'cpp', 'C++ language source file'),
    (5, 'cr', NULL),
    (5, 'cs', 'C# language source file'),
    (5, 'csh', NULL),
    (5, 'css', 'Cascading Stylesheet definitions file'),
    (5, 'cxx', NULL),
    (5, 'd', NULL),
    (5, 'dart', 'Dart language source file'),
    (5, 'dic', NULL),
    (5, 'e', NULL),
    (5, 'ecma', NULL),
    (5, 'el', NULL),
    (5, 'elm', NULL),
    (5, 'erb', NULL),
    (5, 'erl', 'Erlang language source file'),
    (5, 'es', NULL),
    (5, 'es6', NULL),
    (5, 'eta', NULL),
    (5, 'ex', 'Elixir language source file'),
    (5, 'exs', 'Elixir language script file'),
    (5, 'f', NULL),
    (5, 'f77', NULL),
    (5, 'f90', NULL),
    (5, 'for', NULL),
    (5, 'frm', NULL),
    (5, 'fs', 'F# language source file'),
    (5, 'fth', NULL),
    (5, 'fx', NULL),
    (5, 'go', 'Go language source file'),
    (5, 'groovy', 'Groovy language script file'),
    (5, 'h', 'C/C++ language header file'),
    (5, 'h++', 'C++ language header file'),
    (5, 'hbs', NULL),
    (5, 'hh', 'C++ language header file'),
    (5, 'hpp', 'C++ language header file'),
    (5, 'hs', 'Haskell language source file'),
    (5, 'htm', NULL),
    (5, 'html', NULL),
    (5, 'hxx', 'C++ language header file'),
    (5, 'i', NULL),
    (5, 'inc', NULL),
    (5, 'ino', NULL),
    (5, 'ipp', NULL),
    (5, 'ipynb', NULL),
    (5, 'java', 'Java language source file'),
    (5, 'jq', NULL),
    (5, 'jl', 'Julia language source file'),
    (5, 'js', 'JavaScript language source file'),
    (5, 'js2', NULL),
    (5, 'jsf', NULL),
    (5, 'json', NULL),
    (5, 'jsonld', NULL),
    (5, 'jsp', NULL),
    (5, 'jspa', NULL),
    (5, 'jspf', NULL),
    (5, 'jsx', NULL),
    (5, 'ksh', NULL),
    (5, 'kt', 'Kotlin language source file'),
    (5, 'kts', NULL),
    (5, 'l', NULL),
    (5, 'las', NULL),
    (5, 'lasso', NULL),
    (5, 'lhs', 'Literate Haskell language source file'),
    (5, 'lisp', NULL),
    (5, 'lua', 'Lua language source file'),
    (5, 'm', NULL),
    (5, 'm4', NULL),
    (5, 'map', NULL),
    (5, 'mht', NULL),
    (5, 'mhtml', NULL),
    (5, 'ml', 'ML/OCAML language source file'),
    (5, 'mli', NULL),
    (5, 'nut', NULL),
    (5, 'p', NULL),
    (5, 'pas', 'Pascal language header file'),
    (5, 'pbxproj', NULL),
    (5, 'pde', NULL),
    (5, 'php', 'PHP language source file'),
    (5, 'php3', 'PHP language source file'),
    (5, 'php4', 'PHP language source file'),
    (5, 'php5', 'PHP language source file'),
    (5, 'php7', 'PHP language source file'),
    (5, 'phps', NULL),
    (5, 'phtml', NULL),
    (5, 'pl', 'Perl language source file'),
    (5, 'pm', 'Perl language module file'),
    (5, 'pony', NULL),
    (5, 'ps1', 'Powershell language script file'),
    (5, 'psc1', NULL),
    (5, 'psd1', NULL),
    (5, 'psm1', NULL),
    (5, 'psrc', NULL),
    (5, 'pssc', NULL),
    (5, 'pxd', NULL),
    (5, 'pxi', NULL),
    (5, 'py', 'Python language source file'),
    (5, 'pyw', NULL),
    (5, 'pyx', NULL),
    (5, 'r', 'R language source file'),
    (5, 'rb', 'Ruby language source file'),
    (5, 'rbs', NULL),
    (5, 'rc', NULL),
    (5, 'rc2', NULL),
    (5, 'reb', NULL),
    (5, 'red', NULL),
    (5, 'rexx', 'Rexx language source file'),
    (5, 'rkt', NULL),
    (5, 'rs', 'Rust language source file'),
    (5, 's', NULL),
    (5, 'sass', NULL),
    (5, 'sb', NULL),
    (5, 'sb2', NULL),
    (5, 'sb3', NULL),
    (5, 'sbt', NULL),
    (5, 'sc', NULL),
    (5, 'scala', 'Scala language source file'),
    (5, 'scm', NULL),
    (5, 'scpt', NULL),
    (5, 'scss', NULL),
    (5, 'sh', 'Shell script file'),
    (5, 'shtml', NULL),
    (5, 'sln', 'Visual Studio solution file'),
    (5, 'sql', 'SQL language source file'),
    (5, 'stm', NULL),
    (5, 'swift', 'Swift language source file'),
    (5, 'tbc', NULL),
    (5, 'tcl', NULL),
    (5, 'tk', NULL),
    (5, 'ts', 'TypeScript language source file'),
    (5, 'tsx', NULL),
    (5, 'u3', NULL),
    (5, 'vb', 'Visual Basic language source file'),
    (5, 'vbs', 'VBScript language script file'),
    (5, 'vue', NULL),
    (5, 'xht', NULL),
    (5, 'xhtml', NULL),
    (5, 'y', NULL),
    (6, 'eot', NULL),
    (6, 'otf', 'OpenType font file'),
    (6, 'tte', NULL),
    (6, 'ttf', 'TrueType font file'),
    (6, 'woff', NULL),
    (6, 'woff2', NULL),
    (7, 'ai', NULL),
    (7, 'avif', NULL),
    (7, 'bmp', 'Bitmap file'),
    (7, 'bpg', NULL),
    (7, 'cur', NULL),
    (7, 'dib', NULL),
    (7, 'drw', NULL),
    (7, 'ecw', NULL),
    (7, 'eps', 'Encapsulated Postscript file'),
    (7, 'fit', NULL),
    (7, 'fits', NULL),
    (7, 'flif', NULL),
    (7, 'fts', NULL),
    (7, 'gif', 'Graphical Interchange Format file'),
    (7, 'heif', NULL),
    (7, 'icns', NULL),
    (7, 'ico', NULL),
    (7, 'indd', NULL),
    (7, 'jpg', NULL),
    (7, 'jpeg', NULL),
    (7, 'pam', NULL),
    (7, 'pbm', NULL),
    (7, 'pct', NULL),
    (7, 'pcx', NULL),
    (7, 'pdf', 'Portable Document Format file'),
    (7, 'pgf', NULL),
    (7, 'pgm', NULL),
    (7, 'pic', NULL),
    (7, 'pict', NULL),
    (7, 'png', 'Portable Network Graphics file'),
    (7, 'pnm', NULL),
    (7, 'ppm', NULL),
    (7, 'psd', NULL),
    (7, 'psp', NULL),
    (7, 'raw', NULL),
    (7, 'rgb', NULL),
    (7, 'rgba', NULL),
    (7, 'sgi', NULL),
    (7, 'sid', NULL),
    (7, 'svg', NULL),
    (7, 'tif', 'Tagged Image File Format file'),
    (7, 'tiff', 'Tagged Image File Format file'),
    (7, 'webp', NULL),
    -- (8, '1', NULL),
    -- (8, '10', NULL),
    -- (8, '11', NULL),
    -- (8, '12', NULL),
    -- (8, '13', NULL),
    -- (8, '14', NULL),
    -- (8, '15', NULL),
    -- (8, '16', NULL),
    -- (8, '17', NULL),
    -- (8, '18', NULL),
    -- (8, '19', NULL),
    (8, '1st', NULL),
    -- (8, '2', NULL),
    -- (8, '20', NULL),
    -- (8, '3', NULL),
    -- (8, '323', NULL),
    (8, '3dml', NULL),
    -- (8, '4', NULL),
    -- (8, '5', NULL),
    -- (8, '6', NULL),
    -- (8, '7', NULL),
    -- (8, '8', NULL),
    -- (8, '9', NULL),
    (8, 'adoc', NULL),
    (8, 'am', NULL),
    (8, 'app', NULL),
    (8, 'appcache', NULL),
    (8, 'bib', NULL),
    (8, 'brf', NULL),
    (8, 'cabal', NULL),
    (8, 'cfg', NULL),
    (8, 'cnf', NULL),
    (8, 'cnt', NULL),
    (8, 'code-workspace', NULL),
    (8, 'conf', NULL),
    (8, 'csv', 'Comma-separated values file'),
    (8, 'ctl', NULL),
    (8, 'curl', NULL),
    (8, 'dbml', NULL),
    (8, 'dbschema', NULL),
    (8, 'dcurl', NULL),
    (8, 'ddl', NULL),
    (8, 'def', NULL),
    (8, 'dep', NULL),
    (8, 'dfm', NULL),
    (8, 'diff', NULL),
    (8, 'disco', NULL),
    (8, 'dlg', NULL),
    (8, 'dof', NULL),
    (8, 'dpr', NULL),
    (8, 'drl', NULL),
    (8, 'dsc', NULL),
    (8, 'dsp', NULL),
    (8, 'dsw', NULL),
    (8, 'elt', NULL),
    (8, 'eml', NULL),
    (8, 'ent', NULL),
    (8, 'env', NULL),
    (8, 'eps', NULL),
    (8, 'etx', NULL),
    (8, 'exp', NULL),
    (8, 'feature', NULL),
    (8, 'fls', NULL),
    (8, 'flx', NULL),
    (8, 'fly', NULL),
    (8, 'gcd', NULL),
    (8, 'gemspec', NULL),
    (8, 'gradle', NULL),
    (8, 'gv', NULL),
    (8, 'hcl', NULL),
    (8, 'hql', NULL),
    (8, 'htaccess', NULL),
    (8, 'htc', NULL),
    (8, 'ics', NULL),
    (8, 'icz', NULL),
    (8, 'ifb', NULL),
    (8, 'in', NULL),
    (8, 'ini', NULL),
    (8, 'ipr', NULL),
    (8, 'iws', NULL),
    (8, 'jad', NULL),
    (8, 'jam', NULL),
    (8, 'jql', NULL),
    (8, 'layout', NULL),
    (8, 'list', NULL),
    (8, 'log', NULL),
    (8, 'ltx', NULL),
    (8, 'mak', NULL),
    (8, 'mako', NULL),
    (8, 'man', NULL),
    (8, 'manifest', NULL),
    (8, 'markdown', NULL),
    (8, 'master', NULL),
    (8, 'mcurl', NULL),
    (8, 'md', NULL),
    (8, 'me', NULL),
    (8, 'mf', NULL),
    (8, 'mime', NULL),
    (8, 'mml', NULL),
    (8, 'moc', NULL),
    (8, 'mod', NULL),
    (8, 'ms', NULL),
    (8, 'n3', NULL),
    (8, 'nfo', NULL),
    (8, 'nws', NULL),
    (8, 'org', NULL),
    (8, 'patch', NULL),
    (8, 'pem', NULL),
    (8, 'po', NULL),
    (8, 'pod', NULL),
    (8, 'policy', NULL),
    (8, 'properties', NULL),
    (8, 'ps', NULL),
    (8, 'pt', NULL),
    (8, 'rex', NULL),
    (8, 'roff', NULL),
    (8, 'rst', NULL),
    (8, 'rtf', NULL),
    (8, 'rtx', NULL),
    (8, 'sample', NULL),
    (8, 'scc', NULL),
    (8, 'sct', NULL),
    (8, 'scurl', NULL),
    (8, 'sfv', NULL),
    (8, 'sgm', NULL),
    (8, 'sgml', NULL),
    (8, 'sht', NULL),
    (8, 'shtm', NULL),
    (8, 'snap', NULL),
    (8, 'soy', NULL),
    (8, 'spec', NULL),
    (8, 'spot', NULL),
    (8, 'sqc', NULL),
    (8, 'sql', NULL),
    (8, 'st', NULL),
    (8, 'str', NULL),
    (8, 'strings', NULL),
    (8, 'sty', NULL),
    (8, 'sub', NULL),
    (8, 'suml', NULL),
    (8, 't', NULL),
    (8, 'tex', NULL),
    (8, 'text', NULL),
    (8, 'tm', NULL),
    (8, 'tmx', NULL),
    (8, 'toml', NULL),
    (8, 'tr', NULL),
    (8, 'tsv', NULL),
    (8, 'ttl', NULL),
    (8, 'txt', 'Plain text file'),
    (8, 'ui', NULL),
    (8, 'uls', NULL),
    (8, 'uml', NULL),
    (8, 'uri', NULL),
    (8, 'uris', NULL),
    (8, 'url', NULL),
    (8, 'urls', NULL),
    (8, 'user', NULL),
    (8, 'uu', NULL),
    (8, 'vcard', NULL),
    (8, 'vcf', NULL),
    (8, 'vcs', NULL),
    (8, 'vm', NULL),
    (8, 'vrm', NULL),
    (8, 'vssscc', NULL),
    (8, 'wbxml', NULL),
    (8, 'webinfo', NULL),
    (8, 'wml', NULL),
    (8, 'wmls', NULL),
    (8, 'wsc', NULL),
    (8, 'wsd', NULL),
    (8, 'xsp', NULL),
    (8, 'yaml', NULL),
    (8, 'yml', NULL),
    (1, 'adm', NULL),
    (1, 'aps', NULL),
    (1, 'cli', NULL),
    (1, 'clw', NULL),
    (1, 'df2', NULL),
    (1, 'ncb', NULL),
    (1, 'nt', NULL),
    (1, 'nt2', NULL),
    (1, 'orig', NULL),
    (1, 'pc', NULL),
    (1, 'plg', NULL),
    (1, 'sun', NULL),
    (9, 'avi', NULL),
    (9, 'flv', 'Flash-compatible video file'),
    (9, 'mov', 'Apple QuickTime movie file'),
    (9, 'mp4', NULL),
    (9, 'mpeg', NULL),
    (9, 'ogv', NULL),
    (9, 'webm', NULL),
    (9, 'wmv', 'Windows Media Video file'),
    (9, '3gpp', NULL),
    (9, '3gp', NULL),
    (9, '3g2', NULL),
    (10, 'ant', NULL),
    (10, 'atom', NULL),
    (10, 'atomcat', NULL),
    (10, 'atomsrv', NULL),
    (10, 'atomsvc', NULL),
    (10, 'bdsproj', NULL),
    (10, 'ccxml', NULL),
    (10, 'cdxml', NULL),
    (10, 'config', NULL),
    (10, 'csproj', '.NET C# project file'),
    (10, 'dae', NULL),
    (10, 'davmount', NULL),
    (10, 'dbk', NULL),
    (10, 'dbproj', NULL),
    (10, 'dd2', NULL),
    (10, 'docx', NULL),
    (10, 'dotx', NULL),
    (10, 'dtb', NULL),
    (10, 'dtd', NULL),
    (10, 'edmx', NULL),
    (10, 'emma', NULL),
    (10, 'es3', NULL),
    (10, 'et3', NULL),
    (10, 'fsproj', '.NET F# project file'),
    (10, 'fxml', NULL),
    (10, 'gml', NULL),
    (10, 'gpx', NULL),
    (10, 'grxml', NULL),
    (10, 'hal', NULL),
    (10, 'iml', NULL),
    (10, 'ink', NULL),
    (10, 'inkml', NULL),
    (10, 'irp', NULL),
    (10, 'jhm', NULL),
    (10, 'jnlp', NULL),
    (10, 'kml', NULL),
    (10, 'lasxml', NULL),
    (10, 'lbe', NULL),
    (10, 'link66', NULL),
    (10, 'lostxml', NULL),
    (10, 'mads', NULL),
    (10, 'mathml', NULL),
    (10, 'meta4', NULL),
    (10, 'metalink', NULL),
    (10, 'mets', NULL),
    (10, 'mm', NULL),
    (10, 'mml', NULL),
    (10, 'mods', NULL),
    (10, 'mpkg', NULL),
    (10, 'mrcx', NULL),
    (10, 'mscml', NULL),
    (10, 'musicxml', NULL),
    (10, 'mxml', NULL),
    (10, 'ncx', NULL),
    (10, 'omdoc', NULL),
    (10, 'opf', NULL),
    (10, 'opml', NULL),
    (10, 'osfpvg', NULL),
    (10, 'plist', 'Apple Property List file'),
    (10, 'pls', NULL),
    (10, 'pom', NULL),
    (10, 'potx', NULL),
    (10, 'ppsx', NULL),
    (10, 'pptx', NULL),
    (10, 'ps1xml', NULL),
    (10, 'pskcxml', NULL),
    (10, 'qrc', NULL),
    (10, 'rdf', NULL),
    (10, 'res', NULL),
    (10, 'resx', 'Resource file'),
    (10, 'rif', NULL),
    (10, 'rl', NULL),
    (10, 'rld', NULL),
    (10, 'rng', NULL),
    (10, 'rs', NULL),
    (10, 'rsd', NULL),
    (10, 'rss', NULL),
    (10, 'sbml', NULL),
    (10, 'sdkd', NULL),
    (10, 'sdkm', NULL),
    (10, 'settings', NULL),
    (10, 'shf', NULL),
    (10, 'sldx', NULL),
    (10, 'smi', NULL),
    (10, 'smil', NULL),
    (10, 'sru', NULL),
    (10, 'srx', NULL),
    (10, 'ssdl', NULL),
    (10, 'ssml', NULL),
    (10, 'stc', NULL),
    (10, 'std', NULL),
    (10, 'sti', NULL),
    (10, 'stw', NULL),
    (10, 'svg', NULL),
    (10, 'svgz', NULL),
    (10, 'sxc', NULL),
    (10, 'sxd', NULL),
    (10, 'sxg', NULL),
    (10, 'sxi', NULL),
    (10, 'sxm', NULL),
    (10, 'sxw', NULL),
    (10, 'tei', NULL),
    (10, 'teicorpus', NULL),
    (10, 'tfi', NULL),
    (10, 'tld', NULL),
    (10, 'uoml', NULL),
    (10, 'uvt', NULL),
    (10, 'uvvt', NULL),
    (10, 'vbproj', NULL),
    (10, 'vcproj', NULL),
    (10, 'vdproj', NULL),
    (10, 'vxml', NULL),
    (10, 'wadl', NULL),
    (10, 'wbs', NULL),
    (10, 'wsdd', NULL),
    (10, 'wsdl', NULL),
    (10, 'wsf', NULL),
    (10, 'wspolicy', NULL),
    (10, 'x3d', NULL),
    (10, 'x3dz', NULL),
    (10, 'xaml', NULL),
    (10, 'xcbkptlist', NULL),
    (10, 'xcscheme', NULL),
    (10, 'xcworkspacedata', NULL),
    (10, 'xdf', NULL),
    (10, 'xdm', NULL),
    (10, 'xdp', NULL),
    (10, 'xdssc', NULL),
    (10, 'xenc', NULL),
    (10, 'xer', NULL),
    (10, 'xht', NULL),
    (10, 'xhtml', NULL),
    (10, 'xhvml', NULL),
    (10, 'xjb', NULL),
    (10, 'xlf', NULL),
    (10, 'xlsx', NULL),
    (10, 'xltx', NULL),
    (10, 'xml', 'Extensible Markup Language file'),
    (10, 'xop', NULL),
    (10, 'xpdl', NULL),
    (10, 'xpl', NULL),
    (10, 'xps', NULL),
    (10, 'xsd', NULL),
    (10, 'xsl', 'Extensible Stylesheet Language file'),
    (10, 'xslt', 'Extensible Stylesheet Language Transformations file'),
    (10, 'xsm', NULL),
    (10, 'xspf', NULL),
    (10, 'xul', NULL),
    (10, 'xvm', NULL),
    (10, 'xvml', NULL),
    (10, 'yin', NULL),
    (10, 'zaz', NULL),
    (10, 'zmm', NULL)
ON CONFLICT DO NOTHING;

COMMIT;
