-- Copyright 2014 Duncan Mac-Vicar P. <dmacvicar@suse.de>
-- This file is released under the terms of the MIT license
-- RPM Spec LPeg lexer.

local l = require('lexer')
local token, word_match = l.token, l.word_match
local P, R, S = lpeg.P, lpeg.R, lpeg.S

local M = {_NAME = 'rpmspec'}

-- Whitespace.
local ws = token(l.WHITESPACE, l.space^1)
local comment = token(l.COMMENT, '#' * l.nonnewline^0)

local properties = l.word_match({
  'Icon', 'ExclusiveOs', 'ExcludeOs',
  'BuildArch', 'BuildArchitectures', 'ExclusiveArch', 'ExcludeArch',
  'Conflicts', 'Obsoletes', 'Provides', 'Requires', 'Requires(.*)',
  'Enhances', 'Suggests', 'BuildConflicts', 'BuildRequires',
  'Recommends', 'PreReq', 'Supplements',
  'Epoch', 'Serial', 'Nosource', 'Nopatch',
  'AutoReq', 'AutoProv', 'AutoReqProv',
  'Copyright', 'License', 'Summary', 'Summary(.*)',
  'Distribution', 'Vendor', 'Packager', 'Group',
  'Sourced*', 'Patchd*', 'BuildRoot', 'Prefix',
  'Name', 'Version', 'Release', 'Url', 'URL',
})

local archs = l.word_match({
  'i386', 'i486', 'i586', 'i686', 'athlon', 'ia64', 'alpha', 'alphaev5', 'alphaev56', 'alphapca56', 'alphaev6',
  'alphaev67', 'sparc', 'sparcv9', 'sparc64armv3l', 'armv4b', 'armv4lm', 'ips', 'mipsel', 'ppc', 'ppc', 'iseries',
  'ppcpseries', 'ppc64', 'm68k', 'm68kmint', 'Sgi', 'rs6000', 'i370', 's390x', 's390', 'noarch',
})

local macros = token(l.VARIABLE, P('%') * (l.word + l.delimited_range('{}', true, true)))
local line_with_macros = (macros + token(l.STRING, (l.nonnewline - P('%'))^1))^1

local normal_prop = properties * P(':')
local indexed_prop = l.word_match({'Source', 'Patch'}) * l.dec_num * P(':')

local pkg_prop_name = token(l.KEYWORD, normal_prop + indexed_prop)

local pkg_prop_def = pkg_prop_name * ws * line_with_macros * l.newline

local desc_section_decl = token(l.FUNCTION, P('%description')) * l.newline

local desc_section_text = #desc_section_decl * token(l.STRING, (line_with_macros * l.newline)^1)

local build_section_decl = token(l.FUNCTION, P('%') * l.word_match({
  'prep', 'build', 'install', 'clean', 'files', 'changelog',
})) * l.newline

-- TODO:
--  ^# norootforbuild
-- %(ifarch|ifnarch)
-- ^%(changelog|check$|description|files|package)<
-- ^(%(prep$|build$|install$|clean$|(pre|post)(un)?|trigger(in|un|postun)|verifyscript))
-- %(if|else|endif|define|global|undefine|ifos|ifnos)

local bash_with_macros = l.load('bash')
table.insert(bash_with_macros._rules, 1, {'macros', macros})

M._rules = {
  {'whitespace', ws},
  {'comment', comment},
  {'pkg_prop_def', pkg_prop_def},
  {'desc_section_decl', desc_section_decl},
  {'desc_section_text', desc_section_text},
  {'build_section_decl', build_section_decl},
  {'macros', macros},
}

l.embed_lexer(M, bash_with_macros, build_section_decl, #build_section_decl)

M._tokenstyles = {

}

return M