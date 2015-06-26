package epic.preprocess;



/**
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License.  You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

/**
 Modifications 
 Copyright 2014 David Hall

 Licensed under the Apache License, Version 2.0 (the "License")
 you may not use this file except in compliance with the License.
 You may obtain a copy of the License at

 http://www.apache.org/licenses/LICENSE-2.0

 Unless required by applicable law or agreed to in writing, software
 distributed under the License is distributed on an "AS IS" BASIS,
 WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 See the License for the specific language governing permissions and
 limitations under the License.
*/

%%

/**
* Based on Lucene's StandardTokenizerImpl, but heavily modified.
*/
%class TreebankTokenizerImpl
%unicode
%type scala.Tuple2<epic.trees.Span, epic.slab.Token>
%function getNextToken
%pack
%char

%{

private int acro_period = -1;

public final int yychar()
{
    return yychar;
}

final  scala.Tuple2<epic.trees.Span, epic.slab.Token> currentToken() {
    return currentToken(new String(zzBuffer, zzStartRead, zzMarkedPos-zzStartRead).replace('’', '\'').replace('¨','\u0308').replaceAll("-[\n\r]+",""));
}
final scala.Tuple2<epic.trees.Span, epic.slab.Token> currentToken(String value) {
//  return new String(zzBuffer, zzStartRead, zzMarkedPos-zzStartRead);
  return new scala.Tuple2(new epic.trees.Span(epic.trees.Span.apply(yychar(), yychar() + zzMarkedPos - zzStartRead)), new epic.slab.Token(value));
}


%}

%eofval{
  if(yychar() == acro_period) {
      acro_period = -2;
      return new scala.Tuple2(new epic.trees.Span(epic.trees.Span.apply(yychar() - 1, yychar())), new epic.slab.Token("."));
  } else {
    return null;
  }
%eofval}

THAI       = [\u0E00-\u0E59]

// basic word: a sequence of digits & letters (includes Thai to enable ThaiAnalyzer to function)
ALPHANUM   = ({LETTER}|{THAI}|[:digit:]|_)+

// case insensitivity is useful sometimes
a = [aA]
b = [bB]
c = [cC]
d = [dD]
e = [eE]
f = [fF]
g = [gG]
h = [hH]
i = [iI]
j = [jJ]
k = [kK]
l = [lL]
m = [mM]
n = [nN]
o = [oO]
p = [pP]
q = [qQ]
r = [rR]
s = [sS]
t = [tT]
u = [uU]
v = [vV]
w = [wW]
x = [xX]
y = [yY]
z = [zZ]

ALPHA      = ({LETTER}|¨)+

NEWLINE = [\n\r]

// acronyms: U.S.A., I.B.M., etc.
// use a post-filter to remove dots
ABBRNYM    =  {LETTER} "." ({LETTER} ".")+

ACRONYM_DEP	= {ALPHANUM} "." ({ALPHANUM} ".")+

// company names like AT&T and Excite@Home.
COMPANY    =  ([aA][tT][&][tT]|Excite[@]Home)

// hostname
HOST       =  {ALPHANUM} ((".") {ALPHANUM})+

EMDASH = (--|---|[\u2014\u2015\u2e3a\u2e3b\ufe58]+)

DASH = ([\-\u2011\u2012\u2013\u2e1a\ufe63\uff0d])


// url

// url spec lifted from Lucene

// URL and E-mail syntax specifications:
//
//     RFC-952:  DOD INTERNET HOST TABLE SPECIFICATION
//     RFC-1035: DOMAIN NAMES - IMPLEMENTATION AND SPECIFICATION
//     RFC-1123: Requirements for Internet Hosts - Application and Support
//     RFC-1738: Uniform Resource Locators (URL)
//     RFC-3986: Uniform Resource Identifier (URI): Generic Syntax
//     RFC-5234: Augmented BNF for Syntax Specifications: ABNF
//     RFC-5321: Simple Mail Transfer Protocol
//     RFC-5322: Internet Message Format

// http://code.ohloh.net/file?fid=wEylHt__FppVh8Ub_GTsx__CTK4&cid=d0f5PFFYrnk&s=UAX29URLEmailTokenizerImpl&filterChecked=true&fp=473333&mp,=1&ml=1&me=1&md=1&projSelected=true#L0

DomainLabel = [A-Za-z0-9] ([-A-Za-z0-9]* [A-Za-z0-9])?
DomainNameLoose  = {DomainLabel} ("." {DomainLabel})*

IPv4DecimalOctet = "0"{0,2} [0-9] | "0"? [1-9][0-9] | "1" [0-9][0-9] | "2" ([0-4][0-9] | "5" [0-5])
IPv4Address  = {IPv4DecimalOctet} ("." {IPv4DecimalOctet}){3}
IPv6Hex16Bit = [0-9A-Fa-f]{1,4}
IPv6LeastSignificant32Bits = {IPv4Address} | ({IPv6Hex16Bit} ":" {IPv6Hex16Bit})
IPv6Address =                                                  ({IPv6Hex16Bit} ":"){6} {IPv6LeastSignificant32Bits}
            |                                             "::" ({IPv6Hex16Bit} ":"){5} {IPv6LeastSignificant32Bits}
            |                            {IPv6Hex16Bit}?  "::" ({IPv6Hex16Bit} ":"){4} {IPv6LeastSignificant32Bits}
            | (({IPv6Hex16Bit} ":"){0,1} {IPv6Hex16Bit})? "::" ({IPv6Hex16Bit} ":"){3} {IPv6LeastSignificant32Bits}
            | (({IPv6Hex16Bit} ":"){0,2} {IPv6Hex16Bit})? "::" ({IPv6Hex16Bit} ":"){2} {IPv6LeastSignificant32Bits}
            | (({IPv6Hex16Bit} ":"){0,3} {IPv6Hex16Bit})? "::"  {IPv6Hex16Bit} ":"     {IPv6LeastSignificant32Bits}
            | (({IPv6Hex16Bit} ":"){0,4} {IPv6Hex16Bit})? "::"                         {IPv6LeastSignificant32Bits}
            | (({IPv6Hex16Bit} ":"){0,5} {IPv6Hex16Bit})? "::"                         {IPv6Hex16Bit}
            | (({IPv6Hex16Bit} ":"){0,6} {IPv6Hex16Bit})? "::"

URIunreserved = [-._~A-Za-z0-9]
URIpercentEncoded = "%" [0-9A-Fa-f]{2}
URIsubDelims = [!$&'()*+,;=]
URIloginSegment = ({URIunreserved} | {URIpercentEncoded} | {URIsubDelims})*
URIlogin = {URIloginSegment} (":" {URIloginSegment})? "@"
URIquery    = "?" ({URIunreserved} | {URIpercentEncoded} | {URIsubDelims} | [:@/?])*
URIfragment = "#" ({URIunreserved} | {URIpercentEncoded} | {URIsubDelims} | [:@/?])*
URIport = ":" [0-9]{1,5}
URIhostStrict = ("[" {IPv6Address} "]") | {IPv4Address}
URIhostLoose  = ("[" {IPv6Address} "]") | {IPv4Address} | {DomainNameLoose}

URIauthorityStrict =             {URIhostStrict} {URIport}?
URIauthorityLoose  = {URIlogin}? {URIhostLoose}  {URIport}?

HTTPsegment = ({URIunreserved} | {URIpercentEncoded} | [;:@&=])*
HTTPpath = ("/" {HTTPsegment})*
HTTPscheme = [hH][tT][tT][pP][sS]? "://"
HTTPurlFull = {HTTPscheme} {URIauthorityLoose}  {HTTPpath}? {URIquery}? {URIfragment}?
// {HTTPurlNoScheme} excludes {URIlogin}, because it could otherwise accept e-mail addresses
HTTPurlNoScheme =          {URIauthorityStrict} {HTTPpath}? {URIquery}? {URIfragment}?
HTTPurl = {HTTPurlFull} | {HTTPurlNoScheme}

FTPorFILEsegment = ({URIunreserved} | {URIpercentEncoded} | [?:@&=])*
FTPorFILEpath = "/" {FTPorFILEsegment} ("/" {FTPorFILEsegment})*
FTPtype = ";" [tT][yY][pP][eE] "=" [aAiIdD]
FTPscheme = [fF][tT][pP] "://"
FTPurl = {FTPscheme} {URIauthorityLoose} {FTPorFILEpath} {FTPtype}? {URIfragment}?

FILEscheme = [fF][iI][lL][eE] "://"
FILEurl = {FILEscheme} {URIhostLoose}? {FTPorFILEpath} {URIfragment}?

URL = {HTTPurl} | {FTPurl} | {FILEurl}

EMAILquotedString = [\"] ([\u0001-\u0008\u000B\u000C\u000E-\u0021\u0023-\u005B\u005D-\u007E] | [\\] [\u0000-\u007F])* [\"]
EMAILatomText = [A-Za-z0-9!#$%&'*+-/=?\^_`{|}~]
EMAILlabel = {EMAILatomText}+ | {EMAILquotedString}
EMAILlocalPart = {EMAILlabel} ("." {EMAILlabel})*
EMAILdomainLiteralText = {ALPHANUM}|{DomainNameLoose}
//EMAILdomainLiteralText = ([\u0001-\u0008\u000B\u000C\u000E-\u005A\u005E-\u007F]|[\\][\u0000-\u007F])*{ALPHANUM}
// DFA minimization allows {IPv6Address} and {IPv4Address} to be included
// in the {EMAILbracketedHost} definition without incurring any size penalties,
// since {EMAILdomainLiteralText} recognizes all valid IP addresses.
// The IP address regexes are included in {EMAILbracketedHost} simply as a
// reminder that they are acceptable bracketed host forms.
EMAILbracketedHost = "["? ({EMAILdomainLiteralText}+ | {IPv4Address} | [iI][pP][vV] "6:" {IPv6Address}) "]"?
EMAIL = {EMAILlocalPart} "@" ({EMAILbracketedHost})

 //  {ALPHANUM} "://" {HOST} (ALPHANUM|\/)*
// URL =  ({ALPHA}({ALPHANUM}|-)+:(/{1,3}|[a-z0-9%])|www\d{0,3}[.]|[a-z0-9.\-]+[.][a-z]{2,4}/)([^\s()<>]+|\(([^\s()<>]+|(\([^\s()<>]+\)))*\))+(\(([^\s()<>]+|(\([^\s()<>]+\)))*\)|[^\s`!()\[\]{};:'".,<>?«»“”‘’]))


// floating point, serial, model numbers, ip addresses, etc.
// every other segment must have at least one digit
NUM        = ({ALPHANUM} {P} {HAS_DIGIT}
           | {HAS_DIGIT} {P} {ALPHANUM}
           | {ALPHANUM} ({P} {HAS_DIGIT} {P} {ALPHANUM})+
           | {HAS_DIGIT} ({P} {ALPHANUM} {P} {HAS_DIGIT})+
           | {ALPHANUM} {P} {HAS_DIGIT} ({P} {ALPHANUM} {P} {HAS_DIGIT})+
           | {HAS_DIGIT} {P} {ALPHANUM} ({P} {HAS_DIGIT} {P} {ALPHANUM})+)


/* floating point literals */
DoubleLiteral = ({FLit1}|{FLit2}|{FLit3}) {Exponent}?

FLit1    = [0-9]+ \. [0-9]*
FLit2    = \. [0-9]+
FLit3    = [0-9]+
Exponent = [eE] [+-]? [0-9]+

// punctuation
P	         = ("_"|"-"|"/"|"."|",")

Q = [’'`]

PUNCT = ({P}|{Q}|[?!@#$%\^&*_:;\]\[\"»«\202\204\206\207\213\221\222\223\224\225\226\227\233])

// at least one digit
HAS_DIGIT  = ({LETTER}|[:digit:])* [:digit:] ({LETTER}|[:digit:])*


LETTER     = ([:letter:]|¨)

ENGLISH_CLITIC = ({Q}(ll|d|ve|s|re|LL|D|VE|S|RE|m|M|n|N|[eE][mM])?|[nN]{Q}[Tt])

FRENCH_CLITIC = (-t-elles?|-t-ils?|-t-on|-ce|-elles?|-ils?|-je|-la|-les?|-leur|-lui|-mêmes?|-m\'|-moi|-nous|-on|-toi|-tu|-t\'|-vous|-en|-y|-ci|-là)

IRISH_O = [Oo]{Q}

FRENCH_INIT_CLITIC = ([dcjlmnstDCJLNMST]\'|[Qq]u\'|[Jj]usqu\'|[Ll]orsqu\')

CLITIC = ({ENGLISH_CLITIC}|{FRENCH_CLITIC})

INIT_CLITIC = ({FRENCH_INIT_CLITIC})

POLISH_CONDITIONAL_CLITIC = (by)

POLISH_CONDITIONAL_ENDING = (m|ś|śmy|ście)?

POLISH_PAST_ENDING_1 = (ś|śmy|ście)
POLISH_PAST_ENDING_2 = ([mś]?|śmy|ście)

WHITESPACE = \s

EMOTICON = ( [<>]?[BX;8:=][o\-\']?[DdPp()\/3>oO]+|<\/?3+|ಠ_ಠ)

TWITTER_HANDLE = @{ALPHA}{ALPHANUM}?
TWITTER_HASHTAG = #{ALPHANUM}

// blocks of question marks and exclamation marks are one token
LONG_END_PUNCT = [?!][?!1]+

WORD = ({IRISH_O}?{ALPHANUM}+|[Qq]ur{Q}an)

// http://www.englishleap.com/other-resources/abbreviations
ABBR_TITLE = ({g}{e}{n}|{h}{o}{n}|{d}{r}|{m}{r}?{s}?|{r}{e}{v}|{p}{r}{o}{f}|{s}{t}|{j}{r}|{s}{r})
ABBR_GEN = ({a}{l}|{c}{a}|{c}{o}|{i}{n}{c}|{l}{t}{d}|{e}{t}{c}|{v}{s}?|{e}{s}{t}|{f}{i}{g}|{h}{r}{s}?|{m}{t}|{d}{e}{p}{t}|{s}{q}|{o}{z}|{a}{v}{e}|{a}{s}{s}{n})
ABBR = ({ABBR_TITLE}|{ABBR_GEN})


%s OPEN_QUOTE POLISH_CONDITIONAL_MODE JUST_AFTER_PERIOD CLITIC_MODE

%%


<POLISH_CONDITIONAL_MODE>{POLISH_CONDITIONAL_CLITIC} / {POLISH_CONDITIONAL_ENDING}                                      { yybegin(YYINITIAL); return currentToken(); }
<POLISH_CONDITIONAL_MODE>[^b].                                        { throw new RuntimeException("..." + currentToken());}
{EMDASH}                                                 {return currentToken();}
{URL}                                                         { return currentToken(); }

// special words
{c}an / not                                                      {return currentToken();}
{l}em / me                                                      {return currentToken();}
{g}on / na                                                      {return currentToken();}
{g}im / me                                                      {return currentToken();}
{w}an / na                                                      {return currentToken();}
{g}ot / ta                                                      {return currentToken();}

// acronyms that end a sentence

// we can't ask if we're at EOF, so this is a hack to say append a period if we hit EOF and just generated a period
{LETTER}+\.({LETTER}+\.)+       {acro_period = yychar() + yylength(); return currentToken();}
{LETTER}+\./{WHITESPACE}        {return currentToken();}
{ABBR}\.                        {acro_period = yychar() + yylength();  return currentToken();}

// contractions and other clitics
{INIT_CLITIC}                                           {return currentToken();}
<CLITIC_MODE>{CLITIC}                                          {yybegin(YYINITIAL); return currentToken(currentToken()._2().token().replaceAll("’", "'"));}
// make sure the clitic is at the end of the word
{WORD} / {CLITIC}                                        {yybegin(CLITIC_MODE); return currentToken();}
d{Q} / ye                                                        {return currentToken(); }
{Q}[Tt] / is                                                           {return currentToken(); }

// polish clitics
{ALPHANUM}{ALPHANUM}+[lł][aeoiy]? / {POLISH_CONDITIONAL_CLITIC}{POLISH_CONDITIONAL_ENDING}             {yybegin(POLISH_CONDITIONAL_MODE); return currentToken(); }
{ALPHANUM}{ALPHANUM}+[lł][aeoiy]? / {POLISH_PAST_ENDING_1}                    {return currentToken(); }
// need to not let lam through....
{ALPHANUM}{ALPHANUM}+[ł][aeoiy]? / {POLISH_PAST_ENDING_2}                    {return currentToken(); }

// times
[01]?[0-9]{WHITESPACE}?:[0-6][0-9]                              { return currentToken(currentToken()._2().token().replaceAll("\\s+","")); }

// quotes
<YYINITIAL>\"/{WHITESPACE}*{ALPHANUM}              { yybegin(OPEN_QUOTE); return currentToken("``"); }
<YYINITIAL>'/{WHITESPACE}*{ALPHANUM}               { yybegin(OPEN_QUOTE); return currentToken("`"); }
‘                                                  { yybegin(OPEN_QUOTE); return currentToken("`"); }
’                                                  { yybegin(YYINITIAL); return currentToken("'"); }
<OPEN_QUOTE>\"                                                 { yybegin(YYINITIAL); return currentToken("''"); }
“                                                 { yybegin(YYINITIAL); return currentToken("``"); }
”                                                 { yybegin(YYINITIAL); return currentToken("''"); }
\"/.*{ALPHANUM}+                                  { yybegin(OPEN_QUOTE); return currentToken("``"); }
\"                                                { yybegin(YYINITIAL); return currentToken("''"); }




// normal stuff
// dashed words
{WORD}({DASH}{NEWLINE}*{WORD})+                                           {return currentToken();}
{TWITTER_HANDLE}                                                     { return currentToken(); }
{TWITTER_HASHTAG}                                                     { return currentToken(); }
{WORD}                                        {return currentToken();}
{ABBRNYM}                                                      { return currentToken(); }
{COMPANY}                                                      { return currentToken(); }
{EMAIL}                                                        { return currentToken(); }
{HOST}                                                         { return currentToken(); }
{NUM}                                                          { return currentToken(); }
{ACRONYM_DEP}                                                  { return currentToken(); }
{WHITESPACE}                                                   {} 
// \(                                                  {return currentToken("-LRB-");}
// \)                                                  {return currentToken("-RRB-");}
//\{                                                  {return currentToken("-LCB-");}
//\}                                                  {return currentToken("-RCB-");}
//\[                                                  {return currentToken("-LSB-");}
//\]                                                  {return currentToken("-RSB-");}
([.][.]+|…+)                                                 {return currentToken("...");}
{LONG_END_PUNCT}                                        { return currentToken();}
{PUNCT}                                               { return currentToken();}
{EMOTICON}                                          { return currentToken();}
{DASH}{DoubleLiteral}                               { return currentToken();}
.                                                   { return currentToken();}


