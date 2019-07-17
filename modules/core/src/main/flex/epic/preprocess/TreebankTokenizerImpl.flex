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

SLASH = [⁄∕／/]


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
P	         = ("_"|"-"|"."|",")|{SLASH}

Q = [’'`]

PUNCT = ({P}|{Q}|[?!@#$%\^&*_:;\]\[\"»«\202\204\206\207\213\221\222\223\224\225\226\227\233])

// at least one digit
HAS_DIGIT  = ({LETTER}|[:digit:])* [:digit:] ({LETTER}|[:digit:])*


LETTER     = ([:letter:]|¨)

UPPER     = ([:uppercase:])
LOWER     = ([:lowercase:])
CONSONANT = [bcdfghjklmnpqrstvwxzBCDFGHJKLMNPQRSTVWXZł]

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
ABBR_TITLE_1 = ({g}{e}{n}{s}?|{h}{o}{n}|{d}{r}{s}?|{m}{r}?{s}?|{r}{e}{v}|{p}{r}{o}{f}|{s}{t}|{j}{r}|{s}{r}|{m}{e}?{s}{s}{r}{s}|{m}{s}{e}{s}|{m}{d}?{l}{l}{e}{s}?|{a}{t}{t}{y})
ABBR_TITLE_2 = ({c}{a}{p}{t}|{a}{d}{m}|{m}{a}{j}|{p}{r}{e}{s}|{e}{s}{q}{s}?)
ABBR_TITLE = {ABBR_TITLE_1}|{ABBR_TITLE_2}
ABBR_UNIT = ([kKmM]{g}|{l}{b}|{m}{l}|{y}{d}|{m}{i}{n}|{y}{r}|{d}{o}{z}|{t}{b}?{s}{p}|{s}{e}{c}|{y}{b}{p}|{y}{r}|mi|{n}{a}{u}{t}){s}?
ABBR_DATE = {d}{e}{c}|{j}{a}{n}|{f}{e}{b}{r}?|{a}{p}{r}|{j}{u}{n}|{j}{u}{l}|{a}{u}{g}|{s}{e}{p}{t}?|{o}{c}{t}|{n}{o}{v}|{m}{o}{n}|{t}{u}{e}{s}?|{t}{h}{u}({r}{s}?)?|{f}{r}{i}|Wed|WED
ABBR_ADDR = ({a}{v}{e}|{p}{k}{w}{y}|{p}{t}{s}?|{c}{n}{t}{y}|{l}{o}{c}|{h}{w}{y})
ABBR_GEN = ({a}{l}|{c}{a}|{c}{o}|{i}{n}{c}|{l}{t}{d}|{e}{t}{c}|{v}{s}?|{e}{s}{t}|{f}{i}{g}|{h}{r}{s}?|{m}{t}|{d}{e}{p}{t}|{s}{q}|{o}{z}|{a}{s}{s}{n}|{a}{s}{s}{o}{c}|{a}{b}{t}|{t}{e}{l}|{a}vg|{q}{t}{y}|opp|{i}ntro|{b}{r}{o}{s}?|{m}{i}{s}{c}|{e}{s}{p}|{i}{b}{i}{d}|{v}{i}{z}|{a}{t}{t}{n}|{a}{p}{p}?{r}{o}{x})
ABBR_INST = ({u}{n}{i}{v}|{t}{e}{c}{h}|{a}{c}{a}{d}|{e}{x}{c}{h}|{g}{o}{v}{t}?){s}?|{i}?{n}{a}?{t}{l}|{c}{o}{r}{p}
ABBR_TECH = ({e}{s}{c}|{t}{x})
// countries from https://travel.state.gov/content/visas/en/fees/country-acronyms.html and elsewhere
ABBR_GEO_1 = {a}{f}{g}{h}|{a}{l}{b}|{a}{l}{g}{r}|{a}{n}{d}{o}|{a}{n}{g}{l}|{a}{n}{g}{u}|{a}{n}{t}{i}|{a}{r}{g}|{a}{r}{m}|{a}{r}{b}|{a}{s}{t}{l}|{a}{u}{s}{t}|{a}{z}{r}|{b}{a}{m}{a}|{b}{a}{h}{r}|{b}{r}{d}{o}|{b}{y}{s}|{b}{e}{l}{g}|{b}{l}{z}|{b}{e}{n}{n}|{b}{e}{r}{m}|{b}{h}{u}|{b}{o}{l}|{b}{i}{h}|{b}{o}{t}|{b}{r}{a}{z}|{b}{r}{z}{l}|{b}{r}{n}{i}|{b}{u}{l}{g}|{b}{u}{r}{k}|{b}{u}{r}{m}|{b}{r}{n}{d}|{c}{b}{d}{a}|{c}{m}{r}{n}|{c}{a}{v}{i}|{c}{a}{y}{i}|{c}{a}{f}{r}|{c}{h}{i}{l}|{c}{h}{i}{n}|{c}{o}{l}|{c}{o}{m}{o}|{c}{o}{n}{b}|{c}{o}{d}|{c}{s}{t}{r}|{i}{v}{c}{o}|{h}{r}{v}|{c}{y}{p}{r}|{c}{z}{e}{c}|{d}{e}{n}|{d}{j}{i}|{d}{o}{m}{n}|{d}{o}{m}{r}|{e}{c}{u}{a}|{e}{g}{y}{p}|{e}{l}{s}{l}|{e}{g}{n}|{e}{r}{i}|{e}{s}{t}|{e}{t}{h}|{f}{i}{n}|{f}{r}{a}{n}|{g}{a}{b}{n}|{g}{a}{m}|{g}{r}{z}|{g}{e}{r}|{g}{h}{a}{n}|{g}{i}{b}|{g}{r}{c}|{g}{r}{e}{n}|{g}{u}{a}{t}|{g}{n}{e}{a}|{g}{u}{i}{b}|{g}{u}{y}|{h}{a}{t}|{v}{a}{t}|{h}{o}{n}{d}|{h}{o}{k}{o}|{h}{u}{n}{g}|{i}{c}{l}{d}|{i}{m}{d}|{i}{d}{s}{a}|{i}{r}{e}|{i}{s}{r}{l}|{i}{t}{l}{y}|{j}{a}{m}|{j}{p}{n}|{j}{o}{r}{d}|{k}{a}{z}|{k}{e}{n}{y}|{k}{i}{r}{i}|{p}{r}{k}|{k}{o}{r}|{k}{u}{w}{t}|{k}{g}{z}|{l}{a}{t}{v}|{l}{e}{b}{n}|{l}{e}{s}|{l}{i}{b}{r}|{l}{b}{y}{a}|{l}{c}{h}{t}|{l}{i}{t}{h}|{l}{x}{m}|{m}{a}{c}|{m}{k}{d}|{m}{a}{d}{g}|{m}{a}{l}{w}|{m}{l}{a}{s}|{m}{l}{d}{v}|{m}{l}{t}{a}|{m}{a}{u}{r}|{m}{r}{t}{s}|{m}{e}{x}|{m}{l}{d}|{m}{o}{n}|{m}{o}{n}{g}|{m}{o}{n}{t}|{m}{o}{r}{o}|{m}{o}{z}|{n}{a}{m}{b}|{n}{a}{u}|{n}{e}{p}|{n}{e}{t}{h}|{n}{e}{t}{a}|{n}{c}{a}{l}|{n}{z}{l}{d}|{n}{i}{c}|{n}{i}{r}|{n}{r}{a}|{n}{o}{r}{w}|{p}{k}{s}{t}|{p}{a}{l}{a}|{p}{a}{n}|{p}{n}{g}|{p}{a}{r}{a}|{p}{h}{i}{l}|{p}{i}{t}{c}|{q}{t}{a}{r}|{r}{o}{m}|{r}{u}{s}|{r}{w}{n}{d}|{s}{h}{e}{l}|{s}{t}{c}{n}|{s}{l}{c}{a}|{s}{t}{v}{n}|{w}{s}{a}{m}|{s}{m}{a}{r}|{s}{t}{p}{r}|{s}{a}{r}{b}|{s}{e}{n}{g}|{s}{e}{y}{c}|{s}{l}{e}{o}|{s}{i}{n}{g}|{s}{v}{k}|{s}{v}{n}|{s}{l}{m}{n}|{s}{o}{m}{a}|{s}{a}{f}{r}|{s}{p}{n}|{s}{r}{l}|{s}{u}{d}{a}|{s}{u}{r}{m}|{s}{z}{l}{d}|{s}{w}{e}{d}|{s}{w}{d}{n}|{s}{w}{t}{z}|{s}{y}{r}|{t}{w}{a}{n}|{t}{j}{k}|{t}{a}{n}{z}|{t}{a}{z}{n}|{t}{h}{a}{i}|{t}{m}{o}{r}|{t}{o}{n}{g}|{t}{r}{i}{n}|{t}{n}{s}{a}|{t}{r}{k}{y}|{t}{k}{m}|{t}{c}{i}{s}|{t}{u}{v}|{u}{g}{a}{n}|{u}{k}{r}|{u}{a}{e}|{g}{r}{b}{r}|{u}{r}{u}|{u}{z}{b}|{v}{a}{n}{u}|{v}{e}{n}{z}|{v}{t}{n}{m}|{b}{r}{v}{i}|{w}{a}{f}{t}|{s}{s}{a}{h}|{y}{e}{m}|{y}{u}{g}{o}|{z}{a}{m}{b}?|{z}{i}{m}{b}?
ABBR_GEO_2 = ({d}{j}{i}{b}|{b}{u}{r}{m}|{n}{e}{w}{f}|{n}{a}{m}{i}{b}|{s}{w}{i}{t}{z}|{s}{e}{r}{b}|{m}{a}{c}{e}{d}|{c}{a}{n}{a}{d}|{a}{f}{g}|{q}{u}{e}{e}{n}{s}{l})
ABBR_GEO_STATE = ({a}{l}{a}|{a}{r}{i}{z}|{a}{r}{k}|{c}{a}{l}{i}{f}|{c}{o}{l}{o}|{c}{o}{n}{n}|{d}{e}{l}|{d}[.]{c}|{f}{l}{a}|{g}{a}|{i}{l}{l}|{i}{n}{d}|{k}{a}{n}{s}|{k}{y}|{l}{a}|{m}{d}|{m}{a}{s}{s}|{m}{i}{c}{h}|{m}{i}{n}{n}|{m}{o}|{m}{o}{n}{t}|{n}{e}{b}{r}?|{n}{e}{v}|{o}{k}{l}{a}|{o}{r}{e}|{p}{a}|{t}{e}{n}{n}|{t}{e}{x}|{v}{t}|{v}{a}|{w}{a}{s}{h}|{w}.{v}{a}|{w}{i}{s}|{w}{y}{o})
ABBR_GEO = {ABBR_GEO_1}|{ABBR_GEO_2}|{ABBR_GEO_STATE}
ABBR_FIELD = {a}{s}{t}{r}{o}[nNmMLl]?|{b}{i}{o}{l}?|{c}{h}{e}{m}|{e}{c}{o}{n}|{e}{d}{u}{c}|{e}{n}{g}|{g}{e}{o}{g}{r}|{g}{e}{o}{l}|{g}{e}{o}{m}|{s}{c}{i}|{z}{o}{o}{l}
ABBR_WEIRD = ([AaNn]{b}-{w}{h}{g})
ABBR_MUSIC = {c}{r}{e}{s}{c}|{d}{e}{c}{r}{e}{s}{c}|{d}{i}{m}|{d}{i}{v}|{f}{g}|{g}{l}{i}{s}{s}|{g}{r}|{t}{r}|{k}{b}|{k}{l}|{f}{l}|{o}{r}{d}|{p}{e}{d}|{p}{i}{z}{z}|{p}{k}|{p}{o}{s}|{p}{s}|{r}{a}{l}{l}|{r}{i}{t}|{r}{k}|{s}|{s}{f}{o}{r}{z}|{s}{i}{m}|{s}{m}{o}{r}{z}|{s}{o}{r}{d}|{s}{o}{s}{t}|{s}{p}{i}{c}{c}|{s}{t}{a}{c}{c}|{t}{h}{p}|{t}{r}|{t}{r}{e}{m}|{v}{i}{b}|{l}{i}{b}
ABBR_POLISH = {p}skow|{z}akarp|{n}iem|{b}ałt|{b}sł|{b}ułg|{d}łuż|{g}łuż|{m}łpol|{p}ołab|{p}sł|{s}tbułg|{s}ła|{s}łe|{s}łi|{s}łow|{w}łos|{i}slandzki
ABBR_BIBLE = {g}{e}{n}|{g}{e}|{g}{n}|{e}{x}{o}|{e}{x}|{e}{x}{o}{d}|{l}{e}{v}|{l}{e}|{l}{v}|{n}{u}{m}|{n}{u}|{n}{m}|{n}{b}|{d}{e}{u}{t}?|{d}{t}|{j}{o}{s}|{j}{s}{h}|{j}{u}{d}{g}|{j}{d}{g}|{j}{g}|{j}{d}{g}{s}|{r}{t}{h}|{r}{u}|{e}{z}{r}|{n}{e}{h}|{n}{e}|{e}{s}{t}{h}|{e}{s}|{j}{o}{b}|{j}{o}{b}|{j}{b}|{p}{s}{l}{m}|{p}{s}|{p}{s}{a}|{p}{s}{m}|{p}{s}{s}|{p}{r}{o}{v}|{p}{r}|{p}{r}{v}|{e}{c}{c}{l}{e}{s}|{e}{c}|{e}{c}{c}{l}?|{q}{o}{h}|{i}{s}{a}|{i}{s}|{j}{e}{r}|{j}{e}|{j}{r}|{l}{a}{m}|{l}{a}|{e}{z}{e}{k}|{e}{z}{e}|{e}{z}{k}|{d}{a}{n}|{d}{a}|{d}{n}|{h}{o}{s}|{h}{o}|{j}{l}|{a}{m}|{o}{b}{a}{d}|{o}{b}|{j}{n}{h}|{j}{o}{n}|Macc|{m}{i}{c}|{n}{a}{h}|{n}{a}|{h}{a}{b}|{h}{a}{b}|{z}{e}{p}{h}|{z}{e}{p}|{z}{p}|{h}{a}{g}{g}{a}{i}|{h}{a}{g}|{h}{g}|{z}{e}{c}{h}|{z}{e}{c}|{z}{c}|{m}{a}{l}|{m}{a}{l}|{m}{l}|{t}{o}{b}{i}{t}|{t}{o}{b}|{t}{b}|{j}{d}{t}{h}|{j}{d}{t}|{j}{t}{h}|{a}{e}{s}|{a}{d}{d}{e}{s}{t}{h}|{w}{i}{s}{d}|{w}{i}{s}|{w}{s}|{s}{i}{r}{a}{c}{h}|{e}{c}{c}{l}{u}{s}|{b}{a}{r}{u}{c}{h}|{b}{a}{r}|{l}{j}{e}|{t}{h}{e}{s}{s}|{t}{h}{r}|{p}{r}|{a}{z}|{s}{u}{s}|{b}{e}{l}|{b}{e}{l}|{p}{m}{a}|{p}{s}{s}{o}{l}|{l}{a}{o}{d}|{m}{t}|{m}{r}{k}|{m}{k}|{m}{r}|{l}{u}{k}|{l}{k}|{j}{n}|{j}{h}{n}|{a}{c}|{r}{o}{m}|{r}{o}|{r}{m}|{c}{o}|{g}{a}{l}|{g}{a}|{e}{p}{h}{e}{s}|{e}{p}{h}|{c}{o}{l}{l}|{c}{o}{l}{l}({o}{s})?|{t}{i}{t}|{p}{h}{i}{l}{e}{m}|{p}{h}{m}|{h}{e}{b}|{j}{a}{s}|{j}{m}|{j}{u}{d}|{r}{e}{v}|{r}{e}|{v}{u}{l}{g}
ABBR_LEGAL = {a}{n}{n}{o}{t}{s}?|{b}{i}{b}{l}{i}{o}{g}|{b}{k}{s}?|{c}{h}{s}?|{c}{l}{s}?|{c}{o}{l}{s}?|{c}{m}{t}{s}?|{d}{e}{c}{s}?|{d}{e}{p}{t}{s}?|{d}{i}{v}{s}?|{e}{x}{s}?|{f}{i}{g}{s}?|{f}{o}{l}{s}?|{h}{y}{p}{o}|{i}{l}{l}{u}{s}|{i}{n}{t}{r}{o}|{l}|{l}{l}|{p}|{p}{p}|{p}{a}{r}{a}{s}?|{p}{t}{s}?|{p}{m}{b}{l}|{p}{r}{i}{n}{c}{s}?|{s}{c}{h}{e}{d}{s}?|{s}{e}{c}{s}?|{s}{e}{r}|{s}{u}{b}{d}{i}{v}{s}?|{s}{u}{b}{s}{e}{c}{s}?|{s}{u}{p}{p}|{t}{b}{l}{s}?|{t}{i}{t}{s}?|{v}{o}{l}{s}?|{q}{u}{o}{t}{s}?|{b}ankr|{d}et|{j}ur(is)?|({n}on)?{a}cq

// abbreviations that can show up at the end of sentences. If so, period should be kept and split
FINAL_ABBRS = ("&"{c})|{ABBR_GEN}|{ABBR_TITLE}|{ABBR_WEIRD}|{ABBR_GEO}|{ABBR_DATE}|{ABBR_ADDR}|{ABBR_UNIT}|{ABBR_MUSIC}|{ABBR_BIBLE}|{ABBR_POLISH}|{ABBR_LEGAL}|{ABBR_TECH}|{ABBR_INST}|{ABBR_FIELD}

// abbreviations and similar things that end in periods that probably aren't sentence final.
// These tend to show up in more formal settings so I'm less permissive about case.
// e.g. Abh is short for "Abhandlung" which is a german word for treatise
// TODO: leaving out sing, pass, sup, etc... too risky
ABBR_DICT = {a}{c}{c}|{a}{b}{l}|{a}{d}{j}|def|det|aux|{p}lur|decl|{c}onj(ug)?|{a}{b}{b}{r}({e}?{v})?|{n}eg|{p}ron|{n}eutr?|{p}oss|{s}uperl|masc|fem|{i}ntrans|{t}rans|{n}om
ABBR_LANG = (OHG|OLith|OPers|OPr|OE|OIc|OS|ORuss|OFris|OF|ON|ONF|indogerm({a}?{n})?|idg|urgerm|Hitt|senlat|etrusk|{h}ebr|{m}alay|Icel|Balt|{l}{s}orb|ClArm|Cyr|Du)
EXPLICIT_MEDIAL_ABBRS = ({r}{e}{g}|{i}{n}{f}{o}|{p}{r}{o}{b}|{a}{n}{o}{n}|{f}{i}?{g}|{t}{e}{m}{p}|{c}iv)
MEDIAL_ABBR_FIELD = {n}at
MEDIAL_ABBR_TITLE = {a}{d}{m}{i}{n}|{c}{a}{p}
MEDIAL_ABBR_LEGAL = Cent
MEDIAL_ABBR_GEO = Cal|Germ|Ma|Carib|{m}{i}{s}{s}|{c}{h}{i}
MEDIAL_ABBR_INST = Fed
MEDIAL_WEIRD_ABBRS = ({a}{f}{r}|AS|Abh|Abl|Abp)
// 4 consonants in a row followed by a period in the middle of a sentence is probably an abbreviation
MEDIAL_HEUR_ABBR = {CONSONANT}{CONSONANT}({CONSONANT}{CONSONANT}?)?

MEDIAL_ABBRS = ({LETTER}|[:digit:]+|{MEDIAL_WEIRD_ABBRS}|{MEDIAL_HEUR_ABBR}|{EXPLICIT_MEDIAL_ABBRS}|{ABBR_LANG}|{MEDIAL_ABBR_GEO}|{MEDIAL_ABBR_LEGAL}|{MEDIAL_ABBR_TITLE}|{ABBR_DICT}|{MEDIAL_ABBR_INST}|{ABBR_AUTO}|{MEDIAL_ABBR_TITLE}|{MEDIAL_ABBR_FIELD})

// These were automatically scraped from wiktionary. we'll only use them as "medial" abbrs to try
// to avoid ones that are really words
// i filtered a lot of these by hand b/c jflex was dying
ABBR_AUTO_A = Abrom|Adver|Afzel|Ag|Agric|Ags|Ahd|Aitch|Ak|Akad|Alder|Alg|Alt|Ambr|Ambros|Amer|Amo|Andr|Andrz|Anf|Anm|Anz|Ap|Apg|Appl|Apul|Arcang|Arn|Arp|Asch|Assemb|Atl|Atte|Aubl|Aufl|Aus|Austl|Auth|Auto|Av|Avda|Avest|Azer
ABBR_AUTO_B = Bab|Bacig|Balf|Bangl|Bankr|Bartl|Batt|Baumg|Becc|Bech|Bedd|Belr|Benth|Bercht|Berl|Bernh|Bi|Binn|Biv|Boed|Boenn|Boiss|Bonpl|Bor|Borkh|Bornm|Bos|Briq|Brit|Brot|Bull|Burch|Bus|Buxb|Byz
// OK
ABBR_AUTO_C = Cas|Cgy|Christm|Chron|Cir|Cmty|Colom|Com|Comdr|Comm|Commw|Comp|Concil|Condo|Confed|Cong|Consol|Constr|Cor|Corr|Crim|Cust|Cymb
// OK
ABBR_AUTO_D = Dall|Dat|Deb|Def|Dev|Dir|Dist|Distrib|Dor
// OK
ABBR_AUTO_E = Ea|Ecu|Educ|Egy|Elec|Emer|Emp|Ens|Envtl|Ep|Eq|Esd|Eur|Exd|Exp|Ez
// OK
ABBR_AUTO_G = Gend|Glag|Gral|Guad|Guar
// OK
ABBR_AUTO_H = Hants|Hosp|Hous|Hpe|Hsa
// OK
ABBR_AUTO_I = Ii|Indem|Indep|Indon|Indus|Ins|Inst|Inv|Ir|Isr
// OK
ABBR_AUTO_J = Jerem|Jor|Juv
// OK
ABBR_AUTO_K = Kyrg
// OK
ABBR_AUTO_L = Leb|Legis|Leics|Liab|Liber|Liech|Lincs|Litig|Ltée|Lux
ABBR_AUTO_M = Mach|Madag|Magis|Maint|Mar|Mat|Med|Mel|Merch|Micr|Mons|Mortg|Mozam|Mun|Mut|Myan
ABBR_AUTO_N = Nicar
ABBR_AUTO_O = Ouk|Oba|Oc|Octr|Off|Ol|Oll|Ont|Op|Or|Org|Osm
// OK
ABBR_AUTO_P = Pac|Pak|Pers|Pharm|Pol|Prerog|Prod|Prop|Prot|Pte|Ptti|Pub
// OK
ABBR_AUTO_R = Rdguez|Ref|Rehab|Reorg|Rep|Reprod|Res|Ret|Russ|Ry
// OK
ABBR_AUTO_S = Sab|Sar|Sask|Sav|Sci|Se|Sen|Sens|Serv|Sey|Shak|Slovk|Slovn|Soc|Som|Sout|Sta|Staffs|Ste|Subcomm|Sun|Sup|Sur|Surin|Swaz|Syd|Sys
// OK
ABBR_AUTO_T = Taj|Tas|Tbe|Tbni|Telecomm|Terr|Timp|Transcon|Transp|Turkm
// OK
ABBR_AUTO_V = Ven|Venez|Vet|Vic|Vict|Viet|Vill|Vis|Visc|Vla|Voc
// OK
ABBR_AUTO_W = Wa|Worcs
// OK
ABBR_AUTO_X = Xyl
// OK
ABBR_AUTO_a = ab[smpdbvr]?|acct|ad|adv|aeq|aff|agric|ags|alez|alk|amer|anat|ann|ans|apod|appr|app?rox|appt|attd|attn|aud|aum|auth
// OK
ABBR_AUTO_b = beg|bet|bibl|bldgs|boul
// OK
ABBR_AUTO_c = cit|cochl|coef|comb|comm|comms|comp|compar|compl|concr|cond|conf|constr|cont|corp|cos|cres|crossref|cust
// OK
ABBR_AUTO_d = de|deg|dem|dep|desubst|diag|dial|diam|dir|diss|dist
// OK
ABBR_AUTO_e = ea|ect|edd|eg|elec|elev|emp|encyc|eng|engin|environ|ep|epigr|eqn|equiv|estd|eur|eval|exec|exp|expos|ext
// OK
ABBR_AUTO_f = fam|fed|fiń|freq|frwy|fut
// OK
ABBR_AUTO_h = hisp|hist|histt|hnos|hosp|hyp
// OK
ABBR_AUTO_i = ib|id|ie|imp|imper|imperf|impf|impp|indecl|indef|indet|inf|inkl|ins|inst|interj|intr|inv|iter
// OK
ABBR_AUTO_l = lit|loq
// OK
ABBR_AUTO_m = mil|mov|mut|mutat
// OK
ABBR_AUTO_o = obstetr|obv|op|orch|org
// OK
ABBR_AUTO_p = per|perc|perf|pers|pol|poly|prec|pret|prog|prox
// OK
ABBR_AUTO_r = rad|rec|recip|ref|refl|rel|repl|repr|resp|retd|ry
// OK
ABBR_AUTO_s = sce|sci|seq|soc|spec|specif|subf|subpara|subparas|subpt|subsp|subst|substs|substt|subvar|supt|symp|synt|syst
// OK
ABBR_AUTO_t = technol|traf|trans[fl]?|troch|trop|trops
// OK
ABBR_AUTO_u = ult|unabr|unk|usu
// OK
ABBR_AUTO_v = vac|var|ver|vic|vid|vindicat|voc|vocs|voy
ABBR_AUTO = {ABBR_AUTO_A}|{ABBR_AUTO_B}|{ABBR_AUTO_C}|{ABBR_AUTO_D}|{ABBR_AUTO_E}|{ABBR_AUTO_G}|{ABBR_AUTO_H}|{ABBR_AUTO_I}|{ABBR_AUTO_J}|{ABBR_AUTO_K}|{ABBR_AUTO_L}|{ABBR_AUTO_M}|{ABBR_AUTO_N}|{ABBR_AUTO_O}|{ABBR_AUTO_P}|{ABBR_AUTO_R}|{ABBR_AUTO_S}|{ABBR_AUTO_T}|{ABBR_AUTO_V}|{ABBR_AUTO_W}|{ABBR_AUTO_X}|{ABBR_AUTO_a}|{ABBR_AUTO_b}|{ABBR_AUTO_c}|{ABBR_AUTO_d}|{ABBR_AUTO_e}|{ABBR_AUTO_f}|{ABBR_AUTO_h}|{ABBR_AUTO_i}|{ABBR_AUTO_l}|{ABBR_AUTO_m}|{ABBR_AUTO_o}|{ABBR_AUTO_p}|{ABBR_AUTO_r}|{ABBR_AUTO_s}|{ABBR_AUTO_t}|{ABBR_AUTO_u}|{ABBR_AUTO_v}




%s OPEN_QUOTE POLISH_CONDITIONAL_MODE JUST_AFTER_PERIOD CLITIC_MODE

%%


// dates and fractions

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
{LETTER}*\.(({LETTER}|\-)+\.)+       {acro_period = yychar() + yylength(); return currentToken();}
{FINAL_ABBRS}\.                        {acro_period = yychar() + yylength();  return currentToken();}
// medial abbrs shouldn't consume final period. these are things like "1." which in the middle of a
// sentence are probably one token, but probably aren't at the end.
{MEDIAL_ABBRS}\./{WHITESPACE}{ALPHANUM}                        {return currentToken();}
{MEDIAL_ABBRS}/\.                        {return currentToken();}

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
<OPEN_QUOTE>\"                                     { yybegin(YYINITIAL); return currentToken("''"); }
“                                                 { yybegin(YYINITIAL); return currentToken("``"); }
”                                                 { yybegin(YYINITIAL); return currentToken("''"); }
\"/{ALPHANUM}+                                  { yybegin(OPEN_QUOTE); return currentToken("``"); }
\"                                                { yybegin(YYINITIAL); return currentToken("''"); }

// normal stuff
// dashed words
{WORD}({DASH}{NEWLINE}*{WORD})+                                           {return currentToken();}
{TWITTER_HANDLE}                                                     { return currentToken(); }
{TWITTER_HASHTAG}                                                     { return currentToken(); }
{WORD}                                        {return currentToken();}
{WORD}{Q}{WORD}                                        {return currentToken();}
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


