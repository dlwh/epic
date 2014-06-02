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
%type String
%function getNextToken
%pack
%char

%{

public final int yychar()
{
    return yychar;
}

final String currentToken() {
  return new String(zzBuffer, zzStartRead, zzMarkedPos-zzStartRead);
}


private boolean insideQuote = false;
%}

THAI       = [\u0E00-\u0E59]

// basic word: a sequence of digits & letters (includes Thai to enable ThaiAnalyzer to function)
ALPHANUM   = ({LETTER}|{THAI}|[:digit:])+


// acronyms: U.S.A., I.B.M., etc.
// use a post-filter to remove dots
ACRONYM    =  {LETTER} "." ({LETTER} ".")+

ACRONYM_DEP	= {ALPHANUM} "." ({ALPHANUM} ".")+

// company names like AT&T and Excite@Home.
COMPANY    =  {ALPHA} ("&"|"@") {ALPHA}

// email addresses
EMAIL      =  {ALPHANUM} (("."|"-"|"_") {ALPHANUM})* "@" {ALPHANUM} (("."|"-") {ALPHANUM})+

// hostname
HOST       =  {ALPHANUM} ((".") {ALPHANUM})+

// url
URL =      {ALPHANUM} "://" {HOST} (ALPHANUM|\/)*


// floating point, serial, model numbers, ip addresses, etc.
// every other segment must have at least one digit
NUM        = ({ALPHANUM} {P} {HAS_DIGIT}
           | {HAS_DIGIT} {P} {ALPHANUM}
           | {ALPHANUM} ({P} {HAS_DIGIT} {P} {ALPHANUM})+
           | {HAS_DIGIT} ({P} {ALPHANUM} {P} {HAS_DIGIT})+
           | {ALPHANUM} {P} {HAS_DIGIT} ({P} {ALPHANUM} {P} {HAS_DIGIT})+
           | {HAS_DIGIT} {P} {ALPHANUM} ({P} {HAS_DIGIT} {P} {ALPHANUM})+)

// punctuation
P	         = ("_"|"-"|"/"|"."|",")

PUNCT = ({P}|[?!@#$%\^&*_:;\]\[\"\'»«\202\204\206\207\213\221\222\223\224\225\226\227\233])

// at least one digit
HAS_DIGIT  = ({LETTER}|[:digit:])* [:digit:] ({LETTER}|[:digit:])*

ALPHA      = ({LETTER})+

LETTER     = [:letter:]

ENGLISH_CLITIC = ('ll|'d|'ve|'s|'|'re|'LL|'D|'VE|'S|'RE|'m|'M|'n|'N)

FRENCH_CLITIC = (-t-elles?|-t-ils?|-t-on|-ce|-elles?|-ils?|-je|-la|-les?|-leur|-lui|-mêmes?|-m\'|-moi|-nous|-on|-toi|-tu|-t\'|-vous|-en|-y|-ci|-là)


FRENCH_INIT_CLITIC = ([dcjlmnstDCJLNMST]\'|[Qq]u\'|[Jj]usqu\'|[Ll]orsqu\')

CLITIC = ({ENGLISH_CLITIC}|{FRENCH_CLITIC})

INIT_CLITIC = ({FRENCH_INIT_CLITIC})

POLISH_CONDITIONAL_CLITIC = (by)

POLISH_CONDITIONAL_ENDING = (m|ś|śmy|ście)?

POLISH_PAST_ENDING = ([mś]?|śmy|ście)

WHITESPACE = \r\n | [ \r\n\t\f\xA0]

EMOTICON = ( [<>]?[BX;8:=][o\-\']?[DdPp()\/3>oO] | <\/?3+ | ಠ_ಠ)


%s OPEN_QUOTE POLISH_CONDITIONAL_MODE

%%


<POLISH_CONDITIONAL_MODE>{POLISH_CONDITIONAL_CLITIC} / {POLISH_CONDITIONAL_ENDING}                                      { yybegin(YYINITIAL); return currentToken(); }
<POLISH_CONDITIONAL_MODE>[^b].                                        { throw new RuntimeException("..." + currentToken());}
{URL}                                                         { return currentToken(); }

// special words
can / not                                                      {return currentToken();}
Can / not                                                      {return currentToken();}
lem / me                                                      {return currentToken();}
Lem / me                                                      {return currentToken();}
gon / na                                                      {return currentToken();}
Gon / na                                                      {return currentToken();}
gim / me                                                      {return currentToken();}
Gim / me                                                      {return currentToken();}
wan / na                                                      {return currentToken();}
Wan / na                                                      {return currentToken();}
got / ta                                                      {return currentToken();}
Got / ta                                                      {return currentToken();}

// acronyms that end a sentence

[01]?[0-9]{WHITESPACE}?:[0-6][0-9]                              { return currentToken(); }


{LETTER}+\.{LETTER}+ / .$                                       { return currentToken() + ".";}
// contractions
{INIT_CLITIC}                                           {return currentToken();}
{CLITIC}                                           {return currentToken();}
{ALPHANUM}+ / {CLITIC}                             {return currentToken();}
// polish clitics
{ALPHANUM}+[lł][aeoiy]? / {POLISH_CONDITIONAL_CLITIC}{POLISH_CONDITIONAL_ENDING}             {yybegin(POLISH_CONDITIONAL_MODE); return currentToken(); }
{ALPHANUM}+[lł][aeoiy]? / {POLISH_PAST_ENDING}                    {return currentToken(); }
d' / ye                                                        {return currentToken(); }
{ALPHANUM}+ / n't                                              {return currentToken(); }
n't                                                            {return currentToken(); }
{ALPHANUM}+ / N'T                                              {return currentToken(); }
N'T                                                            {return currentToken(); }
'T / is                                                           {return currentToken(); }
't / is                                                           {return currentToken(); }

// quotes
<YYINITIAL>\"                                                  { yybegin(OPEN_QUOTE); return "``"; }
<OPEN_QUOTE>\"                                                 { yybegin(YYINITIAL); return "''"; }




// normal stuff
{ALPHANUM}                                                     { return currentToken(); }
{ACRONYM}                                                      { return currentToken(); }
{COMPANY}                                                      { return currentToken(); }
{EMAIL}                                                        { return currentToken(); }
{HOST}                                                         { return currentToken(); }
{NUM}                                                          { return currentToken(); }
{ACRONYM_DEP}                                                  { return currentToken(); }
{WHITESPACE}                                                   {} 
\(                                                  {return "-LRB-";} 
\)                                                  {return "-RRB-";} 
\{                                                  {return "-LCB-";} 
\}                                                  {return "-RCB-";} 
\[                                                  {return "-LSB-";} 
\]                                                  {return "-RSB-";} 
[.][.]+                                                 {return "...";}
{PUNCT}                                               { return currentToken();}
{EMOTICON}                                          { return currentToken();}
.                                                   { return currentToken();}


