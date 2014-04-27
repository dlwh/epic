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

PUNCT = ({P}|[?!@#$%\^&*_:;\]\[\"\'])

// at least one digit
HAS_DIGIT  = ({LETTER}|[:digit:])* [:digit:] ({LETTER}|[:digit:])*

ALPHA      = ({LETTER})+

LETTER     = [:letter:]

CONTRACTION_SECOND = ('ll|'d|'ve|'s|'|'re|'LL|'D|'VE|'S|'RE|'m|'M|'n|'N)

WHITESPACE = \r\n | [ \r\n\t\f\xA0]

EMOTICON = ( [:=]-?[()\/] | <\/?3+ )


%s OPEN_QUOTE

%%

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
{LETTER}+\.{LETTER}+ / .$                                       { return currentToken() + ".";}
// contractions
{CONTRACTION_SECOND}                                           {return currentToken();}
{ALPHANUM}+ / {CONTRACTION_SECOND}                             {return currentToken();}
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
{URL}                                                         { return currentToken(); }
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


