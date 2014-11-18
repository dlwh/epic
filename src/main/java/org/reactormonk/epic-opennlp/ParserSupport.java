// package org.reactormonk.epic.opennlp

// import java.util.HashMap;
// import java.util.Iterator;
// import java.util.LinkedList;
// import java.util.List;
// import java.util.Map;
// import opennlp.tools.util.Span;
// import opennlp.tools.parser.Parse;

// class ParseConverter {
//   private Map<Integer, Integer> mIndexMap = new HashMap<Integer, Integer>();

//   private Parse mParseForTagger;

//   private final String mSentence;

//   /**
//    * Initializes a new instance.
//    * 
//    * @param sentence
//    * @param tokens
//    */
//   public ParseConverter(String sentence, Span tokens[]) {

//     mSentence = sentence;

//     StringBuilder sentenceStringBuilder = new StringBuilder();

//     String tokenList[] = new String[tokens.length];

//     for (int i = 0; i < tokens.length; i++) {
//       String tokenString = tokens[i].getCoveredText(sentence).toString();
//       String escapedToken = escape(tokenString);
//       tokenList[i] = escapedToken;

//       int escapedStart = sentenceStringBuilder.length();
//       int start = tokens[i].getStart();
//       mIndexMap.put(escapedStart, start);

//       int escapedEnd = escapedStart + escapedToken.length();
//       int end = tokens[i].getEnd();
//       mIndexMap.put(escapedEnd, end);

//       sentenceStringBuilder.append(tokenList[i]);

//       sentenceStringBuilder.append(' ');
//     }

//     // remove last space
//     if (sentenceStringBuilder.length() > 0)
//       sentenceStringBuilder.setLength(sentenceStringBuilder.length() - 1);

//     String tokenizedSentence = sentenceStringBuilder.toString();

//     mParseForTagger = new Parse(tokenizedSentence, 
//         new Span(0, tokenizedSentence.length()), "INC", 1, null);

//     int start = 0;

//     for (String token : tokenList) {
//       mParseForTagger.insert(new Parse(tokenizedSentence, new Span(start,
//           start + token.length()),
//           opennlp.tools.parser.chunking.Parser.TOK_NODE, 0f, 0));

//       start += token.length() + 1;
//     }
//   }

//   private static String escape(String text) {
//     return text;
//   }

//   /**
//    * Creates the parse for the tagger.
//    *  
//    * @return the parse which can be passed to the tagger
//    */
//   Parse getParseForTagger() {
//     return mParseForTagger;
//   }

//   /**
//    * Converts the parse from the tagger back.
//    * 
//    * @param parseFromTagger
//    * @return the final parse
//    */
//   Parse transformParseFromTagger(Parse parseFromTagger) {
//     int start = parseFromTagger.getSpan().getStart();
//     int end = parseFromTagger.getSpan().getEnd();

//     Parse transformedParse = new Parse(mSentence, new Span(
//         mIndexMap.get(start), mIndexMap.get(end)), parseFromTagger.getType(),
//         parseFromTagger.getProb(), parseFromTagger.getHeadIndex());

//     Parse[] parseFromTaggerChildrens = parseFromTagger.getChildren();

//     for (Parse child : parseFromTaggerChildrens) {
//       transformedParse.insert(transformParseFromTagger(child));
//     }

//     return transformedParse;
//   }
// }
