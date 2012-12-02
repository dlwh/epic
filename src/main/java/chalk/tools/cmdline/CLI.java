/*
 * Licensed to the Apache Software Foundation (ASF) under one or more
 * contributor license agreements.  See the NOTICE file distributed with
 * this work for additional information regarding copyright ownership.
 * The ASF licenses this file to You under the Apache License, Version 2.0
 * (the "License"); you may not use this file except in compliance with
 * the License. You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */


package chalk.tools.cmdline;

import java.util.Collections;
import java.util.LinkedHashMap;
import java.util.LinkedList;
import java.util.List;
import java.util.Map;
import java.util.Set;

import chalk.tools.cmdline.chunker.ChunkerConverterTool;
import chalk.tools.cmdline.chunker.ChunkerCrossValidatorTool;
import chalk.tools.cmdline.chunker.ChunkerEvaluatorTool;
import chalk.tools.cmdline.chunker.ChunkerMETool;
import chalk.tools.cmdline.chunker.ChunkerTrainerTool;
import chalk.tools.cmdline.coref.CoreferenceConverterTool;
import chalk.tools.cmdline.coref.CoreferencerTool;
import chalk.tools.cmdline.coref.CoreferencerTrainerTool;
import chalk.tools.cmdline.dictionary.DictionaryBuilderTool;
import chalk.tools.cmdline.doccat.DoccatConverterTool;
import chalk.tools.cmdline.doccat.DoccatTool;
import chalk.tools.cmdline.doccat.DoccatTrainerTool;
import chalk.tools.cmdline.namefind.CensusDictionaryCreatorTool;
import chalk.tools.cmdline.namefind.TokenNameFinderConverterTool;
import chalk.tools.cmdline.namefind.TokenNameFinderCrossValidatorTool;
import chalk.tools.cmdline.namefind.TokenNameFinderEvaluatorTool;
import chalk.tools.cmdline.namefind.TokenNameFinderTool;
import chalk.tools.cmdline.namefind.TokenNameFinderTrainerTool;
import chalk.tools.cmdline.parser.BuildModelUpdaterTool;
import chalk.tools.cmdline.parser.CheckModelUpdaterTool;
import chalk.tools.cmdline.parser.ParserConverterTool;
import chalk.tools.cmdline.parser.ParserTool;
import chalk.tools.cmdline.parser.ParserTrainerTool;
import chalk.tools.cmdline.parser.TaggerModelReplacerTool;
import chalk.tools.cmdline.postag.POSTaggerConverterTool;
import chalk.tools.cmdline.postag.POSTaggerCrossValidatorTool;
import chalk.tools.cmdline.postag.POSTaggerEvaluatorTool;
import chalk.tools.cmdline.postag.POSTaggerTrainerTool;
import chalk.tools.cmdline.sentdetect.SentenceDetectorConverterTool;
import chalk.tools.cmdline.sentdetect.SentenceDetectorCrossValidatorTool;
import chalk.tools.cmdline.sentdetect.SentenceDetectorEvaluatorTool;
import chalk.tools.cmdline.sentdetect.SentenceDetectorTool;
import chalk.tools.cmdline.sentdetect.SentenceDetectorTrainerTool;
import chalk.tools.cmdline.tokenizer.DictionaryDetokenizerTool;
import chalk.tools.cmdline.tokenizer.SimpleTokenizerTool;
import chalk.tools.cmdline.tokenizer.TokenizerConverterTool;
import chalk.tools.cmdline.tokenizer.TokenizerCrossValidatorTool;
import chalk.tools.cmdline.tokenizer.TokenizerMEEvaluatorTool;
import chalk.tools.cmdline.tokenizer.TokenizerMETool;
import chalk.tools.cmdline.tokenizer.TokenizerTrainerTool;
import chalk.tools.util.Version;


public final class CLI {
  
  public static final String CMD = "chalk";
  
  private static Map<String, CmdLineTool> toolLookupMap;
  
  static {
    toolLookupMap = new LinkedHashMap<String, CmdLineTool>();
    
    List<CmdLineTool> tools = new LinkedList<CmdLineTool>();
    
    // Document Categorizer
    tools.add(new DoccatTool());
    tools.add(new DoccatTrainerTool());
    tools.add(new DoccatConverterTool());
    
    // Dictionary Builder
    tools.add(new DictionaryBuilderTool());
    
    // Tokenizer
    tools.add(new SimpleTokenizerTool());
    tools.add(new TokenizerMETool());
    tools.add(new TokenizerTrainerTool());
    tools.add(new TokenizerMEEvaluatorTool());
    tools.add(new TokenizerCrossValidatorTool());
    tools.add(new TokenizerConverterTool());
    tools.add(new DictionaryDetokenizerTool());
    
    // Sentence detector
    tools.add(new SentenceDetectorTool());
    tools.add(new SentenceDetectorTrainerTool());
    tools.add(new SentenceDetectorEvaluatorTool());
    tools.add(new SentenceDetectorCrossValidatorTool());
    tools.add(new SentenceDetectorConverterTool());
    
    // Name Finder
    tools.add(new TokenNameFinderTool());
    tools.add(new TokenNameFinderTrainerTool());
    tools.add(new TokenNameFinderEvaluatorTool());
    tools.add(new TokenNameFinderCrossValidatorTool());
    tools.add(new TokenNameFinderConverterTool());
    tools.add(new CensusDictionaryCreatorTool());
    
    
    // POS Tagger
    tools.add(new chalk.tools.cmdline.postag.POSTaggerTool());
    tools.add(new POSTaggerTrainerTool());
    tools.add(new POSTaggerEvaluatorTool());
    tools.add(new POSTaggerCrossValidatorTool());
    tools.add(new POSTaggerConverterTool());
    
    // Chunker
    tools.add(new ChunkerMETool());
    tools.add(new ChunkerTrainerTool());
    tools.add(new ChunkerEvaluatorTool());
    tools.add(new ChunkerCrossValidatorTool());
    tools.add(new ChunkerConverterTool());
    
    // Parser
    tools.add(new ParserTool());
    tools.add(new ParserTrainerTool()); // trains everything
    tools.add(new ParserConverterTool()); // trains everything
    tools.add(new BuildModelUpdaterTool()); // re-trains  build model
    tools.add(new CheckModelUpdaterTool()); // re-trains  build model
    tools.add(new TaggerModelReplacerTool());
    
    // Coreferencer
    tools.add(new CoreferencerTool());
    tools.add(new CoreferencerTrainerTool());
    tools.add(new CoreferenceConverterTool());
    
    for (CmdLineTool tool : tools) {
      toolLookupMap.put(tool.getName(), tool);
    }
    
    toolLookupMap = Collections.unmodifiableMap(toolLookupMap);
  }
  
  /**
   * @return a set which contains all tool names
   */
  public static Set<String> getToolNames() {
    return toolLookupMap.keySet();
  }
  
  private static void usage() {
    System.out.println("chalk");
    System.out.println("Usage: " + CMD + " TOOL");
    System.out.println("where TOOL is one of:");
    
    // distance of tool name from line start
    int numberOfSpaces = -1;
    for (String toolName : toolLookupMap.keySet()) {
      if (toolName.length() > numberOfSpaces) {
        numberOfSpaces = toolName.length();
      }
    }
    numberOfSpaces = numberOfSpaces + 4;
    
    for (CmdLineTool tool : toolLookupMap.values()) {
      
      System.out.print("  " + tool.getName());
      
      for (int i = 0; i < Math.abs(tool.getName().length() - numberOfSpaces); i++) {
        System.out.print(" ");
      }
      
      System.out.println(tool.getShortDescription());
    }
    
    System.out.println("All tools print help when invoked with help parameter");
    System.out.println("Example: opennlp SimpleTokenizer help");
  }
  
  public static void main(String[] args) {
    
    if (args.length == 0) {
      usage();
      System.exit(0);
    }
    
    String toolArguments[] = new String[args.length -1];
    System.arraycopy(args, 1, toolArguments, 0, toolArguments.length);

    String toolName = args[0];

    //check for format
    String formatName = StreamFactoryRegistry.DEFAULT_FORMAT;
    int idx = toolName.indexOf(".");
    if (-1 < idx) {
      formatName = toolName.substring(idx + 1);
      toolName = toolName.substring(0, idx);
    }
    CmdLineTool tool = toolLookupMap.get(toolName);
    
    try {
      if (null == tool) {
        throw new TerminateToolException(1, "Tool " + toolName + " is not found.");
      }

      if ((0 == toolArguments.length && tool.hasParams()) ||
          0 < toolArguments.length && "help".equals(toolArguments[0])) {
          if (tool instanceof TypedCmdLineTool) {
            System.out.println(((TypedCmdLineTool) tool).getHelp(formatName));
          } else if (tool instanceof BasicCmdLineTool) {
            System.out.println(tool.getHelp());
          }

          System.exit(0);
      }

      if (tool instanceof TypedCmdLineTool) {
        ((TypedCmdLineTool) tool).run(formatName, toolArguments);
      } else if (tool instanceof BasicCmdLineTool) {
        if (-1 == idx) {
          ((BasicCmdLineTool) tool).run(toolArguments);
        } else {
          throw new TerminateToolException(1, "Tool " + toolName + " does not support formats.");
        }
      } else {
        throw new TerminateToolException(1, "Tool " + toolName + " is not supported.");
      }
    }
    catch (TerminateToolException e) {
      
      if (e.getMessage() != null) {
        System.err.println(e.getMessage());
      }

      if (e.getCause() != null) {
        System.err.println(e.getCause().getMessage());
        e.getCause().printStackTrace(System.err);
      }
      
      System.exit(e.getCode());
    }
  }
}
