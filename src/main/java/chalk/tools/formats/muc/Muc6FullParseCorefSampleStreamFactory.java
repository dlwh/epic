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

package chalk.tools.formats.muc;

import java.io.File;
import java.io.FileFilter;
import java.nio.charset.Charset;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import chalk.tools.cmdline.ArgumentParser;
import chalk.tools.cmdline.StreamFactoryRegistry;
import chalk.tools.cmdline.ArgumentParser.ParameterDescription;
import chalk.tools.cmdline.namefind.TokenNameFinderModelLoader;
import chalk.tools.cmdline.params.LanguageFormatParams;
import chalk.tools.cmdline.parser.ParserModelLoader;
import chalk.tools.cmdline.tokenizer.TokenizerModelLoader;
import chalk.tools.coref.CorefSample;
import chalk.tools.formats.DirectorySampleStream;
import chalk.tools.formats.LanguageSampleStreamFactory;
import chalk.tools.formats.convert.FileToStringSampleStream;
import chalk.tools.namefind.NameFinderME;
import chalk.tools.namefind.TokenNameFinder;
import chalk.tools.parser.Parser;
import chalk.tools.parser.ParserFactory;
import chalk.tools.parser.ParserModel;
import chalk.tools.tokenize.Tokenizer;
import chalk.tools.tokenize.TokenizerME;
import chalk.tools.tokenize.TokenizerModel;
import chalk.tools.util.ObjectStream;


/**
 * Factory creates a stream which can parse MUC 6 Coref data and outputs CorefSample
 * objects which are enhanced with a full parse and are suitable to train the Coreference component.
 */
public class Muc6FullParseCorefSampleStreamFactory extends LanguageSampleStreamFactory<CorefSample> {

  interface Parameters extends LanguageFormatParams {

    @ParameterDescription(valueName = "modelFile")
    File getParserModel();
    
    @ParameterDescription(valueName = "modelFile")
    File getTokenizerModel();
    
    @ParameterDescription(valueName = "modelFile")
    File getPersonModel();
    
    @ParameterDescription(valueName = "modelFile")
    File getOrganizationModel();
    
    // TODO: Add other models here !!!
  }
  
  protected Muc6FullParseCorefSampleStreamFactory() {
    super(Parameters.class);
  }

  public ObjectStream<CorefSample> create(String[] args) {
    
    Parameters params = ArgumentParser.parse(args, Parameters.class);
    
    language = params.getLang();
    
    ParserModel parserModel = new ParserModelLoader().load(params.getParserModel());
    Parser parser =  ParserFactory.create(parserModel);
    
    TokenizerModel tokenizerModel = new TokenizerModelLoader().load(params.getTokenizerModel());
    Tokenizer tokenizer = new TokenizerME(tokenizerModel);
    
    ObjectStream<String> mucDocStream = new FileToStringSampleStream(
        new DirectorySampleStream(params.getData(), new FileFilter() {
          
          public boolean accept(File file) {
            return file.getName().toLowerCase().endsWith(".sgm");
          }
        }, false), Charset.forName("UTF-8"));
    
    ObjectStream<RawCorefSample> rawSamples = 
        new MucCorefSampleStream(tokenizer, mucDocStream);
    
    ObjectStream<RawCorefSample> parsedSamples = new FullParseCorefEnhancerStream(parser, rawSamples);
    
    
    // How to load all these nameFinder models ?! 
    // Lets make a param per model, not that nice, but ok!
    
    Map<String, File> modelFileTagMap = new HashMap<String, File>();
    
    modelFileTagMap.put("person", params.getPersonModel());
    modelFileTagMap.put("organization", params.getOrganizationModel());
    
    List<TokenNameFinder> nameFinders = new ArrayList<TokenNameFinder>();
    List<String> tags = new ArrayList<String>();
    
    for (Map.Entry<String, File> entry : modelFileTagMap.entrySet()) {
      nameFinders.add(new NameFinderME(
          new TokenNameFinderModelLoader().load(entry.getValue())));
      tags.add(entry.getKey());
    }
    
    return new MucMentionInserterStream(new NameFinderCorefEnhancerStream(nameFinders.toArray(
        new TokenNameFinder[nameFinders.size()]),
        tags.toArray(new String[tags.size()]), parsedSamples));
  }
  
  public static void registerFactory() {
    StreamFactoryRegistry.registerFactory(CorefSample.class, "muc6full",
        new Muc6FullParseCorefSampleStreamFactory());
  }
}
