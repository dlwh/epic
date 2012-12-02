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

package chalk.tools.formats.convert;

import chalk.tools.cmdline.ArgumentParser;
import chalk.tools.cmdline.StreamFactoryRegistry;
import chalk.tools.cmdline.params.DetokenizerParameter;
import chalk.tools.formats.DetokenizerSampleStreamFactory;
import chalk.tools.formats.ParseSampleStreamFactory;
import chalk.tools.parser.Parse;
import chalk.tools.sentdetect.SentenceSample;
import chalk.tools.util.ObjectStream;

/**
 * <b>Note:</b> Do not use this class, internal use only!
 */
public class ParseToSentenceSampleStreamFactory extends DetokenizerSampleStreamFactory<SentenceSample> {

  interface Parameters extends ParseSampleStreamFactory.Parameters, DetokenizerParameter {
  }
  
  private ParseToSentenceSampleStreamFactory() {
    super(Parameters.class);
  }

  public ObjectStream<SentenceSample> create(String[] args) {
    Parameters params = ArgumentParser.parse(args, Parameters.class);
    language = params.getLang();
    
    ObjectStream<Parse> parseSampleStream = StreamFactoryRegistry.getFactory(Parse.class,
        StreamFactoryRegistry.DEFAULT_FORMAT).create(
        ArgumentParser.filter(args, ParseSampleStreamFactory.Parameters.class));
    
    return new POSToSentenceSampleStream(createDetokenizer(params),
        new ParseToPOSSampleStream(parseSampleStream), 30);
  }
  
  public static void registerFactory() {
    StreamFactoryRegistry.registerFactory(SentenceSample.class,
        "parse", new ParseToSentenceSampleStreamFactory());
  }
}
