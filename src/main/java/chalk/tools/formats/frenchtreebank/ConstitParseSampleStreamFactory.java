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

package chalk.tools.formats.frenchtreebank;

import chalk.tools.cmdline.ArgumentParser;
import chalk.tools.cmdline.StreamFactoryRegistry;
import chalk.tools.cmdline.params.LanguageFormatParams;
import chalk.tools.formats.DirectorySampleStream;
import chalk.tools.formats.LanguageSampleStreamFactory;
import chalk.tools.formats.convert.FileToByteArraySampleStream;
import chalk.tools.parser.Parse;
import chalk.tools.util.ObjectStream;

public class ConstitParseSampleStreamFactory extends LanguageSampleStreamFactory<Parse> {

  interface Parameters extends LanguageFormatParams {    
  }
  
  private ConstitParseSampleStreamFactory() {
    super(Parameters.class);
  }
  
  public ObjectStream<Parse> create(String[] args) {
    
    Parameters params = ArgumentParser.parse(args, Parameters.class);

    language = params.getLang();
    
    return new ConstitParseSampleStream(new FileToByteArraySampleStream(new DirectorySampleStream(params.getData(),
        null, false)));
  }
  
  public static void registerFactory() {
    StreamFactoryRegistry.registerFactory(Parse.class, "frenchtreebank",
        new ConstitParseSampleStreamFactory());
  }
}
