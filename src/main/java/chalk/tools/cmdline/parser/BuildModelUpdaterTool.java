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

package chalk.tools.cmdline.parser;

import java.io.IOException;

import nak.core.AbstractModel;
import chalk.tools.dictionary.Dictionary;
import chalk.tools.parser.Parse;
import chalk.tools.parser.ParserEventTypeEnum;
import chalk.tools.parser.ParserModel;
import chalk.tools.parser.chunking.Parser;
import chalk.tools.parser.chunking.ParserEventStream;
import chalk.tools.util.ObjectStream;


public final class BuildModelUpdaterTool extends ModelUpdaterTool {

  public String getShortDescription() {
    return "trains and updates the build model in a parser model";
  }
  
  @Override
  protected ParserModel trainAndUpdate(ParserModel originalModel,
      ObjectStream<Parse> parseSamples, ModelUpdaterTool.ModelUpdaterParams parameters)
      throws IOException {
    
      Dictionary mdict = ParserTrainerTool.buildDictionary(parseSamples, originalModel.getHeadRules(), parameters.getCutoff());
      
      parseSamples.reset();
      
      // TODO: training individual models should be in the chunking parser, not here
      // Training build
      System.out.println("Training builder");
      nak.data.EventStream bes = new ParserEventStream(parseSamples, 
          originalModel.getHeadRules(), ParserEventTypeEnum.BUILD, mdict);
      AbstractModel buildModel = Parser.train(bes, 
          parameters.getIterations(), parameters.getCutoff());
      
      parseSamples.close();
      
      return originalModel.updateBuildModel(buildModel);
  }
}
