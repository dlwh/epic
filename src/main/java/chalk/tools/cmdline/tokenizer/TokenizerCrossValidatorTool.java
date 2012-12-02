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

package chalk.tools.cmdline.tokenizer;

import java.io.IOException;

import chalk.tools.cmdline.AbstractCrossValidatorTool;
import chalk.tools.cmdline.CmdLineUtil;
import chalk.tools.cmdline.TerminateToolException;
import chalk.tools.cmdline.params.CVParams;
import chalk.tools.cmdline.tokenizer.TokenizerCrossValidatorTool.CVToolParams;
import chalk.tools.dictionary.Dictionary;
import chalk.tools.tokenize.TokenSample;
import chalk.tools.tokenize.TokenizerCrossValidator;
import chalk.tools.tokenize.TokenizerEvaluationMonitor;
import chalk.tools.tokenize.TokenizerFactory;
import chalk.tools.util.eval.FMeasure;
import chalk.tools.util.model.ModelUtil;


public final class TokenizerCrossValidatorTool
    extends AbstractCrossValidatorTool<TokenSample, CVToolParams> {
  
  interface CVToolParams extends CVParams, TrainingParams {
  }

  public TokenizerCrossValidatorTool() {
    super(TokenSample.class, CVToolParams.class);
  }

  public String getShortDescription() {
    return "K-fold cross validator for the learnable tokenizer";
  }
  
  public void run(String format, String[] args) {
    super.run(format, args);

    mlParams = CmdLineUtil.loadTrainingParameters(params.getParams(), false);
    if (mlParams == null) {
      mlParams = ModelUtil.createTrainingParameters(params.getIterations(), params.getCutoff());
    }

    TokenizerCrossValidator validator;
    
    TokenizerEvaluationMonitor listener = null;
    if (params.getMisclassified()) {
      listener = new TokenEvaluationErrorListener();
    }
    
    try {
      Dictionary dict = TokenizerTrainerTool.loadDict(params.getAbbDict());

      TokenizerFactory tokFactory = TokenizerFactory.create(
          params.getFactory(), factory.getLang(), dict,
          params.getAlphaNumOpt(), null);
      validator = new chalk.tools.tokenize.TokenizerCrossValidator(mlParams,
          tokFactory, listener);

      validator.evaluate(sampleStream, params.getFolds());
    }
    catch (IOException e) {
      throw new TerminateToolException(-1, "IO error while reading training data or indexing data: "
          + e.getMessage(), e);
    }
    finally {
      try {
        sampleStream.close();
      } catch (IOException e) {
        // sorry that this can fail
      }
    }
    
    FMeasure result = validator.getFMeasure();
    
    System.out.println(result.toString());
  }
}
