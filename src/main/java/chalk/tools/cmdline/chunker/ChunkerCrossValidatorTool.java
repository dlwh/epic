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

package chalk.tools.cmdline.chunker;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import chalk.tools.chunker.ChunkSample;
import chalk.tools.chunker.ChunkerCrossValidator;
import chalk.tools.chunker.ChunkerEvaluationMonitor;
import chalk.tools.chunker.ChunkerFactory;
import chalk.tools.cmdline.AbstractCrossValidatorTool;
import chalk.tools.cmdline.CmdLineUtil;
import chalk.tools.cmdline.TerminateToolException;
import chalk.tools.cmdline.chunker.ChunkerCrossValidatorTool.CVToolParams;
import chalk.tools.cmdline.params.CVParams;
import chalk.tools.cmdline.params.DetailedFMeasureEvaluatorParams;
import chalk.tools.util.eval.EvaluationMonitor;
import chalk.tools.util.eval.FMeasure;
import chalk.tools.util.model.ModelUtil;


public final class ChunkerCrossValidatorTool
    extends AbstractCrossValidatorTool<ChunkSample, CVToolParams> {
  
  interface CVToolParams extends TrainingParams, CVParams, DetailedFMeasureEvaluatorParams {
  }

  public ChunkerCrossValidatorTool() {
    super(ChunkSample.class, CVToolParams.class);
  }

  public String getShortDescription() {
    return "K-fold cross validator for the chunker";
  }
  
  public void run(String format, String[] args) {
    super.run(format, args);

    mlParams = CmdLineUtil.loadTrainingParameters(params.getParams(), false);
    if (mlParams == null) {
      mlParams = ModelUtil.createTrainingParameters(params.getIterations(),
          params.getCutoff());
    }

    List<EvaluationMonitor<ChunkSample>> listeners = new LinkedList<EvaluationMonitor<ChunkSample>>();
    ChunkerDetailedFMeasureListener detailedFMeasureListener = null;
    if (params.getMisclassified()) {
      listeners.add(new ChunkEvaluationErrorListener());
    }
    if (params.getDetailedF()) {
      detailedFMeasureListener = new ChunkerDetailedFMeasureListener();
      listeners.add(detailedFMeasureListener);
    }

    ChunkerCrossValidator validator;

    try {
      ChunkerFactory chunkerFactory = ChunkerFactory
          .create(params.getFactory());

      validator = new ChunkerCrossValidator(factory.getLang(), mlParams,
          chunkerFactory,
          listeners.toArray(new ChunkerEvaluationMonitor[listeners.size()]));
      validator.evaluate(sampleStream, params.getFolds());
    }
    catch (IOException e) {
      throw new TerminateToolException(-1, "IO error while reading training data or indexing data: " +
          e.getMessage(), e);
    }
    finally {
      try {
        sampleStream.close();
      } catch (IOException e) {
        // sorry that this can fail
      }
    }
    
    if (detailedFMeasureListener == null) {
      FMeasure result = validator.getFMeasure();
      System.out.println(result.toString());
    } else {
      System.out.println(detailedFMeasureListener.toString());
    }
  }
}
