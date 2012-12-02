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

package chalk.tools.cmdline.namefind;

import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

import chalk.tools.cmdline.AbstractEvaluatorTool;
import chalk.tools.cmdline.PerformanceMonitor;
import chalk.tools.cmdline.TerminateToolException;
import chalk.tools.cmdline.namefind.TokenNameFinderEvaluatorTool.EvalToolParams;
import chalk.tools.cmdline.params.DetailedFMeasureEvaluatorParams;
import chalk.tools.cmdline.params.EvaluatorParams;
import chalk.tools.namefind.NameFinderME;
import chalk.tools.namefind.NameSample;
import chalk.tools.namefind.TokenNameFinderEvaluationMonitor;
import chalk.tools.namefind.TokenNameFinderEvaluator;
import chalk.tools.namefind.TokenNameFinderModel;
import chalk.tools.util.ObjectStream;
import chalk.tools.util.eval.EvaluationMonitor;


public final class TokenNameFinderEvaluatorTool
    extends AbstractEvaluatorTool<NameSample, EvalToolParams> {

  interface EvalToolParams extends EvaluatorParams, DetailedFMeasureEvaluatorParams {
  }

  public TokenNameFinderEvaluatorTool() {
    super(NameSample.class, EvalToolParams.class);
  }

  public String getShortDescription() {
    return "Measures the performance of the NameFinder model with the reference data";
  }

  public void run(String format, String[] args) {
    super.run(format, args);

    TokenNameFinderModel model = new TokenNameFinderModelLoader().load(params.getModel());
    
    List<EvaluationMonitor<NameSample>> listeners = new LinkedList<EvaluationMonitor<NameSample>>();
    if (params.getMisclassified()) {
      listeners.add(new NameEvaluationErrorListener());
    }
    TokenNameFinderDetailedFMeasureListener detailedFListener = null;
    if (params.getDetailedF()) {
      detailedFListener = new TokenNameFinderDetailedFMeasureListener();
      listeners.add(detailedFListener);
    }

    TokenNameFinderEvaluator evaluator = new TokenNameFinderEvaluator(
        new NameFinderME(model),
        listeners.toArray(new TokenNameFinderEvaluationMonitor[listeners.size()]));

    final PerformanceMonitor monitor = new PerformanceMonitor("sent");

    ObjectStream<NameSample> measuredSampleStream = new ObjectStream<NameSample>() {

      public NameSample read() throws IOException {
        monitor.incrementCounter();
        return sampleStream.read();
      }

      public void reset() throws IOException {
        sampleStream.reset();
      }

      public void close() throws IOException {
        sampleStream.close();
      }
    };

    monitor.startAndPrintThroughput();

    try {
      evaluator.evaluate(measuredSampleStream);
    } catch (IOException e) {
      System.err.println("failed");
      throw new TerminateToolException(-1, "IO error while reading test data: " + e.getMessage(), e);
    } finally {
      try {
        measuredSampleStream.close();
      } catch (IOException e) {
        // sorry that this can fail
      }
    }

    monitor.stopAndPrintFinalResult();

    System.out.println();

    if(detailedFListener == null) {
      System.out.println(evaluator.getFMeasure());
    } else {
      System.out.println(detailedFListener.toString());
    }
  }
}
