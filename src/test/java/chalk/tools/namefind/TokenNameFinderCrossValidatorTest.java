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

package chalk.tools.namefind;

import static org.junit.Assert.*;

import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.util.Collections;
import java.util.Map;


import org.junit.Test;

import chalk.tools.cmdline.namefind.NameEvaluationErrorListener;
import chalk.tools.namefind.NameSample;
import chalk.tools.namefind.NameSampleDataStream;
import chalk.tools.namefind.TokenNameFinderCrossValidator;
import chalk.tools.util.ObjectStream;
import chalk.tools.util.PlainTextByLineStream;
import chalk.tools.util.TrainingParameters;
import chalk.tools.util.model.ModelType;
import chalk.tools.util.model.ModelUtil;

public class TokenNameFinderCrossValidatorTest {

  private final String TYPE = null;

  @Test
  /**
   * Test that reproduces jira OPENNLP-463
   */
  public void testWithNullResources() throws Exception {

    FileInputStream sampleDataIn = new FileInputStream(new File(getClass()
        .getClassLoader()
        .getResource("chalk/tools/namefind/AnnotatedSentences.txt").toURI()));

    ObjectStream<NameSample> sampleStream = new NameSampleDataStream(
        new PlainTextByLineStream(sampleDataIn.getChannel(), "ISO-8859-1"));

    TrainingParameters mlParams = ModelUtil.createTrainingParameters(70, 1);
    mlParams.put(TrainingParameters.ALGORITHM_PARAM,
        ModelType.MAXENT.toString());

    TokenNameFinderCrossValidator cv = new TokenNameFinderCrossValidator("en",
        TYPE, mlParams, null, null);

    cv.evaluate(sampleStream, 2);

    assertNotNull(cv.getFMeasure());
  }
  
  @Test
  /**
   * Test that tries to reproduce jira OPENNLP-466
   */
  public void testWithNameEvaluationErrorListener() throws Exception {

    FileInputStream sampleDataIn = new FileInputStream(new File(getClass()
        .getClassLoader()
        .getResource("chalk/tools/namefind/AnnotatedSentences.txt").toURI()));

    ObjectStream<NameSample> sampleStream = new NameSampleDataStream(
        new PlainTextByLineStream(sampleDataIn.getChannel(), "ISO-8859-1"));

    TrainingParameters mlParams = ModelUtil.createTrainingParameters(70, 1);
    mlParams.put(TrainingParameters.ALGORITHM_PARAM,
        ModelType.MAXENT.toString());
    
    ByteArrayOutputStream out = new ByteArrayOutputStream();
    NameEvaluationErrorListener listener = new NameEvaluationErrorListener(out); 

    Map<String, Object> resources = Collections.emptyMap();
    TokenNameFinderCrossValidator cv = new TokenNameFinderCrossValidator("en",
        TYPE, mlParams, null, resources, listener);

    cv.evaluate(sampleStream, 2);
    
    assertTrue(out.size() > 0);
    assertNotNull(cv.getFMeasure());
  }
}
