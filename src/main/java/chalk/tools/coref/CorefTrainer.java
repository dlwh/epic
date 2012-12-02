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

package chalk.tools.coref;

import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;
import java.util.Stack;

import chalk.tools.coref.mention.DefaultParse;
import chalk.tools.coref.mention.Mention;
import chalk.tools.coref.mention.MentionContext;
import chalk.tools.coref.mention.MentionFinder;
import chalk.tools.coref.resolver.MaxentResolver;
import chalk.tools.coref.sim.GenderModel;
import chalk.tools.coref.sim.NumberModel;
import chalk.tools.coref.sim.SimilarityModel;
import chalk.tools.coref.sim.TrainSimilarityModel;
import chalk.tools.parser.Parse;
import chalk.tools.util.ObjectStream;


public class CorefTrainer {

  private static boolean containsToken(String token, Parse p) {
    for (Parse node : p.getTagNodes()) {
      if (node.getCoveredText().equals(token))
        return true;
    }
    return false;
  }
  
  private static Mention[] getMentions(CorefSample sample, MentionFinder mentionFinder) {
    
    List<Mention> mentions = new ArrayList<Mention>();
    
    for (chalk.tools.coref.mention.Parse corefParse : sample.getParses()) {

      Parse p = ((DefaultParse) corefParse).getParse();
      
      Mention extents[] = mentionFinder.getMentions(corefParse);
      
      for (int ei = 0, en = extents.length; ei < en;ei++) {

        if (extents[ei].getParse() == null) {

          Stack<Parse> nodes = new Stack<Parse>();
          nodes.add(p);
          
          while (!nodes.isEmpty()) {
            
            Parse node = nodes.pop();
            
            if (node.getSpan().equals(extents[ei].getSpan()) && node.getType().startsWith("NML")) {
              DefaultParse corefParseNode = new DefaultParse(node, corefParse.getSentenceNumber());
              extents[ei].setParse(corefParseNode);
              extents[ei].setId(corefParseNode.getEntityId());
              break;
            }
            
            nodes.addAll(Arrays.asList(node.getChildren()));
          }
        }
      }
      
      mentions.addAll(Arrays.asList(extents));
    }
    
    return mentions.toArray(new Mention[mentions.size()]);
  }
  
  public static void train(String modelDirectory, ObjectStream<CorefSample> samples,
      boolean useTreebank, boolean useDiscourseModel) throws IOException {
    
    TrainSimilarityModel simTrain = SimilarityModel.trainModel(modelDirectory + "/coref/sim");
    TrainSimilarityModel genTrain = GenderModel.trainModel(modelDirectory + "/coref/gen");
    TrainSimilarityModel numTrain = NumberModel.trainModel(modelDirectory + "/coref/num");
    
    useTreebank = true; 
    
    Linker simLinker;
    
    if (useTreebank) {
      simLinker = new TreebankLinker(modelDirectory + "/coref/", LinkerMode.SIM);
    }
    else {
      simLinker = new DefaultLinker(modelDirectory + "/coref/" ,LinkerMode.SIM);
    }
    
    // TODO: Feed with training data ...
    for (CorefSample sample = samples.read(); sample != null; sample = samples.read()) {
      
      Mention[] mentions = getMentions(sample, simLinker.getMentionFinder());
      MentionContext[] extentContexts = simLinker.constructMentionContexts(mentions);
      
      simTrain.setExtents(extentContexts);
      genTrain.setExtents(extentContexts);
      numTrain.setExtents(extentContexts);
    }
    
    simTrain.trainModel();
    genTrain.trainModel();
    numTrain.trainModel();
    
    MaxentResolver.setSimilarityModel(SimilarityModel.testModel(modelDirectory + "/coref"+"/sim"));
    
    // Done with similarity training
    
    // Now train the linkers
 
    // Training data needs to be read in again and the stream must be reset
    samples.reset();
    
    // Now train linkers
    Linker trainLinker;
    if (useTreebank) {
      trainLinker = new TreebankLinker(modelDirectory + "/coref/", LinkerMode.TRAIN, useDiscourseModel);
    }
    else {
      trainLinker = new DefaultLinker(modelDirectory + "/coref/", LinkerMode.TRAIN, useDiscourseModel);
    }
    
    for (CorefSample sample = samples.read(); sample != null; sample = samples.read())  {
      
      Mention[] mentions = getMentions(sample, trainLinker.getMentionFinder());
      trainLinker.setEntities(mentions);
    }
    
    trainLinker.train();
  }
}
