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
import java.io.InputStreamReader;

import chalk.tools.cmdline.CmdLineUtil;
import chalk.tools.cmdline.PerformanceMonitor;
import chalk.tools.tokenize.Tokenizer;
import chalk.tools.tokenize.TokenizerStream;
import chalk.tools.tokenize.WhitespaceTokenStream;
import chalk.tools.util.ObjectStream;
import chalk.tools.util.PlainTextByLineStream;


final class CommandLineTokenizer {

  private final Tokenizer tokenizer;
  
  CommandLineTokenizer(Tokenizer tokenizer) {
    this.tokenizer = tokenizer;
  }
  
  void process() {
    
    ObjectStream<String> untokenizedLineStream =
        new PlainTextByLineStream(new InputStreamReader(System.in));
    
    ObjectStream<String> tokenizedLineStream = new WhitespaceTokenStream(
        new TokenizerStream(tokenizer, untokenizedLineStream));
    
    PerformanceMonitor perfMon = new PerformanceMonitor(System.err, "sent");
    perfMon.start();
    
    try {
      String tokenizedLine;
      while ((tokenizedLine = tokenizedLineStream.read()) != null) {
        System.out.println(tokenizedLine);
        perfMon.incrementCounter();
      }
    }
    catch (IOException e) {
      CmdLineUtil.handleStdinIoError(e);
    }
    
    perfMon.stopAndPrintFinalResult();
  }
}
