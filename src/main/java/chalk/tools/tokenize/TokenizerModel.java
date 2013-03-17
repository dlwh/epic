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


package chalk.tools.tokenize;

import java.io.DataInputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;
import java.net.URL;
import java.util.Map;

import nak.io.BinaryGISModelReader;
import nak.core.LinearModel;
import nak.core.LinearModel;
import chalk.tools.dictionary.Dictionary;
import chalk.tools.util.BaseToolFactory;
import chalk.tools.util.InvalidFormatException;
import chalk.tools.util.model.BaseModel;
import chalk.tools.util.model.ModelUtil;


/**
 * The {@link TokenizerModel} is the model used
 * by a learnable {@link Tokenizer}.
 *
 * @see TokenizerME
 */
public final class TokenizerModel extends BaseModel {

  private static final String COMPONENT_NAME = "TokenizerME";
  
  private static final String TOKENIZER_MODEL_ENTRY = "token.model";

  /**
   * Initializes the current instance.
   * 
   * @param tokenizerModel the model
   * @param manifestInfoEntries the manifest
   * @param tokenizerFactory the factory
   */
  public TokenizerModel(LinearModel tokenizerModel,
      Map<String, String> manifestInfoEntries, TokenizerFactory tokenizerFactory) {
    super(COMPONENT_NAME, tokenizerFactory.getLanguageCode(), manifestInfoEntries, tokenizerFactory);
    artifactMap.put(TOKENIZER_MODEL_ENTRY, tokenizerModel);
    checkArtifactMap();
  }

  /**
   * Initializes the current instance.
   *
   * @param tokenizerLinearModel
   * @param useAlphaNumericOptimization
   * 
   * @deprecated Use
   *             {@link TokenizerModel#TokenizerModel(String, LinearModel, Map, TokenizerFactory)}
   *             instead and pass in a {@link TokenizerFactory}.
   */
  public TokenizerModel(String language, LinearModel tokenizerLinearModel,
      Dictionary abbreviations, boolean useAlphaNumericOptimization,
      Map<String, String> manifestInfoEntries) {
    this(tokenizerLinearModel, manifestInfoEntries, 
        new TokenizerFactory(language, abbreviations, useAlphaNumericOptimization, null));
  }

  /**
   * Initializes the current instance.
   *
   * @param language
   * @param tokenizerLinearModel
   * @param useAlphaNumericOptimization
   * @param manifestInfoEntries
   * 
   * @deprecated Use
   *             {@link TokenizerModel#TokenizerModel(String, LinearModel, Map, TokenizerFactory)}
   *             instead and pass in a {@link TokenizerFactory}.
   */
  public TokenizerModel(String language, LinearModel tokenizerLinearModel,
      boolean useAlphaNumericOptimization, Map<String, String> manifestInfoEntries) {
    this(language, tokenizerLinearModel, null, useAlphaNumericOptimization, manifestInfoEntries);
  }

  /**
   * Initializes the current instance.
   *
   * @param language
   * @param tokenizerLinearModel
   * @param useAlphaNumericOptimization
   * 
   * @deprecated Use
   *             {@link TokenizerModel#TokenizerModel(String, LinearModel, Map, TokenizerFactory)}
   *             instead and pass in a {@link TokenizerFactory}.
   */
  public TokenizerModel(String language, LinearModel tokenizerLinearModel,
      boolean useAlphaNumericOptimization) {
    this(language, tokenizerLinearModel, useAlphaNumericOptimization, null);
  }
  
  /**
   * Initializes the current instance.
   *
   * @param in
   *
   * @throws IOException
   * @throws InvalidFormatException
   */
  public TokenizerModel(InputStream in) throws IOException, InvalidFormatException {
    super(COMPONENT_NAME, in);
  }
  
  public TokenizerModel(File modelFile) throws IOException, InvalidFormatException {
    super(COMPONENT_NAME, modelFile);
  }
  
  public TokenizerModel(URL modelURL) throws IOException, InvalidFormatException {
    super(COMPONENT_NAME, modelURL);
  }

  /**
   * Checks if the tokenizer model has the right outcomes.
   *
   * @param model
   * @return
   */
  private static boolean isModelCompatible(LinearModel model) {
    return ModelUtil.validateOutcomes(model, TokenizerME.SPLIT, TokenizerME.NO_SPLIT);
  }

  @Override
  protected void validateArtifactMap() throws InvalidFormatException {
    super.validateArtifactMap();

    if (!(artifactMap.get(TOKENIZER_MODEL_ENTRY) instanceof LinearModel)) {
      throw new InvalidFormatException("Token model is incomplete!");
    }

    if (!isModelCompatible(getLinearModel())) {
      throw new InvalidFormatException("The maxent model is not compatible with the tokenizer!");
    }
  }

  public TokenizerFactory getFactory() {
    return (TokenizerFactory) this.toolFactory;
  }

  @Override
  protected Class<? extends BaseToolFactory> getDefaultFactory() {
    return TokenizerFactory.class;
  }

  public LinearModel getLinearModel() {
    return (LinearModel) artifactMap.get(TOKENIZER_MODEL_ENTRY);
  }
  
  public Dictionary getAbbreviations() {
    if (getFactory() != null) {
      return getFactory().getAbbreviationDictionary();
    }
    return null;
  }

  public boolean useAlphaNumericOptimization() {
    if (getFactory() != null) {
      return getFactory().isUseAlphaNumericOptmization();
    }
    return false;
  }

  public static void main(String[] args) throws IOException {
    if (args.length < 3){
      System.err.println("TokenizerModel [-alphaNumericOptimization] languageCode packageName modelName");
      System.exit(1);
    }

    int ai = 0;

    boolean alphaNumericOptimization = false;

    if ("-alphaNumericOptimization".equals(args[ai])) {
      alphaNumericOptimization = true;
      ai++;
    }

    String languageCode = args[ai++];
    String packageName = args[ai++];
    String modelName = args[ai];

    LinearModel model = new BinaryGISModelReader(new DataInputStream(
        new FileInputStream(modelName))).getModel();

    TokenizerModel packageModel = new TokenizerModel(languageCode, model,
        alphaNumericOptimization);

    OutputStream out = null;
    try {
      out = new FileOutputStream(packageName);
      packageModel.serialize(out);
    }
    finally {
      if (out != null)
        out.close();
    }
  }
}
