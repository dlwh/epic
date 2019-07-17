package epic.logo


case class LogoOpts(constraintEpsilon: Double = 1e-3,

	miniBatchSize: Int = 1,

	numInnerOptimizationLoops: Int = 10,

	numOuterOptimizationLoops: Int = 10,

	shuffleMinibatches: Boolean = false,

	shuffleSeed: Int = -1,

	convergenceTolerance: Double = 1e-3)
