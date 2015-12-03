package geneticAlgorithm;

import java.util.Vector;

import solver_GA.*;

public class TSPChromosome extends Vector implements Chromosome {

	TSPChromosome(int initialCapacity, int capacityIncrement) {
		super(initialCapacity, capacityIncrement);
	}

	TSPChromosome(int initialCapacity) {
		super(initialCapacity);
	}

	TSPChromosome() {
		super();
	}
}