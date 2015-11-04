package geneticAlgorithm;

import java.util.Vector;

import geneticAlgorithm.interfaceGA.Chromosome;

@SuppressWarnings({ "serial", "rawtypes" })
public class GAChromosome extends Vector implements Chromosome {

	GAChromosome(int initialCapacity, int capacityIncrement) {
		super(initialCapacity, capacityIncrement);
	}

	GAChromosome(int initialCapacity) {
		super(initialCapacity);
	}

	GAChromosome() {
		super();
	}
	
}
