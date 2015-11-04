package geneticAlgorithm;

import geneticAlgorithm.interfaceGA.Chromosome;
import geneticAlgorithm.interfaceGA.Individual;

public class GAIndividual implements Individual {

	private double fitness;
	private int hits; // Number of times the individual solved the problem (nearly) correctly
	private GAChromosome chromosome;
	
	public GAIndividual() {
		this.chromosome = null;
		this.fitness = 0.0;
		this.hits = 0;
	}

	public GAIndividual(GAChromosome chromosome) {
		this.chromosome = chromosome;
		this.fitness = 0.0;
		this.hits = 0;
	}
	
	@Override
	public void setChromosome(Chromosome chromosome) {
		this.chromosome = (GAChromosome) chromosome;
	}

	@Override
	public void setFitness(double fitness) {
		this.fitness = fitness;
	}

	@Override
	public void setHits(int hits) {
		this.hits = hits;
	}

	@Override
	public Chromosome getChromosome() {
		return this.chromosome;
	}

	@Override
	public double getFitness() {
		return this.fitness;
	}

	@Override
	public int getHits() {
		return this.hits;
	}

}
