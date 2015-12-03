package geneticAlgorithm;

import solver_GA.*;

public class TSPIndividual implements Individual {

	private TSPChromosome chromosome;
	private double fitness;
	private int hits; // Number of times the individual solved the problem (nearly) correctly
	private double cost;

	public TSPIndividual() {
		this.chromosome = null;
		this.fitness = 0.0;
		this.hits = 0;
		this.cost = 0.0;
	}

	public TSPIndividual(TSPChromosome chromosome) {
		this.chromosome = chromosome;
		this.fitness = 0.0;
		this.hits = 0;
		this.cost = 0.0;
	}

	public void setChromosome(Chromosome chromosome) {
		this.chromosome = (TSPChromosome) chromosome;
	}

	public void setFitness(double fitness) {
		this.fitness = fitness;
	}

	public void setHits(int hits) {
		this.hits = hits;
	}

	public void setCost(double cost) {
		this.cost = cost;
	}

	public Chromosome getChromosome() {
		return this.chromosome;
	}

	public double getFitness() {
		return this.fitness;
	}

	public int getHits() {
		return this.hits;
	}

	public double getCost() {
		return this.cost;
	}
}
