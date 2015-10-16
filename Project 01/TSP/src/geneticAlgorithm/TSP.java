package geneticAlgorithm;

import global.TSPFileParser;
import solver_GA.*;

//http://www.cs.columbia.edu/~evs/ais/finalprojs/kalina/solver/
//http://www.cs.columbia.edu/~evs/ais/finalprojs/kalina/tsp/

public class TSP implements Problem {

	public static final int FITNESS_SCALER = 100000000;

	public static int GRAPH_SIZE = 12;
	private int POPULATION_SIZE;
	private int MAX_GENERATIONS;
	private int MAX_RUNS;

	private int individualsTested; // Number of indivuduals tested in the current population
	private double averageFitness; // Average fitness for current population
	private double maxFitness; // Maximum fitness for current population
	private double totalFitness; // Sum of all fitnesses for current population
	private TSPIndividual bestIndividual; // Best OVERALL individual

	private int graph[][];

	public static void main(String[] args) {

		TSPFileParser parser;
		TSP tsp = new TSP();

		try {
			parser = new TSPFileParser(args);

			if (parser.isTSPFileIn()) {
				tsp = new TSP(parser.getPopulationSize(), parser.getMaxGenerations(), parser.getMaxRuns(),
						parser.getGraph());
				tsp.printGraph(parser.getGraph());
			} else {
				tsp = new TSP(parser.getPopulationSize(), parser.getMaxGenerations(), parser.getMaxRuns());
			}

		} catch (TSPException e) {
			System.err.println(e);
			System.exit(0);
		}

		Solver solver = new Solver(tsp);
		// start the timer
        long startTime = System.currentTimeMillis();
		solver.breed();
		long endTime = System.currentTimeMillis();
		System.out.println(" ");
		System.out.println("GENETIC ALGORITHM - Elapsed Time: " + (endTime-startTime) + " ms");
	}

	public TSP() {

		this.POPULATION_SIZE = 0;
		this.MAX_GENERATIONS = 0;
		this.MAX_RUNS = 0;
		this.graph = new int[GRAPH_SIZE][GRAPH_SIZE];
		this.initGraph();

		individualsTested = 0;
		averageFitness = 0.0;
		maxFitness = 0.0;
		totalFitness = 0.0;
		bestIndividual = new TSPIndividual();
	}

	public TSP(int populationSize, int maxGenerations, int maxRuns) {

		this.POPULATION_SIZE = populationSize;
		this.MAX_GENERATIONS = maxGenerations;
		this.MAX_RUNS = maxRuns;
		this.graph = new int[GRAPH_SIZE][GRAPH_SIZE];
		this.initGraph();

		individualsTested = 0;
		averageFitness = 0.0;
		maxFitness = 0.0;
		totalFitness = 0.0;
		bestIndividual = new TSPIndividual();
	}

	public TSP(int populationSize, int maxGenerations, int maxRuns, int[][] graph) {

		this.POPULATION_SIZE = populationSize;
		this.MAX_GENERATIONS = maxGenerations;
		this.MAX_RUNS = maxRuns;
		this.graph = graph;
		GRAPH_SIZE = graph.length;

		individualsTested = 0;
		averageFitness = 0.0;
		maxFitness = 0.0;
		totalFitness = 0.0;
		bestIndividual = new TSPIndividual();
	}

	/**
	 * Establish the condition for which the genetic algorithm will terminate.
	 */

	public boolean isTerminationCondition() {
		return false;
	}

	/**
	 * Create random population of individuals and test their initial fitness.
	 */

	public Individual[] initPopulation() {

		// Create a new popultion
		Individual[] population = new TSPIndividual[this.POPULATION_SIZE];

		// Initialize a new population
		for (int i = 0; i < this.POPULATION_SIZE; i++) {
			TSPChromosome chromosome = getRandomChromosome();
			population[i] = new TSPIndividual(chromosome);
		}

		// Test fitness of each individual
		// System.out.println( "Testing fitness of initial population ..." );
		for (int i = 0; i < this.POPULATION_SIZE; i++) {
			testFitness(population, i);
		}

		printStats();

		return (population);
	}

	/**
	 * Pick an individual in the population according to the fraction of the
	 * total fitness in the entire population.
	 * 
	 * @param populaion
	 *            Population of individuals
	 */

	public int select(Individual[] population) {

		double cumulativeFitness = 0.0;
		double totalFraction = totalFitness * Math.random();

		for (int i = 0; i < population.length; i++) {
			cumulativeFitness += population[i].getFitness();
			if (cumulativeFitness >= totalFraction)
				return i;
		}

		return population.length - 1;
	}

	/**
	 * Select two random points in the chromosome of the parent, swap the
	 * contents of the two points, and insert new individual with the new
	 * chromosome into the new population.
	 *
	 * @param curr_population
	 *            Current population of individuals
	 * @param new_population
	 *            New population of individuals
	 * @param parent
	 *            Index of the parent in the current population
	 * @param child
	 *            Index of the child in the new population
	 */

	public void mutate(Individual[] curr_population, Individual[] new_population, int parent, int child) {

		// System.out.println( "MUTATE: parent " + parent + ", child " + child );

		TSPChromosome chromosome = (TSPChromosome) curr_population[parent].getChromosome();

		// System.out.print( "Parent: " );
		// printChromosome(chromosome);

		// Select two random points in the chromosome of the parent
		int point1, point2;
		do {
			point1 = (int) (Math.random() * chromosome.size());
			point2 = (int) (Math.random() * chromosome.size());
		} while (point1 == point2);

		// Swap the contents of the two points
		Integer gene1 = (Integer) chromosome.elementAt(point1);
		Integer gene2 = (Integer) chromosome.elementAt(point2);

		chromosome.setElementAt(gene2, point1);
		chromosome.setElementAt(gene1, point2);

		// System.out.print( "Child: " );
		// printChromosome(chromosome);

		// Insert new individual with the new chromosome into the new population
		new_population[child] = new TSPIndividual(chromosome);
	}

	/**
	 * Randomly pick two cut points in the parents, and copy genes from the
	 * other parent in the same order from the beginning of the chromosome,
	 * omitting genes already used.
	 *
	 * For example:
	 *
	 * p1 = (1 2 3 | 4 5 6 7 | 8 9) 
	 * p2 = (4 5 2 | 1 8 7 6 | 9 3)
	 *
	 * c1 = (x x x | 4 5 6 7 | x x) 
	 * c2 = (x x x | 1 8 6 7 | x x)
	 *
	 * c1 = (2 1 8 | 4 5 6 7 | 9 3) 
	 * c2 = (2 3 4 | 1 8 6 7 | 5 9)
	 *
	 * @param curr_population
	 *            Current population of individuals
	 * @param new_population
	 *            New population of individuals
	 * @param mom
	 *            Index of the first parent in the current population
	 * @param dad
	 *            Index of the second parent in the current population
	 * @param son
	 *            Index of the first child in the new population
	 * @param daughter
	 *            Index of the second child in the new population
	 */

	public void crossover(Individual[] curr_population, Individual[] new_population, int dad, int mom, int son,
			int daughter) {

		// System.out.println( "CROSSOVER: dad " + dad + ", mom " + mom + ", son " + son + ", daughter " + daughter );

		TSPChromosome momChromosome = (TSPChromosome) curr_population[mom].getChromosome();
		TSPChromosome dadChromosome = (TSPChromosome) curr_population[dad].getChromosome();

		boolean[] visitedMom = new boolean[momChromosome.size()];
		boolean[] visitedDad = new boolean[dadChromosome.size()];

		// Pick two random points, which cannot be the first or the last gene
		int point1, point2;
		do {
			point1 = (int) (Math.random() * dadChromosome.size());
			point2 = (int) (Math.random() * dadChromosome.size());
		} while (point1 >= point2 || point1 == 0 || point2 == (dadChromosome.size() - 1));

		/*
		 * System.out.print( "Parent 1: " ); printChromosome(dadChromosome);
		 * System.out.print( "Parent 2: " ); printChromosome(momChromosome);
		 */

		// Declare and intitialize children to dumb values
		TSPChromosome sonChromosome = new TSPChromosome(dadChromosome.size());
		TSPChromosome daughterChromosome = new TSPChromosome(momChromosome.size());
		for (int i = 0; i < dadChromosome.size(); i++) {
			sonChromosome.addElement(new Integer(Integer.MAX_VALUE));
			daughterChromosome.addElement(new Integer(Integer.MAX_VALUE));
		}

		// Copy middle section from parents to children
		for (int i = point1; i <= point2; i++) {
			sonChromosome.setElementAt(dadChromosome.elementAt(i), i);
			daughterChromosome.setElementAt(momChromosome.elementAt(i), i);
			visitedDad[((Integer) dadChromosome.elementAt(i)).intValue()] = true;
			visitedMom[((Integer) momChromosome.elementAt(i)).intValue()] = true;
		}

		// Copy the rest for the first child
		int i = 0, j = 0;
		while (i < point1 && j < momChromosome.size()) {
			Integer gene;
			do {
				gene = (Integer) momChromosome.elementAt(j);
			} while (j++ < momChromosome.size() && visitedDad[gene.intValue()]);
			sonChromosome.setElementAt(gene, i);
			visitedDad[gene.intValue()] = true;
			i++;
		}

		i = point2 + 1;
		while (i < sonChromosome.size() && j < momChromosome.size()) {
			Integer gene;
			do {
				gene = (Integer) momChromosome.elementAt(j);
			} while (j++ < momChromosome.size() && visitedDad[gene.intValue()]);
			sonChromosome.setElementAt(gene, i);
			visitedDad[gene.intValue()] = true;
			i++;
		}

		// Copy the rest for the second child
		i = 0;
		j = 0;
		while (i < point1 && j < dadChromosome.size()) {
			Integer gene;
			do {
				gene = (Integer) dadChromosome.elementAt(j);
			} while (j++ < dadChromosome.size() && visitedMom[gene.intValue()]);
			daughterChromosome.setElementAt(gene, i);
			visitedMom[gene.intValue()] = true;
			i++;
		}

		i = point2 + 1;
		while (i < daughterChromosome.size() && j < dadChromosome.size()) {
			Integer gene;
			do {
				gene = (Integer) dadChromosome.elementAt(j);
			} while (j++ < dadChromosome.size() && visitedMom[gene.intValue()]);
			daughterChromosome.setElementAt(gene, i);
			visitedMom[gene.intValue()] = true;
			i++;
		}

		/*
		 * System.out.print( "Child 1: " ); printChromosome(sonChromosome);
		 * System.out.print( "Child 2: " ); printChromosome(daughterChromosome);
		 */

		new_population[son] = new TSPIndividual(sonChromosome);
		new_population[daughter] = new TSPIndividual(daughterChromosome);
	}

	/**
	 * Copy the parent into the new population without any modifications.
	 *
	 * @param curr_population
	 *            Current population of individuals
	 * @param new_population
	 *            New population of individuals
	 * @param parent
	 *            Index of the parent in the current population
	 * @param child
	 *            Index of the child in the new population
	 */

	public void copy(Individual[] curr_population, Individual[] new_population, int parent, int child) {
		// System.out.println( "COPY: parent " + parent + ", child " + child );
		TSPChromosome chromosome = (TSPChromosome) curr_population[parent].getChromosome();
		new_population[child] = new TSPIndividual(chromosome);
	}

	/**
	 * Test fitness of an individual and set it's fitness in the population.
	 *
	 * @param population
	 *            Population of individuals
	 * @param indiv
	 *            Index of individual in the population
	 */

	public void testFitness(Individual[] population, int indiv) {

		// Clear all global values for the new population
		if (indiv == 0) {
			individualsTested = 0;
			averageFitness = 0.0;
			maxFitness = 0.0;
			totalFitness = 0.0;
		}

		TSPChromosome chromosome = (TSPChromosome) population[indiv].getChromosome();
		double cost = 0.0;
		double fitness = 0.0;

		// Determine the cost of traversing the cities
		int chromosomeSize = chromosome.size();
		for (int i = 0; chromosomeSize > 1 && i < chromosomeSize - 1; i++) {
			int first = ((Integer) chromosome.elementAt(i)).intValue();
			int second = ((Integer) chromosome.elementAt(i + 1)).intValue();
			cost += graph[first][second];
		}

		// Complete Hamiltonian circuit
		int last = ((Integer) chromosome.elementAt(chromosomeSize - 1)).intValue();
		int first = ((Integer) chromosome.elementAt(0)).intValue();
		cost += graph[last][first];

		// Invert the cost to produce fitness
		if (cost == 0.0)
			fitness = FITNESS_SCALER;
		else
			fitness = FITNESS_SCALER / cost;

		// Set individual's fitness and cost
		population[indiv].setFitness(fitness);
		((TSPIndividual) population[indiv]).setCost(cost);

		// Add fitness of current individual to the population's fitness
		totalFitness += fitness;

		if (fitness > maxFitness) {
			maxFitness = fitness;
		}

		if (fitness > bestIndividual.getFitness()) {
			bestIndividual = (TSPIndividual) population[indiv];
		}

		// Calculate average fitness for the current population
		averageFitness = (averageFitness * individualsTested + fitness) / (++individualsTested);

		// System.out.println( "Cost: " + cost + ", Fitness: " + fitness + " " );
	}

	public double getProbCross() {
		return 0.8;
	}

	public double getProbMutate() {
		return 0.05;
	}

	public int getPopulationSize() {
		return this.POPULATION_SIZE;
	}

	public int getMaxGenerations() {
		return this.MAX_GENERATIONS;
	}

	public int getMaxRuns() {
		return this.MAX_RUNS;
	}

	private void initGraph() {

		for (int i = 0; i < GRAPH_SIZE; i++) {
			for (int j = 0; j <= i; j++) {
				if (i == j)
					graph[i][j] = 0;
				else {
					graph[i][j] = (int) (Math.random() * GRAPH_SIZE) + 1;
					graph[j][i] = graph[i][j];
				}

			}
		}
	}

	private void printGraph(int[][] graph) {

		int graphSize = graph.length;

		for (int i = 0; i < graphSize; i++) {
			for (int j = 0; j < graphSize; j++) {
				System.out.print(graph[i][j] + " ");
			}
			System.out.println("");
		}
	}

	private TSPChromosome getRandomChromosome() {

		TSPChromosome chromosome = new TSPChromosome(GRAPH_SIZE);
		boolean[] visited = new boolean[GRAPH_SIZE]; // The array defaults to false

		int city;
		int cityCount = 0;
		while (cityCount < visited.length) {
			city = (int) (Math.random() * GRAPH_SIZE);
			while (visited[city] == true) {
				city = (int) (Math.random() * GRAPH_SIZE);
			}
			chromosome.addElement(new Integer(city));
			visited[city] = true;
			cityCount++;
		}

		return chromosome;
	}

	public void printBestResult() {

		TSPChromosome bestChromosome = (TSPChromosome) bestIndividual.getChromosome();
		printChromosome(bestChromosome);
		System.out.println("Cost of the best chromosome: " + bestIndividual.getCost());
		System.out.println("Fitness of the best chromosome: " + bestIndividual.getFitness());
	}

	public void printStats() {

		System.out.println("");
		System.out.println("Average fitness: " + averageFitness);
		System.out.println("Maximum fitness: " + maxFitness);
		System.out.println("Total fitness: " + totalFitness);
	}

	private void printChromosome(TSPChromosome ch) {

		for (int i = 0; i < ch.size(); i++)
			System.out.print((Integer) ch.elementAt(i) + " ");

		System.out.println("");
	}
}