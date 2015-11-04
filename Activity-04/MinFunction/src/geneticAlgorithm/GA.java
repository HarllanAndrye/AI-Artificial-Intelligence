package geneticAlgorithm;

import java.util.Random;

import geneticAlgorithm.interfaceGA.Individual;
import geneticAlgorithm.interfaceGA.Problem;;

/*
 * Implementação baseada em:
 * http://www.cs.columbia.edu/~evs/ais/finalprojs/kalina
 */

public class GA implements Problem {

	private int populationSize;
	private int maxGenerations;
	private int bitAmount = 5; // quantidade de bits para representar o número x da função
	private double totalFitness; // Sum of all fitnesses for current population
	private int individualsTested; // Number of indivuduals tested in the current population
	private double averageFitness; // Average fitness for current population
	private double maxFitness; // Maximum fitness for current population
	private GAIndividual bestIndividual; // Best OVERALL individual

	public GA(int populationSize, int maxGenerations) {
		this.populationSize = populationSize;
		this.maxGenerations = maxGenerations;
		initVariables();
	}

	public GA() {
		this.populationSize = 0;
		this.maxGenerations = 0;
		initVariables();
	}

	private void initVariables() {
		individualsTested = 0;
		averageFitness = 0.0;
		maxFitness = 0.0;
		totalFitness = 0.0;
		bestIndividual = new GAIndividual();
	}

	/*
	 * Gera o cromossomo de forma aleatória. 
	 * 5 bits: 
	 * - o primeiro indica o sinal do número (1 negativo, 0 positivo); 
	 * - os outros quatro representam o número decimal (valor de x).
	 */
	@SuppressWarnings("unchecked")
	private GAChromosome getChromosome() {
		GAChromosome chromosome = new GAChromosome(bitAmount);
		Random random = new Random();

		for (int i = 0; i < 5; i++) {
			chromosome.addElement(random.nextInt(2));
		}

		return chromosome;
	}

	@Override
	public boolean isTerminationCondition() {
		// TODO Auto-generated method stub
		return false;
	}

	/*
	 * Converte de binário para decimal
	 * https://pablonobrega.wordpress.com/2013/05/30/conversao-de-numeros-entre-bases-binario-decimal-octal-hexadecimal/ 
	 * Primeiro bit:
	 *  - 1: x é ímpar
	 *  - 0: x é par
	 * 1101 = 1*2^0 + 0*2^1 + 1*2^2 + 1*2^3 = 13.
	 */
	private int binaryDecimal(GAChromosome chromosome){
		int number = 0;
		int chromosomeSize = chromosome.size();
		for (int j = chromosomeSize; j > 1; j--) {
			int bit = ((Integer) chromosome.elementAt(j-1)).intValue();
			number += (bit) * Math.pow(2, chromosomeSize - j);
		}
		
		return number;
	}
	
	/*
	 * Iniciando uma nova população
	 */
	@Override
	public Individual[] initPopulation() {
		// Create a new population
		Individual[] population = new GAIndividual[this.populationSize];

		// Initialize a new population
		for (int i = 0; i < this.populationSize; i++) {
			GAChromosome chromosome;
			
			// x pertence à [-10, +10]
			int number = 0;
			do {
				chromosome = getChromosome();
				number = binaryDecimal(chromosome);
			} while( number > 10 );
			
			population[i] = new GAIndividual(chromosome);
		}

		// Test fitness of each individual
		for (int i = 0; i < this.populationSize; i++) {
			testFitness(population, i);
		}

		printStats(0, population);

		return (population);
	}

	/*
	 * Etapa de seleção (aleatória)
	 */
	@Override
	public int select(Individual[] population) {
		// Número aleatório de 0 até totalFitness
		Random random = new Random();
		int probRoulette = random.nextInt((int) totalFitness) + 1;

		/*
		 * Roleta - o que tiver menor valor de f(x), tem mais chances
		 */
		for (int i = 0; i < population.length; i++) {
			if (population[i].getFitness() <= probRoulette) {
				return i;
			}
		}

		return population.length - 1;
	}

	@SuppressWarnings("unchecked")
	@Override
	public void mutate(Individual[] curr_population, Individual[] new_population, int parent, int child) {

		GAChromosome chromosome = (GAChromosome) curr_population[parent].getChromosome();

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

		// Insert new individual with the new chromosome into the new population
		new_population[child] = new GAIndividual(chromosome);
	}

	/**
	 * Randomly pick one cut point in the parents and copy genes. For example:
	 *
	 * p1 = (1 1 0 | 1 0 1) 
	 * p2 = (0 1 1 | 1 1 0)
	 *
	 * c1 = (1 1 0 | x x x) 
	 * c2 = (0 1 1 | x x x)
	 *
	 * c1 = (1 1 0 | 1 1 0) 
	 * c2 = (0 1 1 | 1 0 1)
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
	@SuppressWarnings("unchecked")
	@Override
	public void crossover(Individual[] curr_population, Individual[] new_population, int dad, int mom, int son,
			int daughter) {

		GAChromosome momChromosome = (GAChromosome) curr_population[mom].getChromosome();
		GAChromosome dadChromosome = (GAChromosome) curr_population[dad].getChromosome();

		// Escolha aleatória do corte no cromossomo (apenas 1 ponto)
		int point = (int) (Math.random() * dadChromosome.size());

		// Declare and initialize children to dump values
		GAChromosome sonChromosome = new GAChromosome(dadChromosome.size());
		GAChromosome daughterChromosome = new GAChromosome(momChromosome.size());
		for (int i = 0; i < dadChromosome.size(); i++) {
			sonChromosome.addElement(new Integer(Integer.MAX_VALUE));
			daughterChromosome.addElement(new Integer(Integer.MAX_VALUE));
		}

		// Copia a primeira parte (cromossomo) dos pais para os filhos
		for (int i = 0; i <= point; i++) {
			sonChromosome.setElementAt(dadChromosome.elementAt(i), i);
			daughterChromosome.setElementAt(momChromosome.elementAt(i), i);
		}

		// Copia os dados do pai para a filha
		int i = point + 1;
		while (i < dadChromosome.size()) {
			Integer gene;
			gene = (Integer) dadChromosome.elementAt(i);
			daughterChromosome.setElementAt(gene, i);
			i++;
		}

		// Copia os dados da mãe para o filho
		i = point + 1;
		while (i < momChromosome.size()) {
			Integer gene;
			gene = (Integer) momChromosome.elementAt(i);
			sonChromosome.setElementAt(gene, i);
			i++;
		}

		new_population[son] = new GAIndividual(sonChromosome);
		new_population[daughter] = new GAIndividual(daughterChromosome);
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
	@Override
	public void copy(Individual[] curr_population, Individual[] new_population, int parent, int child) {
		GAChromosome chromosome = (GAChromosome) curr_population[parent].getChromosome();
		new_population[child] = new GAIndividual(chromosome);
	}

	/*
	 * Calcula o f(x)
	 */
	public double solveFunction(GAChromosome chromosome) {
		double function = 0.0;
		int bitParity = ((Integer) chromosome.elementAt(0)).intValue();
		int numberX = binaryDecimal(chromosome);
		if (bitParity == 1)
			numberX *= (-1); // Deixa o número negativo
		
		// Encontrar f(x) = x^2 - 3x + 4
		function = Math.pow(numberX, 2) - (3 * numberX) + 4;
		
		return function;
	}
	
	/*
	 * Calcula o fitness de cada indivíduo e também o fitness global (soma de todos).
	 */
	@Override
	public void testFitness(Individual[] population, int indiv) {
		// Clear all global values for the new population
		if (indiv == 0) {
			individualsTested = 0;
			averageFitness = 0.0;
			maxFitness = 0.0;
			totalFitness = 0.0;
		}

		GAChromosome chromosome = (GAChromosome) population[indiv].getChromosome();
		double function = 0.0;
		function = solveFunction(chromosome);

		// Set individual's fitness
		population[indiv].setFitness(function);

		// Add fitness of current individual to the population's fitness
		totalFitness += function;

		if (function > maxFitness) {
			maxFitness = function;
		}

		// Inicializar o bestIndividual
		if (indiv == 0) {
			bestIndividual = (GAIndividual) population[indiv];
		}
		
		// Tem que ser menor, pois quero o minimo da função
		if (function < bestIndividual.getFitness()) {
			bestIndividual = (GAIndividual) population[indiv];
		}

		// Calculate average fitness for the current population
		averageFitness = (averageFitness * individualsTested + function) / (++individualsTested);
	}

	@Override
	public int getPopulationSize() {
		return this.populationSize;
	}

	@Override
	public int getMaxGenerations() {
		return this.maxGenerations;
	}

	@Override
	public int getMaxRuns() {
		return 1;
	}

	@Override
	public double getProbCross() {
		return 0.6;
	}

	@Override
	public double getProbMutate() {
		return 0.01;
	}

	@Override
	public void printStats(int iteration, Individual[] population) {
		if (iteration == 0)
			System.out.println("-- Initial Generation --");
		else
			System.out.println("\nGeneration: " + iteration);
		
		for (int i = 0; i < populationSize; i++) {
			GAChromosome chromosome = (GAChromosome) population[i].getChromosome();
			printChromosome(chromosome);
		}
		//System.out.println("Average fitness: " + averageFitness);
		//System.out.println("Maximum fitness: " + maxFitness);
		//System.out.println("Total fitness: " + totalFitness);
	}

	@Override
	public void printBestResult() {
		GAChromosome bestChromosome = (GAChromosome) bestIndividual.getChromosome();
		System.out.println("\n--- Result GA ---");
		printChromosome(bestChromosome);
		System.out.println("Fitness of the best chromosome: f(x) = x² - 3x + 4 = " + bestIndividual.getFitness());
	}

	private void printChromosome(GAChromosome ch) {
		System.out.print("Value of x: ");
		for (int i = 0; i < ch.size(); i++)
			System.out.print((Integer) ch.elementAt(i) + " ");

		int bitParity = ((Integer) ch.elementAt(0)).intValue();
		int numberX = binaryDecimal(ch);
		if (bitParity == 1) numberX *= (-1);
		
		System.out.print("= " + numberX + "\n");
	}

}
