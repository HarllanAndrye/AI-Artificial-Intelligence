package geneticAlgorithm;

import geneticAlgorithm.interfaceGA.Individual;
import geneticAlgorithm.interfaceGA.Problem;

public class Solver {

	private static int POPULATION_SIZE;
	private static int MAX_GENERATIONS;
	private static int MAX_RUNS;
	private static double PROB_CROSSOVER; // probabilidade de crossover
	private static double PROB_MUTATION; // probabilidade de mutação

	private Problem problem;

	public Solver(Problem problem) {
		this.problem = problem;
		POPULATION_SIZE = problem.getPopulationSize();
		MAX_GENERATIONS = problem.getMaxGenerations();
		MAX_RUNS = problem.getMaxRuns();
		PROB_CROSSOVER = problem.getProbCross();
		PROB_MUTATION = problem.getProbMutate();
	}

	public void createSolution() {
		for (int run = 0; run < MAX_RUNS; run++) {

			Individual[] curr_population = problem.initPopulation();
			Individual[] new_population = new Individual[POPULATION_SIZE];

			int generation = 0;
			while ((++generation <= MAX_GENERATIONS) && !problem.isTerminationCondition()) {

				for (int i = 0; i < POPULATION_SIZE; i++) {

					int mom = problem.select(curr_population);
					if (Math.random() < PROB_CROSSOVER && i + 1 < POPULATION_SIZE) { // Crossover
						int dad = problem.select(curr_population);
						if (dad == mom) {
							if ( dad < (curr_population.length-1) ) {
								dad++;
							} else {
								dad -= 1;
							}
						}
						problem.crossover(curr_population, new_population, mom, dad, i, i + 1);
						problem.testFitness(new_population, i);
						problem.testFitness(new_population, i + 1);
						i++;
					}

					else if (Math.random() < PROB_MUTATION) { // Mutate the remainder
						problem.mutate(curr_population, new_population, mom, i);
						problem.testFitness(new_population, i);
					}

					else { // Copy the rest
						problem.copy(curr_population, new_population, mom, i);
						problem.testFitness(new_population, i);
					}
				}
				//problem.printStats(generation);
				curr_population = new_population;
				problem.printStats(generation, curr_population);
				new_population = new Individual[POPULATION_SIZE];
			}
			problem.printBestResult();
		}
	}

}
