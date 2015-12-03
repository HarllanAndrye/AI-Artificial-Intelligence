package solver_GA;

public class Solver {

	private static int POPULATION_SIZE;
	private static int MAX_GENERATIONS;
	private static int MAX_RUNS;
	private static double PROB_CROSS;
	private static double PROB_MUTATE;

	private Problem problem;

	public Solver(Problem problem) {
		this.problem = problem;
		POPULATION_SIZE = problem.getPopulationSize();
		MAX_GENERATIONS = problem.getMaxGenerations();
		MAX_RUNS = problem.getMaxRuns();
		PROB_CROSS = problem.getProbCross();
		PROB_MUTATE = problem.getProbMutate();
	}

	public void breed() {

		for (int run = 0; run < MAX_RUNS; run++) {

			Individual[] curr_population = problem.initPopulation();
			Individual[] new_population = new Individual[POPULATION_SIZE];

			int generation = 0;
			while ((++generation <= MAX_GENERATIONS) && !problem.isTerminationCondition()) {

				for (int i = 0; i < POPULATION_SIZE; i++) {

					int mom = problem.select(curr_population);
					if (Math.random() < PROB_CROSS && i + 1 < POPULATION_SIZE) { // Crossover
						int dad = problem.select(curr_population);
						if (dad == mom){ // harllan-Estava cruzando cromossomos iguais, isso não pode.
							if(dad < curr_population.length){
								dad++;
							}else{
								dad -= 1;
							}
						}
						problem.crossover(curr_population, new_population, mom, dad, i, i + 1);
						problem.testFitness(new_population, i);
						problem.testFitness(new_population, i + 1);
						i++;
					}

					else if (Math.random() < PROB_MUTATE) { // Mutate the remainder
						problem.mutate(curr_population, new_population, mom, i);
						problem.testFitness(new_population, i);
					}

					else { // Copy the rest
						problem.copy(curr_population, new_population, mom, i);
						problem.testFitness(new_population, i);
					}
				}
				problem.printStats();
				curr_population = new_population;
				new_population = new Individual[POPULATION_SIZE];
			}
			problem.printBestResult();
		}
	}
}