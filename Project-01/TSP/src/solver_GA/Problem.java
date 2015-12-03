package solver_GA;

public interface Problem {

	/**
	 * An extra conditions to prematurely stop new generations from being
	 * produced. If it is not needed in the specific implementation, simply
	 * return false.
	 *
	 * @return The termination condition
	 */

	public boolean isTerminationCondition();

	/**
	 * Create random population of individuals and test their initial fitness.
	 */

	public Individual[] initPopulation();

	/**
	 * Pick an individual in the population according to the desired criteria.
	 *
	 * @param populaion
	 *            Population of individuals
	 */

	public int select(Individual[] population);

	/**
	 * Mutate the parent from the current population and put the mutant (child)
	 * in the new population.
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

	public void mutate(Individual[] curr_population, Individual[] new_population, int parent, int child);

	/**
	 * Crossover mom and dad in the current population and put the resulting
	 * mutants (son and daughter) in the new population
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

	public void crossover(Individual[] curr_population, Individual[] new_population, int dad, int mom, int child,
			int daughter);

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

	public void copy(Individual[] curr_population, Individual[] new_population, int parent, int child);

	/**
	 * Test fitness of an individual and set it's fitness in the population.
	 *
	 * @param population
	 *            Population of individuals
	 * @param indiv
	 *            Index of individual in the population
	 */

	public void testFitness(Individual[] population, int indiv);

	public int getPopulationSize();

	public int getMaxGenerations();

	public int getMaxRuns();

	public double getProbCross();

	public double getProbMutate();

	public void printStats();

	public void printBestResult();
}