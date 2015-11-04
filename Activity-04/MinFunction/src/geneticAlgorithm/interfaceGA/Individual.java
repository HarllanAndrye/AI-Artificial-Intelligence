package geneticAlgorithm.interfaceGA;

public interface Individual {

	public void setChromosome(Chromosome chromosome);

	public void setFitness(double fitness);

	public void setHits(int hits);

	public Chromosome getChromosome();

	public double getFitness();

	public int getHits();
}
