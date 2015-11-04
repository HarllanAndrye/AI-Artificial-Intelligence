package particleSwarmOpt;

public interface PSOConstants {
	int SWARM_SIZE = 4;
	int MAX_ITERATION = 10;
	int PROBLEM_DIMENSION = 1; // Quantas variáveis tem (x, y, ...) para o problema (função).
	double C1 = 0.5; // Influência individual
	double C2 = 0.5; // Influência do enxame
}
