package test;

import geneticAlgorithm.GA;
import geneticAlgorithm.Solver;
import particleSwarmOpt.PSO;;

public class Main {

	public static void main(String[] args) {
		int choose = 3;
		if (choose == 1) {
			executeGA(); // Genetic Algorithm
		}
		else if (choose == 2) {
			executeACO(); // Ant Colony Optimizacion
		}
		else executePSO();
		
		//converteBinarioParaDecimal("1010"); // Teste
	}
	
	private static void executeGA() {
		GA ga = new GA(4, 10);
		Solver solver = new Solver(ga);
		solver.createSolution();
	}
	
	private static void executeACO() {
		
	}
	
	private static void executePSO(){
		PSO pso = new PSO();
		pso.execute();
	}

	public static int converteBinarioParaDecimal(String numBinario) {
		int valor = 0;
		int bit = 0;

		for (int i = numBinario.length(); i > 0; i--) {
			bit = Integer.parseInt(numBinario.charAt(i - 1) + "");
			valor += bit * Math.pow(2, (numBinario.length() - i));
		}

		return valor;
	}

}
