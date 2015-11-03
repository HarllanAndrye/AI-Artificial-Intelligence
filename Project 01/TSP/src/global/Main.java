package global;

import aStar.AStar;
import depthFirstSearch.DepthFirstSearch;
import greedySearch.NearestNeighbour;
import geneticAlgorithm.TSPException;
import antColonyOpt.AcoTsp;
import java.util.LinkedList;
import java.util.List;
import java.util.Random;

public class Main {

	private static int[][] theMatrix;
	private static int qtCities;
	private static List<String> argList = new LinkedList<String>();

	/**
	 * Gera uma matriz aleatória de acordo com a quantidade de cidades passadas como parâmetro
	 * @param qtdCities
	 */
	public static void generatorMatrix(int qtdCities) {
		theMatrix = new int[qtdCities][qtdCities];
		qtCities = qtdCities;
		Random generator = new Random();

		for (int i = 0; i < qtdCities; i++) {
			for (int j = i; j < qtdCities; j++) {
				if (i == j)
					theMatrix[i][j] = 0;
				else {
					theMatrix[i][j] = generator.nextInt(1000);
					theMatrix[j][i] = theMatrix[i][j];
				}
			}
		}
	}

	public static String printMatrix() {
		String str = new String();
		for (int i = 0; i < qtCities; i++) {
			for (int j = 0; j < qtCities; j++) {
				if (j == qtCities - 1)
					str += theMatrix[i][j] + "\n";
				else
					str += theMatrix[i][j] + ", ";
			}
		}
		return str;
	}

	public static void parser(String fileTSP) {
		TSPFileParser parser;
		
		try {
			parser = new TSPFileParser(fileTSP); 
			theMatrix = parser.getGraph();
		} catch (TSPException e) {
			System.err.println(e);
			System.exit(0);
		}
	}

	public static void aco(String file){
		//http://adibaba.github.io/ACOTSPJava/
		//argList.add("--acs");
		argList.add("--ants");
		argList.add("20");

		argList.add("--time");
		argList.add("2");

		argList.add("--tours");
		argList.add("2");

		argList.add("--tries");
		argList.add("2");

		argList.add("-i");
		argList.add(file);
		
		AcoTsp.main(argList.toArray(new String[0]));
	}
	
	public static void main(String[] args) {
		int choose = 1; // Só para não comentar o código quando rodar outro algoritmo
		if (choose == 1) {
			aco("tours/brazil58.tsp");
		}
		else {
			qtCities = 262;
			theMatrix = new int[qtCities][qtCities];
	
			// Arquivos com as distâncias entre as cidades (TSPLIB)
			//parser("/home/harllan/workspace/TSP/tours/brazil58.tsp");
			//parser("/home/harllan/workspace/TSP/tours/eil101.tsp");
			parser("/home/harllan/workspace/TSP/tours/gil262.tsp");
			
			// Gera uma matriz aleatória
			//generatorMatrix(15);
			
			System.out.print(printMatrix());
	
			// Algoritmos: executar separadamente
			//AStar s = new AStar(theMatrix, 0, qtCities);
			//DepthFirstSearch s = new DepthFirstSearch(theMatrix, 0, qtCities);
			NearestNeighbour s = new NearestNeighbour(theMatrix, 0, qtCities);
			
			s.execute();
			System.out.println(s.result);
		}
	}

}
