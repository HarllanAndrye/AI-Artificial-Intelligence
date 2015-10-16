package global;

import java.io.*;
import java.util.*;

import geneticAlgorithm.TSPCoordinate;
import geneticAlgorithm.TSPException;

public class TSPFileParser {

	private final String USAGE_MSG = "Usage: java TSP population_size max_generations max_runs [file.tsp]";
	private final String FORMAT_MSG = "The command-line arguments have an incorrect format!";

	private int populationSize;
	private int maxGenerations;
	private int maxRuns;
	private boolean isTSPFileIn;
	private int graph[][];

	private String fileName;

	public TSPFileParser(String[] args) throws TSPException {

		if (args.length < 3)
			throw new TSPException(USAGE_MSG);

		try {
			this.populationSize = Integer.parseInt(args[0]);
			this.maxGenerations = Integer.parseInt(args[1]);
			this.maxRuns = Integer.parseInt(args[2]);
		} catch (NumberFormatException e) {
			throw new TSPException(FORMAT_MSG);
		}

		if (args.length == 4) {
			this.fileName = args[3];
			this.graph = parseFile(this.fileName);
			this.isTSPFileIn = true;
		} else {
			this.fileName = null;
			this.graph = null;
			this.isTSPFileIn = false;
		}
	}
	
	//@Harllan
	public TSPFileParser(String fileTSP) throws TSPException{
		this.graph = parseFile(fileTSP);
	}

	public int getPopulationSize() {
		return this.populationSize;
	}

	public int getMaxGenerations() {
		return this.maxGenerations;
	}

	public int getMaxRuns() {
		return this.maxRuns;
	}

	public boolean isTSPFileIn() {
		return this.isTSPFileIn;
	}

	public int[][] getGraph() {
		return this.graph;
	}

	//@Harllan
	//Function for brazil58.tsp
	//(http://codenav.org/code.html?project=/org/choco-solver/choco-graph/3.3.0&path=/Source%20Packages/org.chocosolver.samples.tsp/brazil58.tsp)
	private static void halfMatrix(int[][] dist, BufferedReader buf) throws IOException {
		int n = dist.length;
		String line;
		line = buf.readLine();
		String[] lineNumbers;
		for (int i = 0; i < n - 1; i++) {
			if (!line.equalsIgnoreCase("EOF") && !line.equalsIgnoreCase(" EOF")) {
				line = line.replaceAll(" * ", " ");
				lineNumbers = line.split(" ");
				int off = 0;
				if (lineNumbers[0].equals("")) {
					off++;
				}
				for (int k = 0; k < lineNumbers.length - off; k++) {
					dist[i][i + k + 1] = Integer.parseInt(lineNumbers[k + off]);
					dist[i + k + 1][i] = dist[i][i + k + 1];
				}
				line = buf.readLine();
			}
		}
	}

	public static int[][] parseFile(String fileName) throws TSPException {

		// Supported file types
		int EUC_2D = 1;
		int GEO = 2;
		int EXPLICIT = 3;

		Vector coords = new Vector();
		int fileType = 0;
		int[][] graphBrazil = new int[58][58]; //harllan

		try {
			BufferedReader in = new BufferedReader(new FileReader(fileName));
			String line;
			boolean nodeCoordSection = false;

			while ((line = in.readLine()) != null) {
				if (!line.equalsIgnoreCase("EOF") && !line.equalsIgnoreCase(" EOF") && !line.equals("")) {
					if (!line.equalsIgnoreCase("NODE_COORD_SECTION") && !line.equalsIgnoreCase("EDGE_WEIGHT_FORMAT: UPPER_ROW") && !nodeCoordSection) {
						if (line.equalsIgnoreCase("EDGE_WEIGHT_TYPE : EUC_2D")) //harllan-acrescentado_espaço_antes_de_:
							fileType = EUC_2D;
						else if (line.equalsIgnoreCase("EDGE_WEIGHT_TYPE : GEO")) //harllan-acrescentado_espaço_antes_de_:
							fileType = GEO;
						else if (line.equalsIgnoreCase("EDGE_WEIGHT_TYPE: EXPLICIT")) //brazil58.tsp
							fileType = EXPLICIT;
					} else if (line.equalsIgnoreCase("NODE_COORD_SECTION") || line.equalsIgnoreCase("EDGE_WEIGHT_FORMAT: UPPER_ROW")) {
						nodeCoordSection = true;
					} else { // All the numbers are in this part
						
						if (fileType == EXPLICIT){
							halfMatrix(graphBrazil, in); // harllan
						}
						else{
						
							StringTokenizer strTok = new StringTokenizer(line, " \t");
							try {
	
								strTok.nextToken(); // Discard the city number
								if (fileType == EUC_2D || fileType == GEO) {
									double x = Double.valueOf(strTok.nextToken()).doubleValue();
									double y = Double.valueOf(strTok.nextToken()).doubleValue();
									coords.addElement(new TSPCoordinate(x, y));
								} else
									throw new TSPException("Unrecognized file format!");
							} catch (NoSuchElementException e) {
								throw new TSPException("Could not parse file " + "'" + fileName + "'!");
							}
							
						}
					}
				}

			}
		} catch (FileNotFoundException e) {
			throw new TSPException("File " + "'" + fileName + "'" + " not found in the current directory!");
		} catch (IOException e) {
			throw new TSPException("Could not read from file " + "'" + fileName + "'!");
		}

		if (fileType != EXPLICIT){
			int graph[][] = new int[coords.size()][coords.size()];
			
			for (int i = 0; i < graph.length; i++) {
				for (int j = 0; j <= i; j++) {
					if (i == j)
						graph[i][j] = 0;
					else {
						if (fileType == EUC_2D) {
							double dX = ((TSPCoordinate) coords.elementAt(i)).getX()
									- ((TSPCoordinate) coords.elementAt(j)).getX();
							double dY = ((TSPCoordinate) coords.elementAt(i)).getY()
									- ((TSPCoordinate) coords.elementAt(j)).getY();
							graph[i][j] = (int) Math.round(Math.sqrt(dX * dX + dY * dY));
						} else if (fileType == GEO) {
							double deg = Math.floor(((TSPCoordinate) coords.elementAt(i)).getX());
							double min = ((TSPCoordinate) coords.elementAt(i)).getX() - deg;
							double latitudeI = Math.PI * (deg + 5.0 * min / 3.0) / 180.0;
	
							deg = Math.floor(((TSPCoordinate) coords.elementAt(i)).getY());
							min = ((TSPCoordinate) coords.elementAt(i)).getY() - deg;
							double longitudeI = Math.PI * (deg + 5.0 * min / 3.0) / 180.0;
	
							deg = Math.floor(((TSPCoordinate) coords.elementAt(j)).getX());
							min = ((TSPCoordinate) coords.elementAt(j)).getX() - deg;
							double latitudeJ = Math.PI * (deg + 5.0 * min / 3.0) / 180.0;
	
							deg = Math.floor(((TSPCoordinate) coords.elementAt(j)).getY());
							min = ((TSPCoordinate) coords.elementAt(j)).getY() - deg;
							double longitudeJ = Math.PI * (deg + 5.0 * min / 3.0) / 180.0;
	
							double RRR = 6378.388;
							double q1 = Math.cos(longitudeI - longitudeJ);
							double q2 = Math.cos(latitudeI - latitudeJ);
							double q3 = Math.cos(latitudeI + latitudeJ);
							graph[i][j] = (int) Math
									.round((RRR * Math.acos(0.5 * ((1.0 + q1) * q2 - (1.0 - q1) * q3)) + 1.0));
						} else
							throw new TSPException("Unrecognized file format!");
						graph[j][i] = graph[i][j];
					}
	
				}
			}
	
			return graph;
		}
		else{
			return graphBrazil;
		}
	}

}
