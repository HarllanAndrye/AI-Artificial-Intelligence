package greedySearch;

import java.util.ArrayList;

//https://code.google.com/p/tspuib/source/browse/trunk/TravelingSalesMan/src/travelingsalesman/?r=13

public class NearestNeighbour {
    
	int qtdCities;
	int[][] matrix;
    int sourceCity;

    public String result = new String();
    
    ArrayList followedRoute;
    int nodes = 0;
    int routeCost = 0;   
    
    /** Creates a new instance of NearestNeighbour */
    public NearestNeighbour(int[][] matrix, int sourceCity, int qtdcities) {
    	this.matrix = matrix;
        this.sourceCity = sourceCity;
        this.qtdCities = qtdcities;
    }
    
    /**
     * executes the algorithm
     */
    public String execute () {
        followedRoute = new ArrayList();
        followedRoute.add(sourceCity);
        nodes++;
        
        result =  "NEAREST NEIGHBOUR SEARCH\n\n";
        
        long startTime = System.currentTimeMillis();
        search(sourceCity);
        long endTime = System.currentTimeMillis();
        
        result += "\nBetter solution: "+followedRoute.toString() + "\n";
        result += "Cost: " + routeCost + "\n";
        result += "Visited Nodes: "+nodes+"\n";
        result += "Elapsed Time: "+(endTime-startTime)+" ms\n";
        
        return result;         
    }   
    
    /**
     * gets the cost of going from city "a" to city "b"
     */
    public int getCost(int a, int b) {
        return this.matrix[a][b];
    }
    
    /**
     * @param from node where we start the search.
     */
    public void search (int from) {
        int currentTown = from;
        
        while (nodes != this.qtdCities) {
            // choose the closest town
            int lowestDistance = Integer.MAX_VALUE;
            int chosen = -1;
            for (int i=0; i < this.qtdCities; i++) {
                if (!followedRoute.contains(i)) {
                	int tempDistance = getCost(currentTown, i);
                    if (tempDistance < lowestDistance) {
                        lowestDistance = tempDistance;
                        chosen = i;
                    }
                }
            }
            routeCost += getCost(currentTown, chosen);
            followedRoute.add(chosen);
            currentTown = chosen;
            nodes++;
        }
        // add the last town
        routeCost += getCost(currentTown, sourceCity);
        followedRoute.add(sourceCity);
        nodes++;
    }    
}