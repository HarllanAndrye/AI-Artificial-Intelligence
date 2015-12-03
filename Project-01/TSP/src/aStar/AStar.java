package aStar;

import global.Town;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.PriorityQueue;

//https://code.google.com/p/tspuib/source/browse/trunk/TravelingSalesMan/src/travelingsalesman/?r=13

public class AStar {
    
	int qtdCities;
	int[][] matrix;
	
    int sourceCity;
    PriorityQueue<Town> opened = new PriorityQueue<Town>(200, 
        new Comparator<Town>() {
          public int compare(Town a, Town b) {
            return a.f - b.f;
          }
        }
      );  
    public String result = new String();
    
    ArrayList optimumRoute, followedRoute;
    int nodes = 0;
    int routeCost = 0;
    int optimumCost = Integer.MAX_VALUE;     
    
    // Estimation of the cost between two cities, it can overestimate the real value (h' > h),
    // so the algorithm it's not optimum.
    int HEURISTICCONSTANT = 15;
    
    /**
     * Gets the heuristic value for a given depth
     * The level 0 has the maximum value.
     */
    private int getHeuristicValue (int level) {
    	//return HEURISTICCONSTANT * (this.qtdCities - level);
    	return (this.qtdCities - level); //Harllan-A constante gera uma heurística não admissível
    }
    
    /** Creates a new instance of AStar */
    public AStar(int[][] matrix, int sourceCity, int qtdcities) {
    	this.matrix = matrix;
        this.sourceCity = sourceCity;        
        this.qtdCities = qtdcities;
    }
    
    /**
     * gets the cost of going from city "a" to city "b"
     */
    public int getCost(int a, int b) {
        return this.matrix[a][b];
    }
    
    /**
     * executes the algorithm
     */
    public String execute() {

        // have we found the solution?
        boolean solution = false;
        
        // start the timer
        long startTime = System.currentTimeMillis();
        
        // initial town
        opened.add(new Town(sourceCity, 0, getHeuristicValue(0), 0));
        
        while (!opened.isEmpty() && !solution) {
            // gets the city with lower g value
            Town currentTown = opened.poll();
            nodes++;

            // rebuild the followed route for the selected town
            Town aux = currentTown;
            followedRoute = new ArrayList();
            followedRoute.add(aux.number);
            while (aux.level != 0) {
                aux = aux.parent;
                followedRoute.add(0, aux.number);
            }
            
            if (currentTown.level == this.qtdCities) {
                solution = true;
                optimumRoute = followedRoute;
                optimumCost = currentTown.g;
            } else {
                
            	for (int i=0; i<this.qtdCities; i++) {
                    // have we visited this city in the current followed route?
                    boolean visited = followedRoute.contains(i);
                    boolean isSolution = (followedRoute.size() == this.qtdCities)&&(i == sourceCity);

                    if (!visited || isSolution) {
                    	Town childTown = new Town(i, currentTown.g + getCost(currentTown.number, i),
                                getHeuristicValue(currentTown.level + 1), currentTown.level + 1);
                        childTown.parent = currentTown;
                        opened.add(childTown);  
                    }
                }                
            }
        }
        long endTime = System.currentTimeMillis();
        
        result =  "A STAR SEARCH\n\n";     
        result += "Better solution: "+optimumRoute.toString() + "\n";
        result += "Cost: " + optimumCost + "\n";
        result += "Visited Nodes: "+nodes+"\n";
        result += "Elapsed Time: "+(endTime-startTime)+" ms\n";
        
        return result;        
    }    
    
}