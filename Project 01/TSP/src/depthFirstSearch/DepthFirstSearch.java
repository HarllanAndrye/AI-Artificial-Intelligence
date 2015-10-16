package depthFirstSearch;

import java.util.ArrayList;

//https://code.google.com/p/tspuib/source/browse/trunk/TravelingSalesMan/src/travelingsalesman/?r=13

public class DepthFirstSearch {
    
	int[][] matrix;
	int qtdCities;
	
    int sourceCity;
    public String result = new String();
    
    ArrayList initialRoute, optimumRoute;
    int nodes = 0;
    int routeCost = 0;
    int optimumCost = Integer.MAX_VALUE;
    
    
    /** Creates a new instance of DepthFirstSearch */
    public DepthFirstSearch(int[][] matrix, int sourceCity, int qtdcities) {
        
        this.matrix = matrix;
        this.sourceCity = sourceCity;
        this.qtdCities = qtdcities;
    }
    
    /**
     * executes the algorithm
     */
    public String execute () {
        
        initialRoute = new ArrayList();
        initialRoute.add(sourceCity);
        optimumRoute = new ArrayList();
        nodes++;
        
        result =  "DEPTH FIRST SEARCH\n\n";
        
        long startTime = System.currentTimeMillis();
        search(sourceCity, initialRoute);
        long endTime = System.currentTimeMillis();
        
        result += "\nBetter solution: "+optimumRoute.toString() + "\n";
        result += "Cost: " + optimumCost + "\n";
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
     * @param route followed route for arriving to node "from".
     */
    public void search (int from, ArrayList followedRoute) {
        
        // we've found a new solution
    	if (followedRoute.size() == this.qtdCities) {
            
            followedRoute.add(sourceCity);
            nodes++;
            
            // update the route's cost
            routeCost += getCost(from, sourceCity);
            
            if (routeCost < optimumCost) {
                optimumCost = routeCost;
                optimumRoute = (ArrayList)followedRoute.clone();
            }
            
            result += followedRoute.toString() + "// Cost: "+routeCost + "\n";
            
            // update the route's cost (back to the previous value)
            routeCost -= getCost(from, sourceCity);
        }
        else {
        	for (int to=0; to<this.qtdCities; to++){
                if (!followedRoute.contains(to)) {
                    
                    ArrayList increasedRoute = (ArrayList)followedRoute.clone();
                    increasedRoute.add(to);
                    nodes++;
                    
                    // update the route's cost
                    routeCost += getCost(from, to);
                    
                    search(to, increasedRoute);
                    
                    // update the route's cost (back to the previous value)
                    routeCost -= getCost(from, to);
                }
            }
        }
        
    }
    
}