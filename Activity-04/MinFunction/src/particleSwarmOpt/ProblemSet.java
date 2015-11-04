package particleSwarmOpt;

/*
 * Minimizar f(x) = x^2 - 3x + 4
 * [-10, 10]
 */

public class ProblemSet {
	public static final double LOC_X_LOW = -10; // limite minimo do valor de x
	public static final double LOC_X_HIGH = 10; // limite máximo do valor de x
	public static final double VEL_LOW = -1;
	public static final double VEL_HIGH = 1;
	
	public static final double ERR_TOLERANCE = 1E-20; // the smaller the tolerance, the more accurate the result, 
	                                                  // but the number of iteration is increased
	
	public static double evaluate(Location location) {
		double result = 0;
		double x = location.getLoc()[0]; // valor de x
		
		result = Math.pow(x, 2) - (3 * x) + 4;
		
		return result;
	}
}
