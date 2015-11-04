package particleSwarmOpt;

import java.util.Random;
import java.util.Vector;

/*
 * Baseado em:
 * https://gandhim.wordpress.com/2010/04/04/particle-swarm-optimization-pso-sample-code-using-java/
 */

public class PSO implements PSOConstants {
	private Vector<Particle> swarm = new Vector<Particle>();
	private double[] pBest = new double[SWARM_SIZE];
	private Vector<Location> pBestLocation = new Vector<Location>();
	private double gBest;
	private Location gBestLocation;
	private double[] fitnessValueList = new double[SWARM_SIZE];
	
	Random generator = new Random();
	
	public void execute() {
		initializeSwarm();
		updateFitnessList();
		
		for(int i=0; i<SWARM_SIZE; i++) {
			pBest[i] = fitnessValueList[i];
			pBestLocation.add(swarm.get(i).getLocation());
		}
		
		int t = 0;
		double err = 9999;
		
		while(t < MAX_ITERATION && err > ProblemSet.ERR_TOLERANCE) {
			// step 1 - update pBest
			for(int i=0; i<SWARM_SIZE; i++) {
				if(fitnessValueList[i] < pBest[i]) {
					pBest[i] = fitnessValueList[i];
					pBestLocation.set(i, swarm.get(i).getLocation());
				}
			}
				
			// step 2 - update gBest
			int bestParticleIndex = PSOUtility.getMinPos(fitnessValueList);
			if(t == 0 || fitnessValueList[bestParticleIndex] < gBest) {
				gBest = fitnessValueList[bestParticleIndex];
				gBestLocation = swarm.get(bestParticleIndex).getLocation();
			}
			
			for(int i=0; i<SWARM_SIZE; i++) {
				double r1 = generator.nextDouble();
				double r2 = generator.nextDouble();
				
				Particle p = swarm.get(i);
				
				// step 3 - update velocity
				double[] newVel = new double[PROBLEM_DIMENSION];
				newVel[0] = (int) ( p.getVelocity().getPos()[0] + 
						(r1 * C1) * (pBestLocation.get(i).getLoc()[0] - p.getLocation().getLoc()[0]) +
						(r2 * C2) * (gBestLocation.getLoc()[0] - p.getLocation().getLoc()[0]) );
				Velocity vel = new Velocity(newVel);
				p.setVelocity(vel);
				
				// step 4 - update location
				double[] newLoc = new double[PROBLEM_DIMENSION];
				newLoc[0] = (int) ( p.getLocation().getLoc()[0] + newVel[0] );
				Location loc = new Location(newLoc);
				p.setLocation(loc);
			}
			
			err = ProblemSet.evaluate(gBestLocation) - 0; // minimizing the functions means it's getting closer to 0
			
			
			System.out.println("\nITERATION " + (t+1) + ": ");
			for (int i = 0; i < SWARM_SIZE; i++) {
				System.out.println("   Particle " + (i+1) + ", x = " + swarm.get(i).getLocation().getLoc()[0]);
			}
			System.out.println("---");
			System.out.println("   Best X: " + gBestLocation.getLoc()[0]);
			System.out.println("   f(x) = x²-3x+4 = " + ProblemSet.evaluate(gBestLocation));
			
			
			t++;
			updateFitnessList();
		}
		
		System.out.println("\nThe solution found is: x = " + (int)gBestLocation.getLoc()[0]);
		//System.out.println("     Best X: " + (int)gBestLocation.getLoc()[0]);
	}
	
	public void initializeSwarm() {
		Particle p;
		int rnd = (int) ( ProblemSet.VEL_LOW + generator.nextDouble() * (ProblemSet.VEL_HIGH - ProblemSet.VEL_LOW) );
		for(int i=0; i<SWARM_SIZE; i++) {
			p = new Particle();
			
			// randomize location inside a space defined in Problem Set
			double[] loc = new double[PROBLEM_DIMENSION];
			loc[0] = (int) ( ProblemSet.LOC_X_LOW + generator.nextDouble() * (ProblemSet.LOC_X_HIGH - ProblemSet.LOC_X_LOW) );
			Location location = new Location(loc);
			
			// randomize velocity in the range defined in Problem Set
			double[] vel = new double[PROBLEM_DIMENSION];
			//vel[0] = (int) ( ProblemSet.VEL_LOW + generator.nextDouble() * (ProblemSet.VEL_HIGH - ProblemSet.VEL_LOW) );
			vel[0] = rnd; // Todos inicializam com a mesma velocidade
			Velocity velocity = new Velocity(vel);
			
			p.setLocation(location);
			p.setVelocity(velocity);
			swarm.add(p);
		}
	}
	
	public void updateFitnessList() {
		for(int i=0; i<SWARM_SIZE; i++) {
			fitnessValueList[i] = swarm.get(i).getFitnessValue();
		}
	}
}
