package com.husseinmahfouz.matsim.dmc.mode_choice.parameters;

import org.eqasim.core.simulation.mode_choice.parameters.ModeParameters;

public class LeedsModeParameters extends ModeParameters {
	public class LeedsCarParameters {
		// public double betaInsideUrbanArea;
		// public double betaCrossingUrbanArea;
		public double betaCommuting;
		public double betaAmPeak;
		public double betaPmPeak;
		public double lambdaTravelTime;
	}

	public class LeedsPTParameters {
		public double betaOutofVehicleTime_u_min;
		public double lambdaTravelTime;
		public double lambdaOutofVehicleTime;
		public double betaAmPmPeak;
	}

	public class LeedsBikeParameters {
		// public double betaInsideUrbanArea;
		public double betaMale;
		public double betaStudent;
		public double lambdaTravelTime;
	}

	public class LeedsWalkParameters {
		public double betaStudent;
		public double lambdaTravelTime;
		public double betaAge18to29;
	}

	public class LeedsTaxiParameters {
		public double alpha_u;
		public double betaTravelTime_u_min;
		public double betaAmPeak;
		public double betaPmPeak;
		public double betaMale;
		public double betaAge18to24;
		public double betaAge25to29;
		public double lambdaTravelTime;

	}


	public final LeedsCarParameters leedsCar = new LeedsCarParameters();
	public final LeedsBikeParameters leedsBike = new LeedsBikeParameters();
	public final LeedsPTParameters leedsPT = new LeedsPTParameters();
	public final LeedsWalkParameters leedsWalk = new LeedsWalkParameters();
	public final LeedsTaxiParameters leedsTaxi = new LeedsTaxiParameters();

	public static LeedsModeParameters buildDefault() {
		LeedsModeParameters parameters = new LeedsModeParameters();

		// Cost
		parameters.betaCost_u_MU = -0.8362;
		// parameters.lambdaCostEuclideanDistance = 0;
		// parameters.referenceEuclideanDistance_km = 40.0;


		// Car
		parameters.car.alpha_u = 0.0; // Everything is relative to car
		parameters.car.betaTravelTime_u_min = -0.2426;

		parameters.car.additionalAccessEgressWalkTime_min = 4.0;
		parameters.car.constantParkingSearchPenalty_min = 4.0;

		// parameters.leedsCar.betaInsideUrbanArea = -0.5;
		// parameters.leedsCar.betaCrossingUrbanArea = -1.0;
		parameters.leedsCar.lambdaTravelTime = 0.5424;
		parameters.leedsCar.betaCommuting = -0.1478;
		parameters.leedsCar.betaAmPeak = -0.1637;
		parameters.leedsCar.betaPmPeak = 0.1282;

		// PT
		parameters.pt.alpha_u = -0.0929;
		parameters.pt.betaInVehicleTime_u_min = -0.1281;
		// parameters.pt.betaLineSwitch_u = -0.17;
		// parameters.pt.betaWaitingTime_u_min = -0.0484;
		// parameters.pt.betaAccessEgressTime_u_min = -0.0804;

		parameters.leedsPT.betaOutofVehicleTime_u_min = -1.1484;
		parameters.leedsPT.betaAmPmPeak = -0.0998;
		parameters.leedsPT.lambdaTravelTime = 0.5424;
		parameters.leedsPT.lambdaOutofVehicleTime = 0.1452; // Box-cox lambda parameter for OVT

		// Bike
		parameters.bike.alpha_u = -4.0728;
		parameters.bike.betaTravelTime_u_min = -0.3343;
		// parameters.bike.betaAgeOver18_u_a = -0.0496;

		// parameters.leedsBike.betaInsideUrbanArea = 1.5;
		parameters.leedsBike.betaMale = 1.1047;
		parameters.leedsBike.betaStudent = 1.1559;
		parameters.leedsBike.lambdaTravelTime = 0.5424;


		// Walk
		parameters.walk.alpha_u = 3.0294;
		parameters.walk.betaTravelTime_u_min = -0.6774;

		parameters.leedsWalk.betaStudent = 0.6964;
		parameters.leedsWalk.lambdaTravelTime = 0.5424;
		parameters.leedsWalk.betaAge18to29 = 0.6964;

		// Taxi
		parameters.leedsTaxi.alpha_u = -1.8075;
		parameters.leedsTaxi.betaTravelTime_u_min = -0.4525;
		parameters.leedsTaxi.betaAmPeak = -0.1709;
		parameters.leedsTaxi.betaPmPeak = -0.1423;
		parameters.leedsTaxi.betaMale = -0.6434;
		parameters.leedsTaxi.betaAge18to24 = 1.5014;
		parameters.leedsTaxi.betaAge25to29 = 0.9324;
		parameters.leedsTaxi.lambdaTravelTime = 0.5424;


		return parameters;
	}
}
