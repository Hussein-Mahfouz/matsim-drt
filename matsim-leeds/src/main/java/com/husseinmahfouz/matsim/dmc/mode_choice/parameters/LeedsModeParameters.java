package com.husseinmahfouz.matsim.dmc.mode_choice.parameters;

import org.eqasim.core.simulation.mode_choice.parameters.ModeParameters;

public class LeedsModeParameters extends ModeParameters {
	public class LeedsCarParameters {
		public double betaInsideUrbanArea;
		public double betaCrossingUrbanArea;
		public double lambdaIVT;
		public double shiftCommuting;
	}

	public class LeedsBikeParameters {
		public double betaInsideUrbanArea;
		public double betaMale;
		public double betaStudent;
		public double lambdaIVT;
	}

	public class LeedsPTParameters {
		public double lambdaIVT;
		public double lambdaOVT;
		public double OutofVehicleTime_u_min;
	}


	public class LeedsWalkParameters {
		public double lambdaIVT;
		public double betaStudent;
	}


	public final LeedsCarParameters leedsCar = new LeedsCarParameters();
	public final LeedsBikeParameters leedsBike = new LeedsBikeParameters();
	public final LeedsPTParameters leedsPT = new LeedsPTParameters();
	public final LeedsWalkParameters leedsWalk = new LeedsWalkParameters();

	public static LeedsModeParameters buildDefault() {
		LeedsModeParameters parameters = new LeedsModeParameters();

		// Cost
		parameters.betaCost_u_MU = -0.8362;
		parameters.lambdaCostEuclideanDistance = -0.4;
		parameters.referenceEuclideanDistance_km = 40.0;

		// Car
		parameters.car.alpha_u = 1.35;
		parameters.car.betaTravelTime_u_min = -0.06;

		parameters.car.additionalAccessEgressWalkTime_min = 4.0;
		parameters.car.constantParkingSearchPenalty_min = 4.0;

		parameters.leedsCar.betaInsideUrbanArea = -0.5;
		parameters.leedsCar.betaCrossingUrbanArea = -1.0;
		parameters.leedsCar.lambdaIVT = 0.5424;
		parameters.leedsCar.shiftCommuting = -0.1478;

		// PT
		parameters.pt.alpha_u = 0.0;
		parameters.pt.betaLineSwitch_u = -0.17;
		parameters.pt.betaInVehicleTime_u_min = -0.017;
		parameters.pt.betaWaitingTime_u_min = -0.0484;
		parameters.pt.betaAccessEgressTime_u_min = -0.0804;

		parameters.leedsPT.lambdaIVT = 0.5424;
		parameters.leedsPT.lambdaOVT = 0.1452; // Box-cox lambda parameter for OVT
		parameters.leedsPT.OutofVehicleTime_u_min = -1.1484;



		// Bike
		parameters.bike.alpha_u = -2.0;
		parameters.bike.betaTravelTime_u_min = -0.05;
		parameters.bike.betaAgeOver18_u_a = -0.0496;

		parameters.leedsBike.betaInsideUrbanArea = 1.5;
		parameters.leedsBike.betaMale = 1.1047;
		parameters.leedsBike.betaStudent = 1.1559;
		parameters.leedsBike.lambdaIVT = 0.5424;


		// Walk
		parameters.walk.alpha_u = 1.43;
		parameters.walk.betaTravelTime_u_min = -0.15;

		parameters.leedsWalk.lambdaIVT = 0.5424;


		return parameters;
	}
}
