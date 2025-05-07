package com.husseinmahfouz.matsim.dmc.mode_choice.parameters;

import org.eqasim.core.simulation.mode_choice.ParameterDefinition;

public class LeedsCostParameters implements ParameterDefinition {
	public double carCostPerKm;
	public double busFare;
	public double taxi_inititalCharge;
	public double taxiCostPerMinute;
	public double taxiCostPerKm;
	public double railFareBase;
	public double railFarePerKm;
	// public double drtFareBase;
	// public double drtFarePerKm;
	// public double drtFareBaseFeeder;
	// public double drtFarePerKmFeeder;

	public static LeedsCostParameters buildDefault() {
		LeedsCostParameters parameters = new LeedsCostParameters();
		// car
		parameters.carCostPerKm = 0.3;
		// bus
		parameters.busFare = 2.0;
		parameters.railFareBase = 3.0;
		parameters.railFarePerKm = 1.0;

		// // drt
		// parameters.drtFareBase = 2.0;
		// parameters.drtFarePerKm = 0.0;
		// // drt feeder: // to try scenarios where DRT is cheaper when used as a feeder
		// parameters.drtFareBaseFeeder = 0.2;
		// parameters.drtFarePerKmFeeder = 0.0;
		// taxi
		parameters.taxi_inititalCharge = 2.5;
		parameters.taxiCostPerMinute = 0.2;
		parameters.taxiCostPerKm = 0.5;

		return parameters;
	}
}
