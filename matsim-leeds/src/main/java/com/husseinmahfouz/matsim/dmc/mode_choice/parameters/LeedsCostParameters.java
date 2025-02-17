package com.husseinmahfouz.matsim.dmc.mode_choice.parameters;

import org.eqasim.core.simulation.mode_choice.ParameterDefinition;

public class LeedsCostParameters implements ParameterDefinition {
	public double carCost_EUR_km;
	public double busFare;
	public double taxi_inititalCharge;
	public double taxiCostPerMinute;
	public double taxiCostPerKm;
	public double railFareBase;
	public double railFarePerKm;

	public static LeedsCostParameters buildDefault() {
		LeedsCostParameters parameters = new LeedsCostParameters();
        // car
		parameters.carCost_EUR_km = 0.15;
        // bus
		parameters.busFare = 2.0;
		parameters.railFareBase = 3.0;
		parameters.railFarePerKm = 1.0;

		// taxi
		parameters.taxi_inititalCharge = 2.5;
		parameters.taxiCostPerMinute = 0.2;
		parameters.taxiCostPerKm = 0.5;

		return parameters;
	}
}
