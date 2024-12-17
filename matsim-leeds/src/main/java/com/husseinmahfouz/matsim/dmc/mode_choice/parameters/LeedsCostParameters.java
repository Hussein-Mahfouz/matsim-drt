package com.husseinmahfouz.matsim.dmc.mode_choice.parameters;

import org.eqasim.core.simulation.mode_choice.ParameterDefinition;

public class LeedsCostParameters implements ParameterDefinition {
	public double carCost_EUR_km;
	public double busFare;
	public double railFareBase;
	public double railFarePerKm;

	public static LeedsCostParameters buildDefault() {
		LeedsCostParameters parameters = new LeedsCostParameters();

		parameters.carCost_EUR_km = 0.15;

		parameters.busFare = 2.0;
		parameters.railFareBase = 3.0;
		parameters.railFarePerKm = 1.0;

		return parameters;
	}
}
