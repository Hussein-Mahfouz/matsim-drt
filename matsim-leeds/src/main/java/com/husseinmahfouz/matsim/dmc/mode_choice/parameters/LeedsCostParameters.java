package com.husseinmahfouz.matsim.dmc.mode_choice.parameters;

import org.eqasim.core.simulation.mode_choice.ParameterDefinition;

public class LeedsCostParameters implements ParameterDefinition {
	public double carCost_EUR_km;
	public double busFare;
	// public double railFare;

	public static LeedsCostParameters buildDefault() {
		LeedsCostParameters parameters = new LeedsCostParameters();

		parameters.carCost_EUR_km = 0.15;

		parameters.busFare = 2.0;
		// parameters.railFare = 5.0;

		return parameters;
	}
}
