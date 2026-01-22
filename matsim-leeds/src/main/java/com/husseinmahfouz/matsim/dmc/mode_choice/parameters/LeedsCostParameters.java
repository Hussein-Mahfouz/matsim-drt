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
	// DRT standalone
    public double drtFareBase;
    public double drtFarePerKm;
    public double drtFreeDistance_km;  // Distance included in base fare
    
    // DRT feeder
    public double drtFareBaseFeeder;
    public double drtFarePerKmFeeder;
    public double drtFreeDistanceFeeder_km;  // Distance included in base fare for feeder


	public static LeedsCostParameters buildDefault() {
		LeedsCostParameters parameters = new LeedsCostParameters();
		// car
		parameters.carCostPerKm = 0.3;
		// bus
		parameters.busFare = 2.0;
		parameters.railFareBase = 3.0;
		parameters.railFarePerKm = 1.0;

		// drt
		parameters.drtFareBase = 2.0;
		parameters.drtFarePerKm = 0.0;
		parameters.drtFreeDistance_km = 0.0;  // Example: if 2.0, then first two km do not have an additional fare/km
		// drt feeder: // to try scenarios where DRT is cheaper when used as a feeder
		parameters.drtFareBaseFeeder = 0.2;
		parameters.drtFarePerKmFeeder = 0.0;
		parameters.drtFreeDistanceFeeder_km = 0.0;  
		// taxi
		parameters.taxi_inititalCharge = 2.5;
		parameters.taxiCostPerMinute = 0.2;
		parameters.taxiCostPerKm = 0.5;

		return parameters;
	}
}
