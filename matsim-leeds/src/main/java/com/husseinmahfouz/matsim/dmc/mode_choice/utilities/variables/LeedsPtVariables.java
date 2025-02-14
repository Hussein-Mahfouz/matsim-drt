package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables;

import org.eqasim.core.simulation.mode_choice.utilities.variables.PtVariables;


public class LeedsPtVariables extends PtVariables {
    public final double railTravelTime_min;
    public final double busTravelTime_min;
    public final double railWaitingTime_min;
    public final double busWaitingTime_min;
    public final double railAccessEgressTime_min;
    public final double busAccessEgressTime_min;


    // delegate means get variables using the parent class
    public LeedsPtVariables(PtVariables delegate, double railTravelTime_min,
            double busTravelTime_min, double railWaitingTime_min, double busWaitingTime_min,
            double railAccessEgressTime_min, double busAccessEgressTime_min) {
        super(delegate.inVehicleTime_min, delegate.waitingTime_min, delegate.accessEgressTime_min,
                delegate.numberOfLineSwitches, delegate.cost_MU, delegate.euclideanDistance_km);

        this.railTravelTime_min = railTravelTime_min;
        this.busTravelTime_min = busTravelTime_min;
        this.railWaitingTime_min = railWaitingTime_min;
        this.busWaitingTime_min = busWaitingTime_min;
        this.railAccessEgressTime_min = railAccessEgressTime_min;
        this.busAccessEgressTime_min = busAccessEgressTime_min;
    }
}
