package com.husseinmahfouz.matsim.dmc.mode_choice.costs;

import java.util.List;

// import org.apache.logging.log4j.LogManager;
// import org.apache.logging.log4j.Logger;

import org.eqasim.core.simulation.mode_choice.cost.AbstractCostModel;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsCostParameters;
import org.matsim.api.core.v01.population.Leg;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

/**
 * This DRTCostModel is an alternative to the basic one. It incentivises using DRT as a feeder by
 * making feeder trips free (0r cheaper) (similar to TfL hopper fare)
 **/

public class LeedsDrtCostModel extends AbstractCostModel {
    // private static final Logger logger = LogManager.getLogger(LeedsDrtCostModelFeeder.class);
    private final LeedsCostParameters costParameters;


    @Inject
    public LeedsDrtCostModel(LeedsCostParameters costParameters) {
        super("drt");
        this.costParameters = costParameters;
    }

    @Override
    public double calculateCost_MU(Person person, DiscreteModeChoiceTrip trip,
            List<? extends PlanElement> elements) {
        boolean isFeederDrt = false;
        boolean isDrtOnly = false;

        // Iterate through the plan elements to check for DRT and feeder DRT modes
        for (PlanElement element : elements) {
            if (element instanceof Leg) {
                Leg leg = (Leg) element;
                // String mode = leg.getMode();

                // Check the routingMode attribute.
                // This distinguishes between drt and drt_feeder modes
                String routingMode = leg.getRoutingMode();

                if (routingMode != null && routingMode.contains("feeder")) {
                    isFeederDrt = true;
                    break; // No need to check further if it's a feeder DRT trip
                } else if (routingMode != null && routingMode.contains("drt")) {
                    isDrtOnly = true;
                }
            }
        }

        // Calculate the cost based on the trip type
        double tripDistance_km = getInVehicleDistance_km(elements);
        if (isFeederDrt) {
            // Feeder DRT trip
            // It is much cheaper (same as TfL hopper fare). Add 0.1 to avoid downstream issues
            return (costParameters.drtFareBaseFeeder
                    + costParameters.drtFarePerKmFeeder * tripDistance_km) + 0.1;
        } else if (isDrtOnly) {
            // DRT-only trip
            return (costParameters.drtFareBase + costParameters.drtFarePerKm * tripDistance_km)
                    + 0.1;
        } else {
            // If a trip has no DRT leg, the cost is set to a very high value
            // TODO: figure out why these trips exist. I think it is because
            // FeederDrtModeAvailabilityWrapper adds DRT and DRT_feeder modes
            // from the config to all individuals (regardless of drt service
            // area).So even though a person has these mode available, there is no feasible plan
            // with DRT. In that case, it is not an issue
            // The serviceareaconstraint is applied right before the MNL (to eliminate modes), but
            // the
            // utility of each mode is calculated before the constraints are applied?

            // logger.warn("LeedsDrtCostModelFeeder received a trip with no DRT legs.");
            return 10000; // Return a high cost as this trip is not feasible with DRT (probably
            // unnecessary as it is excluded downstream, but we need a number)
        }

    }

    // Old simpe LeedsDrtCostModel logic (commented out for reference). No distinguisheing between
    // standalone and feeder:


    // @Override
    // public double calculateCost_MU(Person person, DiscreteModeChoiceTrip trip,
    // List<? extends PlanElement> elements) {
    // double tripDistance_km = getInVehicleDistance_km(elements);
    // // add 0.1 to avoid log(0) in DrtUtilityEstimator: Math.log(variables.cost_MU)
    // return (costParameters.drtFareBase + costParameters.drtFarePerKm * tripDistance_km) + 0.1;
    // }

}
