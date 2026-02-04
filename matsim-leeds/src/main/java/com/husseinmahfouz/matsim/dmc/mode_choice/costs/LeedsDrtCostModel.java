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
 * DRT cost model with distance-based pricing.
 * 
 * Pricing structure:
 * - Base fare covers first N km (drtFreeDistance_km)
 * - Additional km charged at drtFarePerKm
 * - Feeder trips have same logic but separate parameters. We make 
 *   feeder cheaper through a 'hopper fare'
 */

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
            return calculateFeederCost(tripDistance_km);
        } else if (isDrtOnly) {
            return calculateStandaloneCost(tripDistance_km);
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

    /**
     * Calculate cost for standalone DRT trip.
     * 
     * Structure: Base fare + (distance beyond free threshold) * perKm rate
     * Example with base=£2, freeDistance=3km, perKm=£0.50:
     *   - 2 km trip: £2.00 (within free distance)
     *   - 5 km trip: £2.00 + (5-3) * £0.50 = £3.00
     */
    private double calculateStandaloneCost(double tripDistance_km) {
        double baseFare = costParameters.drtFareBase;
        double perKmRate = costParameters.drtFarePerKm;
        double freeDistance = costParameters.drtFreeDistance_km;
        
        // Calculate chargeable distance (distance beyond free threshold)
        double chargeableDistance_km = Math.max(0, tripDistance_km - freeDistance);
        
        // Total cost = base + chargeable distance * rate
        double cost = baseFare + (chargeableDistance_km * perKmRate);
        
        // Add small epsilon to avoid log(0) in utility estimator
        return cost + 0.1;
    }

    /**
     * Calculate cost for feeder DRT trip.
     * 
     * Typically cheaper than standalone (like TfL hopper fare).
     * Same structure but with feeder-specific parameters.
     */
    private double calculateFeederCost(double tripDistance_km) {
        double baseFare = costParameters.drtFareBaseFeeder;
        double perKmRate = costParameters.drtFarePerKmFeeder;
        double freeDistance = costParameters.drtFreeDistanceFeeder_km;
        
        // Calculate chargeable distance
        double chargeableDistance_km = Math.max(0, tripDistance_km - freeDistance);
        
        // Total cost = base + chargeable distance * rate
        double cost = baseFare + (chargeableDistance_km * perKmRate);
        
        // Add small epsilon to avoid log(0) in utility estimator
        return cost + 0.1;
    }

    // Old simple LeedsDrtCostModel logic (commented out for reference). No distinguisheing between
    // standalone and feeder:


    // @Override
    // public double calculateCost_MU(Person person, DiscreteModeChoiceTrip trip,
    // List<? extends PlanElement> elements) {
    // double tripDistance_km = getInVehicleDistance_km(elements);
    // // add 0.1 to avoid log(0) in DrtUtilityEstimator: Math.log(variables.cost_MU)
    // return (costParameters.drtFareBase + costParameters.drtFarePerKm * tripDistance_km) + 0.1;
    // }

}
