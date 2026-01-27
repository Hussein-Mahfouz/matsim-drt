package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.modes.drt.mode_choice.utilities.estimators.DrtUtilityEstimator;
import org.eqasim.core.simulation.modes.drt.mode_choice.variables.DrtVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsDrtPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;
import com.husseinmahfouz.matsim.drt.rejections.DrtPenaltyController;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

/**
 * DRT utility estimator for Leeds model.
 * 
 * Utility equation (mirrors Bus structure):
 * U_drt = ASC_drt
 *       + (β_IVT + β_peak * 1(Peak)) * f(IVT, λ_time)
 *       + β_OVT * f(WaitTime + WalkTime, λ_OVT)
 *       + β_cost * ln(Cost)
 *       + β_penalty * π_k
 * 
 * where:
 * - f(x, λ) = (x^λ - 1) / λ is the Box-Cox transformation
 * - π_k is the dynamic rejection penalty at iteration k
 * - Peak shift is combined with IVT coefficient BEFORE transformation
 */
public class LeedsDrtUtilityEstimator extends DrtUtilityEstimator {
    private final LeedsModeParameters parameters;
    private final LeedsPersonPredictor personPredictor;
    private final LeedsDrtPredictor drtPredictor;
    private final LeedsSpatialPredictor spatialPredictor;
    private final DrtPenaltyController penaltyController;

    @Inject
    public LeedsDrtUtilityEstimator(LeedsModeParameters parameters,
            LeedsPersonPredictor personPredictor, 
            LeedsDrtPredictor drtPredictor,
            LeedsSpatialPredictor spatialPredictor,
            DrtPenaltyController penaltyController) {
        super(parameters, drtPredictor.delegate);

        this.parameters = parameters;
        this.personPredictor = personPredictor;
        this.drtPredictor = drtPredictor;
        this.spatialPredictor = spatialPredictor;
        this.penaltyController = penaltyController;
    }

    /**
     * Calculates In-Vehicle Time utility with combined peak shift.
     * 
     * Formula: (β_IVT + β_peak * 1(Peak)) * f(IVT, λ)
     */
    protected double estimateTravelTimeUtility(DrtVariables variables, 
            LeedsSpatialVariables spatialVars) {
        double lambda = parameters.leedsPT.lambdaTravelTime;
        boolean isPeak = spatialVars.isAMPeak || spatialVars.isPMPeak;
        
        // Start with base IVT coefficient
        double combinedCoeff = parameters.drt.betaTravelTime_u_min;
        
        // Add peak shift if applicable
        if (isPeak) {
            combinedCoeff += parameters.leedsDrt.betaAmPmPeak;
        }
        
        // Apply Box-Cox transformation with combined coefficient
        return combinedCoeff * ((Math.pow(variables.travelTime_min, lambda) - 1) / lambda);
    }

    /**
     * Calculates Out-of-Vehicle Time utility (waiting + access/egress walk).
     * 
     * Formula: β_OVT * f(WaitTime + WalkTime, λ_OVT)
     */
    protected double estimateOutOfVehicleTimeUtility(DrtVariables variables) {
        double lambda = parameters.leedsPT.lambdaOutofVehicleTime;
        double OVT = variables.waitingTime_min + variables.accessEgressTime_min;

        if (OVT > 1) {
            return parameters.drt.betaWaitingTime_u_min
                    * ((Math.pow(OVT, lambda) - 1) / lambda);
        }
        return 0.0;
    }

    @Override
    protected double estimateMonetaryCostUtility(DrtVariables variables) {
        if (variables.cost_MU > 0) {
            return parameters.betaCost_u_MU * Math.log(variables.cost_MU);
        }
        return 0.0;
    }

    /**
     * Calculates rejection penalty based on global rejection rate control.
     * 
     * Uses PID controller to adjust penalty based on deviation from target rate.
     * Returns 0 if penalty method is disabled.
     */
    protected double estimateRejectionPenalty(DiscreteModeChoiceTrip trip) {
        String mode = trip.getInitialMode();
        double penalty = penaltyController.getCurrentPenalty(mode);
        return parameters.leedsDrt.betaRejectionPenalty_u * penalty;
    }

    @Override
    public double estimateUtility(Person person, DiscreteModeChoiceTrip trip,
            List<? extends PlanElement> elements) {
        LeedsPersonVariables personVariables = personPredictor.predictVariables(person, trip, elements);
        DrtVariables drtVariables = drtPredictor.predictVariables(person, trip, elements);
        LeedsSpatialVariables spatialVariables = spatialPredictor.predictVariables(person, trip, elements);

        double utility = 0.0;

        utility += estimateConstantUtility();
        utility += estimateTravelTimeUtility(drtVariables, spatialVariables);
        utility += estimateOutOfVehicleTimeUtility(drtVariables);
        utility += estimateMonetaryCostUtility(drtVariables);
        utility += estimateRejectionPenalty(trip);

        return utility;
    }
}