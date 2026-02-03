package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.estimators.PtUtilityEstimator;
import org.eqasim.core.simulation.mode_choice.utilities.variables.PtVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;

import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPtPredictor;

import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPtVariables;

import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsPtUtilityEstimator extends PtUtilityEstimator {
    private final LeedsModeParameters parameters;
    private final LeedsPersonPredictor personPredictor;
    private final LeedsPtPredictor ptPredictor;
    private final LeedsSpatialPredictor spatialPredictor;

    @Inject
    public LeedsPtUtilityEstimator(LeedsModeParameters parameters, LeedsPtPredictor ptPredictor,
            LeedsPersonPredictor personPredictor, LeedsSpatialPredictor spatialPredictor) {
        super(parameters, ptPredictor.delegate);
        this.parameters = parameters;
        this.ptPredictor = ptPredictor;
        this.personPredictor = personPredictor;
        this.spatialPredictor = spatialPredictor;
    }

    protected double estimateConstantUtility(LeedsPtVariables variables) {
        if (variables.railTravelTime_min > variables.busTravelTime_min) {
            return parameters.leedsPT.alpha_u_Rail;
        } else {
            return parameters.leedsPT.alpha_u_Bus;
        }
    }

    /**
     * Calculates In-Vehicle Time utility with combined peak shift.
     * 
     * Bus:  (β_IVT_bus + β_peak * 1(Peak)) * f(IVT, λ_time)
     * Rail: (β_IVT_rail + β_peak * 1(Peak)) * IVT  (linear, no Box-Cox)
     */
    protected double estimateInVehicleTimeUtility(LeedsPtVariables variables, 
            LeedsSpatialVariables spatialVars) {
        double lambda = parameters.leedsPT.lambdaTravelTime;
        double utility = 0.0;
        boolean isPeak = spatialVars.isAMPeak || spatialVars.isPMPeak;

        // Rail (linear IVT, no Box-Cox)
        if (variables.railTravelTime_min > 0) {
            double railCoeff = parameters.leedsPT.betaInVehicleTimeRail_u_min;
            if (isPeak) {
                railCoeff += parameters.leedsPT.betaAmPmPeakRail;
            }
            utility += railCoeff * variables.railTravelTime_min;
        }

        // Bus (Box-Cox transformed IVT)
        if (variables.busTravelTime_min > 1) {
            double busCoeff = parameters.leedsPT.betaInVehicleTimeBus_u_min;
            if (isPeak) {
                busCoeff += parameters.leedsPT.betaAmPmPeakBus;
            }
            utility += busCoeff * ((Math.pow(variables.busTravelTime_min, lambda) - 1) / lambda);
        }

        return utility;
    }

    protected double estimateOutOfVehicleTimeUtility(LeedsPtVariables variables) {
        double utility = 0.0;
        double lambda = parameters.leedsPT.lambdaOutofVehicleTime;
        
        double OVT_rail = variables.railAccessEgressTime_min + variables.railWaitingTime_min;
        double OVT_bus = variables.busAccessEgressTime_min + variables.busWaitingTime_min;
        
        // Rail OVT (Box-Cox)
        if (OVT_rail > 1) {
            utility += parameters.leedsPT.betaOutofVehicleTimeRail_u_min
                    * ((Math.pow(OVT_rail, lambda) - 1) / lambda);
        }
        // Bus OVT (Box-Cox)
        if (OVT_bus > 1) {
            utility += parameters.leedsPT.betaOutofVehicleTimeBus_u_min
                    * ((Math.pow(OVT_bus, lambda) - 1) / lambda);
        }

        return utility;
    }

    @Override
    protected double estimateMonetaryCostUtility(PtVariables variables) {
        double utility = 0.0;
        double cost = variables.cost_MU;
        if (cost > 0) {
            utility += parameters.betaCost_u_MU * Math.log(cost);
        }
        return utility;
    }

    // Income shift specified for bus only
    protected double estimateIncomeUtility(LeedsPtVariables ptVariables,
            LeedsPersonVariables variables) {
        double utility = 0.0;
        if (ptVariables.busTravelTime_min > ptVariables.railTravelTime_min) {
            if (variables.indIncomeSPC > 50000) {
                utility += parameters.leedsPT.betaIncome50k;
            }
        }
        return utility;
    }

    @Override
    public double estimateUtility(Person person, DiscreteModeChoiceTrip trip,
            List<? extends PlanElement> elements) {
        LeedsPersonVariables variables = personPredictor.predictVariables(person, trip, elements);
        LeedsPtVariables ptVariables = ptPredictor.predictVariables(person, trip, elements);
        LeedsSpatialVariables spatialVariables =
                spatialPredictor.predictVariables(person, trip, elements);

        double utility = 0.0;

        utility += estimateConstantUtility(ptVariables);
        utility += estimateInVehicleTimeUtility(ptVariables, spatialVariables);  // Pass spatial vars
        utility += estimateOutOfVehicleTimeUtility(ptVariables);
        utility += estimateMonetaryCostUtility(ptVariables);
        utility += estimateIncomeUtility(ptVariables, variables);

        return utility;
    }
}