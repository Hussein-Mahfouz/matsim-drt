package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.UtilityEstimator;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsTaxiPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.TaxiVariables;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsTaxiUtilityEstimator implements UtilityEstimator {
    private final LeedsModeParameters parameters;
    private final LeedsPersonPredictor personPredictor;
    private final LeedsSpatialPredictor spatialPredictor;
    private final LeedsTaxiPredictor taxiPredictor;

    @Inject
    public LeedsTaxiUtilityEstimator(LeedsModeParameters parameters,
            LeedsPersonPredictor personPredictor, LeedsSpatialPredictor spatialPredictor,
            LeedsTaxiPredictor taxiPredictor) {
        this.parameters = parameters;
        this.personPredictor = personPredictor;
        this.spatialPredictor = spatialPredictor;
        this.taxiPredictor = taxiPredictor;
    }

    protected double estimateConstantUtility() {
        return parameters.leedsTaxi.alpha_u;
    }

    /**
     * Calculates travel time utility with combined peak shifts.
     * 
     * Formula: (β_time + β_AM*1(AM) + β_PM*1(PM)) * f(TT, λ)
     */
    protected double estimateTravelTimeUtility(TaxiVariables taxiVariables,
            LeedsSpatialVariables spatialVariables) {
        
        double lambda = parameters.leedsTaxi.lambdaTravelTime;
		//Sum all components (IVT + Access/Egress + Wait) for Total Travel Time
        double travelTime = taxiVariables.travelTime_min 
                          + taxiVariables.accessEgressTime_min 
                          + taxiVariables.waitingTime_min;
        
        // Start with base travel time coefficient
        double combinedCoeff = parameters.leedsTaxi.betaTravelTime_u_min;
        
        // Add AM peak shift if applicable
        if (spatialVariables.isAMPeak) {
            combinedCoeff += parameters.leedsTaxi.betaAmPeak;
        }
        
        // Add PM peak shift if applicable
        if (spatialVariables.isPMPeak) {
            combinedCoeff += parameters.leedsTaxi.betaPmPeak;
        }
        
        // Apply Box-Cox transformation with combined coefficient
        return combinedCoeff * ((Math.pow(travelTime, lambda) - 1) / lambda);
    }

    protected double estimateMonetaryCostUtility(TaxiVariables variables) {
        double utility = 0.0;
        double cost = variables.cost_MU;
        if (cost > 0) {
            utility += parameters.betaCost_u_MU * Math.log(cost);
        }
        return utility;
    }

    protected double estimateGenderUtility(LeedsPersonVariables variables) {
        double utility = 0.0;
        if (variables.isMale) {
            utility += parameters.leedsTaxi.betaMale;
        }
        return utility;
    }

    protected double estimateAgeUtility(LeedsPersonVariables variables) {
        double utility = 0.0;
        if (variables.age >= 18 && variables.age <= 24) {
            utility += parameters.leedsTaxi.betaAge18to24;
        } else if (variables.age >= 25 && variables.age <= 29) {
            utility += parameters.leedsTaxi.betaAge25to29;
        }
        return utility;
    }

    protected double estimateIncomeUtility(LeedsPersonVariables variables) {
        double utility = 0.0;
        if (variables.indIncomeSPC >= 40000.0 && variables.indIncomeSPC <= 50000.0) {
            utility += parameters.leedsTaxi.betaIncome40kto50k;
        }
        return utility;
    }

    @Override
    public double estimateUtility(Person person, DiscreteModeChoiceTrip trip,
            List<? extends PlanElement> elements) {
        LeedsPersonVariables personVariables =
                personPredictor.predictVariables(person, trip, elements);
        LeedsSpatialVariables spatialVariables =
                spatialPredictor.predictVariables(person, trip, elements);
        TaxiVariables taxiVariables = taxiPredictor.predictVariables(person, trip, elements);

        double utility = 0.0;

        utility += estimateConstantUtility();
        utility += estimateTravelTimeUtility(taxiVariables, spatialVariables);
        utility += estimateMonetaryCostUtility(taxiVariables);
        utility += estimateGenderUtility(personVariables);
        utility += estimateAgeUtility(personVariables);
        utility += estimateIncomeUtility(personVariables);

        return utility;
    }
}