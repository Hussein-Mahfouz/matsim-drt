package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.estimators.CarUtilityEstimator;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.CarPredictor;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.PersonPredictor;
import org.eqasim.core.simulation.mode_choice.utilities.variables.CarVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsCarUtilityEstimator extends CarUtilityEstimator {
    private final LeedsModeParameters parameters;
    private final LeedsSpatialPredictor spatialPredictor;
    private final CarPredictor carPredictor;

    @Inject
    public LeedsCarUtilityEstimator(LeedsModeParameters parameters,
            LeedsSpatialPredictor spatialPredictor, PersonPredictor personPredictor,
            CarPredictor carPredictor) {
        super(parameters, carPredictor);

        this.parameters = parameters;
        this.spatialPredictor = spatialPredictor;
        this.carPredictor = carPredictor;
    }

    /**
     * Calculates travel time utility with combined beta shifts.
     * 
     * Formula: (β_time + β_commute*1(Commute) + β_AM*1(AM) + β_PM*1(PM)) * f(TT, λ)
     */
    protected double estimateTravelTimeUtility(CarVariables carVariables, 
            LeedsSpatialVariables spatialVariables) {
        
        double lambda = parameters.leedsCar.lambdaTravelTime;
        double travelTime = carVariables.travelTime_min;
        
        // Start with base travel time coefficient
        double combinedCoeff = parameters.car.betaTravelTime_u_min;
        
        // Add commuting shift if applicable
        if (spatialVariables.isCommuting) {
            combinedCoeff += parameters.leedsCar.betaCommuting;
        }
        
        // Add AM peak shift if applicable
        if (spatialVariables.isAMPeak) {
            combinedCoeff += parameters.leedsCar.betaAmPeak;
        }
        
        // Add PM peak shift if applicable
        if (spatialVariables.isPMPeak) {
            combinedCoeff += parameters.leedsCar.betaPmPeak;
        }
        
        // Apply Box-Cox transformation with combined coefficient
        return combinedCoeff * ((Math.pow(travelTime, lambda) - 1) / lambda);
    }

    @Override
    protected double estimateMonetaryCostUtility(CarVariables variables) {
        double utility = 0.0;
        double cost = variables.cost_MU;
        // Use log transformation, avoiding log(0)
        if (cost > 0) {
            utility += parameters.betaCost_u_MU * Math.log(cost);
        }
        return utility;
    }

    @Override
    public double estimateUtility(Person person, DiscreteModeChoiceTrip trip,
            List<? extends PlanElement> elements) {
        LeedsSpatialVariables spatialVariables =
                spatialPredictor.predictVariables(person, trip, elements);
        CarVariables carVariables = carPredictor.predictVariables(person, trip, elements);

        double utility = 0.0;

        utility += estimateConstantUtility();
        utility += estimateTravelTimeUtility(carVariables, spatialVariables);
        utility += estimateMonetaryCostUtility(carVariables);

        return utility;
    }
}