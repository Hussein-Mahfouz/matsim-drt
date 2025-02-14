package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.estimators.WalkUtilityEstimator;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.WalkPredictor;
import org.eqasim.core.simulation.mode_choice.utilities.variables.WalkVariables;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.PersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
// import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsWalkUtilityEstimator extends WalkUtilityEstimator {
    private final LeedsModeParameters parameters;
    // private final LeedsSpatialPredictor spatialPredictor;
    private final LeedsPersonPredictor predictor;
    private final WalkPredictor walkPredictor;

    @Inject
    public LeedsWalkUtilityEstimator(LeedsModeParameters parameters,
            LeedsSpatialPredictor spatialPredictor, PersonPredictor personPredictor,
            LeedsPersonPredictor predictor, WalkPredictor walkPredictor) {
        super(parameters, walkPredictor);

        this.parameters = parameters;
        // this.spatialPredictor = spatialPredictor;
        this.predictor = predictor;
        this.walkPredictor = walkPredictor;
    }

    @Override
    protected double estimateTravelTimeUtility(WalkVariables variables) {
        double lambda = parameters.leedsWalk.lambdaTravelTime;
        // box-cox transformation
        return parameters.walk.betaTravelTime_u_min
                * ((Math.pow(variables.travelTime_min, lambda) - 1) / lambda);
    }



    protected double estimateStudentUtility(LeedsPersonVariables variables) {
        double utility = 0.0;

        if (variables.isStudent) {
            utility += parameters.leedsWalk.betaStudent;
        }

        return utility;
    }

    protected double estimateAgeUtility(LeedsPersonVariables variables) {
		double utility = 0.0;

		// Example logic based on age
		if (variables.age >= 18 && variables.age <= 29) {
			utility += parameters.leedsWalk.betaAge18to29;
		}

		return utility;
	}

    @Override
    public double estimateUtility(Person person, DiscreteModeChoiceTrip trip,
            List<? extends PlanElement> elements) {
        LeedsPersonVariables variables_person = predictor.predictVariables(person, trip, elements);
        WalkVariables walkVariables = walkPredictor.predictVariables(person, trip, elements);

        double utility = 0.0;

        utility += estimateConstantUtility();
        utility += estimateTravelTimeUtility(walkVariables);
        utility += estimateStudentUtility(variables_person);

        return utility;
    }
}

