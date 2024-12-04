package com.husseinmahfouz.matsim.dmc.mode_choice.utilities.estimators;

import java.util.List;

import org.eqasim.core.simulation.mode_choice.utilities.estimators.BikeUtilityEstimator;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.BikePredictor;
import org.eqasim.core.simulation.mode_choice.utilities.predictors.PersonPredictor;
import org.eqasim.core.simulation.mode_choice.utilities.variables.BikeVariables;
import com.husseinmahfouz.matsim.dmc.mode_choice.parameters.LeedsModeParameters;
// import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsSpatialPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.predictors.LeedsPersonPredictor;
import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsPersonVariables;
// import com.husseinmahfouz.matsim.dmc.mode_choice.utilities.variables.LeedsSpatialVariables;
import org.matsim.api.core.v01.population.Person;
import org.matsim.api.core.v01.population.PlanElement;
import org.matsim.contribs.discrete_mode_choice.model.DiscreteModeChoiceTrip;

import com.google.inject.Inject;

public class LeedsBikeUtilityEstimator extends BikeUtilityEstimator {
	private final LeedsModeParameters parameters;
	// private final LeedsSpatialPredictor spatialPredictor;
	private final LeedsPersonPredictor predictor;
	private final BikePredictor bikePredictor;

	@Inject
	public LeedsBikeUtilityEstimator(LeedsModeParameters parameters,
			// LeedsSpatialPredictor spatialPredictor,
			PersonPredictor personPredictor, LeedsPersonPredictor predictor,
			BikePredictor bikePredictor) {
		super(parameters, personPredictor, bikePredictor);

		this.parameters = parameters;
		// this.spatialPredictor = spatialPredictor;
		this.predictor = predictor;
		this.bikePredictor = bikePredictor;
	}

	// protected double estimateUrbanUtility(LeedsSpatialVariables variables) {
	// 	double utility = 0.0;

	// 	if (variables.hasUrbanOrigin && variables.hasUrbanDestination) {
	// 		utility += parameters.leedsBike.betaInsideUrbanArea;
	// 	}

	// 	return utility;
	// }

	@Override
	protected double estimateTravelTimeUtility(BikeVariables variables) {
		double lambda = parameters.leedsBike.lambdaTravelTime;
		// box-cox transformation
		return parameters.bike.betaTravelTime_u_min
				* ((Math.pow(variables.travelTime_min, lambda) - 1) / lambda);
	}

	protected double estimateGenderUtility(LeedsPersonVariables variables) {
		double utility = 0.0;

		if (variables.isMale) {
			utility += parameters.leedsBike.betaMale;
		}

		return utility;
	}

	protected double estimateStudentUtility(LeedsPersonVariables variables) {
		double utility = 0.0;

		if (variables.isStudent) {
			utility += parameters.leedsBike.betaStudent;
		}

		return utility;
	}

	@Override
	public double estimateUtility(Person person, DiscreteModeChoiceTrip trip,
			List<? extends PlanElement> elements) {
		// LeedsSpatialVariables variables = spatialPredictor.predictVariables(person, trip,
		// elements);
		LeedsPersonVariables variables_person = predictor.predictVariables(person, trip, elements);
		BikeVariables bikeVariables = bikePredictor.predictVariables(person, trip, elements);

		double utility = 0.0;

		utility += estimateConstantUtility();
		// utility += estimateUrbanUtility(variables);
		utility += estimateTravelTimeUtility(bikeVariables);
		utility += estimateGenderUtility(variables_person);
		utility += estimateStudentUtility(variables_person);

		return utility;
	}
}
